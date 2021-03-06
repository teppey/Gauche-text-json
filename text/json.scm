;;;
;;; text.json - read/write JSON
;;;
;;;  Copyright (c) 2011-2013  Teppei Hamada  <temada@gmail.com>
;;;
;;;  Redistribution and use in source and binary forms, with or without
;;;  modification, are permitted provided that the following conditions
;;;  are met:
;;;
;;;  1. Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;  2. Redistributions in binary form must reproduce the above copyright
;;;     notice, this list of conditions and the following disclaimer in the
;;;     documentation and/or other materials provided with the distribution.
;;;
;;;  3. Neither the name of the authors nor the names of its contributors
;;;     may be used to endorse or promote products derived from this
;;;     software without specific prior written permission.
;;;
;;;  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

;;; RFC4627  Javascript Object Notation (JSON)
;;; http://www.ietf.org/rfc/rfc4627

(define-module text.json
  (use gauche.dictionary)
  (use gauche.parameter :only (make-parameter parameterize))
  (use gauche.sequence)
  (use text.parse :only (next-token-of peek-next-char))
  (use util.match :only (match match-lambda match-lambda*))
  (export <json-read-error>
          <json-write-error>
          json-mime-type
          json-object-fn
          json-array-fn
          list-as-json-array
          json-indent-string
          json-read
          json-write
          json-write*
          ))
(select-module text.json)


;; ---------------------------------------------------------
;; Conditions
;;
(define-condition-type <json-read-error> <error> #f)
(define-condition-type <json-write-error> <error> #f)


;; ---------------------------------------------------------
;; Assoc list wrapper class
;;
(define-class <alist> (<dictionary> <collection>)
  ([pairs :init-value '() :init-keyword :pairs]))

(define (unwrap obj)
  (if (is-a? obj <alist>)
    (reverse! (~ obj 'pairs))
    obj))

(define (wrap-alist obj)
  (if (list? obj)
    (make <alist> :pairs obj)
    obj))

;; <collection> interface
(define-method call-with-iterator ([object <alist>] proc . _)
  (let* ([pairs (~ object 'pairs)]
         [end? (lambda () (null? pairs))]
         [next (lambda () (begin0 (car pairs) (pop! pairs)))])
    (proc end? next)))

;; <dictionary> interface
(define-method dict-get ((object <alist>) key . default)
  (or (and-let* ([p (assoc key (~ object 'pairs))])
        (cdr p))
      (if (null? default) #f (car default))))

(define-method dict-put! ((object <alist>) key value)
  (push! (~ object 'pairs) (cons key value)))

(define-method dict-exists? ((object <alist>) key)
  (boolean (assoc key (~ object 'pairs))))

;; assoc list allows duplicate keys
(define-method dict-delete! ((object <alist>) key)
  object)

(define-method dict-fold ((object <alist>) proc seed)
  (fold (^(pair seed)
          (proc (car pair) (cdr pair) seed))
        seed (~ object 'pairs)))


;; ---------------------------------------------------------
;; Scanner
;;
(define (make-token type value) (vector type value))
(define (token-type token) (vector-ref token 0))
(define (token-value token) (vector-ref token 1))
(define (token? token) (vector? token))

(define (make-scanner iport)
  (define *token-buffer* #f)
  (define *white-space-chars* #[\x20\x09\x0a\x0d])
  (define *struct-chars-table*
    (hash-table 'eqv?
      '(#\[ . begin-array ) '(#\] . end-array)
      '(#\{ . begin-object) '(#\} . end-object)
      '(#\: . name-separator) '(#\, . value-separator)))

  (define (skip-ws)
    (let loop ([c (peek-char)])
      (when (and (not (eof-object? c))
                 (char-set-contains? *white-space-chars* c))
        (loop (peek-next-char)))))

  (define (scan-string)
    (read-char)  ; skip '"'
    (with-output-to-string
      (lambda ()
        (let loop ([c (read-char)])
          (cond [(eof-object? c)
                 (errorf <json-read-error> "unexpected EOF at ~a" (position))]
                [(eqv? c #\")]
                [(eqv? c #\\)
                 (write-char c)
                 (write-char (read-char))
                 (loop (read-char))]
                [else
                  (write-char c)
                  (loop (read-char))])))))

  (define (scan-number)
    (define (sign)
      (case (peek-char)
        [(#\-) (read-char) -1]
        [(#\+) (read-char)  1]
        [else 1]))
    (define (int)
      (next-token-of char-numeric?))
    (define (frac)
      (if (eqv? (peek-char) #\.)
        (let1 dot (read-char)
          #`",|dot|,(next-token-of char-numeric?)")
        #f))
    (define (exponent)
      (if (char-set-contains? #[eE] (peek-char))
        (let1 e (read-char)
          #`",|e|,(next-token-of (pa$ char-set-contains? #[-+0-9]))")
        #f))
    (let* ([sign (sign)] [int (int)] [frac (frac)] [expo (exponent)])
      (list sign int frac expo)))

  (define (scan-literal)
    (next-token-of char-lower-case?))

  (define (scan)
    (skip-ws)
    (let1 c (peek-char)
      (cond [(eof-object? c)
             (make-token 'eof (read-char))]
            [(eqv? c #\")
             (make-token 'string (scan-string))]
            [(char-set-contains? #[-+0-9] c)
             (make-token 'number (scan-number))]
            [(char-set-contains? #[tfn] c)
             (make-token 'literal (scan-literal))]
            [(hash-table-get *struct-chars-table* c #f)
             => (cut make-token <> (read-char))]
            [else
              (errorf <json-read-error> "unexpected char: ~s at ~a" c (position))])))

  (define (get)
    (if *token-buffer*
      (begin0 *token-buffer*
        (set! *token-buffer* #f))
      (with-input-from-port iport scan)))

  (define (put! token)
    (set! *token-buffer* token))

  (define (position)
    #`",(port-position-prefix iport),(port-tell iport)")

  (match-lambda*
    [('get) (get)]
    [('put! token) (put! token)]
    [('position) (position)]
    [badmsg (error "make-scanner -- unknown message:" badmsg)]))


;; ---------------------------------------------------------
;; Parser
;;
(define *scanner* (make-parameter #f))

(define (%get-token) ((*scanner*) 'get))
(define (%unget-token! token) ((*scanner*) 'put! token))
(define (%position) ((*scanner*) 'position))

(define (%raise-unexpected-token token)
  (errorf <json-read-error> "unexpected token: ~s at ~a"
          (if (token? token) (token-value token) token)
          (%position)))

(define-syntax %assert-token
  (syntax-rules ()
    [(_ expected)
     (%assert-token expected memq)]
    [(_ expected cmpfn)
     (let1 token (%get-token)
       (if (cmpfn (token-type token) expected)
         token
         (%raise-unexpected-token token)))]))

(define-syntax %assert-char
  (syntax-rules ()
    [(_ expect-char-set)
     (%assert-char expect-char-set "unexpected character")]
    [(_ expect-char-set msg)
     (rlet1 c (read-char)
       (unless (and (char? c) (char-set-contains? expect-char-set c))
           (errorf <json-read-error> "~a: ~s at ~a" msg c (%position))))]))


;; parse entry point
(define (parse-json iport)
  (parameterize ([*scanner* (make-scanner iport)])
    (let1 token (%assert-token '(begin-object begin-array))
      (%unget-token! token)
      (rlet1 json (parse-any)
        (%assert-token '(eof))))))

(define (parse-any)
  (let1 token (%get-token)
    (case (token-type token)
      [(begin-object)
       (let1 dict (if-let1 thunk (json-object-fn)
                    (thunk)
                    (make <alist>))
         (parse-object dict))]
      [(begin-array)
       (receive (class size)
         (if-let1 thunk (json-array-fn)
           (thunk)
           (values <vector> #f))
         (parse-array class size))]
      [(string)  (parse-string token)]
      [(number)  (parse-number token)]
      [(literal) (parse-literal token)]
      [else      (%raise-unexpected-token token)])))

(define (parse-object dict)
  (let loop ([token (%assert-token '(string end-object))])
    (if (eq? (token-type token) 'end-object)
      (unwrap dict)
      (let1 key (parse-string token)
        (%assert-token '(name-separator))
        (let1 value (parse-any)
          (dict-put! dict key value)
          (let1 token (%assert-token '(value-separator end-object))
            (case (token-type token)
              [(end-object)
               (unwrap dict)]
              [(value-separator)
               (loop (%assert-token '(string)))])))))))

(define (parse-array class size)
  (with-builder (class add! get :size size)
    (let loop ([token (%get-token)])
      (if (eq? (token-type token) 'end-array)
        (get)
        (begin
          (%unget-token! token)
          (add! (parse-any))
          (let1 token (%assert-token '(value-separator end-array))
            (if (eq? (token-type token) 'value-separator)
              (loop (%assert-token '(string number literal begin-object begin-array)))
              (loop token))))))))

(define (parse-string token)
  (define *unescaped* #[\u0020\u0021\u0023-\u005b\u005d-\U0010ffff])
  (define (parse-escaped-string)
    (case (read-char)
      [(#\u) (write-char (parse-unicode-char))]
      [(#\b) (write-char #\x08)]
      [(#\f) (write-char #\page)]
      [(#\n) (write-char #\newline)]
      [(#\r) (write-char #\return)]
      [(#\t) (write-char #\tab)]
      [(#\" #\\ #\/) => write-char]
      [else (errorf <json-read-error>
                    "invalid control character at ~a" (%position))]))
  (define (parse-unicode-char)
    (letrec-syntax
      ([escaped->number
         (syntax-rules ()
           [(_) (let* ([msg "invalide unicode escape sequence"]
                       [c1 (%assert-char #[0-9a-fA-F] msg)]
                       [c2 (%assert-char #[0-9a-fA-F] msg)]
                       [c3 (%assert-char #[0-9a-fA-F] msg)]
                       [c4 (%assert-char #[0-9a-fA-F] msg)])
                  (string->number (string c1 c2 c3 c4) 16))])]
       [surrogate-pair
         (syntax-rules ()
           [(_ hi)
            (begin
              (%assert-char #[\\])
              (%assert-char #[u])
              (let1 low (escaped->number)
                (ucs->char
                  (+ #x10000 (ash (logand hi #x03FF) 10) (logand low #x03FF)))))])])
      (let1 n (escaped->number)
        (if (<= #xD800 n #xDBFF)
          (surrogate-pair n)
          (ucs->char n)))))

  (with-string-io (token-value token)
    (lambda ()
      (let loop ([c (read-char)])
        (cond [(eof-object? c)]
              [(char-set-contains? *unescaped* c)
               (write-char c)
               (loop (read-char))]
              [(eqv? c #\\)
               (parse-escaped-string)
               (loop (read-char))]
              [else (errorf <json-read-error>
                      "unexpected character: ~s at ~a" c (%position))])))))

(define (parse-literal token)
  (case (string->symbol (token-value token))
    [(true)  #t]
    [(false) #f]
    [(null)  'null]
    [else => (cut errorf <json-read-error>
                  "unexpected literal: ~s at ~a" <> (%position))]))

(define (parse-number token)
  (define (exponent sign int frac expo)
    (rxmatch-if (#/[eE]([-+])?(\d+)/ expo)
      (#f exp-sign factor)
      (let1 exp-sign (or (and exp-sign (string=? exp-sign "-") -1) 1)
        (* sign (* (exact->inexact (+ int frac))
                   (expt 10 (* exp-sign (string->number factor))))))
      (%raise-unexpected-token token)))
  (match (token-value token)
    [(_ (? #/^0\d+/ int) _ _)
     (errorf <json-read-error> "leading zero: ~s at ~a" int (%position))]
    [(sign int #f #f)
     (* sign (string->number int))]
    [(sign int frac #f)
     (* sign (+ (string->number int) (string->number #`"0,frac")))]
    [(sign int #f expo)
     (exponent sign (string->number int) 0 expo)]
    [(sign int frac expo)
     (exponent sign (string->number int) (string->number #`"0,frac") expo)]))


;; ---------------------------------------------------------
;; Writer
;;
(define %json-current-indent (make-parameter #f))

(define (make-indent level string)
  (vector level string (with-output-to-string
                         (^() (dotimes (_ level) (display string))))))
(define (indent-level indent) (vector-ref indent 0))
(define (indent-string indent) (vector-ref indent 1))
(define (indent-computed indent) (vector-ref indent 2))

(define-syntax increase-indent
  (syntax-rules ()
    [(_ body ...)
     (parameterize
       ([%json-current-indent
          (and-let* ([i (%json-current-indent)])
            (make-indent (+ (indent-level i) 1) (indent-string i)))])
       body ...)]))

(define-syntax display-if-pretty
  (syntax-rules ()
    [(_ args ...)
     (when (%json-current-indent)
       (for-each display (list args ...)))]))

(define (newline-and-indent)
  (display-if-pretty #\newline (indent-computed (%json-current-indent))))


(define (format-json obj)
  (if (and (is-a? obj <collection>)
           (not (string? obj)))
    (format-any obj)
    (error <json-write-error>
           "expect <collection> instance except <string>, but got"
           obj)))

(define (format-any obj)
  (cond
    [(number? obj)  (format-number obj)]
    [(string? obj)  (format-string obj)]
    [(boolean? obj) (format-literal-boolean obj)]
    [(symbol? obj)  (format-literal-symbol obj)]
    [(or (is-a? obj <dictionary>)
         (and (not (list-as-json-array))
              (list? obj)))
     (format-object (wrap-alist obj))]
    [(is-a? obj <sequence>) (format-array obj)]
    [else (error <json-write-error> "unexpected object" obj)]))

;; from rfc.json
(define (format-number obj)
  (cond [(or (not (real? obj)) (not (finite? obj)))
         (error <json-write-error> "real number expected, but got" obj)]
        [(and (rational? obj) (not (integer? obj)))
         (display (exact->inexact obj))]
        [else (display obj)]))

(define *escape-table*
  (hash-table 'eqv?
    '(#\x08     . "\\b")
    '(#\"       . "\\\"")
    '(#\\       . "\\\\")
    '(#\page    . "\\f")
    '(#\newline . "\\n")
    '(#\return  . "\\r")
    '(#\tab     . "\\t")))

(define (format-string obj)
  (write-char #\")
  (with-input-from-string obj
    (lambda ()
      (until (read-char) eof-object? => c
        (if (char-set-contains? #[\x00-\x7f] c)
          (display (hash-table-get *escape-table* c c))
          (format #t "\\u~4,'0X" (char->ucs c))))))
  (write-char #\"))

(define (format-literal-boolean obj)
  (display (if obj "true" "false")))

(define (format-literal-symbol obj)
  (if-let1 r (memq obj '(null true false))
    (display (symbol->string (car r)))
    (error <json-write-error> "unexpected symbol" obj)))

(define (format-object obj)
  (write-char #\{)
  (increase-indent
    (dict-fold obj
      (^(key value comma)
        (display comma)
        (newline-and-indent)
        (format-string key)
        (display #\:)
        (display-if-pretty #\space)
        (format-any value)
        #\,)
      ""))
  (newline-and-indent)
  (write-char #\}))

(define (format-array obj)
  (write-char #\[)
  (increase-indent
    (fold (^(value comma)
            (display comma)
            (newline-and-indent)
            (format-any value)
            #\,)
          "" obj))
  (newline-and-indent)
  (write-char #\]))


;; ---------------------------------------------------------
;; External API
;;

;; RFC4627, Section 6
(define-constant json-mime-type "application/json")


;; Reader Parameter: json-object-fn
;;
;;   The value of this parameter specifies thunk or #f. Thunk must be
;;   returns instance of <dictionary> subclass. The value was #f, JSON
;;   object to be alist.
(define json-object-fn
  (make-parameter #f (match-lambda
                       [#f #f]
                       [(? procedure? thunk) thunk]
                       [badarg (error "procedure or #f required, but got" badarg)])))


;; Reader Parameter: json-array-fn
;;
;;   Parameter value is thunk or #f. Thunk must be returns two values;
;;   <sequence> subclass and size. These values are used as call-with-
;;   builder arguments.
(define json-array-fn
  (make-parameter #f (match-lambda
                       [#f #f]
                       [(? procedure? thunk) thunk]
                       [badarg (error "procedure or #f required, but got" badarg)])))


;; Writer Parameter: list-as-json-array
;;
;;   If a true value is given, list was formatted to JSON array. Otherwise,
;;   writer regard list as alist, it format to JSON object.
(define list-as-json-array (make-parameter #f))


;; Writer Parameter: json-indent-string
;;
;;   This parameter specifies the character, string, or #f. If a character
;;   or string is given, it is used as indent string at each indentation
;;   level. If parameter values is #f, output was not indented.
(define json-indent-string
  (make-parameter (make-string 2 #\space)
                  (^v (cond [(not v) ""]
                            [(char? v) (make-string 1 v)]
                            [(string? v) v]
                            [else (error "required char/string/#f, but got" v)]))))


(define (json-read :optional (input (current-input-port)))
  (cond [(string? input)
         (call-with-input-string input parse-json)]
        [(input-port? input)
         (parse-json input)]
        [else
          (error "input port or string required, but got" input)]))

(define (json-write obj :optional (output (current-output-port)))
  (cond [(not output)
         (with-output-to-string (pa$ format-json obj))]
        [(output-port? output)
         (with-output-to-port output (pa$ format-json obj))]
        [else
          (error "output port required, but got" output)]))

;; Same as json-write except that output was indented for easy to see.
(define (json-write* obj :optional (output (current-output-port)))
  (parameterize ([%json-current-indent (make-indent 0 (json-indent-string))])
    (json-write obj output)))


(provide "text/json")
