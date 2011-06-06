;;;
;;; text.json - read/write json
;;;

;;; RFC4627  Javascript Object Notation (JSON)
;;; http://www.ietf.org/rfc/rfc4627

(define-module text.json
  (use gauche.dictionary)
  (use gauche.parameter :only (make-parameter parameterize))
  (use gauche.sequence)
  (use srfi-13 :only (string-null? string-for-each))
  (use text.parse :only (skip-while assert-curr-char next-token-of))
  (use util.match :only (match match-lambda))
  (export json-object
          json-array
          json-object-from-alist?
          json-mime-type
          json-read
          json-write
          json-write*
          json-indent-width
          ))
(select-module text.json)

;; ---------------------------------------------------------
;; Reader Parameters
;;
;;  json-object
;;    Thunk must be returns instance of <dictionary>
;;    subclass.  When this parameter value is #f, JSON
;;    object to be alist.
(define json-object
  (make-parameter #f
    (match-lambda
      [#f #f]
      [(? procedure? thunk) thunk]
      [badarg (error "procedure or #f required, but got" badarg)])))

;;  json-array
;;    Thunk must be returns two values, <sequence> subclass
;;    and size for call-with-builder.  Size may be a #f.
(define json-array
  (make-parameter #f
    (match-lambda
      [#f #f]
      [(? procedure? thunk) thunk]
      [badarg (error "procedure or #f required, but got" badarg)])))

;; ---------------------------------------------------------
;; Writer Parameters
;;
(define json-object-from-alist? (make-parameter #t))
(define %json-pretty-print? (make-parameter #f))
(define %json-indent-level  (make-parameter 0))
(define json-indent-width
  (make-parameter 2
    (^n (or (and (integer? n) (>= n 0) (x->integer n))
            (error "required positive integer or zero, but got" n)))))


;; ---------------------------------------------------------
;; Alist Wrapper Class
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
  (if-let1 p (assoc key (~ object 'pairs))
    (cdr p)
    (if (null? default) #f (car default))))

(define-method dict-put! ((object <alist>) key value)
  (dict-delete! object key)
  (set! (~ object 'pairs) (acons key value (~ object 'pairs))))

(define-method dict-exists? ((object <alist>) key)
  (boolean (assoc key (~ object 'pairs))))

(define-method dict-delete! ((object <alist>) key)
  (set! (~ object 'pairs) (remove (^p (equal? (car p) key)) (~ object 'pairs))))

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
(define (unget-token! scanner token) (scanner token))

(define (make-scanner iport)
  (define *token-buffer* #f)
  (define *white-space-chars* #[\x20\x09\x0a\x0d])
  (define *struct-chars-table*
    (hash-table 'eqv?
      '(#\[ . begin-array )
      '(#\] . end-array)
      '(#\{ . begin-object)
      '(#\} . end-object)
      '(#\: . name-separator)
      '(#\, . value-separator)))
  (define (struct-char? c)
    (hash-table-get *struct-chars-table* c #f))
  (define (scan)
    (skip-while *white-space-chars*)
    (let1 c (peek-char)
      (cond [(eof-object? c)
             (make-token 'eof (read-char))]
            [(char=? c #\")
             (make-token 'string (scan-string))]
            [(char-set-contains? #[-+0-9] c)
             (make-token 'number (scan-number))]
            [(char-set-contains? #[tfn] c)
             (make-token 'literal (scan-literal))]
            [(struct-char? c)
             => (cut make-token <> (read-char))]
            [else
              (error "invalid JSON form")])))

  (case-lambda
    [() (if *token-buffer*
          (begin0 *token-buffer*
            (set! *token-buffer* #f))
          (with-input-from-port iport scan))]
    [(token) (set! *token-buffer* token)]))

(define (scan-string)
  (read-char)  ; skip '"'
  (with-output-to-string
    (lambda ()
      (let loop ([c (read-char)])
        (cond [(eof-object? c)
               (error "unexpected EOF")]
              [(char=? c #\")]
              [(char=? c #\\)
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
    (if (char=? (peek-char) #\.)
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


;; ---------------------------------------------------------
;; Parser
;;
(define (%assert-token scanner expected :optional (cmpfn memq))
  (let1 token (scanner)
    (if (cmpfn (token-type token) expected)
      token
      (error "unexpected token:" (token-value token)))))

(define (parse-json scanner)
  (let1 token (%assert-token scanner '(begin-object begin-array))
    (unget-token! scanner token)
    (rlet1 json (parse-any scanner)
      (%assert-token scanner '(eof)))))

(define (parse-any scanner)
  (let1 token (scanner)
    (case (token-type token)
      [(begin-object)
       (let1 dict (if-let1 thunk (json-object)
                    (thunk)
                    (make <alist>))
         (parse-object dict scanner))]
      [(begin-array)
       (receive (class size)
         (if-let1 thunk (json-array)
           (thunk)
           (values <vector> #f))
         (parse-array class size scanner))]
      [(string)  (parse-string (token-value token))]
      [(number)  (parse-number (token-value token))]
      [(literal) (parse-literal (token-value token))]
      [else      (error "unexpected token:" (token-value token))])))

(define (parse-object dict scanner)
  (let/cc break
    (while #t
      (let1 token (%assert-token scanner '(string end-object))
        (if (eq? (token-type token) 'end-object)
          (break (unwrap dict))
          (let1 key (parse-string (token-value token))
            (%assert-token scanner '(name-separator))
            (let1 value (parse-any scanner)
              (dict-put! dict key value)
              (let1 token (%assert-token scanner '(value-separator end-object))
                (case (token-type token)
                  [(end-object) (break (unwrap dict))]
                  [(value-separator)
                   (let1 token (%assert-token scanner '(string))
                     (unget-token! scanner token))])))))))))

(define (parse-array class size scanner)
  (with-builder (class add! get :size size)
    (let loop ([token (scanner)])
      (if (eq? (token-type token) 'end-array)
        (get)
        (begin
          (unget-token! scanner token)
          (add! (parse-any scanner))
          (let1 token (%assert-token scanner '(value-separator end-array))
            (if (eq? (token-type token) 'value-separator)
              (loop (%assert-token scanner
                                   '(string number literal begin-object begin-array)))
              (loop token))))))))

(define (parse-string value)
  (with-string-io value
    (lambda ()
      (until (read-char) eof-object? => c
        (if (not (char=? c #\\))
          (display c)
          (let1 cc (read-char)
            (cond [(char=? cc #\u) (display (parse-unicode-char))]
                  [(char=? cc #\b) (display #\x08)]
                  [(char=? cc #\f) (display #\page)]
                  [(char=? cc #\n) (display #\newline)]
                  [(char=? cc #\r) (display #\return)]
                  [(char=? cc #\t) (display #\tab)]
                  [else (display cc)])))))))

(define (parse-unicode-char)
  (define (escaped->number)
    (let1 hex4 (with-output-to-string
                (lambda ()
                  (dotimes (_ 4)
                    (display (assert-curr-char #[0-9a-fA-F] "unexpected char")))))
      (string->number hex4 16)))
  (define (surrogate-pair hi)
    (assert-curr-char #\\ "unexpected char")
    (assert-curr-char #\u "unexpected char")
    (let1 low (escaped->number)
      (ucs->char (+ #x10000 (ash (logand hi #x03FF) 10) (logand low #x03FF)))))
  (let1 n (escaped->number)
    (if (<= #xD800 n #xDBFF)
      (surrogate-pair n)
      (ucs->char n))))

(define (parse-literal value)
  (case (string->symbol value)
    [(true)  #t]
    [(false) #f]
    [(null)  'null]
    [else (error "unexpected literal" value)]))

(define (parse-number parts)
  (define (exponent sign int frac expo)
    (rxmatch-let (#/[eE]([-+])?(\d+)/ expo)
      (#f exp-sign factor)
      (let1 exp-sign (or (and exp-sign (string=? exp-sign "-") -1) 1)
        (* sign (* (exact->inexact (+ int frac))
                   (expt 10 (* exp-sign (string->number factor))))))))
  (match parts
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
(define-syntax display-if-pretty
  (syntax-rules ()
    [(_ x) (when (%json-pretty-print?) (display x))]))

(define-syntax newline-and-indent
  (syntax-rules ()
    [(_) (begin
           (display-if-pretty #\newline)
           (display-if-pretty
             (make-string
               (* (%json-indent-level) (json-indent-width)) #\space)))]))

(define-syntax with-indent
  (syntax-rules ()
    [(_ body ...)
     (parameterize ([%json-indent-level (+ 1 (%json-indent-level))])
       body ...)]))

(define (format-json obj)
  (if (and (is-a? obj <collection>)
           (not (string? obj)))
    (format-any obj)
    (error "expect insntance of <collection> except <string>, bug got" obj)))

(define (format-any obj)
  (cond
    [(number? obj)  (format-number obj)]
    [(string? obj)  (format-string obj)]
    [(boolean? obj) (format-literal-boolean obj)]
    [(symbol? obj)  (format-literal-symbol obj)]
    [(or (is-a? obj <dictionary>)
         (and (json-object-from-alist?)
              (list? obj)))
     (format-object (wrap-alist obj))]
    [(is-a? obj <sequence>) (format-array obj)]
    [else (error "unexpected object" obj)]))

;; from rfc.json
(define (format-number obj)
  (cond [(not (real? obj))
         (error "real number expected, but got" obj)]
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
  (display "\"")
  (string-for-each
    (^c (if (char-set-contains? #[\x00-\x7f] c)
          (display (hash-table-get *escape-table* c c))
          (format #t "\\u~4,'0X" (char->ucs c))))
    obj)
  (display "\""))

(define (format-literal-boolean obj)
  (display (if obj "true" "false")))

(define (format-literal-symbol obj)
  (if-let1 r (memq obj '(null true false))
    (display (symbol->string (car r)))
    (error "unexpected symbol" obj)))

(define-method format-object ([obj <dictionary>])
  (display #\{)
  (with-indent
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
  (display #\}))

(define-method format-array ([obj <sequence>])
  (display #\[)
  (with-indent
    (fold (^(value comma)
            (display comma)
            (newline-and-indent)
            (format-any value)
            #\,)
          "" obj))
  (newline-and-indent)
  (display #\]))


;; ---------------------------------------------------------
;; External API
;;
(define-constant json-mime-type "application/json")

(define (json-read :optional (input (current-input-port)))
  (cond [(string? input)
         (call-with-input-string input (.$ parse-json make-scanner))]
        [(input-port? input)
         (parse-json (make-scanner input))]
        [else
          (error "input port or string required, but got" input)]))

(define (json-write obj :optional (output (current-output-port)))
  (cond [(not output)
         (with-output-to-string (pa$ format-json obj))]
        [(output-port? output)
         (with-output-to-port output (pa$ format-json obj))]
        [else
          (error "output port required, but got" output)]))

(define (json-write* obj :optional (output (current-output-port)))
  (parameterize ([%json-pretty-print? #t])
    (json-write obj output)))

(provide "text/json")
