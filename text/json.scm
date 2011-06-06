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
  (use text.parse :only (skip-while))
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
(define (unget-token! scanner token)
  (scanner token))

(define (make-scanner iport)
  (define *white-space-chars* #[\x20\x09\x0a\x0d])
  (define *struct-chars-table*
    (hash-table 'eqv?
      '(#\[ . begin-array )
      '(#\] . end-array)
      '(#\{ . begin-object)
      '(#\} . end-object)
      '(#\: . name-separator)
      '(#\, . value-separator)))
  (define (scan)
    (define (struct-char? c)
      (hash-table-get *struct-chars-table* c #f))
    (skip-while *white-space-chars*)
    (let1 c (peek-char)
      (cond [(eof-object? c)
             (values (read-char) #f)]
            [(char=? c #\")
             (get-string-token)]
            [(char-set-contains? #[-+0-9] c)
             (get-number-token)]
            [(char-set-contains? #[tfn] c)
             (get-literal-token)]
            [(struct-char? c)
             => (^(type)
                  (read-char)
                  (values type #f))]
            [else
              (error "invalid JSON form")])))
  (define *token-buffer* #f)
  (case-lambda
    [() (if *token-buffer*
          (begin0 *token-buffer*
            (set! *token-buffer* #f))
          (receive (type value)
            (with-input-from-port iport scan)
            (make-token type value)))]
    [(token) (set! *token-buffer* token)]))

(define (get-string-token)
  (read-char)
  (let loop ([c (read-char)]
             [out (open-output-string)])
    (cond [(eof-object? c)
           (error "unexpected EOF at" (get-output-string out))]
          [(char=? c #\")
           (values 'string (get-output-string out))]
          [(char=? c #\\)
           (write-char c out)
           (write-char (read-char) out)
           (loop (read-char) out)]
          [else
            (write-char c out)
            (loop (read-char) out)])))

(define (get-number-token)
  (let ([sign (open-output-string)]
        [int  (open-output-string)]
        [frac (open-output-string)]
        [expo (open-output-string)])
    ;; sign
    (when (char-set-contains? #[-+] (peek-char))
      (write-char (read-char) sign))
    ;; integer
    (let loop ([c (peek-char)])
      (when (char-set-contains? #[0-9] c)
        (write-char (read-char) int)
        (loop (peek-char))))
    ;; fraction
    (when (char=? (peek-char) #\.)
      (write-char (read-char) frac)
      (let loop ([c (peek-char)])
        (when (char-set-contains? #[0-9] c)
          (write-char (read-char) frac)
          (loop (peek-char)))))
    ;; exponent
    (when (char-set-contains? #[eE] (peek-char))
      (write-char (read-char) expo)
      (let loop ([c (peek-char)])
        (when (char-set-contains? #[-+0-9] c)
          (write-char (read-char) expo)
          (loop (peek-char)))))

    (values 'number (map (^(out)
                           (let1 s (get-output-string out)
                             (if (string-null? s) #f s)))
                         (list sign int frac expo)))))

(define (get-literal-token)
  (let loop ([c (peek-char)]
             [out (open-output-string)])
    (if (and (char? c) (char-set-contains? #[a-z] c))
      (begin (write-char (read-char) out)
             (loop (peek-char) out))
      (values 'literal (get-output-string out)))))


;; ---------------------------------------------------------
;; Parser
;;
(define (parse-json input)
  (let* ([scanner (make-scanner input)]
         [token (scanner)])
    (if (memq (token-type token) '(begin-object begin-array))
      (begin (unget-token! scanner token)
             (parse-any scanner))
      (error "Invalid JSON syntax"))))

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
      [else      (error "invalid JSON syntax")])))

(define (parse-object dict scanner)
  (let loop ([token (scanner)])
    (case (token-type token)
      [(end-object) (unwrap dict)]
      [(value-separator) (loop (scanner))]
      [(string)
       (let ([sep (scanner)])
         (unless (eq? (token-type sep) 'name-separator)
           (error "invalid JSON object syntax"))
         (let ([key (parse-string (token-value token))])
           (dict-put! dict key (parse-any scanner))
           (loop (scanner))))]
      [else (error "invalid JSON object syntax" token)]
      )))

(define (parse-array class size scanner)
  (with-builder (class add! get :size size)
    (let loop ([token (scanner)])
      (case (token-type token)
        [(end-array) (get)]
        [(value-separator) (loop (scanner))]
        [else (unget-token! scanner token)
              (add! (parse-any scanner))
              (loop (scanner))]))))

(define (parse-string value)
  (with-string-io value
    (lambda ()
      (let loop ([c (read-char)] [chars '()])
        (cond
          [(eof-object? c)
           (display (list->string (reverse! chars)))]
          [(char=? c #\\)
           (let ([cc (read-char)])
             (cond
               [(char=? cc #\u)
                (let1 uc (parse-unicode-char)
                  (loop (read-char) (cons uc chars)))]
               [(char=? cc #\") (loop (read-char) (cons cc chars))]
               [(char=? cc #\\) (loop (read-char) (cons cc chars))]
               [(char=? cc #\/) (loop (read-char) (cons cc chars))]
               [(char=? cc #\b) (loop (read-char) (cons #\x08 chars))]
               [(char=? cc #\f) (loop (read-char) (cons #\page chars))]
               [(char=? cc #\n)
                (loop (read-char) (cons #\newline chars))]
               [(char=? cc #\r)
                (loop (read-char) (cons #\return chars))]
               [(char=? cc #\t) (loop (read-char) (cons #\tab chars))]
               [else (loop (read-char) (cons cc chars))]))]
          [else (loop (read-char) (cons c chars))])))))

(define (parse-unicode-char)
  (let ([chars '()])
    (dotimes (_ 4)
      (push! chars (read-char)))
    (let ([n (string->number (list->string (reverse! chars)) 16)])
      (cond [(not n)
             (error "invalid unicode form")]
            [(<= #xD800 n #xDBFF)
             (surrogate-pair n)]
            [else
              (ucs->char n)]))))

(define (surrogate-pair hi)
  (read-char)  ; skip `\'
  (read-char)  ; skip `u'
  (let1 chars '()
    (dotimes (_ 4)
      (push! chars (read-char)))
    (let ([low (string->number (list->string (reverse! chars)) 16)])
      (unless (or low (<= #xDC00 low #xDFFF))
        (error "invalide unicode surrogate pair"))
      (ucs->char (+ #x10000 (ash (logand hi #x03FF) 10) (logand low #x03FF))))))

(define (parse-literal value)
  (case (string->symbol value)
    [(true)  #t]
    [(false) #f]
    [(null)  'null]
    [else (error "unexpected literal" value)]))

(define (parse-number parts)
  (define (sign->number s)
    (or (and s (string=? s "-") -1) 1))
  (define (exponent sign int frac expo)
    (rxmatch-let (#/[eE]([-+])?(\d+)/ expo)
      (#f exp-sign factor)
      (* sign (* (exact->inexact (+ int frac))
                 (expt 10 (* (sign->number exp-sign)
                             (string->number factor)))))))
  (let1 sign (sign->number (car parts))
    (match (cdr parts)
      [(int #f #f)
       (* sign (string->number int))]
      [(int frac #f)
       (* sign (+ (string->number int) (string->number #`"0,frac")))]
      [(int #f expo)
       (exponent sign (string->number int) 0 expo)]
      [(int frac expo)
       (exponent sign (string->number int) (string->number #`"0,frac") expo)]
      )))


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

(define (json-write* obj :optional (output (current-output-port)))
  (parameterize ([%json-pretty-print? #t])
    (json-write obj output)))

(provide "text/json")
