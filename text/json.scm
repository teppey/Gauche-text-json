(define-module text.json
  (use srfi-13)
  (use srfi-43)
  (use text.parse)
  (use util.match)
  (export json-mime-type
          json-read
          json-read-from-string
          json-write
          json-write-to-string))

(select-module text.json)

;; ---------------------------------------------------------
;; Scanner
;;
(define *white-space-chars* #[\x20\x09\x0a\x0d])
(define *struct-chars-table*
  '((#\[ . begin-array )
    (#\] . end-array)
    (#\{ . begin-object)
    (#\} . end-object)
    (#\: . name-separator)
    (#\, . value-separator)))

(define *token-buffer* #f)
(define (unget-token token) (set! *token-buffer* token))
(define (token-type token)  (car token))
(define (token-value token) (cdr token))

(define (make-scanner iport)
  (lambda ()
    (if *token-buffer*
      (begin0 *token-buffer*
        (set! *token-buffer* #f))
      (receive (type value)
        (with-input-from-port iport scan)
        (cons type value)))))

(define (scan)
  (define (struct-char? c)
    (assv c *struct-chars-table*))

  (skip-while *white-space-chars*)
  (let1 c (peek-char)
    (cond ((eof-object? c)
           (values (read-char) #f))
          ((char=? c #\")
           (get-string-token))
          ((char-set-contains? #[-+0-9] c)
           (get-number-token))
          ((char-set-contains? #[tfn] c)
           (get-symbol-token))
          ((struct-char? c)
           => (lambda (p)
                (read-char)
                (values (cdr p) #f)))
          (else
            (error "invalid json form")))))

(define (get-string-token)
  (read-char)
  (let loop ((c (read-char))
             (out (open-output-string)))
    (cond ((char=? c #\")
           (values 'string (get-output-string out)))
          ((char=? c #\\)
           (write-char c out)
           (write-char (read-char) out)
           (loop (read-char) out))
          (else
            (write-char c out)
            (loop (read-char) out)))))

(define (get-number-token)
  (let ((sign (open-output-string))
        (int  (open-output-string))
        (frac (open-output-string))
        (expo (open-output-string)))
    ;; sign
    (when (char-set-contains? #[-+] (peek-char))
      (write-char (read-char) sign))
    ;; integer
    (let loop ((c (peek-char)))
      (when (char-set-contains? #[0-9] c)
        (write-char (read-char) int)
        (loop (peek-char))))
    ;; fraction
    (when (char=? (peek-char) #\.)
      (write-char (read-char) frac)
      (let loop ((c (peek-char)))
        (when (char-set-contains? #[0-9] c)
          (write-char (read-char) frac)
          (loop (peek-char)))))
    ;; exponent
    (when (char-set-contains? #[eE] (peek-char))
      (write-char (read-char) expo)
      (let loop ((c (peek-char)))
        (when (char-set-contains? #[-+0-9] c)
          (write-char (read-char) expo)
          (loop (peek-char)))))

    (values 'number (map (lambda (out)
                           (let1 s (get-output-string out)
                             (if (string-null? s) #f s)))
                         (list sign int frac expo)))))

(define (get-symbol-token)
  (let loop ((c (peek-char))
             (out (open-output-string)))
    (if (and (char? c) (char-set-contains? #[a-z] c))
      (begin (write-char (read-char) out)
             (loop (peek-char) out))
      (values 'symbol (get-output-string out)))))


;; ---------------------------------------------------------
;; Parser
(define (parse-json input)
  (let* ((scanner (make-scanner input))
         (token (scanner)))
    (case (token-type token)
      ((begin-object) (parse-object '() scanner values))
      ((begin-array)  (parse-array '() scanner values))
      (else           (error "json must be object or array")))))

(define (parse-any scanner cont)
  (let1 token (scanner)
    (case (token-type token)
      ((begin-object) (parse-object '() scanner cont))
      ((begin-array)  (parse-array '() scanner cont))
      ((string)       (cont (parse-string (token-value token))))
      ((number)       (cont (parse-number (token-value token))))
      ((symbol)      (cont (parse-symbol (token-value token))))
      (else           (error "bad json syntax")))))

(define (parse-object entries scanner cont)
  (let1 token (scanner)
    (case (token-type token)
      [(end-object)      (cont (reverse! entries))]
      [(value-separator) (parse-object entries scanner cont)]
      [(string)
       (let ((sep (scanner)))
         (unless (eq? (token-type sep) 'name-separator)
           (error "object key-value pair must separated `:'"))
         (let ((key (parse-string (token-value token))))
           (parse-any scanner
                      (lambda (value)
                        (parse-object (acons key value entries)
                                      scanner cont)))))]
      [else (error "invalid json object syntax" token)]
      )))

(define (parse-array elements scanner cont)
  (let1 token (scanner)
    (case (token-type token)
      [(end-array)       (cont (list->vector (reverse! elements)))]
      [(value-separator) (parse-array elements scanner cont)]
      [else (unget-token token)
            (parse-any scanner
                       (lambda (elt)
                         (parse-array (cons elt elements)
                                      scanner
                                      cont)))]
      )))

(define (parse-string value)
  (with-string-io value
    (lambda ()
      (let loop ((c (read-char))
                 (chars '()))
        (cond ((eof-object? c)
               (display (list->string (reverse! chars))))
              ((char=? c #\\)
               (let ((cc (read-char)))
                 (cond ((char=? cc #\u)
                        (let1 uc (parse-unicode-char)
                          (loop (read-char) (cons uc chars))))
                       ((char=? cc #\") (loop (read-char) (cons cc chars)))
                       ((char=? cc #\\) (loop (read-char) (cons cc chars)))
                       ((char=? cc #\/) (loop (read-char) (cons cc chars)))
                       ((char=? cc #\b) (loop (read-char) (cons #\x08 chars)))
                       ((char=? cc #\f) (loop (read-char) (cons #\page chars)))
                       ((char=? cc #\n) (loop (read-char) (cons #\newline chars)))
                       ((char=? cc #\r) (loop (read-char) (cons #\return chars)))
                       ((char=? cc #\t) (loop (read-char) (cons #\tab chars)))
                       (else (loop (read-char) (cons cc chars))))))
              (else (loop (read-char) (cons c chars))))))))

(define (parse-unicode-char)
  (let ((chars '()))
    (dotimes (_ 4)
      (push! chars (read-char)))
    (let ((n (string->number (list->string (reverse! chars)) 16)))
      (cond ((not n)
             (error "invalid unicode syntax"))
            ((<= #xD800 n #xDBFF)
             (surrogate-pair n))
            (else
              (ucs->char n))))))

(define (surrogate-pair hi)
  (read-char)  ; skip `\'
  (read-char)  ; skip `u'
  (let1 chars '()
    (dotimes (_ 4)
      (push! chars (read-char)))
    (let ((low (string->number (list->string (reverse! chars)) 16)))
      (unless (or low (<= #xDC00 low #xDFFF))
        (error "invalide unicode surrogate pair"))
      (ucs->char (+ #x10000 (ash (logand hi #x03FF) 10) (logand low #x03FF))))))

(define (parse-symbol value)
  (let1 sym (string->symbol value)
    (if (memq sym '(true false null))
      sym
      (error "unknown symbol" sym))))

(define (parse-number parts)
  (define (sign->number s)
    (or (and s (string=? s "-") -1) 1))
  (define (exponent sign int frac expo)
    (rxmatch-let (#/[eE]([-+])?(\d+)/ expo)
      (#f exp-sign factor)
      (* sign (* (exact->inexact (+ int frac))
                 (expt 10 (* (sign->number exp-sign) (string->number factor)))))))
  (let1 sign (sign->number (car parts))
    (match (cdr parts)
      [(int #f #f)     (* sign (string->number int))]
      [(int frac #f)   (* sign (+ (string->number int) (string->number #`"0,frac")))]
      [(int #f expo)   (exponent sign (string->number int) 0 expo)]
      [(int frac expo) (exponent sign (string->number int) (string->number #`"0,frac") expo)])))



;; ---------------------------------------------------------
;; Writer
(define (format-json obj)
  (cond ((null? obj))
        ((pair? obj)   (format-object obj))
        ((vector? obj) (format-array obj))
        ((number? obj) (format-number obj))
        ((string? obj) (format-string obj))
        ((eq? obj 'true) (display 'true))
        ((eq? obj 'false) (display 'false))
        ((eq? obj 'null) (display 'null))
        (else
          (error "unknown json object" obj))))

;; from rfc.json
(define (format-number obj)
  (cond ((not (real? obj))
         (error "real number expected, but got" obj))
        ((and (rational? obj) (not (integer? obj)))
         (display (exact->inexact obj)))
        (else (display obj))))

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
    (lambda (c)
      (if (char-set-contains? #[\x00-\x7f] c)
        (if (hash-table-exists? *escape-table* c)
          (display (hash-table-get *escape-table* c))
          (display c))
        (format #t "\\u~4,'0X" (char->ucs c))))
    obj)
  (display "\""))

;; from rfc.json
(define (format-object obj)
  (display "{")
  (fold (lambda (pair comma)
          (display comma)
          (format-string (car pair))
          (display ":")
          (format-json (cdr pair))
          ",")
        "" obj)
  (display "}"))

(define (format-array obj)
  (display "[")
  (let1 last (- (vector-length obj) 1)
    (vector-for-each
      (lambda (index elt)
        (format-json elt)
        (when (< index last)
          (display ",")))
      obj))
  (display "]"))


;; ---------------------------------------------------------
;; External API
;;
(define-constant json-mime-type "application/json")

(define (json-read :optional (input (current-input-port)))
  (parse-json input))

(define (json-read-from-string string)
  (call-with-input-string string json-read))

(define (json-write obj :optional (output (current-output-port)))
  (with-output-to-port output (cut format-json obj)))

(define (json-write-to-string obj)
  (call-with-output-string (cut json-write obj <>)))


(provide "text/json")
