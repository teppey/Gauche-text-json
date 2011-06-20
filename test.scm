;;;
;;; Test text.json
;;;

(use gauche.test)
(use gauche.parameter)
(use gauche.collection)
(use gauche.dictionary)
(use gauche.sequence)
(use file.util)
(use srfi-1)

(test-start "text.json")
(use text.json)
(test-module 'text.json)


;; The following test section "read" and "write" based on
;; ext/peg/test.scm

(test-section "read")

(define (test-primitive str val)
  (test* "primitive" `(("x" . ,val)) (json-read str)))
(test-primitive "{\"x\": 100 }" 100)
(test-primitive "{\"x\" : -100}" -100)
(test-primitive "{\"x\":  +100 }" 100)
(test-primitive "{\"x\": 12.5} " 12.5)
(test-primitive "{\"x\":-12.5}" -12.5)
(test-primitive "{\"x\":+12.5}"  12.5)
(test-primitive "{\"x\": 1.25e1 }" 12.5)
(test-primitive "{\"x\":125e-1}" 12.5)
(test-primitive "{\"x\":1250.0e-2}" 12.5)
(test-primitive "{\"x\":  false  }" #f)
(test-primitive "{\"x\":true}" #t)
(test-primitive "{\"x\":null}" 'null)
(test-primitive "{\"x\": \"abc\\\"\\\\\\/\\b\\f\\n\\r\\t\\u0040abc\"}"
                "abc\"\\/\u0008\u000c\u000a\u000d\u0009@abc")

(test* "parsing an object"
       '(("Image"
          ("Width"  . 800)
          ("Height" . 600)
          ("Title"  . "View from 15th Floor")
          ("Thumbnail"
           ("Url"    . "http://www.example.com/image/481989943")
           ("Height" . 125)
           ("Width"  . "100"))
          ("IDs" . #(116 943 234 38793))))
       (json-read
         "{
         \"Image\": {
         \"Width\":  800,
         \"Height\": 600,
         \"Title\":  \"View from 15th Floor\",
         \"Thumbnail\": {
         \"Url\":    \"http://www.example.com/image/481989943\",
         \"Height\": 125,
         \"Width\":  \"100\"
         },
         \"IDs\": [116, 943, 234, 38793]
         }
         }"))

(test* "parsing an array containing two objects"
       '#((("precision" . "zip")
           ("Latitude"  . 37.7668)
           ("Longitude" . -122.3959)
           ("Address"   . "")
           ("City"      . "SAN FRANCISCO")
           ("State"     . "CA")
           ("Zip"       . "94107")
           ("Country"   . "US"))
          (("precision" . "zip")
           ("Latitude"  . 37.371991)
           ("Longitude" . -122.026020)
           ("Address"   . "")
           ("City"      . "SUNNYVALE")
           ("State"     . "CA")
           ("Zip"       . "94085")
           ("Country"   . "US")))
       (json-read
         "[
           {
           \"precision\": \"zip\",
           \"Latitude\":  37.7668,
           \"Longitude\": -122.3959,
           \"Address\":   \"\",
           \"City\":      \"SAN FRANCISCO\",
           \"State\":     \"CA\",
           \"Zip\":       \"94107\",
           \"Country\":   \"US\"
           },
           {
           \"precision\": \"zip\",
           \"Latitude\":  37.371991,
           \"Longitude\": -122.026020,
           \"Address\":   \"\",
           \"City\":      \"SUNNYVALE\",
           \"State\":     \"CA\",
           \"Zip\":       \"94085\",
           \"Country\":   \"US\"
           }
           ]"))

(test-section "write")

(define (test-writer name obj)
  (test* name obj
         (json-read (json-write obj #f))))

(test-writer "writing an object"
             '(("Image"
                ("Width"  . 800)
                ("Height" . 600)
                ("Title"  . "View from 15th Floor")
                ("Thumbnail"
                 ("Url"    . "http://www.example.com/image/481989943")
                 ("Height" . 125)
                 ("Width"  . "100"))
                ("IDs" . #(116 943 234 38793)))))

(test-writer "writing an array containing two objects"
             '#((("precision" . "zip")
                 ("Latitude"  . 37.7668)
                 ("Longitude" . -122.3959)
                 ("Address"   . "")
                 ("City"      . "SAN FRANCISCO")
                 ("State"     . "CA")
                 ("Zip"       . "94107")
                 ("Country"   . "US"))
                (("precision" . "zip")
                 ("Latitude"  . 37.371991)
                 ("Longitude" . -122.026020)
                 ("Address"   . "")
                 ("City"      . "SUNNYVALE")
                 ("State"     . "CA")
                 ("Zip"       . "94085")
                 ("Country"   . "US"))))


(test-section "custom mapping")
(define sample-json-text
  "{
    \"Image\": {
          \"Width\":  800,
          \"Height\": 600,
          \"Title\":  \"View from 15th Floor\",
          \"Thumbnail\": {
              \"Url\":    \"http://www.example.com/image/481989943\",
              \"Height\": 125,
              \"Width\":  \"100\"
          },
          \"IDs\": [116, 943, 234, 38793]
        }
   }")

;; object -> hash-table, array -> list
(parameterize ([json-object-fn (cut make-hash-table 'string=?)]
               [json-array-fn  (cut values <list> #f)])
  (let1 o (json-read sample-json-text)
    (test* "hash-table?" #t (hash-table? o))
    (test* "hash-table?" #t (hash-table? (~ o "Image")))
    (test* "list?" #t (list? (~ o "Image" "IDs")))
    (test* "list len" 4 (length (~ o "Image" "IDs")))
    (test* "lookup" "100" (~ o "Image" "Thumbnail" "Width"))
    ))

;; treemap and specified array size
(parameterize ([json-object-fn (cut make-tree-map string=? string<?)]
               [json-array-fn (cut values <vector> 4)])
  (let1 o (json-read sample-json-text)
    (test* "tree-map?" #t (tree-map? o))
    (test* "tree-map?" #t (tree-map? (~ o "Image")))
    (test* "lookup" "100" (~ o "Image" "Thumbnail" "Width"))
    (test* "key order" '("Height" "IDs" "Thumbnail" "Title" "Width")
           (tree-map-keys (~ o "Image")))
    (test* "vector" #(116 943 234 38793) (~ o "Image" "IDs"))
    ))

;; User defined dictoinary
(define-class <my-dict> (<dictionary>)
  ((keys :init-value '()) (values :init-value '())))

(define x->symbol (.$ string->symbol x->string))

(define-method dict-get ([d <my-dict>] [key <symbol>] . default)
  (let1 default (get-optional default #f)
    (or (and-let* ([p (assq key (zip (~ d 'keys) (~ d 'values)))])
          (cadr p))
        default)))

(define-method dict-put! ([d <my-dict>] key value)
  (let loop ([ks (~ d 'keys)] [vs (~ d 'values)])
    (cond [(null? ks)
           (set!-values ((~ d 'keys) (~ d 'values))
                        (values (cons (x->symbol key) (~ d 'keys))
                                (cons value (~ d 'values))))]
          [(eq? (car ks) key) (set-car! vs value)]
          [else (loop (cdr ks) (cdr vs))])))

(define-method dict-exists? ([d <my-dict>] [key <symbol>])
  (boolean (dict-get d key)))

(define-method dict-delete! ([d <my-dict>] [key <symbol>])
  (let1 key (x->symbol key)
    (let loop ([ks (~ d 'keys)]   [new-ks '()]
               [vs (~ d 'values)] [new-vs '()])
      (cond [(null? ks)
             (set!-values ((~ d 'keys) (~ d 'values))
                          (values (reverse new-ks)
                                  (reverse new-vs)))]
            [(eq? (car ks) key)
             (loop (cdr ks) (cdr vs) new-ks new-vs)]
            [else
              (loop (cdr ks) (cdr vs)
                    (cons (car ks) new-ks)
                    (cons (car vs) new-vs))]))))

(define-method dict-fold ([d <my-dict>] proc seed)
  (fold proc seed (~ d 'keys) (~ d 'values)))

;; User defined sequence class
(define-class <my-seq-meta> (<class>) ())
(define-class <my-seq> (<sequence>)
  ([elements :init-value '() :init-keyword :elements]
   [len :init-value 0 :init-keyword :len])
  :metaclass <my-seq-meta>)

(define-method call-with-builder ([class <my-seq-meta>] proc . _)
  (let* ([elements '()]
         [add! (^e (push! elements (+ e 1)))]
         [get (^() (let1 len (length elements)
                     (make class :elements (reverse! elements) :len len)))])
    (proc add! get)))

(define-method referencer ([seq <my-seq>])
  (lambda (seq index . rest)
    (apply list-ref (~ seq 'elements) index rest)))

(define-method modifier ([seq <my-seq>])
  (lambda (seq index value)
    (set! (~ (~ seq 'elements) index) value)))

;; Testing user defined dictionary and sequence
(parameterize ([json-object-fn (cut make <my-dict>)]
               [json-array-fn (cut values <my-seq> #f)])
  (let1 o (json-read sample-json-text)
    (test* "<my-dict>" #t (is-a? o <my-dict>))
    (test* "<my-seq>" #t (is-a? (dict-get (dict-get o 'Image) 'IDs) <my-seq>))
    (test* "lookup" 125
           (dict-get (dict-get (dict-get o 'Image) 'Thumbnail) 'Height))
    (let* ([1+ (pa$ + 1)]
           [seq (dict-get (dict-get o 'Image) 'IDs)])
      (test* "seq +1" (1+ 116) (ref seq 0))
      (test* "seq +1" (1+ 943) (ref seq 1))
      (test* "seq +1" (1+ 234) (ref seq 2))
      (test* "seq +1" (1+ 38793) (ref seq 3))
      (dec! (ref seq 0))
      (test* "modify seq" 116 (ref seq 0))
      )
    ))

;; list -> JSON array
(parameterize ([list-as-json-array #t])
  (test* "list -> array" "[1,2,3]" (json-write '(1 2 3) #f))
  (test* "alist is not object" (test-error)
         (json-write '(("foo" . 1) ("bar" . 2))))
  )

;; Writer accepts only instance of <collection> except <string>
(test* "not json" (test-error) (json-write 1 #f))
(test* "not json" (test-error) (json-write "foo" #f))
(test* "not json" (test-error) (json-write 'true #f))

;; Writer allow symbol `true' and 'false'
(test* "symbol true -> literal" "[true]" (json-write #(true) #f))
(test* "symbol false -> literal" "[false]" (json-write #(false) #f))

;; Inalid syntax
(test* "scalar" (test-error) (json-read "1"))
(test* "scalar" (test-error) (json-read "\"foo\""))
(test* "scalar" (test-error) (json-read "true"))
(test* "scalar" (test-error) (json-read "false"))
(test* "scalar" (test-error) (json-read "null"))
(test* "unexpected literal" (test-error) (json-read "foo"))
(test* "unexpected literal" (test-error) (json-read "[foo]"))
(test* "trailing comma1" (test-error) (json-read "[,]"))
(test* "trailing comma2" (test-error) (json-read "[1,]"))
(test* "trailing comma3" (test-error) (json-read "{,}"))
(test* "trailing comma4" (test-error) (json-read "{\"foo\":1,}"))
(test* "leading comma1" (test-error) (json-read "[,1]"))
(test* "leading comma2" (test-error) (json-read "{,\"foo\":1}"))
(test* "bad object form1" (test-error) (json-read "{\"foo\"}"))
(test* "bad object form2" (test-error) (json-read "{\"foo\",}"))
(test* "bad object form3" (test-error) (json-read "{\"foo\":,}"))
(test* "bad object form4" (test-error) (json-read "{:,}"))
(test* "bad object form5" (test-error) (json-read "{:1,}"))
(test* "bad object form6" (test-error) (json-read "{foo:1}"))
(test* "no comma" (test-error) (json-read "[1 2]"))
(test* "no comma" (test-error) (json-read "{\"foo\":1 \"bar\":2}"))

(test* "write object" "{\"foo\":1,\"bar\":2}" (json-write '(("foo" . 1) ("bar" . 2)) #f))

(test* "write boolean" "[true]" (json-write #(#t) #f))

(test* "read empty object" '() (json-read "{}"))
(test* "write empty object" "{}" (json-write (json-read "{}") #f))
(test* "read empty array" #() (json-read "[]"))
(test* "write empty object" "{}" (json-write '() #f))

; pretty-print
(test-section "pretty-print")
(test* "pp" "[\n]" (json-write* #() #f))
(test* "pp" "[
  [
  ]
]" (json-write* #(#()) #f))
(test* "pp" "[
  [
    [
      [
      ],
      1,
      {
      }
    ]
  ]
]" (json-write* #(#(#(#() 1 ()))) #f))
(test* "pp" "{\n}" (json-write* '() #f))
(test* "pp" "{
  \"foo\": {
  }
}" (json-write* '(["foo" . ()]) #f))

(test* "pp" "[
  {
  }
]" (json-write* #(()) #f))

(test* "pp" "[
  1
]" (json-write* #(1) #f))

(test* "pp" "[
  1,
  2
]" (json-write* #(1 2) #f))

(test* "pp" "[
  1,
  {
    \"foo\": \"x\"
  },
  2
]" (json-write* #(1 (("foo" . "x")) 2) #f))

(test* "pp" "{
  \"x\": [
    1,
    {
      \"foo\": [
        2,
        3
      ],
      \"bar\": 4
    },
    5
  ]
}" (json-write* '(["x" . #(1 (["foo" . #(2 3)] ["bar" . 4]) 5)]) #f))

(test* "pp" "[
1
]" (parameterize ([json-indent-width 0]) (json-write* #(1) #f)))

(test* "pp" "[
 1
]" (parameterize ([json-indent-width 1]) (json-write* #(1) #f)))

(test* "pp" "{
 \"foo\": 1,
 \"bar\": [
  2,
  3
 ]
}" (parameterize ([json-indent-width 1])
     (json-write* '(["foo" . 1] ["bar" . #(2 3)]) #f)))

(test-section "json_checker")

;; All test files are take from http://json.org/JSON_checker/test.zip

(define (json-checker-name&path type num)
  (let1 name (format #f "~a~d" type num)
    (values name (build-path "." "json_checker" #`",|name|.json"))))

;; pass cases
(dotimes (n 3)
  (receive (name path) (json-checker-name&path 'pass (+ n 1))
    (test* name #t (boolean (with-input-from-file path json-read)))))

;; fail cases
(define *ignore-fail-tests*
  '(18 ;  too deep nested array
    ))
(dotimes (n 33)
  (unless (memv (+ n 1) *ignore-fail-tests*)
    (receive (name path) (json-checker-name&path 'fail (+ n 1))
      (test* name (test-error <json-read-error>)
             (with-input-from-file path json-read)))))

;; epilogue
(test-end)

