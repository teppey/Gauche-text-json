;;;
;;; Test text.json
;;;

(use gauche.test)
(use gauche.parameter)
(use file.util)

(test-start "text.json")
(use text.json)
(test-module 'text.json)


(test-section "read")

;; The following test cases based on Gauche-trunk/ext/peg/test.scm
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

(parameterize ([json-object-fn (cut make-hash-table 'string=?)]
               [json-array-fn  (cut values <list> #f)])
  (let1 o (json-read sample-json-text)
    (test* "hashtable?" #t (is-a? o <hash-table>))
    (test* "hashtable?" #t (is-a? (~ o "Image") <hash-table>))
    (test* "list?" #t (is-a? (~ o "Image" "IDs") <list>))
    (test* "lookup" "100" (~ o "Image" "Thumbnail" "Width"))))

(parameterize ([list-as-json-array #t])
  (test* "hash-table -> object" "{\"foo\":1,\"bar\":2}"
         (json-write (hash-table 'string=? '("foo" . 1) '("bar" . 2)) #f))
  (test* "list -> array" "[1,2,3]" (json-write '(1 2 3) #f))
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
(define (json-checker-name&path type num)
  (let1 name (format #f "~a~d" type num)
    (values name (build-path "." "json_checker" #`",|name|.json"))))

;; pass cases
(dotimes (n 3)
  (receive (name path) (json-checker-name&path 'pass (+ n 1))
    (test* name #t (boolean (with-input-from-file path json-read)))))

;; fail cases
(dotimes (n 33)
  (receive (name path) (json-checker-name&path 'fail (+ n 1))
    (test* name (test-error <json-read-error>)
           (with-input-from-file path json-read))))

;; epilogue
(test-end)

