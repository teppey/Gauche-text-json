;;;
;;; Test text.json
;;;

(use gauche.test)

(test-start "text.json")
(use text.json)
(test-module 'text.json)

;;; The following code from Gauche-trunk/ext/peg/test.scm

(test* "write object" "{\"foo\":1,\"bar\":2}" (json-write '(("foo" . 1) ("bar" . 2)) #f))

(test* "write boolean" "[true]" (json-write #(#t) #f))

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

;; epilogue
(test-end)

