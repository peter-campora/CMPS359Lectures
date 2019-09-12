#lang racket

(require 2htdp/image)
(require test-engine/racket-tests)

;;Number -> Number
;;Return the volume of an equilateral cube
;;Given: 10  expected: 1000
;;Given: 2 expected: 8
(define (cvolume length) -1)
(check-expect (cvolume 10) 1000)
(check-expect (cvolume 2) 8)

;;String -> 1String
;;Takes in any non-empty string and produces a string of length 1
;;Given: "ABC" expected: 'A'
;;Given: "A" expected: 'A'
;;Given: "" expected: an error
(define (string-first str) "")
(check-expect (string-first "ABC") #\A)
(check-expect (string-first "A") #\A)


;;String -> 1String
;;Takes in any non-empty string and produces a string of length 1
;;Given: "ABC" expected: 'C'
;;Given: "A" expected: 'A'
;;Given: "" expected: an error
(define (string-last str) "")
(check-expect (string-last "ABC") #\C)
(check-expect (string-last "A") #\A)

;;Image -> String
;;Takes in an image and classifies whether it is tall or wide
;;Given: (circle 10 "solid" "blue") expected: "square"
;;Given: (empty-scene 20 10 ...) expected: "wide"
;;Given: (empty-scene 10 20 ...) expected: "tall"
(define (image-classify image) "")
(check-expect (image-classify (circle 10 "solid" "blue")) "square")
(check-expect (image-classify (empty-scene 20 10 "white")) "wide")
(check-expect (image-classify (empty-scene 10 20 "white")) "tall")

;;Image -> Area
;;Takes in an image and returns the area of the given rectangular image
;;Given: (circle 10 "solid" "blue") expected: 400
;;Given: (empty-scene 10 20 ...) expected: 200
(define (image-area image) -1)
(check-expect (image-area  (circle 10 "solid" "blue")) 400)
(check-expect (image-area (empty-scene 10 20 "white")) 200)


;;String String -> String
;;Takes in two string and returns a new string
;;that concatenates the second string to the first with a '_' inbetween
;;Given: "foo" "bar" expected: "foo_bar"
;;Given: "" "bar" expected: "_bar"
;;Given: "foo" "" expected: "foo"
(define (string-join str1 str2) "")
(check-expect (string-join "foo" "bar") "foo_bar")
(check-expect (string-join "" "bar") "_bar")
(check-expect (string-join "foo" "") "foo_")

;;String Number -> String
;;Take in a string and a number and insert a - at that
;;character location in the string
;;Given: "foobar" 3 expected: "foo_bar"
;;Given: "helloworld" 5 expected: "hello_world"
(define (string-insert str index) "")
(check-expect (string-insert  "foobar" 3) "foo_bar")
(check-expect (string-insert "helloworld" 5) "hello_world")

;;String Number -> String
;;Delete the character at position i in a string
;;Given: "foo_bar" 3 expected: "foobar"
;;Given: "hello_world" 5 expected: "helloworld
(define (string-delete str index ) "")
(check-expect (string-delete "foo_bar" 3) "foobar")
(check-expect (string-delete "hello_world" 5) "helloworld")


(test)

