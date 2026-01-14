(in-package #:cl-stringr/tests)

(define-test "Basic Operations"
  (is equalp #(5 nil 3) (str-length #("hello" nil "foo")))
  (is equalp #(1) (str-length "a"))
  
  (is equalp #("abcd" nil) (str-c #("ab" "cd") #("cd" nil)))
  (is equalp #("a1" "b2") (str-c #("a" "b") #("1" "2")))

  (is equalp #("hel" nil) (str-sub #("hello" nil) 0 3))
  (is equalp #("ell") (str-sub "hello" 1 4))

  (is equalp #("foo" nil "bar") (str-trim #("  foo  " nil "bar ")))
  
  (is equalp #("ABC" nil) (str-to-upper #("abc" nil)))
  (is equalp #("abc" nil) (str-to-lower #("ABC" nil)))
  (is equalp #("Hello World" nil) (str-to-title #("hello world" nil)))
  (is equalp #("Hello world" nil) (str-to-sentence #("HELLO WORLD" nil))))

(define-test "Pattern Matching"
  (is equalp #(t nil t) (str-detect #("apple" nil "banana") "a"))
  (is equalp #("apple" "pear" "banana") (str-subset #("apple" "pear" "banana") "a"))
  (is equalp #(1 1 3) (str-count #("apple" "pear" "banana") "a"))
  (is equalp #("app" nil "ban") (str-extract #("apple" nil "banana") "[a-z]{3}"))
  (let ((res (str-extract-all #("apple" "" "banana") "[a-z]")))
    (is equalp (elt (elt res 0) 0) "a")
    (is equalp (length (elt res 1)) 0)))

(define-test "Replacement"
  (is equalp #("a-pple" nil "b-anana") (str-replace #("apple" nil "banana") "([a-z])" "\\1-"))
  (is equalp #("a-p-p-l-e-" "" "b-a-n-a-n-a-") (str-replace-all #("apple" "" "banana") "([a-z])" "\\1-")))

(define-test "Splitting and Joining"
  (is equalp #(#("a" "b" "c") #("d" "e")) (str-split #("a,b,c" "d,e") ","))
  (is equal "a-b-c" (str-join #("a" "b" "c") "-"))
  (is equalp nil (str-join #("a" nil "c") "-")))

(define-test "String Interpolation"
  (let ((x "world"))
    (declare (ignorable x))
    (is equalp #("Hello world") (str-glue "Hello {x}" :x "world"))
    (is equalp #("a is 1, b is 2") (str-glue "a is {a}, b is {b}" :a 1 :b 2))))

(define-test "Lisp DSL"
  (with-str-context (name "Alice" age 30)
    (is equal "Hello Alice, age 30" (elt (glue "Hello {name}, age {age}") 0)))
  (with-str-context (:name "Bob" :age 40)
    (is equal "Hello Bob, age 40" (elt (glue "Hello {name}, age {age}") 0)))
  
  (is equalp #(5 nil 3) (map-str #'length #("hello" nil "foo"))))
