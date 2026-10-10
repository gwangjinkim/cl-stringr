(asdf:defsystem #:cl-stringr
  :description "Common Lisp port of R's stringr"
  :author "Gwang-Jin Kim <gwang.jin.kim.phd@gmail.com>"
  :license "MIT"
  :version "0.0.1"
  :serial t
  :depends-on ("cl-ppcre" "alexandria" "cl-vctrs-lite" "cffi" "local-time")
  :components ((:file "package")
               (:file "utils")
               (:file "basics")
               (:file "icu")
               (:file "modifiers")
               (:file "pattern")
               (:file "replacement")
               (:file "structure")
               (:file "glue")
               (:file "dsl"))
  :in-order-to ((asdf:test-op (asdf:test-op #:cl-stringr/tests))))

(asdf:defsystem #:cl-stringr/tests
  :description "Tests for cl-stringr"
  :author "Josephus"
  :license "MIT"
  :depends-on ("cl-stringr" "parachute")
  :components ((:module "tests"
                :components ((:file "package")
                             (:file "main"))))
  :perform (asdf:test-op (op c)
             (declare (ignore op c))
             ;; Signal failure to the caller (e.g. CI) instead of only printing.
             (let ((report (uiop:symbol-call :parachute :test :cl-stringr/tests)))
               (when (eq :failed (uiop:symbol-call :parachute :status report))
                 (error "cl-stringr tests failed")))))
