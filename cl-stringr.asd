(asdf:defsystem #:cl-stringr
  :description "Common Lisp port of R's stringr"
  :author "Gwang-Jin Kim <gwang.jin.kim.phd@gmail.com>"
  :license "MIT"
  :version "0.0.1"
  :serial t
  :depends-on ("cl-ppcre" "alexandria" "cl-vctrs-lite")
  :components ((:file "package")
               (:file "utils")
               (:file "basics")
               (:file "pattern")
               (:file "replacement")
               (:file "structure")
               (:file "glue")
               (:file "dsl")))

(asdf:defsystem #:cl-stringr/tests
  :description "Tests for cl-stringr"
  :author "Josephus"
  :license "MIT"
  :depends-on ("cl-stringr" "parachute")
  :components ((:module "tests"
                :components ((:file "package")
                             (:file "main")))))
