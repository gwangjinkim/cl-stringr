(in-package #:cl-stringr)

(define-str-op str-replace (s pattern replacement)
  "Replaces the first occurrence of PATTERN with REPLACEMENT in each string."
  (cl-ppcre:regex-replace pattern s replacement))

(define-str-op str-replace-all (s pattern replacement)
  "Replaces all occurrences of PATTERN with REPLACEMENT in each string."
  (cl-ppcre:regex-replace-all pattern s replacement))
