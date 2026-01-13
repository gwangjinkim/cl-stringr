(in-package #:cl-stringr)

(define-str-op str-split (s pattern)
  "Splits each string into pieces based on a pattern."
  (coerce (cl-ppcre:split pattern s) 'vector))

(defun str-join (string-vector &optional (collapse ""))
  "Joins elements of a vector into a single string."
  (let ((vec (if (and (vectorp string-vector) (not (stringp string-vector))) string-vector (vector string-vector))))
    (if (some #'null (coerce vec 'list))
        nil
        (format nil (format nil "~~{~~a~~^~a~~}" collapse) (coerce vec 'list)))))
