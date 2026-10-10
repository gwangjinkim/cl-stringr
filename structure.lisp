(in-package #:cl-stringr)

(defun str-split (s pattern)
  "R: stringr::str_split(). Split by a modifier pattern, retaining empty pieces.
Boundary objects return Unicode segments; missing inputs produce nested NA."
  (pattern-map :split s pattern))

(defun str-join (string-vector &optional (collapse ""))
  "Joins elements of a vector into a single string."
  (let ((vec (if (and (vectorp string-vector) (not (stringp string-vector))) string-vector (vector string-vector))))
    (if (some #'missing-p (coerce vec 'list))
        (missing-result (coerce vec 'list))
        (format nil (format nil "~~{~~a~~^~a~~}" collapse) (coerce vec 'list)))))
