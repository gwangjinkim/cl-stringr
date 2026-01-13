(in-package #:cl-stringr)

(define-str-op str-detect (s pattern)
  "Detects the presence of a pattern in each string."
  (if (cl-ppcre:scan pattern s) t nil))

(defun str-subset (s pattern)
  "Returns strings that match the pattern."
  (let* ((vec (if (and (vectorp s) (not (stringp s))) s (vector s)))
         (matches (str-detect vec pattern))
         (result (make-array (count t matches) :fill-pointer 0 :adjustable t)))
    (loop for i from 0 below (length vec)
          when (elt matches i)
          do (vector-push-extend (elt vec i) result))
    result))

(define-str-op str-extract (s pattern)
  "Extracts the first match of the pattern in each string."
  (cl-ppcre:scan-to-strings pattern s))

(define-str-op str-count (s pattern)
  "Counts the number of times a pattern appears in each string."
  (let ((count 0))
    (cl-ppcre:do-matches (s2 e2 pattern s)
      (incf count))
    count))

(define-str-op str-extract-all (s pattern)
  "Extracts all matches of the pattern in each string."
  (let (results)
    (cl-ppcre:do-matches-as-strings (m pattern s)
      (push m results))
    (coerce (nreverse results) 'vector)))
