(in-package #:cl-stringr)

(define-str-op str-length (s)
  "Returns the number of characters in each string."
  (length s))

(defun str-c (&rest vectors)
  "Concatenates multiple string vectors element-wise."
  (apply #'vmap (lambda (&rest strings)
                  (apply #'concatenate 'string strings))
         vectors))

(define-str-op str-sub (s start &optional end)
  "Extracts substrings from each string."
  (subseq s start end))

(define-str-op str-trim (s)
  "Removes leading and trailing whitespace."
  (string-trim '(#\Space #\Tab #\Newline #\Return) s))
