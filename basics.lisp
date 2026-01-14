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

(define-str-op str-to-upper (s)
  "Converts strings to uppercase."
  (string-upcase s))

(define-str-op str-to-lower (s)
  "Converts strings to lowercase."
  (string-downcase s))

(define-str-op str-to-title (s)
  "Converts strings to title case (capitalizes first letter of each word)."
  (string-capitalize s))

(define-str-op str-to-sentence (s)
  "Converts strings to sentence case (capitalizes first letter of the string)."
  (let ((lower (string-downcase s)))
    (if (plusp (length lower))
        (concatenate 'string (string (char-upcase (char lower 0))) (subseq lower 1))
        lower)))
