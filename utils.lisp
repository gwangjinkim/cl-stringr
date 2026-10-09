(in-package #:cl-stringr)

(defun missing-p (x)
  "True for the two missing markers: NIL (legacy) and cl-vctrs-lite:*na*."
  (or (null x) (cl-vctrs-lite:na-p x)))

(defun missing-result (args)
  "Result for a position with a missing input: *NA* if any input was *NA*
(what cl-readr produces), otherwise NIL (the legacy marker)."
  (if (some #'cl-vctrs-lite:na-p args) cl-vctrs-lite:*na* nil))

(defun vmap (fn &rest vectors)
  "Applies FN to element-wise combinations of VECTORS.
If any element in a position is missing (NIL or *NA*), FN is not called and
that position is *NA* if an input was *NA*, else NIL.
If an argument is not a vector (and not a string), it is treated as a scalar."
  (if (null vectors)
      (make-array 0 :fill-pointer 0 :adjustable t)
      (let* ((vecs (mapcar (lambda (v) (if (and (vectorp v) (not (stringp v))) v (vector v))) vectors))
             (max-len (loop for v in vecs maximize (length v)))
             (result (make-array max-len :initial-element nil)))
        (loop for i from 0 below max-len
              do (let ((args (loop for v in vecs
                                   for is-vector = (> (length v) 1)
                                   collect (elt v (if is-vector i 0)))))
                   (if (some #'missing-p args)
                       (setf (elt result i) (missing-result args))
                       (setf (elt result i) (apply fn args)))))
        result)))

(defmacro define-str-op (name lambda-list doc &body body)
  "Defines a vectorized string operation with missing-value (NIL/*NA*) handling."
  (let ((vec-args (loop for arg in lambda-list
                        unless (member arg '(&rest &optional &key))
                        collect arg)))
    `(defun ,name ,lambda-list
       ,doc
       (vmap (lambda ,vec-args ,@body) ,@vec-args))))
