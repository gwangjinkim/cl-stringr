(in-package #:cl-stringr)

(defun vmap (fn &rest vectors)
  "Applies FN to element-wise combinations of VECTORS.
If any element in a position is NIL, returns NIL for that position.
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
                   (if (some #'null args)
                       (setf (elt result i) nil)
                       (setf (elt result i) (apply fn args)))))
        result)))

(defmacro define-str-op (name lambda-list doc &body body)
  "Defines a vectorized string operation with NIL-handling."
  (let ((vec-args (loop for arg in lambda-list
                        unless (member arg '(&rest &optional &key))
                        collect arg)))
    `(defun ,name ,lambda-list
       ,doc
       (vmap (lambda ,vec-args ,@body) ,@vec-args))))
