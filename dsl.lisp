(in-package #:cl-stringr)

(defmacro with-str-context (vars &body body)
  "Context macro to bind variables for str-glue.
Supports: 
1. (var val var val ...)
2. (:key val :key val ...)
3. ((var val) (var val) ...)"
  (let* ((bindings (cond
                     ((keywordp (first vars))
                      (loop for (k v) on vars by #'cddr 
                            collect (list (intern (string k)) v)))
                     ((and (listp (first vars)) (not (null (first vars))))
                      vars)
                     (t
                      (loop for (var val) on vars by #'cddr
                            collect (list var val)))))
         (keys (loop for b in bindings collect (intern (string (first b)) "KEYWORD"))))
    `(let ,bindings
       (flet ((glue (template &rest extra-data)
                (apply #'str-glue template 
                       (append (list ,@(loop for b in bindings
                                             for key in keys
                                             append (list key (first b))))
                               extra-data))))
         ,@body))))

(defun map-str (fn vector)
  "Applies FN to each string in VECTOR, handling NIL."
  (vmap fn vector))

(defun filter-str (pattern vector)
  "Alias for str-subset for more Lisp-like naming."
  (str-subset vector pattern))
