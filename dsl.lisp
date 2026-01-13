(in-package #:cl-stringr)

(defmacro with-str-context (vars &body body)
  "Context macro to bind variables for str-glue.
vars should be a plist or a list of (var val) pairs."
  (let ((bindings (if (keywordp (first vars))
                      (loop for (k v) on vars by #'cddr collect `(,k ,v))
                      vars)))
    `(let ,(mapcar (lambda (b) (if (listp b) b `(,b ,b))) bindings)
       (flet ((glue (template &rest extra-data)
                (apply #'str-glue template 
                       (append (list ,@(loop for b in bindings
                                             append (let ((name (if (listp b) (first b) b)))
                                                      (list (intern (string name) "KEYWORD") name))))
                               extra-data))))
         ,@body))))

(defun map-str (fn vector)
  "Applies FN to each string in VECTOR, handling NIL."
  (vmap fn vector))

(defun filter-str (pattern vector)
  "Alias for str-subset for more Lisp-like naming."
  (str-subset vector pattern))
