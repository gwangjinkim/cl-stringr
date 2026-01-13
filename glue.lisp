(in-package #:cl-stringr)

(defun %str-glue-eval (expr env)
  "Evaluates EXPR in the context of ENV (a list of symbols it knows about) or at least looking up variables.
Since we want a simple glue, we might just look up symbols in the current environment or dynamic scope.
Real R glue evaluates in the environment. In CL, we might need a macro for that, but str-glue is a function."
  (let ((val (or (and env (cdr (assoc expr env :test #'string-equal)))
                 (let ((sym (find-symbol (string-upcase expr))))
                   (if (and sym (boundp sym))
                       (symbol-value sym)
                       (format nil "{MISSING: ~a}" expr))))))
    (format nil "~a" val)))

(defun str-glue (template &rest data)
  "Interpolates variables into strings. template can be a string or vector of strings.
data can be a plist of values to use if available."
  (let ((vec (if (and (vectorp template) (not (stringp template))) template (vector template)))
        (env (loop for (k v) on data by #'cddr collect (cons (string k) v))))
    (vmap (lambda (s)
            (cl-ppcre:regex-replace-all "{([^}]+)}" s
                                        (lambda (target-string start end match-start match-end reg-starts reg-ends)
                                          (declare (ignore target-string start end match-start match-end))
                                          (let ((expr (subseq s (elt reg-starts 0) (elt reg-ends 0))))
                                            (%str-glue-eval expr env)))))
          vec)))
