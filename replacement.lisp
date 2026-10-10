(in-package #:cl-stringr)

(defun str-replace (s pattern replacement)
  "R: stringr::str_replace(). Replace the first match, accepting pattern modifiers.
Regex replacement backreferences use backslash-digit; fixed/coll replacements
are literal. NA propagates and boundary patterns signal errors."
  (pattern-map :replace s pattern replacement))

(defun str-replace-all (s pattern replacement)
  "R: stringr::str_replace_all(). Replace all nonoverlapping matches.
Modifier options and literal fixed/coll replacement semantics are retained."
  (pattern-map :replace-all s pattern replacement))
