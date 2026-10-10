(in-package #:cl-stringr)

(defun input-strings (s)
  ;; Pattern constructors coerce FALSE to text, while existing string consumers
  ;; retain their legacy NIL missing marker. Owner-specific character conversion
  ;; is shared so factor/date/timestamp containers never use their host printer.
  (let ((values (string-vector s)))
    (when (and (vectorp s) (not (stringp s)))
      (dotimes (i (length s)) (when (null (aref s i)) (setf (aref values i) nil))))
    values))

(defun recycle-size (&rest vectors)
  (let ((sizes (mapcar #'length vectors)))
    (let ((n (if (member 0 sizes) 0 (apply #'max sizes))))
      (unless (every (lambda (size) (or (= size n) (= size 1))) sizes)
        (fail-pattern "String and pattern lengths must be equal or one."))
      n)))

(defun pattern-map (operation s pattern &optional replacement)
  (let* ((strings (input-strings s))
         (object (if (pattern-object-p pattern) pattern (regex pattern)))
         (values (pattern-values object))
         (nested (member operation '(:extract-all :split)))
         (boundary-p (eq (pattern-kind object) :boundary))
         ;; Bare empty patterns mean character boundaries for these consumers.
         (bare-empty (and (not (pattern-object-p pattern))
                          (= (length values) 1) (equal (aref values 0) "")))
         (replacements (when (member operation '(:replace :replace-all)) (input-strings replacement)))
         (n (if replacements (recycle-size strings values replacements) (recycle-size strings values)))
         (result (make-array n)))
    (when bare-empty (setf object (boundary :character) boundary-p t))
    (when (and boundary-p (member operation '(:detect :replace :replace-all)))
      (fail-pattern "This string operation does not accept boundary patterns."))
    (when (and (plusp n) (not boundary-p) (find "" values :test #'equal))
      (warn-pattern "Empty search patterns are not supported."))
    (dotimes (i n)
      (let* ((string (aref strings (if (= (length strings) 1) 0 i)))
             (value (aref values (if (= (length values) 1) 0 i)))
             (rep (when replacements (aref replacements (if (= (length replacements) 1) 0 i))))
             (missing (or (missing-p string) (and (not boundary-p) (missing-p value))
                          (and replacements (missing-p rep))
                          (and (not boundary-p) (equal value "")))))
        (setf (aref result i)
              (if missing
                  (let ((na (if (or (cl-vctrs-lite:na-p string) (cl-vctrs-lite:na-p value)
                                    (cl-vctrs-lite:na-p rep) (equal value ""))
                                cl-vctrs-lite:*na* nil)))
                    (if nested (vector na) na))
                  (let* ((spans (pattern-spans string object value)) (first (car spans)))
                    (ecase operation
                      (:detect (not (null first)))
                      (:count (length spans))
                      (:extract (if first (subseq string (car first) (cdr first)) cl-vctrs-lite:*na*))
                      (:extract-all (map 'vector (lambda (span) (subseq string (car span) (cdr span))) spans))
                      (:split
                       (if boundary-p
                           (map 'vector (lambda (span) (subseq string (car span) (cdr span))) spans)
                           (coerce (loop with start = 0 with pieces = nil
                                         for span in spans
                                         do (push (subseq string start (car span)) pieces)
                                            (setf start (cdr span))
                                         finally (return (nreverse (cons (subseq string start) pieces)))) 'vector)))
                      ((:replace :replace-all)
                       (if (eq (pattern-kind object) :regex)
                           (icu-regex-replace string value rep (pattern-options object) (eq operation :replace-all))
                           (with-output-to-string (out)
                             (let ((start 0))
                               (dolist (span (if (eq operation :replace) (when first (list first)) spans))
                                 (write-string string out :start start :end (car span))
                                 (write-string rep out) (setf start (cdr span)))
                               (write-string string out :start start)))))))))))
    (if (and (member operation '(:count :extract :replace :replace-all))
             (position nil result))
        result
        (cl-vctrs-lite:make-typed-column result
             (ecase operation (:detect :bool) (:count :int) (:extract :string)
                    ((:extract-all :split) :list) ((:replace :replace-all) :string))))))

(defun str-detect (s pattern)
  "R: stringr::str_detect(). Detect scalar or vector patterns, propagating NA.
Accepts regex/fixed/coll objects with strict scalar recycling; boundary is invalid."
  (let ((result (pattern-map :detect s pattern)))
    result))

(defun pattern-matches-p (pattern string)
  "R: stringr::str_detect(). Match a scalar pattern using native ICU Unicode semantics; NA propagates.
Regex, fixed and coll objects are accepted. Boundary patterns signal an error."
  (let ((result (str-detect string pattern)))
    (unless (= 1 (length result)) (fail-pattern "A scalar pattern is required."))
    (aref result 0)))

(defun str-subset (s pattern)
  "R: stringr::str_subset(). Keep definite pattern matches and drop NA."
  (let* ((strings (input-strings s)) (matches (str-detect strings pattern)))
    (unless (= (length strings) (length matches)) (fail-pattern "Subset pattern must not expand strings."))
    (coerce (loop for s across strings for match across matches when (eq match t) collect s) 'vector)))

(defun str-extract (s pattern)
  "R: stringr::str_extract(). Return the first match or NA, with modifier support."
  (pattern-map :extract s pattern))

(defun str-count (s pattern)
  "R: stringr::str_count(). Count nonoverlapping matches or Unicode boundary segments."
  (pattern-map :count s pattern))

(defun str-extract-all (s pattern)
  "R: stringr::str_extract_all(). Return vectors of matches, including nested NA."
  (pattern-map :extract-all s pattern))

(defun str-equal (x y &rest options &key (locale "en") (ignore-case nil) &allow-other-keys)
  "R: stringr::str_equal(). Compare canonically equivalent strings with ICU.
Scalar recycling and NA propagation apply; collation options match COLL."
  (let* ((object (apply #'coll "" :locale locale :ignore-case ignore-case options))
         (xs (input-strings x)) (ys (input-strings y)) (n (recycle-size xs ys)) (out (make-array n)))
    (dotimes (i n)
      (let ((a (aref xs (if (= 1 (length xs)) 0 i))) (b (aref ys (if (= 1 (length ys)) 0 i))))
        (setf (aref out i) (if (or (missing-p a) (missing-p b)) (missing-result (list a b))
                              (icu-equal-p a b (pattern-options object))))))
    (cl-vctrs-lite:make-typed-column out :bool)))
