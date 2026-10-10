(in-package #:cl-stringr)

;;; Rank-two arrays retain metadata privately, including empty prototypes.
;;; Public matrix APIs expose it as additional values: columns, row/outer
;;; names, type. Keys are weak, so metadata cannot retain discarded matrices.
(defvar *string-matrix-metadata* (trivial-garbage:make-weak-hash-table :test #'eq :weakness :key))
(defun %string-matrix (rows columns type &optional labels names)
  (let ((out (make-array (list (length rows) columns) :initial-element cl-vctrs-lite:*na*)))
    (loop for row in rows for i from 0 do
      (loop for cell in row for j from 0 do (setf (aref out i j) cell)))
    (setf (gethash out *string-matrix-metadata*) (list labels names type)) out))
(defun %matrix-values (matrix)
  (destructuring-bind (columns rows type) (gethash matrix *string-matrix-metadata*)
    (values matrix (and columns (copy-seq columns)) (and rows (copy-seq rows)) type)))
(defun %sr2-names (input)
  (cond ((and input (cl-vctrs-lite:named-list-p input)) (coerce (mapcar #'car input) 'vector))
        ((and (cl-vctrs-lite:column-p input) (not (listp input))) (cl-vctrs-lite:col-names input))
        ((pattern-object-p input) (and (pattern-names input) (coerce (pattern-names input) 'vector)))))
(defun %preserved-names (string pattern)
  (let* ((strings (input-strings string))
         (patterns (if (pattern-object-p pattern) (pattern-values pattern) (string-vector pattern))))
    (when (or (= 1 (length patterns)) (= (length strings) (length patterns))) (%sr2-names string))))
(defun %name-result (result string pattern)
  (let ((names (%preserved-names string pattern)))
    (when names (setf (cl-vctrs-lite:col-names result) names)) result))
(defun %sr2-strings (input)
  ;; Logical columns have an unambiguous FALSE cell; mixed legacy character
  ;; vectors retain NIL as their established missing-input marker.
  (if (and (cl-vctrs-lite:column-p input) (not (listp input))
           (eq :bool (cl-vctrs-lite:col-type input)))
      (string-vector input) (input-strings input)))
(defun %check-sr2-pattern (pattern &optional allow-na)
  (unless (pattern-object-p pattern)
    (let ((values (cond ((or (stringp pattern) (and allow-na (cl-vctrs-lite:na-p pattern))) (vector pattern))
                        ((and pattern (cl-vctrs-lite:named-list-p pattern)) (coerce (mapcar #'cdr pattern) 'vector))
                        ((vectorp pattern) pattern)
                        (t (fail-pattern "Pattern must be a character vector or modifier.")))))
      (unless (every (lambda (value) (or (stringp value) (and allow-na (cl-vctrs-lite:na-p value)))) values)
        (fail-pattern "Plain patterns must contain nonmissing character values.")))))
(defun %pack-matrix-list (matrices names)
  (cond ((null names) matrices)
        ((and (plusp (length names)) (every #'stringp names)) (map 'list #'cons names matrices))
        (t (let ((out (cl-vctrs-lite:make-typed-column (coerce matrices 'vector) :list)))
             (setf (cl-vctrs-lite:col-names out) names) out))))
(defun %sr2-context (string pattern)
  (%check-sr2-pattern pattern)
  (let* ((strings (%sr2-strings string)) (object (if (pattern-object-p pattern) pattern (regex pattern)))
         (patterns (pattern-values object)) (size (recycle-size strings patterns)))
    (values strings object patterns size)))
(defun %bare-empty-p (pattern object)
  (and (not (pattern-object-p pattern)) (= 1 (length (pattern-values object)))
       (equal "" (aref (pattern-values object) 0))))
(defun %sr2-detection (string pattern negate &optional edge)
  (boolean-option negate :negate)
  (multiple-value-bind (strings object patterns size) (%sr2-context string pattern)
    (when (or (%bare-empty-p pattern object) (eq :boundary (pattern-kind object)))
      (fail-pattern "Detection does not accept empty or boundary patterns."))
    (when (and (plusp size) (not (and edge (eq :regex (pattern-kind object)))) (find "" patterns :test #'equal))
      (warn-pattern "Empty search patterns are not supported."))
    (let ((out (make-array size)))
      (dotimes (i size)
        (let* ((s (aref strings (if (= 1 (length strings)) 0 i)))
               (p (aref patterns (if (= 1 (length patterns)) 0 i)))
               (empty (and (equal p "") (not (and edge (eq :regex (pattern-kind object)))))))
          (setf (aref out i)
                (if (or (missing-p s) (missing-p p) empty)
                    (if empty cl-vctrs-lite:*na* (missing-result (list s p)))
                    (let ((match
                            (if edge
                                (ecase (pattern-kind object)
                                  (:regex (not (null (icu-regex-spans s
                                              (if (eq edge :start) (format nil "^(~a)" p) (format nil "(~a)$" p))
                                              (pattern-options object)))))
                                  (:fixed (let* ((n (length p)) (len (length s)))
                                            (and (<= n len)
                                                 (not (null (fixed-spans (if (eq edge :start) (subseq s 0 n) (subseq s (- len n)))
                                                                        p (getf (pattern-options object) :ignore-case)))))))
                                  (:coll (icu-coll-edge-p s p (pattern-options object) (eq edge :end))))
                                (not (null (pattern-spans s object p))))))
                      (if negate (not match) match))))))
      (%name-result (cl-vctrs-lite:make-typed-column out :bool) string pattern))))
(defun str-detect (string pattern &key (negate nil))
  "R: stringr::str_detect(). Return pattern detection with optional :NEGATE.
Regex/fixed/coll modifiers, strict scalar recycling, names and NA are retained.
Bare empty and boundary patterns error; NIL remains the legacy missing input."
  (%sr2-detection string pattern negate))
(defun str-starts (string pattern &key (negate nil))
  "R: stringr::str_starts(). Detect a match at the start; :NEGATE inverts definite matches."
  (%sr2-detection string pattern negate :start))
(defun str-ends (string pattern &key (negate nil))
  "R: stringr::str_ends(). Detect a match at the end; fixed/coll suffixes may overlap."
  (%sr2-detection string pattern negate :end))
(defun str-which (string pattern &key (negate nil))
  "R: stringr::str_which(). Return zero-based indices of definite matches, preserving names."
  (let* ((mask (str-detect string pattern :negate negate))
         (indices (loop for match across mask for i from 0 when (eq match t) collect i))
         (out (cl-vctrs-lite:make-typed-column (coerce indices 'vector) :int))
         (names (cl-vctrs-lite:col-names mask)))
    (when names (setf (cl-vctrs-lite:col-names out) (map 'vector (lambda (i) (aref names i)) indices))) out))
(defun str-subset (string pattern &key (negate nil))
  "R: stringr::str_subset(). Keep definite matches and preserve the original column type/names.
NA conditions are dropped. Scalar inputs do not expand when a pattern vector selects positions."
  (let* ((mask (str-detect string pattern :negate negate))
         (indices (loop for match across mask for i from 0 when (eq match t) collect i))
         (named (and string (cl-vctrs-lite:named-list-p string)))
         (values (cond (named (coerce (mapcar #'cdr string) 'vector))
                       ((and (cl-vctrs-lite:column-p string) (not (listp string))) string)
                       ((listp string) (coerce string 'vector)) (t (vector string))))
         (names (%sr2-names string))
         (out (cl-vctrs-lite:restore-column
               (map 'vector (lambda (i) (if (< i (cl-vctrs-lite:col-length values))
                                            (cl-vctrs-lite:col-ref values i) cl-vctrs-lite:*na*)) indices)
               (cl-vctrs-lite:vec-prototype values))))
    (when names (setf (cl-vctrs-lite:col-names out)
                     (map 'vector (lambda (i) (if (< i (length names)) (aref names i) cl-vctrs-lite:*na*)) indices)))
    out))
(defun str-count (string &optional (pattern ""))
  "R: stringr::str_count(). Count nonoverlapping matches; the default counts Unicode graphemes."
  (%check-sr2-pattern pattern)
  (%name-result (pattern-map :count (%sr2-strings string) pattern) string pattern))
(defun %extract-groups (string pattern group)
  (multiple-value-bind (matrix labels names type) (str-match string pattern)
    (declare (ignore type))
    (let* ((size (array-dimension matrix 1)) (height (array-dimension matrix 0))
           (logical (or (eq group t) (and (cl-vctrs-lite:column-p group) (not (listp group))
                                         (eq :bool (cl-vctrs-lite:col-type group)))))
           (groups (if (cl-vctrs-lite:column-p group) (cl-vctrs-lite:col->list group) (list group)))
           ;; Translate native capture indices before applying R matrix-index
           ;; truncation. In particular native -.5 means R group .5 (full match).
           (indices (mapcar (lambda (g)
                              (cond ((cl-vctrs-lite:na-p g) :na)
                                    (logical (if g 2 1))
                                    ((realp g) (truncate (+ 2 g)))
                                    (t (fail-pattern "Group selectors must be numeric or logical.")))) groups))
           (negative (some (lambda (i) (and (integerp i) (minusp i))) indices))
           (selected
             (if negative
                 (progn
                   (when (some (lambda (i) (or (eq i :na) (plusp i))) indices)
                     (fail-pattern "Only zero indices may mix with negative group selectors."))
                   (loop for i below size unless (member (- (1+ i)) indices) collect i))
                 (loop for i in indices unless (eql i 0)
                       collect (if (eq i :na) :na
                                   (progn (when (> i size) (fail-pattern "Capture group is out of bounds.")) (1- i))))))
           (width (length selected))
           (columns (and labels (map 'vector (lambda (i) (if (eq i :na) cl-vctrs-lite:*na* (aref labels i))) selected))))
      (labels ((cell (row index) (if (eq index :na) cl-vctrs-lite:*na* (aref matrix row index))))
        (if (or (= 1 width) (= 1 height))
            (let ((out (cl-vctrs-lite:make-typed-column
                        (coerce (if (= 1 width)
                                    (loop for row below height collect (cell row (first selected)))
                                    (mapcar (lambda (i) (cell 0 i)) selected)) 'vector) :string)))
              ;; Dropping one row repeats that row's name across selected
              ;; cells, including a present empty names attribute. Without
              ;; row names, selected capture labels name nonempty results.
              (cond ((and (= 1 height) names)
                     (setf (cl-vctrs-lite:col-names out)
                           (make-array (length out) :initial-element (aref names 0))))
                    ((and (= 1 height) (> width 1) columns)
                     (setf (cl-vctrs-lite:col-names out) columns))
                    ((/= 1 height) (%name-result out string pattern)))
              out)
            (%matrix-values
             (%string-matrix (loop for row below height collect (mapcar (lambda (i) (cell row i)) selected))
                             width :string columns names)))))))
(defun str-extract (string pattern &key (group nil))
  "R: stringr::str_extract(). Extract a first match or selected capture groups.
Native GROUP 0 is R group 1; -1 selects the full match. Numeric/logical vectors,
missing indices and R column-exclusion semantics are supported. Selected groups
may return a matrix, with labels/row names/:STRING as additional values."
  (%check-sr2-pattern pattern)
  (if group (%extract-groups string pattern group)
      (%name-result (pattern-map :extract (%sr2-strings string) pattern) string pattern)))
(defun str-extract-all (string pattern &key (simplify nil))
  "R: stringr::str_extract_all(). Return list-column vectors of matches, or a rank-two array with :SIMPLIFY.
Matrix additional values are column labels, row names and :STRING; unmatched padding is empty text."
  (boolean-option simplify :simplify)
  (%check-sr2-pattern pattern)
  (let ((out (%name-result (pattern-map :extract-all (%sr2-strings string) pattern) string pattern)))
    (if simplify
        (let* ((width (loop for row across out maximize (length row) into n finally (return (or n 0))))
               (rows (loop for row across out collect
                       (append (coerce row 'list) (make-list (- width (length row)) :initial-element "")))))
          (%matrix-values (%string-matrix rows width :string nil (%preserved-names string pattern))))
        out)))
(defun %locate (string pattern all)
  (multiple-value-bind (strings object patterns size) (%sr2-context string pattern)
    (when (%bare-empty-p pattern object) (setf object (boundary :character)))
    (let ((boundary-p (eq :boundary (pattern-kind object))) (rows nil) (matrices nil) (labels nil))
      (when (and (plusp size) (not boundary-p) (find "" patterns :test #'equal)) (warn-pattern "Empty search patterns are not supported."))
      (dotimes (i size)
        (let* ((s (aref strings (if (= 1 (length strings)) 0 i)))
               (p (aref patterns (if (= 1 (length patterns)) 0 i)))
               (missing (or (missing-p s) (and (not boundary-p) (or (missing-p p) (equal p "")))))
               (matches (if missing (list (list cl-vctrs-lite:*na* cl-vctrs-lite:*na*))
                            (mapcar (lambda (span) (list (car span) (1- (cdr span)))) (pattern-spans s object p)))))
          (if all
              (progn (push (%string-matrix matches 2 :int #("start" "end")) matrices) (push #("start" "end") labels))
              (push (or (first matches) (list cl-vctrs-lite:*na* cl-vctrs-lite:*na*)) rows))))
      (if all
          (let* ((names (%preserved-names string pattern))
                 (out (%pack-matrix-list (nreverse matrices) names)))
            (values out (coerce (nreverse labels) 'vector) names :int))
          (%matrix-values (%string-matrix (nreverse rows) 2 :int #("start" "end") (%preserved-names string pattern)))))))
(defun str-locate (string pattern)
  "R: stringr::str_locate(). Return a start/end matrix with zero-based inclusive endpoints.
Zero-width matches have END=START-1. Additional values: column labels, row names, :INT."
  (%locate string pattern nil))
(defun str-locate-all (string pattern)
  "R: stringr::str_locate_all(). Return proper lists/alists of start/end matrices (D12 edge names use typed :LIST).
Additional values: per-matrix column labels, outer names, :INT. Positions are zero-based inclusive."
  (%locate string pattern t))
(defun %match (string pattern all)
  (multiple-value-bind (strings object patterns size) (%sr2-context string pattern)
    (unless (and (eq :regex (pattern-kind object)) (not (%bare-empty-p pattern object)))
      (fail-pattern "Capture matching requires a regular expression."))
    (when (and (plusp size) (find "" patterns :test #'equal)) (warn-pattern "Empty search patterns are not supported."))
    (let ((counts (make-array (length patterns))) (capture-names (make-array (length patterns)))
          (rows nil) (matrices nil) (labels nil))
      (dotimes (i (length patterns))
        (if (cl-vctrs-lite:na-p (aref patterns i))
            (setf (aref counts i) 1 (aref capture-names i) nil)
            (multiple-value-bind (ignored count names) (icu-regex-groups "" (aref patterns i) (pattern-options object))
              (declare (ignore ignored)) (setf (aref counts i) count (aref capture-names i) names))))
      (dotimes (i size)
        (let* ((at (if (= 1 (length patterns)) 0 i)) (s (aref strings (if (= 1 (length strings)) 0 i)))
               (p (aref patterns at)) (count (aref counts at)) (names (aref capture-names at))
               (matches (if (or (missing-p s) (missing-p p) (equal p "")) (list (make-list count :initial-element cl-vctrs-lite:*na*))
                            (nth-value 0 (icu-regex-groups s p (pattern-options object))))))
          (if all
              (progn (push (%string-matrix matches count :string names) matrices) (push names labels))
              (push (or (first matches) (make-list count :initial-element cl-vctrs-lite:*na*)) rows))))
      (if all
          (let* ((names (%preserved-names string pattern))
                 (out (%pack-matrix-list (nreverse matrices) names)))
            (values out (coerce (nreverse labels) 'vector) names :string))
          (let* ((count (if (plusp (length counts)) (reduce #'max counts) 1))
                 (names (when (= 1 (length patterns)) (aref capture-names 0))))
            (%matrix-values (%string-matrix
                             (loop for row in (nreverse rows) collect (append row (make-list (- count (length row)) :initial-element cl-vctrs-lite:*na*)))
                             count :string names (%preserved-names string pattern))))))))
(defun str-match (string pattern)
  "R: stringr::str_match(). Return a rank-two full-match/capture matrix; unmatched groups are NA.
Additional values: capture labels (or NIL), row names and :STRING. Only regex patterns are accepted."
  (%match string pattern nil))
(defun str-match-all (string pattern)
  "R: stringr::str_match_all(). Return proper lists/alists of capture matrices (D12 edge names use typed :LIST).
Additional values: per-matrix capture labels, outer names, :STRING. Captures retain Unicode character offsets."
  (%match string pattern t))
(defun invert-match (locations)
  "R: stringr::invert_match(). Complement a start/end location matrix.
R boundary sentinels 0/-1 translate uniformly to -1/-2; they are not substring negative offsets.
Additional values are start/end labels, NIL row names and :INT."
  (let ((metadata (gethash locations *string-matrix-metadata*)))
    (unless (and (arrayp locations) (= 2 (array-rank locations)) (= 2 (array-dimension locations 1))
                 (equalp #("start" "end") (first metadata)))
      (fail-pattern "Invert-match requires a labeled start/end matrix."))
    (%matrix-values
     (%string-matrix
      (loop for i from 0 to (array-dimension locations 0)
            collect (list (if (zerop i) -1 (let ((end (aref locations (1- i) 1)))
                                               (if (cl-vctrs-lite:na-p end) end (1+ end))))
                          (if (= i (array-dimension locations 0)) -2
                              (let ((start (aref locations i 0))) (if (cl-vctrs-lite:na-p start) start (1- start))))))
      2 :int #("start" "end")))))
(defun %like-regex (pattern)
  ;; stringr 1.6 retains regex metacharacters and escapes; only unescaped
  ;; SQL wildcards outside [%]/[_] bracket tokens are rewritten.
  (concatenate 'string "^"
    (with-output-to-string (out)
      (loop for i below (length pattern) for c = (char pattern i) do
        (if (and (find c "%_")
                 (not (and (plusp i) (find (char pattern (1- i)) "\\[")))
                 (not (and (< (1+ i) (length pattern)) (char= (char pattern (1+ i)) #\]))))
            (write-string (if (char= c #\%) ".*" ".") out) (write-char c out)))) "$") )
(defun %like (string pattern ignore-case)
  (when (pattern-object-p pattern) (fail-pattern "LIKE requires plain text patterns."))
  (%check-sr2-pattern pattern t)
  ;; Check original lengths before stringr's paste0 conversion turns a zero
  ;; pattern vector into one anchored empty pattern.
  (recycle-size (%sr2-strings string) (string-vector pattern))
  (let ((patterns (string-vector pattern)))
    (unless (every (lambda (x) (or (stringp x) (cl-vctrs-lite:na-p x)))
                   (cond ((stringp pattern) (vector pattern)) ((vectorp pattern) pattern)
                         ((cl-vctrs-lite:named-list-p pattern) (coerce (mapcar #'cdr pattern) 'vector))
                         ((null pattern) #()) ((listp pattern) (coerce pattern 'vector)) (t (vector pattern))))
      (fail-pattern "LIKE patterns must be character vectors."))
    (let ((converted (if (zerop (length patterns)) (vector "^$")
                         (map 'vector (lambda (p) (%like-regex (if (cl-vctrs-lite:na-p p) "NA" p))) patterns))))
      (%name-result (str-detect string (regex converted :ignore-case ignore-case)) string pattern))))
(defun str-like (string pattern &key (ignore-case nil supplied))
  "R: stringr::str_like(). Match SQL percent/underscore wildcards over the entire string.
Case-sensitive by default; supplied :IGNORE-CASE warns as deprecated, matching stringr 1.6."
  (when supplied (warn-pattern "STR-LIKE :IGNORE-CASE is deprecated; use STR-ILIKE.") (boolean-option ignore-case :ignore-case))
  (%like string pattern ignore-case))
(defun str-ilike (string pattern)
  "R: stringr::str_ilike(). Match SQL wildcards with ICU Unicode case-insensitive regex semantics."
  (%like string pattern t))
