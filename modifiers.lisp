(in-package #:cl-stringr)

(defstruct (pattern-object (:constructor %make-pattern (kind values options &optional names))
                           (:conc-name pattern-))
  "An immutable-by-convention stringr pattern vector and modifier options."
  (kind :regex :read-only t) (values #() :read-only t) (options nil :read-only t) (names nil :read-only t))

(defun numeric-string (number)
  #+sbcl
  (when (floatp number)
    (when (sb-ext:float-nan-p number) (return-from numeric-string "NaN"))
    (when (sb-ext:float-infinity-p number)
      (return-from numeric-string (if (minusp number) "-Inf" "Inf"))))
  (if (integerp number) (format nil "~d" number)
      (if (zerop number) "0"
          (let* ((negative (minusp number))
                 (scientific (string-trim '(#\Space) (format nil "~,14e" (abs (coerce number 'double-float)))))
                 (epos (position-if (lambda (c) (find c "eEdDfFsSlL")) scientific))
                 (exponent (parse-integer scientific :start (1+ epos)))
                 (mantissa (string-right-trim "." (string-right-trim "0" (subseq scientific 0 epos))))
                 (digits (remove #\. mantissa))
                 (decimal (1+ exponent))
                 (fixed (cond ((<= decimal 0) (concatenate 'string "0." (make-string (- decimal) :initial-element #\0) digits))
                              ((>= decimal (length digits)) (concatenate 'string digits (make-string (- decimal (length digits)) :initial-element #\0)))
                              (t (concatenate 'string (subseq digits 0 decimal) "." (subseq digits decimal)))))
                 (exp (format nil "~ae~a~2,'0d" mantissa (if (minusp exponent) "-" "+") (abs exponent)))
                 (result (if (<= (length fixed) (length exp)) fixed exp)))
            (if negative (concatenate 'string "-" result) result)))))

(defun finite-time-p (value)
  (and (realp value)
       #+sbcl (not (and (floatp value) (or (sb-ext:float-nan-p value) (sb-ext:float-infinity-p value))))))

(defun time-decimals (seconds)
  (let* ((fraction (- (abs seconds) (floor (abs seconds))))
         (text (numeric-string fraction)) (epos (position #\e text))
         (mantissa (if epos (subseq text 0 epos) text))
         (exponent (if epos (parse-integer text :start (1+ epos)) 0))
         (dot (position #\. mantissa)))
    (if (zerop fraction) 0
        (min 6 (max 0 (- (if dot (- (length mantissa) dot 1) 0) exponent))))))

(defun time-strings (values)
  ;; hms aligns the hour field and uses a shared fractional precision (max 6).
  (let* ((finite (remove-if-not #'finite-time-p (coerce values 'list)))
         (digits (if finite (reduce #'max finite :key #'time-decimals) 0))
         (scale (expt 10 digits))
         (width (max 2 (if finite (reduce #'max finite :key
                          (lambda (s) (length (format nil "~d" (floor (abs s) 3600))))) 2))))
    (map 'vector
         (lambda (seconds)
           (cond ((cl-vctrs-lite:na-p seconds) seconds)
                 ((not (finite-time-p seconds))
                  #+sbcl (if (sb-ext:float-nan-p seconds) cl-vctrs-lite:*na*
                             (if (minusp seconds) "-Inf:NaN:NaNNaN" "Inf:NaN:NaNNaN"))
                  #-sbcl (fail-pattern "Nonfinite hms values are unsupported."))
                 (t (let* ((ticks (round (* (abs seconds) scale)))
                           (whole (floor ticks scale)) (fraction (mod ticks scale))
                           (hours (floor whole 3600)) (minutes (floor (mod whole 3600) 60))
                           (secs (mod whole 60)))
                      (concatenate 'string (if (minusp seconds) "-" "")
                                   (format nil "~v@a:~2,'0d:~2,'0d" width (format nil "~2,'0d" hours) minutes secs)
                                   (if (zerop digits) "" (format nil ".~v,'0d" digits fraction)))))))
         values)))

(defun temporal-string (timestamp kind)
  (multiple-value-bind (nsec second minute hour day month year)
      (local-time:decode-timestamp timestamp :timezone local-time:+utc-zone+)
    (let ((date (format nil "~4,'0d-~2,'0d-~2,'0d" year month day)))
      (if (or (eq kind :date) (and (zerop nsec) (zerop second) (zerop minute) (zerop hour)))
          date
          (concatenate 'string date (format nil " ~2,'0d:~2,'0d:~2,'0d" hour minute second)
                       (if (zerop nsec) "" (string-right-trim "0" (format nil ".~9,'0d" nsec))))))))

(defun bare-string (value)
  (cond ((stringp value) value) ((cl-vctrs-lite:na-p value) value)
        ((eq value t) "TRUE") ((null value) "FALSE")
        ((realp value) (numeric-string value))
        (t (princ-to-string value))))

(define-condition pattern-error (cl-vctrs-lite:tidy-error simple-error) ()
  (:documentation "An invalid stringr pattern, option or operation."))
(define-condition pattern-warning (cl-vctrs-lite:tidy-warning simple-warning) ()
  (:documentation "A recoverable stringr pattern coercion or ICU warning."))
(defun fail-pattern (format-control &rest args)
  (error 'pattern-error :format-control format-control :format-arguments args))
(defun warn-pattern (format-control &rest args)
  (warn 'pattern-warning :format-control format-control :format-arguments args))
(defun input-names (input)
  (cond ((pattern-object-p input) (pattern-names input))
        ((cl-vctrs-lite:named-list-p input) (mapcar #'car input))))
(defun pattern-named-values (object)
  "R: stringr modifier attributes. Return the modifier's character values with names as a string-keyed alist."
  (if (pattern-names object)
      (mapcar #'cons (pattern-names object) (coerce (pattern-values object) 'list))
      (pattern-values object)))
(defun string-vector (input &optional warn-coercion)
  (let* ((named (cl-vctrs-lite:named-list-p input))
         (kind (if (cl-vctrs-lite:column-p input) (cl-vctrs-lite:col-type input) :string))
         (values (cond ((pattern-object-p input) (pattern-values input))
                       ((stringp input) (vector input))
                       (named (coerce (mapcar #'cdr input) 'vector))
                       ((vectorp input) input)
                       ((cl-vctrs-lite:column-p input) (coerce (cl-vctrs-lite:col->list input) 'vector))
                       ((null input) #())
                       ((listp input) (coerce input 'vector))
                       (t (vector input)))))
    (when (and warn-coercion (or (pattern-object-p input) (null input)
                                (and (listp input) (not named))
                                (and (cl-vctrs-lite:column-p input) (not (or (listp input) (vectorp input))))
                                (and (zerop (length values)) (member kind '(:bool :int :double :date :datetime :time :duration :factor)))
                                (not (every (lambda (x) (or (stringp x) (cl-vctrs-lite:na-p x))) values))))
      (warn-pattern "Coercing pattern to a character vector."))
    (if (eq kind :time) (time-strings values)
        (map 'vector (lambda (x) (if (or (cl-vctrs-lite:na-p x) (not (member kind '(:date :datetime))))
                                       (bare-string x) (temporal-string x kind))) values))))

(defun boolean-option (value key)
  (unless (member value '(t nil)) (fail-pattern "~a must be TRUE or FALSE." key)) value)

(defun validate-options (options boolean-keys other-keys)
  (unless (evenp (length options)) (fail-pattern "Options must be key/value pairs."))
  (loop for (key value) on options by #'cddr
        do (cond ((member key boolean-keys) (boolean-option value key))
                 ((member key other-keys))
                 (t (fail-pattern "Unknown pattern option ~a." key))))
  options)

(defun regex (pattern &rest options &key (ignore-case nil) (multiline nil)
                                       (comments nil) (dotall nil) &allow-other-keys)
  "R: stringr::regex(). Construct an ICU regular-expression pattern vector.
Options include :ignore-case, :multiline, :comments, :dotall, :literal,
:unix-lines, :uword, :error-on-unknown-escapes, :time-limit, :stack-limit.
NA and empty vectors are retained; syntax is validated when consumed."
  (validate-options options '(:ignore-case :multiline :comments :dotall)
                    '(:literal :unix-lines :uword :error-on-unknown-escapes
                      :time-limit :stack-limit :dot-all :multi-line))
  (%make-pattern :regex (string-vector pattern t)
                 (append options (list :ignore-case ignore-case :multiline multiline
                                       :comments comments :dotall dotall)) (input-names pattern)))

(defun fixed (pattern &rest options &key (ignore-case nil) &allow-other-keys)
  "R: stringr::fixed(). Match literal characters with optional simple case folding.
PATTERN can be scalar text, a character vector, empty, or NA."
  (validate-options options '(:ignore-case) nil)
  (boolean-option ignore-case :ignore-case)
  (%make-pattern :fixed (string-vector pattern t) (list :ignore-case ignore-case) (input-names pattern)))

(defun coll (pattern &rest options &key (ignore-case nil) (locale "en") &allow-other-keys)
  "R: stringr::coll(). Match with native ICU locale-aware Unicode collation.
:locale defaults to English; ICU strength and collation options are accepted.
NA/empty values are retained; invalid ICU options fail when consumed."
  (validate-options options '(:ignore-case)
                    '(:locale :strength :uppercase-first :alternate-shifted :case-level
                      :normalization :normalisation :french :numeric))
  (boolean-option ignore-case :ignore-case)
  (unless (stringp locale) (fail-pattern "Locale must be a string."))
  (%make-pattern :coll (string-vector pattern t)
                 (append options (list :ignore-case ignore-case :locale locale)) (input-names pattern)))

(defun boundary (&optional (type :character) &rest options &key
                    (skip-word-none cl-vctrs-lite:*na*) (locale "en") &allow-other-keys)
  "R: stringr::boundary(). Segment Unicode characters, words, lines or sentences.
TYPE is :character, :word, :line-break or :sentence (string names also work).
:skip-word-none defaults to true for words and false for other boundaries."
  (let ((kind (cond ((keywordp type) type)
                    ((stringp type) (intern (string-upcase (substitute #\- #\_ type)) :keyword))
                    (t (fail-pattern "Boundary type must be a single name.")))))
    (unless (member kind '(:character :word :line-break :sentence)) (fail-pattern "Invalid boundary type."))
    (unless (cl-vctrs-lite:na-p skip-word-none) (boolean-option skip-word-none :skip-word-none))
    (validate-options (loop for (key value) on options by #'cddr unless (eq key :skip-word-none)
                           append (list key value))
                      nil '(:locale :skip-word-number :skip-word-letter :skip-word-kana :skip-word-ideo
                        :skip-line-soft :skip-line-hard :skip-sentence-term :skip-sentence-sep))
    (unless (stringp locale) (fail-pattern "Locale must be a string."))
    (%make-pattern :boundary (vector cl-vctrs-lite:*na*)
                   (append (list :type kind :skip-word-none
                                 (if (cl-vctrs-lite:na-p skip-word-none) (eq kind :word) skip-word-none))
                           (loop for (key value) on options by #'cddr
                                 unless (eq key :skip-word-none) append (list key value))
                           (list :locale locale)))))

(defun str-escape (string)
  "R: stringr::str_escape(). Escape ICU regular-expression metacharacters.
NA is retained; names, vector size and empty inputs are preserved."
  (let ((result (map 'vector (lambda (s) (if (cl-vctrs-lite:na-p s) s
                              (with-output-to-string (out)
                                (loop for char across s do
                                  (when (find char ".^$\\|*+?{}[]()") (write-char #\\ out))
                                  (write-char char out))))) (string-vector string))))
    (if (input-names string) (mapcar #'cons (input-names string) (coerce result 'list))
        (cl-vctrs-lite:make-typed-column result :string))))

(defun fixed-spans (string pattern ignore-case)
  (loop with start = 0 with n = (length pattern)
        for position = (search pattern string :start2 start
                               :test (if ignore-case (lambda (a b) (= (icu-call "u_toupper" :int32 (char-code a) :int32)
                                                               (icu-call "u_toupper" :int32 (char-code b) :int32))) #'char=))
        while position collect (cons position (+ position n))
        do (setf start (+ position n))))

(defun pattern-spans (string object value)
  (ecase (pattern-kind object)
    (:regex (icu-regex-spans string value (pattern-options object)))
    (:fixed (fixed-spans string value (getf (pattern-options object) :ignore-case)))
    (:coll (icu-coll-spans string value (pattern-options object)))
    (:boundary (icu-boundary-spans string (pattern-options object)))))

(defun pattern-scanner (pattern &key ignore-case)
  "R: stringr modifiers. Compile a scalar regex/fixed/coll pattern for legacy CL-PPCRE integrations.
Use PATTERN-MATCHES-P for native ICU matching; a scanner cannot express all
ICU Unicode or locale semantics. Empty, NA and boundary patterns signal errors."
  (let* ((object (if (pattern-object-p pattern) pattern (regex pattern :ignore-case ignore-case)))
         (values (pattern-values object)))
    (unless (and (= 1 (length values)) (stringp (aref values 0))
                 (plusp (length (aref values 0))) (not (eq (pattern-kind object) :boundary)))
      (fail-pattern "A scanner needs a nonempty scalar pattern."))
    (cl-ppcre:create-scanner (if (eq (pattern-kind object) :regex) (aref values 0)
                                (aref (str-escape (aref values 0)) 0))
                            :case-insensitive-mode (getf (pattern-options object) :ignore-case)
                            :multi-line-mode (getf (pattern-options object) :multiline)
                            :single-line-mode (getf (pattern-options object) :dotall)
                            :extended-mode (getf (pattern-options object) :comments))))

(eval-when (:load-toplevel :execute)
  (dolist (entry '((pattern-object-p . "R: stringr modifier objects. Test for a native pattern object.")
                   (pattern-kind . "R: stringr modifier class. Return :regex, :fixed, :coll or :boundary.")
                   (pattern-values . "R: stringr modifier vector. Return the stored character values, including NA.")
                   (pattern-options . "R: stringr modifier options. Return native options as a keyword plist.")
                   (pattern-names . "R: names() on a stringr modifier. Return optional string names.")))
    (setf (documentation (car entry) 'function) (cdr entry))))
