(in-package #:cl-stringr)

;;; Native ICU works on UTF-16 offsets; public stringr indices remain Lisp
;;; character offsets. Every opened matcher/iterator/collator is closed in
;;; UNWIND-PROTECT, including errors and warnings.
(defvar *icu-library* nil)
(defvar *icu-suffix* nil)
(defvar *icu-symbols* (make-hash-table :test #'equal))

(defun close-icu-library (library)
  (unless (cffi:null-pointer-p library)
    (cffi:foreign-funcall "dlclose" :pointer library :int)))
(defun scoped-icu-symbol (name library)
  (let ((pointer (cffi:foreign-funcall "dlsym" :pointer library :string name :pointer)))
    (unless (cffi:null-pointer-p pointer) pointer)))

(defun %initialize-icu (configured)
  (unless *icu-library*
    (let* ((local (mapcar (lambda (file) (namestring (asdf:system-relative-pathname
                              :cl-stringr (concatenate 'string ".native/icu-71.1/lib/" file))))
                          '("libicui18n.dylib" "libicui18n.so")))
           (names (if configured (list configured)
                      (append local '("libicui18n.so" "libicui18n.dylib" "/usr/lib/libicucore.A.dylib")))))
      (setf *icu-library* (loop for name in names
                               for lib = (let ((handle (cffi:foreign-funcall "dlopen" :string name :int 2 :pointer)))
                                           (unless (cffi:null-pointer-p handle) handle))
                               when lib return lib
                               finally (fail-pattern "ICU 71.1 is required. Run scripts/bootstrap-icu.sh or set CL_STRINGR_ICU_LIBRARY."))))
    (setf *icu-suffix* (loop for suffix in (cons "" (loop for v from 99 downto 50 collect (format nil "_~d" v)))
                            when (scoped-icu-symbol (concatenate 'string "uregex_open" suffix) *icu-library*) return suffix
                            finally (fail-pattern "ICU regex symbols are unavailable.")))
    (cffi:with-foreign-object (version :uint8 4)
      (cffi:foreign-funcall-pointer (icu-symbol "u_getVersion") () :pointer version :void)
      (unless (and (= 71 (cffi:mem-aref version :uint8 0)) (= 1 (cffi:mem-aref version :uint8 1)))
        (let ((major (cffi:mem-aref version :uint8 0)) (minor (cffi:mem-aref version :uint8 1)))
          (close-icu-library *icu-library*)
          (setf *icu-library* nil *icu-suffix* nil)
          (clrhash *icu-symbols*)
          (fail-pattern "ICU ~d.~d differs from pinned ICU 71.1. Run scripts/bootstrap-icu.sh or set CL_STRINGR_ICU_LIBRARY." major minor)))))
  *icu-library*)

(defun load-icu (&optional (configured (uiop:getenv "CL_STRINGR_ICU_LIBRARY")))
  (handler-case (%initialize-icu configured)
    (error (condition)
      (when *icu-library* (ignore-errors (close-icu-library *icu-library*)))
      (setf *icu-library* nil *icu-suffix* nil)
      (clrhash *icu-symbols*)
      (error condition))))

(defun icu-symbol (name)
  (unless *icu-library* (load-icu))
  (or (gethash name *icu-symbols*)
      (setf (gethash name *icu-symbols*)
            (or (scoped-icu-symbol (concatenate 'string name *icu-suffix*) *icu-library*)
                (fail-pattern "ICU function ~a is unavailable." name)))))

(defmacro icu-call (name &rest args)
  `(cffi:foreign-funcall-pointer (icu-symbol ,name) () ,@args))

(defun check-icu (status operation)
  (let ((code (cffi:mem-ref status :int32)))
    (when (plusp code) (fail-pattern "ICU ~a failed (status ~d)." operation code))))

(defun utf16-units (string)
  (let ((units (make-array 0 :element-type '(unsigned-byte 16) :adjustable t :fill-pointer 0))
        (indices (make-array 0 :adjustable t :fill-pointer 0)))
    (loop for char across string for index from 0 for code = (char-code char)
          do (vector-push-extend index indices)
             (if (< code #x10000)
                 (vector-push-extend code units)
                 (let ((n (- code #x10000)))
                   (vector-push-extend (+ #xd800 (ash n -10)) units)
                   (vector-push-extend index indices)
                   (vector-push-extend (+ #xdc00 (logand n #x3ff)) units))))
    (vector-push-extend (length string) indices)
    (values units indices)))

(defmacro with-utf16 ((pointer length indices string) &body body)
  (let ((units (gensym)))
    `(multiple-value-bind (,units ,indices) (utf16-units ,string)
       (let ((,length (length ,units)))
         (cffi:with-foreign-object (,pointer :uint16 (1+ ,length))
           (dotimes (i ,length) (setf (cffi:mem-aref ,pointer :uint16 i) (aref ,units i)))
           (setf (cffi:mem-aref ,pointer :uint16 ,length) 0)
           (locally ,@body))))))

(defun native-boolean (value key)
  (when (cl-vctrs-lite:na-p value) (fail-pattern "~a cannot be NA when consumed." key))
  (cond ((null value) nil) ((eq value t) t) ((numberp value) (not (zerop value)))
        (t (fail-pattern "~a must be logical or numeric." key))))

(defun regex-flags (options)
  (loop for (key flag) on '(:unix-lines 1 :ignore-case 2 :comments 4
                           :multiline 8 :literal 16 :dotall 32 :uword 256
                           :error-on-unknown-escapes 512)
          by #'cddr when (native-boolean (getf options key) key) sum flag))

(defun apply-regex-limits (matcher options status)
  (dolist (option '((:time-limit . "uregex_setTimeLimit") (:stack-limit . "uregex_setStackLimit")))
    (when (getf options (car option))
      (let ((value (getf options (car option))))
        (unless (and (realp value) (<= -2147483648 value 2147483647))
          (fail-pattern "Invalid regex resource limit."))
        (cffi:foreign-funcall-pointer (icu-symbol (cdr option)) () :pointer matcher
            :int32 (truncate value) :pointer status :void))))
  (check-icu status "regex options"))

(defun check-locale (locale)
  ;; ICU returns U_USING_DEFAULT_WARNING even for valid English locales; stringi
  ;; warns only when the requested locale language is not in its resource set.
  (let* ((language (subseq locale 0 (or (position #\_ locale) (position #\- locale) (length locale))))
         (count (icu-call "uloc_countAvailable" :int32)))
    (unless (or (equal locale "") (equal locale "root")
                (loop for i below count for name = (icu-call "uloc_getAvailable" :int32 i :string)
                      thereis (string= language name :end2 (or (position #\_ name) (length name)))))
      (warn-pattern "ICU locale ~a falls back to the default locale." locale))))

(defun icu-regex-spans (string pattern options)
  (with-utf16 (pat plen pmap pattern)
    (declare (ignore pmap))
    (with-utf16 (text tlen indices string)
      (cffi:with-foreign-object (status :int32)
        (setf (cffi:mem-ref status :int32) 0)
        (let ((matcher (icu-call "uregex_open" :pointer pat :int32 plen
                                 :uint32 (regex-flags options) :pointer (cffi:null-pointer)
                                 :pointer status :pointer)))
          (check-icu status "regex compilation")
          (unwind-protect
               (progn
                 (icu-call "uregex_setText" :pointer matcher :pointer text :int32 tlen :pointer status :void)
                 (apply-regex-limits matcher options status)
                 (loop while (not (zerop (icu-call "uregex_findNext" :pointer matcher :pointer status :int8)))
                       for start = (icu-call "uregex_start" :pointer matcher :int32 0 :pointer status :int32)
                       for end = (icu-call "uregex_end" :pointer matcher :int32 0 :pointer status :int32)
                       collect (cons (aref indices start) (aref indices end)) into spans
                       finally (check-icu status "regex matching") (return spans)))
            (icu-call "uregex_close" :pointer matcher :void)))))))

(defun with-collator (options function)
  (check-locale (getf options :locale "en"))
  (cffi:with-foreign-object (status :int32)
    (setf (cffi:mem-ref status :int32) 0)
    (let ((collator (icu-call "ucol_open" :string (getf options :locale "en") :pointer status :pointer)))
      (check-icu status "collator locale")
      (unwind-protect
           (progn
             (let ((strength (getf options :strength)))
               (icu-call "ucol_setStrength" :pointer collator
                         :int (if strength (case (if (realp strength) (truncate strength) strength) (1 0) (2 1) (3 2) (4 3) (5 15)
                                                   (otherwise (fail-pattern "Invalid collation strength.")))
                                  (if (getf options :ignore-case) 1 2)) :void))
             (dolist (entry '((:french . 0) (:alternate-shifted . 1) (:uppercase-first . 2)
                              (:case-level . 3) (:normalization . 4)
                              (:numeric . 7)))
               (when (member (car entry) options)
                 (let* ((v (getf options (car entry)))
                        (setting (case (car entry)
                                   (:alternate-shifted (if (native-boolean v :alternate-shifted) 20 21))
                                   (:uppercase-first (if (cl-vctrs-lite:na-p v) 16
                                                       (if (native-boolean v :uppercase-first) 25 24)))
                                   (otherwise (if (native-boolean v (car entry)) 17 16)))))
                   (icu-call "ucol_setAttribute" :pointer collator :int (cdr entry) :int setting
                             :pointer status :void))))
             (when (and (member :normalisation options) (not (member :normalization options)))
               (icu-call "ucol_setAttribute" :pointer collator :int 4
                         :int (if (native-boolean (getf options :normalisation) :normalisation) 17 16)
                         :pointer status :void))
             (check-icu status "collator options")
             (funcall function collator status))
        (icu-call "ucol_close" :pointer collator :void)))))

(defun icu-coll-spans (string pattern options)
  (with-collator options
    (lambda (collator status)
      (when (zerop (length string)) (return-from icu-coll-spans nil))
      (with-utf16 (pat plen pmap pattern)
        (declare (ignore pmap))
        (with-utf16 (text tlen indices string)
          (let ((searcher (icu-call "usearch_openFromCollator" :pointer pat :int32 plen
                                    :pointer text :int32 tlen :pointer collator
                                    :pointer (cffi:null-pointer) :pointer status :pointer)))
            (check-icu status "collation search")
            (unwind-protect
                 (loop for start = (icu-call "usearch_next" :pointer searcher :pointer status :int32)
                       until (= start -1)
                       for len = (icu-call "usearch_getMatchedLength" :pointer searcher :int32)
                       collect (cons (aref indices start) (aref indices (+ start len))) into spans
                       finally (check-icu status "collation search") (return spans))
              (icu-call "usearch_close" :pointer searcher :void))))))))

(defun icu-equal-p (x y options)
  (with-collator options
    (lambda (collator status)
      (declare (ignore status))
      (with-utf16 (xp xlen xm x)
        (declare (ignore xm))
        (with-utf16 (yp ylen ym y)
          (declare (ignore ym))
          (zerop (icu-call "ucol_strcoll" :pointer collator :pointer xp :int32 xlen
                          :pointer yp :int32 ylen :int)))))))

(defun icu-boundary-spans (string options)
  (check-locale (getf options :locale "en"))
  (loop for (key value) on options by #'cddr
        when (search "SKIP-" (symbol-name key)) do (native-boolean value key))
  (with-utf16 (text len indices string)
    (cffi:with-foreign-object (status :int32)
      (setf (cffi:mem-ref status :int32) 0)
      (let* ((kind (getf options :type :character))
             (iterator (icu-call "ubrk_open" :int (ecase kind (:character 0) (:word 1) (:line-break 2) (:sentence 3))
                                 :string (getf options :locale "en") :pointer text :int32 len :pointer status :pointer)))
        (check-icu status "boundary iterator")
        (unwind-protect
             (loop for start = (icu-call "ubrk_first" :pointer iterator :int32) then end
                   for end = (icu-call "ubrk_next" :pointer iterator :int32)
                   until (= end -1)
                   for rule = (icu-call "ubrk_getRuleStatus" :pointer iterator :int32)
                   for skip = (native-boolean (case kind
                                (:word (getf options (cond ((< rule 100) :skip-word-none)
                                                           ((< rule 200) :skip-word-number)
                                                           ((< rule 300) :skip-word-letter)
                                                           ((< rule 400) :skip-word-kana)
                                                           (t :skip-word-ideo))))
                                (:line-break (getf options (if (< rule 100) :skip-line-soft :skip-line-hard)))
                                (:sentence (getf options (if (< rule 100) :skip-sentence-term :skip-sentence-sep)))) :boundary-skip)
                   unless skip collect (cons (aref indices start) (aref indices end)))
          (icu-call "ubrk_close" :pointer iterator :void))))))

(defun utf16-to-string (pointer length)
  (coerce
   (loop for i from 0 below length for code = (cffi:mem-aref pointer :uint16 i)
         collect (code-char (if (<= #xd800 code #xdbff)
                               (+ #x10000 (ash (- code #xd800) 10)
                                  (- (cffi:mem-aref pointer :uint16 (incf i)) #xdc00)) code)))
   'string))

(defun icu-replacement (replacement)
  ;; R stringr uses backslash-digit capture references; ICU uses dollars.
  (with-output-to-string (out)
    (loop for i from 0 below (length replacement) for char = (char replacement i)
          do (cond ((char= char #\$) (write-string "\\$" out))
                   ((char= char #\\)
                    (when (< (1+ i) (length replacement))
                      (let ((next (char replacement (incf i))))
                        (if (digit-char-p next)
                            (progn (write-char #\$ out) (write-char next out))
                            (progn (write-char #\\ out) (write-char next out))))))
                   (t (write-char char out))))))

(defun icu-regex-replace (string pattern replacement options all)
  (with-utf16 (pat plen pm pattern)
    (declare (ignore pm))
    (with-utf16 (text tlen tm string)
      (declare (ignore tm))
      (with-utf16 (rep rlen rm (icu-replacement replacement))
        (declare (ignore rm))
        (cffi:with-foreign-object (status :int32)
          (setf (cffi:mem-ref status :int32) 0)
          (let ((matcher (icu-call "uregex_open" :pointer pat :int32 plen
                                   :uint32 (regex-flags options) :pointer (cffi:null-pointer)
                                   :pointer status :pointer)))
            (check-icu status "replacement compilation")
            (unwind-protect
                 (progn
                   (icu-call "uregex_setText" :pointer matcher :pointer text :int32 tlen :pointer status :void)
                   (apply-regex-limits matcher options status)
                   (let* ((function (icu-symbol (if all "uregex_replaceAll" "uregex_replaceFirst")))
                          (required (cffi:foreign-funcall-pointer function () :pointer matcher :pointer rep :int32 rlen
                                      :pointer (cffi:null-pointer) :int32 0 :pointer status :int32)))
                     (unless (= (cffi:mem-ref status :int32) 15) (check-icu status "replacement sizing"))
                     (setf (cffi:mem-ref status :int32) 0)
                     (cffi:with-foreign-object (dest :uint16 (1+ required))
                       (let ((actual (cffi:foreign-funcall-pointer function () :pointer matcher :pointer rep :int32 rlen
                                        :pointer dest :int32 (1+ required) :pointer status :int32)))
                         (check-icu status "replacement")
                         (utf16-to-string dest actual)))))
              (icu-call "uregex_close" :pointer matcher :void))))))))

(defun icu-regex-groups (string pattern options)
  "Return every full/capture substring, capture count and optional names.
UTF-16 boundaries translate to Lisp character indices; unmatched groups are NA."
  (with-utf16 (pat plen pmap pattern)
    (declare (ignore pmap))
    (with-utf16 (text tlen indices string)
      (cffi:with-foreign-object (status :int32)
        (setf (cffi:mem-ref status :int32) 0)
        (let ((matcher (icu-call "uregex_open" :pointer pat :int32 plen
                                 :uint32 (regex-flags options) :pointer (cffi:null-pointer)
                                 :pointer status :pointer)))
          (check-icu status "capture compilation")
          (unwind-protect
               (progn
                 (icu-call "uregex_setText" :pointer matcher :pointer text :int32 tlen :pointer status :void)
                 (apply-regex-limits matcher options status)
                 (let* ((count (icu-call "uregex_groupCount" :pointer matcher :pointer status :int32))
                        (labels (make-array (1+ count) :initial-element "")) (named nil) (rows nil))
                   ;; Candidate names are checked against the compiled matcher,
                   ;; so lookbehind, escaped tokens and comments cannot invent groups.
                   (loop for at = (search "(?<" pattern) then (search "(?<" pattern :start2 (+ at 3))
                         while at for end = (position #\> pattern :start (+ at 3))
                         when end do
                           (let ((name (subseq pattern (+ at 3) end)))
                             (when (and (plusp (length name)) (every #'alphanumericp name))
                               (with-utf16 (np nlen nm name)
                                 (declare (ignore nm))
                                 (let ((group (icu-call "uregex_groupNumberFromName" :pointer matcher :pointer np
                                                       :int32 nlen :pointer status :int32)))
                                   (if (and (zerop (cffi:mem-ref status :int32)) (<= 1 group count))
                                       (setf (aref labels group) name named t)
                                       (setf (cffi:mem-ref status :int32) 0)))))))
                   (loop while (not (zerop (icu-call "uregex_findNext" :pointer matcher :pointer status :int8))) do
                     (push (loop for group from 0 to count
                                 for start = (icu-call "uregex_start" :pointer matcher :int32 group :pointer status :int32)
                                 for end = (icu-call "uregex_end" :pointer matcher :int32 group :pointer status :int32)
                                 collect (if (minusp start) cl-vctrs-lite:*na*
                                             (subseq string (aref indices start) (aref indices end)))) rows))
                   (check-icu status "capture matching")
                   (values (nreverse rows) (1+ count) (and named labels))))
            (icu-call "uregex_close" :pointer matcher :void)))))))

(defun icu-coll-edge-p (string pattern options end-p)
  "Use ICU's first/last search so overlapping suffix matches remain visible."
  (with-collator options
    (lambda (collator status)
      (when (zerop (length string)) (return-from icu-coll-edge-p nil))
      (with-utf16 (pat plen pm pattern)
        (declare (ignore pm))
        (with-utf16 (text tlen tm string)
          (declare (ignore tm))
          (let ((searcher (icu-call "usearch_openFromCollator" :pointer pat :int32 plen
                                   :pointer text :int32 tlen :pointer collator
                                   :pointer (cffi:null-pointer) :pointer status :pointer)))
            (check-icu status "collation edge search")
            (unwind-protect
                 (let* ((start (cffi:foreign-funcall-pointer (icu-symbol (if end-p "usearch_last" "usearch_first")) ()
                                                           :pointer searcher :pointer status :int32))
                        (size (if (= start -1) 0 (icu-call "usearch_getMatchedLength" :pointer searcher :int32))))
                   (check-icu status "collation edge search")
                   (and (/= start -1) (if end-p (= (+ start size) tlen) (zerop start))))
              (icu-call "usearch_close" :pointer searcher :void))))))))
