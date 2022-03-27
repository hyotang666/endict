(in-package :cl-user)

(defpackage :endict
  (:use :cl)
  (:export ;;;; CONFIGURATION
           #:*print-human-friendly*
           ;;;; MAIN API
           #:load-dictionary.txt
           ;;;; INTERFACES
           #:article ; for etym and defintion.
           ;;;; OBJECTS
           ;;;; WORD
           #:word ; type name.
           ;; readers.
           #:word-name
           #:word-pronounce
           #:word-suffix
           #:word-syllable
           #:word-plural
           #:word-classes
           #:word-etyms
           #:word-definitions
           ;;;; PLURAL
           #:plural
           #:single
           #:plural-defs
           #:single-defs
           ;;;; ETYM
           #:etym ; type name.
           #:categorized-etym ; type name.
           ;; reader.
           #:etym-article
           #:categorized-etym-category
           ;;;; DEFINITION
           #:anonymous-definition
           #:definition
           #:numbering-definition
           #:note
           ;; reader
           #:numbering-definition-label))

(in-package :endict)

(declaim (optimize speed))

;;;; CONFIGURATION

(defvar *print-human-friendly*
  nil
  "Control that printing notation is human friendly rather than lisp friendly.
In other words, binds *print-readably* to NIL.
This is used to use implementation-specific #S notation without #A notation for string.
The printing check flow is done in order of Friendly, Readably or Escape")

(defun skip-header (input)
  (loop :for line := (read-line input nil nil)
        :while line
        :if (uiop:string-prefix-p "Produced by" line)
          :do (peek-char t input) ; to discard empty lines.
              (loop-finish)))

(defun slurp-line (input)
  (let ((name (read-line input nil nil)))
    (when name
      (locally
       (declare (simple-string name))
       (string-right-trim #.(string #\Return) name)))))

(defun section (name input)
  "Slurp the section of the NAME from INPUT. Return two values.
1. A list of the lines.
2. A name of next section if exists otherwise NIL."
  (do ((line #0=(slurp-line input) #0#)
       acc
       (ignore '("M." "P." "X.")))
      ((null line) (values (cons name (nreverse acc)) nil))
    (locally ; Without locally, the declaration scope includes end form and
             ; update form above.
     (declare (type (simple-array character (*)) line name))
     (if acc
         (unless (member line ignore :test #'equal)
           (if (and ;; The name must have one alphabet at least.
                    (find-if #'upper-case-p line)
                    (every
                      (lambda (c)
                        (or (upper-case-p c) (not (alphanumericp c))))
                      line)
                    ;; The name never have bracket nor paren.
                    (not (find-if (lambda (char) (find char "[]()")) line))
                    ;; The name never start with "&".
                    (not (uiop:string-prefix-p "&" line))
                    ;; The name never have "..".
                    (not (search ".." line))
                    ;; Next name must start with same alphabet or next alphabet of the NAME.
                    (or (string= line name :end1 1 :end2 1)
                        (char= (char line 0)
                               (code-char (1+ (char-code (char name 0))))))
                    ;; The name line must below empty line.
                    (equal "" (car acc)))
               (return (values (cons name (nreverse acc)) line))
               (push line acc)))
         (if (and (not (equal "" line))
                  (every (lambda (c) (or (upper-case-p c) (char= #\Space c)))
                         line))
             (setq name (format nil "~A ~A" name line))
             (push line acc))))))

;;;; NAME

(defun name (name) (delete "" (the list (ppcre:split "; " name)) :test #'equal))

;;;; SECONDARY-SECTION

(defun secondary-section (section)
  "Accept SECTION. Return two values.
  1. A line which should have pronouce, word-class and Etym.
  2. The SECTION which lacks secondary section part.
NOTE: First value may NIL and warned if such line does not exist."
  (loop :for (content . rest) :of-type (simple-string . list) :on (cdr section)
        :if (or (equal "" content)
                (and (ppcre:scan "^ *\\(a\\)" content)
                     (setq rest (cons content rest))))
          :do (loop-finish)
        :collect content :into results
        :finally (return
                  (if results
                      (values (format nil "~{~A~^ ~}" results) rest)
                      (values (warn "Missing secondary section for ~S"
                                    (car section))
                              rest)))))

;;;; INTERFACES

(defgeneric article (obj))

;;;; ETYM

(defstruct etym (article (error "ARTICLE is required.") :type string))

(defmethod article ((this etym)) (etym-article this))

(defun pprint-article (output article &rest noise)
  (declare (ignore noise))
  (pprint-logical-block (output nil)
    (loop :for (line . rest) :of-type (simple-string . list) :on article
          :if (search "  " line)
            :do (write-line line output)
          :else
            :do (funcall (formatter "~{~A~^ ~:_~}") output
                         (uiop:split-string line :separator " "))
                (when rest
                  (write-char #\Space output)
                  (pprint-newline :fill output)))))

(defmethod print-object ((this etym) output)
  (cond
    (*print-human-friendly*
     (let (*print-readably*)
       (call-next-method)))
    (*print-readably* (call-next-method))
    (*print-escape*
     (print-unreadable-object (this output :type t :identity t)))
    (*print-pretty*
     (pprint-logical-block (output nil :prefix "Etym: [" :suffix "]")
       (pprint-article output (list (etym-article this)))))
    (t (funcall (formatter "Etym: [~A]") output (etym-article this)))))

(defstruct (categorized-etym (:include etym))
  (category (error "CATEGORY is required.") :type string))

(defmethod print-object ((this categorized-etym) output)
  (call-next-method)
  (cond ((or *print-human-friendly* *print-readably* *print-escape*))
        (*print-pretty*
         (pprint-logical-block (output nil :prefix "(" :suffix ")")
           (pprint-article output (list (categorized-etym-category this)))))
        (t
         (funcall (formatter "(~A)") output (categorized-etym-category this)))))

(declaim
 (ftype (function (simple-string)
         (values (or null simple-string) (or null simple-string) &optional))
        parse-category))

(defun parse-category (definition)
  (multiple-value-bind (start end)
      (ppcre:scan "\\([^\\)]+\\) *$" definition)
    (if start
        (values (subseq definition
                        (if (uiop:string-prefix-p "[" definition)
                            1
                            0)
                        (or (position #\] definition :end start :from-end t)
                            start))
                (subseq definition (1+ start)
                        (position #\) definition :start start :end end)))
        (values (string-trim "[]" definition) nil))))

(declaim
 (ftype (function ((or null simple-string))
         (values list (or null simple-string) &optional))
        etym))

(defun etym (secondary-section)
  "Return two values.
  1. A List of ETYM objects.
  2. The SECONDARY-SECTION string which lacks etym part."
  (let ((position (ppcre:scan " *Etym:" secondary-section)))
    (if position
        (locally
         (declare (simple-string secondary-section))
         (values (loop :for content :of-type simple-string
                            :in (ppcre:split " *Etym: *" secondary-section
                                             :start position)
                       :unless (equal "" content)
                         :collect (multiple-value-bind (article category)
                                      (parse-category content)
                                    (if category
                                        (make-categorized-etym :article article
                                                               :category category)
                                        (make-etym :article article))))
                 (subseq secondary-section 0 position)))
        (values nil secondary-section))))

(defun discard-option (but-etym)
  ;; For simplicity's sake, we discard the optional part due to its chaotic format. WTF.
  ;; NOTE: This is designed to be used for the second value of ETYM.
  (let* (;; To discard garbage i.e. "(),".
         (canonicalized
          (the (or (simple-array character (*)) simple-base-string)
               (ppcre:regex-replace-all "\\(\\)," but-etym "")))
         (result
          (let ((position (ppcre:scan " *[([]" canonicalized)))
            (if position
                (subseq canonicalized 0 position)
                canonicalized))))
    (assert (not (equal "" result)))
    result))

(deftype word-class ()
  '(member :noun
           :plural :single
           :article :verb
           :adverb :obs
           :interjection :preposition
           :conjunction :past-participle))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; To muffle compiler claims as 'undefined variable:'.
  (unless (boundp '+word-class+)
    (defconstant +word-class+
      '(("n." . :noun) ("n.." . :noun) ("n. pl." . :plural) ("n.pl." . :plural)
        ("pl." . :plural) ("n. sing." . :single) ("n.sing." . :single)
        ("a." . :article) ("a" . :article) ("v. t." . :verb) ("v.t." . :verb)
        ("v.t" . :verb) ("v. i." . :varb) ("v.i." . :varb) ("v." . :verb)
        ("adv." . :adverb) ("i." . :verb) ("obs." . :obs)
        ("interj." . :interjection) ("prep." . :preposition)
        ("conj." . :conjunction) ("p. p." . :past-participle)))))

(defun parse-word-class (string)
  (loop :for token :in (ppcre:split " ?& ?" string)
        :for word-class = (cdr (assoc token +word-class+ :test #'equal))
        :when word-class
          :collect word-class))

;;;; PRONOUNCE

(defun pronounce (pronounce-part)
  "Return two values.
  1. List of pronounces.
  2. List of word-classes."
  (let ((split (ppcre:split ", ?" pronounce-part)))
    (case (list-length split)
      (0 (values nil nil))
      (1 (values (list (string-trim "." (the simple-string (car split)))) nil))
      (2
       (destructuring-bind
           (first second)
           split
         (values (list first) (parse-word-class second))))
      ;; For simplicity's sake, we use only first pronounce and last word-class.
      (otherwise
       (values (list (car split))
               (loop :for elt :in (reverse (cdr split))
                     :for word-class := (parse-word-class elt)
                     :when word-class
                       :return word-class))))))

;;;; PLURAL

(defstruct plural (defs (error "DEFS is required.") :type list #|of string|#))

(defmethod print-object ((this plural) output)
  (cond
    (*print-human-friendly*
     (let (*print-readably*)
       (call-next-method)))
    (*print-readably* (call-next-method))
    (*print-escape*
     (print-unreadable-object (this output :type t)
       (write (car (plural-defs this)) :stream output :escape t)))
    (t (funcall (formatter "pl. ~{~A~^ ~}") output (plural-defs this)))))

(defstruct single (defs (error "DEFS is required.") :type list #|of string|#))

(defmethod print-object ((this single) output)
  (cond
    (*print-human-friendly*
     (let (*print-readably*)
       (call-next-method)))
    (*print-readably* (call-next-method))
    (*print-escape*
     (print-unreadable-object (this output :type t)
       (write (car (single-defs this)) :stream output :escape t)))
    (t (funcall (formatter "sing. ~{~A~^ ~}") output (single-defs this)))))

(defun parse-pronounce-part (but-option)
  ;; NOTE: This is designed to be used for return value of DISCARD-OPTION.
  "Return three values.
  1. PRONOUNCE object.
  2. List of word-classes.
  3. PLURAL, SINGLE object or NIL."
  (destructuring-bind
      (main . sub)
      (delete "" (the list (ppcre:split "; ?" but-option)) :test #'equal)
    (multiple-value-call #'values
      (pronounce main)
      (when sub
        (cond
          ((uiop:string-prefix-p "pl" (car sub))
           (make-plural :defs (cdr (ppcre:split "\\. ?" (car sub)))))
          ((uiop:string-prefix-p "sing." (car sub))
           (make-single :defs (cdr (ppcre:split "\\. ?" (car sub)))))
          ((search "pl." (the simple-string (car sub)))
           (make-plural :defs (loop :for sub :of-type simple-string
                                         :in (uiop:split-string (car sub)
                                                                :separator ",")
                                    :collect (let ((start
                                                    (nth-value 1
                                                               (ppcre:scan
                                                                 "pl. ?" sub))))
                                               (if start
                                                   (subseq sub start
                                                           (position #\. sub
                                                                     :start start))
                                                   (string-right-trim "."
                                                                      sub))))))
          ;; Ignore chaotic exceptions. (70/113408 contents.)
          ;; I do not have enough passion to fight against such corner cases.
          (t nil))))))

;;;; DEFINITION

(defstruct anonymous-definition (article nil :type list #|of string|#))

(defmethod article ((this anonymous-definition))
  (anonymous-definition-article this))

(defmethod print-object ((this anonymous-definition) output)
  (cond
    (*print-human-friendly*
     (let (*print-readably*)
       (call-next-method)))
    (*print-readably* (call-next-method))
    (*print-escape*
     (print-unreadable-object (this output :type t :identity t)))
    (*print-pretty*
     (pprint-article output (anonymous-definition-article this)))
    (t
     (funcall (formatter "~{~A~^~%~}") output
              (anonymous-definition-article this)))))

(defstruct (definition (:include anonymous-definition)))

(defmethod print-object ((this definition) output)
  (cond
    ((or *print-human-friendly* *print-readably* *print-escape*)
     (call-next-method))
    (*print-pretty*
     (funcall (formatter "Defn: ~/endict:pprint-article/") output
              (definition-article this)))
    (t
     (funcall (formatter "Defn: ~{~A~^~%~}") output
              (definition-article this)))))

(defstruct (numbering-definition (:include anonymous-definition))
  (label (error "LABEL is required.") :type (unsigned-byte 8)))

(defmethod print-object ((this numbering-definition) output)
  (cond
    ((or *print-human-friendly* *print-readably* *print-escape*)
     (call-next-method))
    (*print-pretty*
     (funcall (formatter "~D. ~/endict:pprint-article/") output
              (numbering-definition-label this)
              (numbering-definition-article this)))
    (t
     (funcall (formatter "~D. ~{~A~^~%~}") output
              (numbering-definition-label this)
              (numbering-definition-article this)))))

(defstruct (note (:include anonymous-definition)))

(defmethod print-object ((this note) output)
  (cond
    ((or *print-human-friendly* *print-readably* *print-escape*)
     (call-next-method))
    (*print-pretty*
     (funcall (formatter "Note: ~/endict:pprint-article/") output
              (note-article this)))
    (t (funcall (formatter "Note: ~{~A~^~%~}") output (note-article this)))))

(defun parse-defn (section)
  (labels ((collect-definitions (list)
             (loop :with args := list
                   :with object
                   :while args
                   :do (setf (values object args)
                               (multiple-value-call #'set-article
                                 (next-object (car args))
                                 (cdr args)))
                   :collect object))
           (next-object (line)
             (let (label position) ; used only in third clause.
               (cond
                 ((uiop:string-prefix-p "Defn:" line)
                  (values (make-definition)
                          (list (ppcre:regex-replace "Defn: *" line ""))))
                 ((uiop:string-prefix-p "Note:" line)
                  (values (make-note)
                          (list (ppcre:regex-replace "Note: *" line ""))))
                 ((and (not (equal "" line))
                       (digit-char-p (char line 0))
                       (setf (values label position)
                               (parse-integer line :junk-allowed t))
                       (array-in-bounds-p line position)
                       (char= #\. (char line position)))
                  (values (make-numbering-definition :label label)
                          (list
                            (subseq line
                                    (or (position-if-not
                                          (lambda (c) (find c ". ")) line
                                          :start position)
                                        position)))))
                 (t (values (make-anonymous-definition) (list line))))))
           (set-article (definition article list)
             (loop :for (line . rest) :on list
                   :until (equal "" line)
                   :do (push line article)
                   :finally (setf (anonymous-definition-article definition)
                                    (nreverse article))
                            (return (values definition rest)))))
    (declare
      (ftype (function ((simple-array character (*)))
              (values anonymous-definition list &optional))
             next-object))
    (collect-definitions section)))

;;;; WORD

(defstruct word
  (name (error "NAME is required.") :type list #|of string|#)
  (pronounce (error "PRONOUNCE is required.") :type list #|of string|#)
  (suffix nil :type list #|of string|#)
  (syllable nil :type list #|of (unsigned-byte 4)|#)
  (plural nil :type (or plural single null))
  (classes nil :type list #|of word-class|#)
  (etyms nil :type list #|of etym|#)
  (definitions nil :type list))

(defmethod print-object ((this word) output)
  (cond
    (*print-human-friendly*
     (let (*print-readably*)
       (call-next-method)))
    ((or *print-readably* *print-escape*) (call-next-method))
    (*print-pretty*
     (with-slots (name pronounce plural classes etyms definitions)
         this
       (pprint-logical-block (output nil)
         (pprint-newline :mandatory output)
         (funcall (formatter "~{~A~^; ~} ~:@_") output name)
         (funcall (formatter "~{~A~^; ~} ~:@_") output pronounce)
         (funcall (formatter "~@[~A ~:@_~]") output plural)
         (funcall (formatter "~@[~{~S~^ ~} ~:@_~]") output classes)
         (funcall (formatter "~{~A~:@_~} ~:@_") output etyms)
         (funcall (formatter "~{~A~^ ~:@_~:@_~}") output definitions))))
    (t
     (with-slots (name pronounce plural classes etyms definitions)
         this
       (pprint-logical-block (output nil)
         (pprint-newline :mandatory output)
         (funcall (formatter "~{~A~^; ~} ~%") output name)
         (funcall (formatter "~{~A~^; ~} ~%") output pronounce)
         (funcall (formatter "~@[~A ~%~]") output plural)
         (funcall (formatter "~@[~{~S~^ ~} ~%~]") output classes)
         (funcall (formatter "~{~A~%~} ~%") output etyms)
         (funcall (formatter "~{~A~^ ~2%~}") output definitions))))))

(defun last-vowels (pronounce)
  #+sbcl ; Out of our responsibility.
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let (last)
    (ppcre:do-matches (start #:end "[êaôà&üeùáiöjïoûuëæéy]+" pronounce)
      (setq last start))
    last))

(defun canonicalize-pronounce (pronounce)
  (ppcre:regex-replace "(\"|`|'|\\.|:|-|\"\")? ?( n|}|v. t|v.t)?\\.?$"
                       (string-downcase pronounce) ""))

(defun suffix (pronounce)
  (when pronounce
    (flet ((canonicalize-for-suffix (pronounce)
             (cond ;; for "paste" -> "aste", not "e".
                   ((ppcre:scan "\\w[s|c|t|r|v|m]e$" pronounce)
                    (string-right-trim "e" pronounce))
                   ;; for "abandoned" -> "oned", not "ed".
                   ((let ((start (ppcre:scan "ed$" pronounce)))
                      ;; for "abbreviated" -> "ed", not "ated".
                      (when start
                        (locally
                         (declare (type (unsigned-byte 6) start))
                         (not
                           (find (char pronounce (1- start)) "\"`rdplbt")))))
                    (ppcre:regex-replace "ed$" pronounce ""))
                   (t pronounce))))
      (declare
        (ftype (function (simple-string) (values simple-string &optional))
               canonicalize-for-suffix))
      (let* ((canon (canonicalize-pronounce pronounce))
             (temp (canonicalize-for-suffix canon)))
        (subseq canon (or (last-vowels temp) 0))))))

(defun syllable (pronounce)
  (let ((count 1))
    (declare (type (unsigned-byte 4) count)
             #+sbcl ; out of our responsibility.
             (sb-ext:muffle-conditions sb-ext:compiler-note))
    (ppcre:do-matches (#:start #:end ; Instead of ignore declaration.
                       "[^\\w']+" (canonicalize-pronounce pronounce))
      (incf count))
    count))

(defun parse-section (section)
  (multiple-value-bind (secondary-section rest)
      (secondary-section section)
    (multiple-value-bind (etyms but-etym)
        (etym secondary-section)
      (multiple-value-bind (pronounce word-classes plural)
          (and but-etym (parse-pronounce-part (discard-option but-etym)))
        (make-word :name (name (car section))
                   :pronounce pronounce
                   :suffix (mapcar #'suffix pronounce)
                   :syllable (loop :for p :in pronounce
                                   :collect (ignore-errors (syllable p)))
                   :plural plural
                   :classes word-classes
                   :etyms etyms
                   :definitions (parse-defn rest))))))

;;;; LOAD.

(defun load-dictionary.txt ()
  (with-open-file (in (truename
                        (uiop:subpathname
                          (asdf:system-source-directory
                            (asdf:find-system :endict))
                          "dictionary.txt")))
    (skip-header in)
    (loop :with section
          :with next-name := (slurp-line in)
          :do (setf (values section next-name) (section next-name in))
          :collect (parse-section section)
          :while next-name)))

