(in-package :cl-user)

(defpackage :endict
  (:use :cl)
  (:export))

(in-package :endict)

(defun skip-header (input)
  (loop :for line := (read-line input nil nil)
        :while line
        :if (uiop:string-prefix-p "Produced by" line)
          :do (peek-char t input) ; to discard empty lines.
              (loop-finish)))

(defun slurp-line (input)
  (let ((name (read-line input nil nil)))
    (when name
      (string-right-trim #.(string #\Return) name))))

(defun section (name input)
  "Slurp the section of the NAME from INPUT. Return two values.
1. A list of the lines.
2. A name of next section if exists otherwise NIL."
  (do ((line #0=(slurp-line input) #0#)
       acc
       (ignore '("M." "P." "X.")))
      ((null line) (values (cons name (nreverse acc)) nil))
    (if acc
        (unless (member line ignore :test #'equal)
          (if (and ;; The name must have one alphabet at least.
                   (find-if #'upper-case-p line)
                   (every
                     (lambda (c) (or (upper-case-p c) (not (alphanumericp c))))
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
            (push line acc)))))

;;;; NAME

(defstruct name
  (representations (error "REPRESENTATIONS is required.") :type list
                   #|of string|#))

(defmethod print-object ((this name) output)
  (cond (*print-readably* (call-next-method))
        (*print-escape*
         (print-unreadable-object (this output :type nil :identity nil)
           (write-string (car (name-representations this)) output)))
        (t (format output "~{~A~^; ~}" (name-representations this)))))

(defun name (name)
  (make-name :representations (uiop:split-string name :separator "; ")))

;;;; SECONDARY-SECTION

(defun secondary-section (section)
  "Accept SECTION. Return two values.
  1. A line which should have pronouce, category and Etym.
  2. The SECTION which lacks secondary section part.
NOTE: First value may NIL and warned if such line does not exist."
  (loop :for (content . rest) :on (cdr section) ; To ignore name part.
        :if (or (equal "" content)
                (uiop:string-prefix-p "(a)" (string-left-trim " " content)))
          :do (loop-finish)
        :collect content :into results
        :finally (return
                  (if results
                      (values (format nil "~{~A~^ ~}" results) rest)
                      (values (warn "Missing secondary section for ~S"
                                    (car section))
                              rest)))))

;;;; ETYM

(defstruct etym (defs (error "DEFS is required.") :type list #|of string|#))

(defmethod print-object ((this etym) output)
  (cond (*print-readably* (call-next-method))
        (*print-escape*
         (print-unreadable-object (this output :type t :identity t)))
        (t
         (loop :for (def . rest) :on (etym-defs this)
               :do (pprint-logical-block (output nil :prefix "Etym: ")
                     (write-string def output)
                     (when rest
                       (pprint-newline :mandatory output)))))))

(defun etym (secondary-section)
  "Return two values.
  1. An ETYM object if exists otherwise NIL.
  2. The SECONDARY-SECTION string which lacks etym part."
  (let ((position (search "Etym:" secondary-section)))
    (if position
        (values (make-etym :defs (loop :for content
                                            :in (ppcre:split "Etym:"
                                                             secondary-section
                                                             :start position)
                                       :for trim = (string-trim " " content)
                                       :unless (equal "" trim)
                                         :collect trim))
                (string-right-trim " " (subseq secondary-section 0 position)))
        (values nil secondary-section))))

(defun discard-option (but-etym)
  ;; For simplicity's sake, we discard the optional part due to its chaotic format. WTF.
  ;; NOTE: This is designed to be used for the second value of ETYM.
  (let* (;; To discard garbage i.e. "(),".
         (canonicalized (ppcre:regex-replace-all "\\(\\)," but-etym ""))
         (result
          (string-right-trim " "
                             (subseq canonicalized 0
                                     (position-if (lambda (c) (find c "(["))
                                                  canonicalized)))))
    (assert (not (equal "" result)))
    result))

(unless (boundp '+category+)
  (eval-when (:compile-toplevel :load-toplevel :execute)
    ;; To muffle compiler claims as 'undefined variable:'.
    (defconstant +category+
      '(("n." . :noun) ("n.." . :noun) ("n. pl." . :plural) ("n.pl." . :plural)
        ("pl." . :plural) ("n. sing." . :single) ("n.sing." . :single)
        ("a." . :article) ("a" . :article) ("v. t." . :verb) ("v.t." . :verb)
        ("v.t" . :verb) ("v. i." . :varb) ("v.i." . :varb) ("v." . :verb)
        ("adv." . :adverb) ("i." . :verb) ("obs." . :obs)
        ("interj." . :interjection) ("prep." . :preposition)
        ("conj." . :conjunction) ("p. p." . :past-participle)
        ("obs." . :obs)))))

(defun parse-category (string)
  (loop :for token :in (ppcre:split " ?& ?" string)
        :for category = (cdr (assoc token +category+ :test #'equal))
        :when category
          :collect category))

;;;; PRONOUNCE

(defstruct pronounce
  (defs (error "DEFS is required.") :type list #|of string|#))

(defmethod print-object ((this pronounce) output)
  (cond (*print-readably* (call-next-method))
        (*print-escape*
         (print-unreadable-object (this output)
           (write (car (pronounce-defs this)) :stream output :escape t)))
        (t (format output "~{~A~^ ~}" (pronounce-defs this)))))

(defun pronounce (pronounce-part)
  "Return two values.
  1. PRONOUNCE object.
  2. List of categories."
  (let ((split (ppcre:split ", ?" pronounce-part)))
    (case (length split)
      (0 (error "WTF! pronounce-part: ~S" pronounce-part))
      (1
       (values (make-pronounce :defs (list (string-trim "." (car split))))
               nil))
      (2
       (destructuring-bind
           (first second)
           split
         (values (make-pronounce :defs (list first)) (parse-category second))))
      ;; For simplicity's sake, we use only first pronounce and last category.
      (otherwise
       (values (make-pronounce :defs (list (car split)))
               (loop :for elt :in (reverse (cdr split))
                     :for category := (parse-category elt)
                     :when category
                       :return category))))))

