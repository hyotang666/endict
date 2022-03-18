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

(defun canonicalize-name (name)
  (make-name :representations (uiop:split-string name :separator "; ")))