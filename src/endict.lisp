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
       acc)
      ((null line) (values (cons name (nreverse acc)) nil))
    (if acc
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
            (push line acc))
        (if (every (lambda (c) (or (upper-case-p c) (char= #\Space c))) line)
            (setq name (format nil "~A ~A" name line))
            (push line acc)))))
