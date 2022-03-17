; vim: ft=lisp et
(in-package :asdf)
(defsystem "endict.test"
  :version
  "0.0.0"
  :depends-on
  (:jingoh "endict")
  :components
  ((:file "endict"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :endict args)))