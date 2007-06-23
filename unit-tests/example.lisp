#|
We use this file to test compile time log statement removal
|#

(in-package #:common-lisp-user)

(defpackage #:log5-example
  (:use #:common-lisp #:log5))

(in-package #:log5-example)

(defun test-logging ()
  (log-for (dribble) "dribble")
  (log-for (trace) "trace")
  (log-for (info) "info")
  (log-for (warn) "warn")
  (log-for (error) "error")
  (log-for (fatal) "fatal"))

