(in-package #:common-lisp-user)

(defpackage #:log5-test-system
	    (:use #:common-lisp #:asdf))
(in-package #:log5-test-system)

(defsystem log5-test
  :version "0.1.0"
  :author "Gary Warren King <gwking@metabang.com>"  
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT License (see the fike COPYING for details)"
  :description "Log5 is a Common Lisp logging library"
  :components
  ((:module "unit-tests"
    :components ((:file "package")
		 (:file "definitions" :depends-on ("package"))
		 (:file "unit-tests" :depends-on ("definitions"))
		 (:file "tests" :depends-on ("definitions"))
		 (:file "speed-test" :depends-on ("definitions"))
		 )))
  :depends-on (:lift :log5))

 