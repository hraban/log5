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
  ((:module 
    "setup"
    :pathname "unit-tests/"
    :components ((:file "package")
		 (:file "definitions" :depends-on ("package"))
		 (:file "utilities" :depends-on ("package"))))
   (:module 
    "unit-tests"
    :depends-on ("setup")
    :pathname "unit-tests/"
    :components ((:file "unit-tests")
		 (:file "tests")
		 (:file "speed-test")
		 (:file "test-negated-categories")
			)))
  :depends-on (:lift :log5))

 