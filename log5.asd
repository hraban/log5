(in-package #:common-lisp-user)

(defpackage #:log5-system
	    (:use #:common-lisp #:asdf))
(in-package #:log5-system)

(defsystem log5
  :components
  ((:module "dev"
    :components ((:file "log5")
		 (:file "port" :depends-on ("log5"))
		 ) 
  )))

 