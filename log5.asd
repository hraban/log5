(in-package #:common-lisp-user)

(defpackage #:log5-system
	    (:use #:common-lisp #:asdf))
(in-package #:log5-system)

(defsystem log5
  :version "0.3.2"
  :author "Gary Warren King <gwking@metabang.com>"  
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT License (see the fike COPYING for details)"
  :description "Log5 is a Common Lisp logging library"
  :components
  ((:module "dev"
    :components ((:file "log5")
		 (:file "category-defs" :depends-on ("log5")) 
		 (:file "port" :depends-on ("log5"))
		 (:file "config" :depends-on ("log5"))
		 ) 
  ))
  :in-order-to ((test-op (load-op :log5-test)))
  :perform (test-op :after (op c)
                    (describe 
		     (funcall (intern (symbol-name '#:run-tests) :lift) 
			      :suite '#:log5-test))))

 (defmethod operation-done-p 
           ((o test-op) (c (eql (find-system 'log5))))
  (values nil))
