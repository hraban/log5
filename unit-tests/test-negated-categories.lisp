(in-package #:log5-test)

(defcategory nc-foo)
(defcategory nc-bar)   

(deftestsuite test-negated-categories (log5-test)
  ()
  (:teardown
   (stop-sender 'test-negated-categories :warn-if-not-found-p nil)))

(addtest (test-negated-categories)
  check-category-specification
  (ensure-same
   (canonize-category-specification '(and nc-foo (not nc-bar)))
   '(and nc-foo (not nc-bar)) :test 'equal))

;; maybe use ensure-cases
(addtest (test-negated-categories)
  start-sender
  (let ((string
	 (with-output-to-string (stream)
	   (start-sender-fn 'test-negated-categories 
			    '(and nc-foo (not nc-bar)) '(message)
			    'stream-sender :location stream)
	   (log-for (nc-foo) "absence is negation?")
	   (log-for (nc-bar) "no")
	   (log-for (nc-foo (not nc-bar)) "yes")
	   (log-for (and (not nc-foo) (not nc-bar)) "no")
	   #+(or)
	   ;; not supported
	   (log-for (not (and nc-foo nc-bar)) "no")
	   #+(or)
	   ;; not supported
	   (log-for (not (not (nc-foo (not nc-bar)))) "yes")
	   )))
    (ensure-same string (format nil "absence is negation?~&yes~%")
		 :test 'string=)))


