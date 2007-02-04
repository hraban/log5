(in-package #:log5-test)

(deftestsuite test-canonize-category-name (log5-test)
  ()
  (:dynamic-variables
   (*package* (find-package :log5-test)))
  (:tests ((ensure-same (canonize-category-name 'lift::a :simple? t) 'lift::a))
	  ((ensure-same (canonize-category-name 
			 (intern (symbol-name 'a)) :simple? t) 'a))
	  ((ensure-same (canonize-category-name :a :simple? t) 'a))))

(deftestsuite test-canonize-category-specification (log5-test)
  ()
  (:equality-test 'equal)
  (:dynamic-variables
   (*package* (find-package :log5-test)))
  (:setup ((update-category-spec 'lift::a nil) 
	   (update-category-spec ':b nil) 
	   (update-category-spec 'c nil)))
  #+(or)
  (:dynamic-variables
   (*name->category* (make-hash-table :test #'equal))
   (*category-specs* (make-hash-table :test #'equal)))
  (:tests ((ensure-same 
	    (canonize-category-specification '(and lift::a (or :b c)))
	    '(and lift::a (or b c)))))
  (:tests ((ensure-same 
	    (canonize-category-specification '(and lift::a (:b c)))
	    '(and lift::a (or b c))))))

#+(or)
(let* ((a (defcategory a))
       (b (defcategory b))
       (c (defcategory c (or a b)))
       (d (defcategory d))
       (e (defcategory e (and d c))))
  (category-expanded-specification e))

(deftestsuite test-determine-category-variables (log5-test)
  ())

(addtest (test-determine-category-variables)
  simple-list
  (ensure-same 
   (determine-category-variables '(a b (and c d)))
   (values '(a b c d) nil)))

(addtest (test-determine-category-variables)
  simple-list-with-not
  (ensure-same 
   (determine-category-variables '(a b (and (not c) d)))
   (values '(a b d) '(c))))

#+(or)
;; let's call this illegal...
(addtest (test-determine-category-variables)
  not-with-or
  (ensure-same 
   (determine-category-variables '(not (or a b)))
   (values 'nil '(a b))))


#+(or)
(run-tests)