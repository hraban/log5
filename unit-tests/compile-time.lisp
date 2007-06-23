(in-package #:log5-test)

#+(or)
(run-tests :suite 'compile-time-log-switch)

(deftestsuite compile-time-log-switch (log5-test)
  ()
  (:equality-test #'string=))

(defun compile-with-log-category (category-spec)
  (setf (log5:compile-category-spec) category-spec)
  (let ((source (asdf:system-relative-pathname 
		 'log5-test "unit-tests/example.lisp"))
	(output  (asdf:system-relative-pathname 
		 'log5-test "unit-tests/example.fasl")))
    (compile-file source :output-file output)
    (load output))
  (let ((r 
	 (with-output-to-string (stream)
	   (log5::start-sender-fn 
	    'test-it '(dribble+) '(message) 
	    'stream-sender :location stream)
	   (funcall (intern (symbol-name 'test-logging) 
			    :log5-example)))))
    r))

#|
(log-for (dribble) "hi")
(stop-all-senders)
(log5::sender-responds-to-category-p 
 (log5::log5-expanded-compile-category-spec (log-manager))
 (update-category-spec nil '(dribble)))

(trace log5::%log-p log5::sender-responds-to-category-p )
(untrace)
|#

#+(or)
(compile-with-log-category '(warn+))
(compile-with-log-category nil)

(addtest (compile-time-log-switch)
  all-permitted
  (let ((result (compile-with-log-category nil)))
    (print result)
  (ensure-same result
	       "\"dribble\"
\"trace\"
\"info\"
\"warn\"
\"error\"
\"fatal\"")))


(addtest (compile-time-log-switch)
  warn+
  (let ((result (compile-with-log-category '(warn+))))
    (print result)
  (ensure-same result
	       "\"warn\"
\"error\"
\"fatal\"")))




	       