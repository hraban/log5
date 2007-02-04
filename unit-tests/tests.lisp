(in-package #:log5-test)

(deftestsuite test-stream-sender (log5-test)
  ())

#+(or)
(deftestsuite test-stream-sender-with-stream (test-stream-sender)
  ((sender-name (gensym))
   (string-stream (make-string-output-stream))
   (sender nil))
  (:teardown (stop-sender-fn sender-name :warn-if-not-found-p nil))
  :equality-test #'string-equal)

(deftestsuite test-stream-sender-with-stream (test-stream-sender)
  (sender-name
   string-stream
   (sender nil))
  (:setup
   (setf sender-name (gensym)
	 string-stream (make-string-output-stream)))   
  (:teardown (stop-sender-fn sender-name :warn-if-not-found-p nil))
  :equality-test #'string-equal)

(addtest (test-stream-sender-with-stream)
  no-log-no-output 
  (log-for warn "nothing")
  (ensure-same (get-output-stream-string string-stream) ""))

(addtest (test-stream-sender-with-stream)
  log-output
  (setf sender
	(start-sender-fn sender-name '(warn+) 
			 '("test" category message) 
			 'stream-sender 
			 :location string-stream))
  (log-for warn "nothing")
  (ensure-same 
   (get-output-stream-string string-stream) 
   (format nil "\"test\" \"~a\" \"nothing\"" 'warn)))
