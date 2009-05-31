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

(deftestsuite test-category-combinations (log5-test)
  ()
  (:equality-test #'string=))

(defcategory b-1)
(defcategory b-2)
(defcategory all-b (or b-1 b-2))

(addtest (test-category-combinations)
  just-info
  (ensure-same
   (strip-whitespace-and-quotes
    (with-logging-captured-to-string (info)
      (log-for info "A")
      (log-for b-1 "B")
      (log-for (or info b-1) "C")
      (log-for (and info b-1) "D")
      (log-for (info b-1) "E")
      ))
   "ACDE"))

(addtest (test-category-combinations)
  just-b-1
  (ensure-same
   (strip-whitespace-and-quotes
    (with-logging-captured-to-string (b-1)
      (log-for info "A")
      (log-for b-1 "B")
      (log-for (or info b-1) "C")
      (log-for (and info b-1) "D")
      (log-for (info b-1) "E")
      ))
   "BCDE"))

(addtest (test-category-combinations)
  or-info-b-1
  (ensure-same
   (strip-whitespace-and-quotes
    (with-logging-captured-to-string (or info b-1)
      (log-for info "A")
      (log-for b-1 "B")
      (log-for (or info b-1) "C")
      (log-for (and info b-1) "D")
      (log-for (info b-1) "E")
      ))
   "ABCDE"))

(addtest (test-category-combinations)
  and-info-b-1
  (ensure-same
   (strip-whitespace-and-quotes
    (with-logging-captured-to-string (and info b-1)
      (log-for info "A")
      (log-for b-1 "B")
      (log-for (or info b-1) "C")
      (log-for (and info b-1) "D")
      (log-for (info b-1) "E")
      ))
   "CDE"))

(addtest (test-category-combinations)
  info-b-1-same-as-or
  (ensure-same
   (strip-whitespace-and-quotes
    (with-logging-captured-to-string (info b-1)
      (log-for info "A")
      (log-for b-1 "B")
      (log-for (or info b-1) "C")
      (log-for (and info b-1) "D")
      (log-for (info b-1) "E")
      ))
   "ABCDE"))

(addtest (test-category-combinations)
  info-not-b-1
  (ensure-same
   (strip-whitespace-and-quotes
    (with-logging-captured-to-string (and info (not b-1))
      (log-for info "A")
      (log-for b-1 "B")
      (log-for (or info b-1) "C")
      (log-for (and info b-1) "D")
      (log-for (info b-1) "E")
      ))
   "A"))
    
