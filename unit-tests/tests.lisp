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
  (let ((there :there))
    (log-for warn "nothing ~a to ~a" :here there))
  (ensure-same 
   (get-output-stream-string string-stream) 
   (format nil "test ~a nothing here to there~%" 'warn)))

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
    
;;;;

(deftestsuite test-message-formatting (log5-test)
  ()
  (:equality-test #'string=))

(addtest (test-message-formatting)
  simple-message
  (ensure-same 
   (with-logging-captured-to-string (info)
     (log-for info "hello there"))
   "hello there
"))

(addtest (test-message-formatting)
  formatted-string-with-arguments
  (ensure-same 
   (with-logging-captured-to-string (info)
     (log-for info "hello there ~a" "gary"))
   "hello there gary
"))

(addtest (test-message-formatting)
  formatted-string-with-tilde-and-arguments
  (ensure-same 
   (with-logging-captured-to-string (info)
     (log-for info "hello there ~a. ~
how are you?" "gary"))
   "hello there gary. how are you?
"))

(addtest (test-message-formatting)
  formatted-string-with-tilde
  (ensure-same 
   (with-logging-captured-to-string (info)
     (log-for info "hello there, ~
how are you?"))
   "hello there, how are you?
"))

;;;

#|
(deftestsuite test-debugging (log5-test)
  ()
  (:equality-test #'string=))

  

(addtest (test-debugging)
  captures
  (ensure-same 
   (with-debugging-captured-to-string (info)
     (log-for info "ji"))

(let ((*debug-io* (make-string-output-stream)))
  (format *debug-io* "he")
  (get-output-stream-string *debug-io*))

(debugging '(info) :reset? t)

(log-for info "ji")

(debugging nil :reset? t )

(debugging 

|#

;;;
