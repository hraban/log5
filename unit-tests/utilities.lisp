(in-package #:log5-test)

(defmacro with-logging-captured-to-string (category-spec &body body)
  (let ((gsender-name (gensym "name -"))
	(gstream (gensym "stream-")))
    `(let ((,gsender-name (gensym))
	   (,gstream (make-string-output-stream)))
       (unwind-protect
	    (progn
	      (start-sender-fn ,gsender-name ',category-spec 
			       '(message) 
			       'stream-sender 
			       :location ,gstream)
	      ,@body)
	 (stop-sender-fn ,gsender-name :warn-if-not-found-p nil))
       (get-output-stream-string ,gstream))))

(defmacro with-debugging-captured-to-string (category-spec &body body)
  `(let ((*debug-io* (make-string-output-stream)))
     (unwind-protect
	  (progn
	    (debugging ',category-spec)
	    ,@body)
       (undebugging ',category-spec))
     (get-output-stream-string *debug-io*)))

(defparameter +whitespace-characters+
  (list #\Space #\Newline #\Tab #\Page #\Null #\Linefeed)
  "A list of characters that should be treated as whitespace. See, 
for example, [whitespacep][].")

(defun whitespacep (char)
  "Returns true if `char` is an element of [+whitespace-characters+][]
and nil otherwise."
  (not (null (find char +whitespace-characters+ :test #'char=))))

(defun string-trim-if (predicate string &key 
		       (start 0) (end (1- (length string))))
  (cond ((>= end start) 
	 (loop for ch across string 
	    while (funcall predicate ch) do (incf start))
	 (loop for ch = (aref string end)
	    while (funcall predicate ch) do (decf end))
	 (subseq string start (1+ end)))
	(t
	 "")))

(defun trim-whitespace (string &key (start 0) (end (1- (length string))))
  (string-trim-if #'whitespacep string :start start :end end))

(defun clean-string-if (string predicate)
  (with-output-to-string (s)
    (loop for ch across string 
       unless (funcall predicate ch) do (write-char ch s))))

(defun strip-whitespace (string)
  (clean-string-if string #'whitespacep))

(defun strip-whitespace-and-quotes (string)
  (clean-string-if string (lambda (ch) (or (whitespacep ch) (char= ch #\")))))

