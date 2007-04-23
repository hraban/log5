(in-package #:log5-test)

(defcategory log5-speed-test)

(defun log5-speed-test (loggers)
  (loop repeat loggers do 
       (log-for (log5-speed-test info) "info message"))
  ;; do some work to keep us honest
  (+ 1 loggers))

(defun measure-log5-speed (loggers count)
  (let ((x 0) s b)
    (lift::measure s b 
      (loop repeat count do
	   (setf x (log5-speed-test loggers)))
       x)
    (values s b 0)))

(defvar *handy-special-variable* nil)

(defun benchmark-with-loggers (count logger-counts)
  (loop for loggers in logger-counts collect
       (multiple-value-bind (seconds conses result)
	   (measure-log5-speed loggers count)
	 (declare (ignore result))
	 (list loggers count seconds conses *handy-special-variable*))))

(defun benchmark-with-senders (senders-that-log
			       senders-that-ignore
			       count
			       logger-counts)
  (log5:stop-all-senders)
  (when (probe-file "/tmp/foo.log") (delete-file "/tmp/foo.log"))
  (loop repeat senders-that-log 
     for i from 1 do
       (start-sender (intern (format nil "TEST-LOG-~d" i))
		     (log5:stream-sender :location "/tmp/foo.log")
		     :output-spec '(time message)
		     :category-spec '(and info log5-speed-test)))
  (loop repeat senders-that-ignore 
     for i from 1 do
       (start-sender (intern (format nil "TEST-IGNORE-~d" i))
		     (log5:stream-sender :location "/tmp/foo.log")
		     :output-spec '(time message)
		     :category-spec '(and dribble log5-speed-test)))
  (setf *handy-special-variable* 
	(list senders-that-log senders-that-ignore))
  (benchmark-with-loggers count logger-counts))

(defun benchmark-log5 (experiments)
  (loop for experiment in experiments collect
       (progn
	 (print experiment)
	 (destructuring-bind (loggers ignorers logger-count count)
	     experiment
	   (benchmark-with-senders
	    loggers ignorers logger-count (list count))))))

#|
(prof:with-profiling (:type :time) 
  (benchmark-with-senders 0 20 10000 10))
(prof:show-flat-profile)
|#

#|
#+(or)
(benchmark-log5 '((0 0 0 1000)
		  (0 0 10 1000)
		  (0 0 20 1000)
		  (10 0 0 1000)
		  (10 0 10 1000)
		  (10 0 20 1000)
		  (0 10 0 1000)
		  (0 10 10 1000)
		  (0 10 20 1000)))

(defun rpt (data)
  (format t "~&Active ~vTIgnoring ~vTLoggers ~vTLoops ~vT Seconds ~vTConses ~vTSeconds"
	  1 2 1 1 1 5)
  (format t "~&Senders ~vTSenders ~vTper call"
	  1 53)
  (loop for datum in data do
       (destructuring-bind ((loops loggers seconds conses (active ignoring)))
	   datum
	 (format t "~&~vd ~vd ~vd ~v:d ~v,2f ~v:d ~vT~@[~8,2g~]~:*~:[     ---~;~]"  
		 6 active 8 ignoring 7 loggers 
		 8 loops 8 seconds 8 conses
		 1
		 (if (plusp (* (+ active ignoring) loggers loops))
		     (/ seconds (* (+ active ignoring) loggers loops))
		     (/ seconds (* (max loggers 1) loops)))))))

(rpt 
 '(((1000 0 0.0d0 80 (0 0))) ((1000 10 0.001d0 80 (0 0)))
    ((1000 20 0.0d0 80 (0 0))) ((1000 0 0.0d0 80 (10 0)))
    ((1000 10 7.501d0 876356776 (10 0)))
    ((1000 20 14.77d0 1752732464 (10 0))) ((1000 0 0.0d0 96 (0 10)))
    ((1000 10 0.009d0 96 (0 10))) ((1000 20 0.016d0 96 (0 10)))))

(rpt
;; with senders, still no trouble
;; cost is ~ (/ 0.173 (* 20 10 10000)) == 8.65e-8
 '(((10000  0   0.0d0         704 ( 0 10)))
 ((10000 10   0.086d0       704 ( 0 10)))
 ((10000 20   0.173d0      1912 ( 0 10)))))

;; senders that do log
;; cost is ~ (/ 110.86 (* 20 10 10000)) == 5.54e-5
;; cost is for formatting
 (rpt '(((10000  0   0.0d0         704 (10  0)))
 ((10000 10  55.329d0 336650712 (10  0)))
 ((10000 20 110.868d0 673454904 (10  0)))))

;; no senders, no troubles
;; cost of a log-for statement is ~ (/ 0.533 (* 20 1000000)) == 2.67e-8
(benchmark-log5 '((0 0 0 1000000)
		  (0 0 10 1000000)
		  (0 0 20 1000000)))

(rpt '(((1000000 0 0.0d0 704 (0 0)))
       ((1000000 10 0.268d0 704 (0 0)))
       ((1000000 20 0.533d0 704 (0 0)))))

log5-test> (prof:with-profiling (:type :time) 
	     (benchmark-with-senders 0 20 1000000 '(10)))
((10 1000000 16.532d0 704 (0 20)))
log5-test> (prof:show-flat-profile)
Sampling stopped after 611 samples taken for the time profiler.

Sample represents 16.5 seconds of processor time (out of a total of 16.5)

Times below 1.0% will be suppressed.

  %     %     self  total            self   total  Function
 Time  Cum.   secs   secs    calls ms/call ms/call   name
 28.0  28.0    4.6   16.1                          log5::handle-message
 27.2  55.2    4.5    4.5                          excl::aref_1d
 19.3  74.5    3.2    3.2                          (:discriminator
                                                    (:few-class-reader
                                                     nil))
  9.5  84.0    1.6    1.6                          "q_wrapper_slots_std"
  9.2  93.1    1.5    1.5                          length
  2.0  95.1    0.3    0.3                          "where_am_i_2"
  1.6  96.7    0.3    0.3                          log5::active-categories


log5-test> (prof:with-profiling (:type :time) 
	     (benchmark-with-senders 20 0 10000 '(10)))
((10 10000 107.193d0 673462320 (20 0)))
log5-test> (prof:show-flat-profile)
Sampling stopped after 2635 samples taken for the time profiler.

Sample represents 70.0 seconds of processor time (out of a total of 70.0)

Times below 1.0% will be suppressed.

  %     %     self  total            self   total  Function
 Time  Cum.   secs   secs    calls ms/call ms/call   name
 18.9  18.9   13.3   13.3                          "_fclose"
 12.1  31.0    8.4    8.4                          "_sprintf"
  6.9  37.9    4.8    4.8                          "_strrchr"
  2.0  39.9    1.4   18.1                          ... excl::do-xp-printing
  2.0  41.9    1.4   12.1                          excl::write+
  1.7  43.7    1.2    1.2                          (:efft
                                                    excl:sc-write-chars
                                                    :latin1-base)
  1.7  45.4    1.2    2.9                          excl::attempt-to-output
  1.7  47.1    1.2    1.2                          (:discriminator
                                                    (:caching
                                                     (class)
                                                     nil))
  1.7  48.8    1.2    1.2                          "bind_one_spec"
  1.6  50.4    1.1    4.1                          ... excl::initialize-xp-simple-stream
  1.5  51.8    1.0    1.0                          "unbind_count"
  1.5  53.3    1.0    1.0                          "gc_setf_protect"
  1.4  54.8    1.0    1.0                          excl::enqueue
  1.4  56.2    1.0    5.5                          (:internal
                                                    excl::defresource-1
                                                    1)
  1.4  57.6    1.0    1.0                          "_atexit"
  1.4  58.9    1.0    2.3                          excl::get-printer
  1.3  60.3    0.9    0.9                          "gsgc_setf_protect_1"
  1.2  61.5    0.8    4.8                          (flet
                                                    (method
                                                     print-object
                                                     (t t))
                                                    excl::print-object-1)
; No value
log5-test> 

;; 0.86
#+(or)
(progn 
  (when (probe-file "/tmp/bar.log")
    (delete-file "/tmp/bar.log"))
  (let (s b)
    (lift::measure s b
      (with-open-file (s "/tmp/bar.log" :if-exists :supersede
			 :if-does-not-exist :create :direction :output)
	(loop repeat 10000 do
	     (let ((file-length (ignore-errors (file-length s))))
	       (when file-length (file-position s (file-length s)))
	       (fresh-line s)
	       (princ (get-universal-time) s)
	       (princ #\Space s)
	       (princ "a message" s)
	       (terpri s)
	       (force-output s)))))
    (values s b)))

;; 0.39
#+(or)
(progn 
  (when (probe-file "/tmp/bar.log")
    (delete-file "/tmp/bar.log"))
  (lift::measure 
   (lambda ()
     (with-open-file (s "/tmp/bar.log" :if-exists :supersede
			:if-does-not-exist :create :direction :output)
       (loop repeat 10000 do
	    (princ (get-universal-time) s)
	    (princ #\Space s)
	    (princ "a message" s)
	    (terpri s)
	    (force-output s))))))

#+(or)
(lift::measure 
 (lambda ()
  (loop repeat 1000000 do
       (with-open-file (s "/tmp/bar.log" :if-exists :append
			  :if-does-not-exist :create :direction :output)
       (princ (get-universal-time) s)
       (princ #\Space s)
       (princ "a message" s)
       (terpri s)))))

|#

