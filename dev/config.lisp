(in-package #:log5)

(defun configure-from-file (path &key (stop-senders-first? nil))
  (when stop-senders-first?
    (stop-all-senders))
  (let ((*package* *package*)
	(*read-eval* nil)
	(form nil))
    (with-open-file (in path
			:direction :input
			:if-does-not-exist :error)
      (loop while (not (eq (setf form (read in nil :eof nil)) :eof)) collect
	   (destructuring-bind (name (kind &rest args &key &allow-other-keys)
				     outputs categories)
	       form
	     (assert (typep name 'symbol))
	     (assert (subtypep kind 'basic-sender))
	     (assert (every 'valid-output-p outputs))
	     (apply #'start-sender-fn name categories outputs
		    kind (massage-arguments args)))))))

(defun massage-arguments (args)
  (loop for arg in args collect
       (cond ((eq arg '*standard-output*) *standard-output*)
	     (t arg))))

