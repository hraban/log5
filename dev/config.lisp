(in-package #:log5)

(defun configuration-file (&key (name "logging") (type "config")
			   (prefer-current-directory-p t))
  "Returns the path to the standard log5 configuration file. This is used by `configure-from-file` to setup logging activitiy. `Configuration-file looks in the 
user's home directory (using `user-homedir-pathname`) and the directory specified by `*default-pathname-defaults*`. The default is to use a file named \"logging.config\" but you can use the `:name` and `:type` parameters to customize this. If files exist in both directories, the `configuration-file` will use the one in the home directory unless `:prefer-current-directory-p` is true."
  (let ((current-file (probe-file
		       (make-pathname 
			:name name
			:type type)))
        (home-file (probe-file 
		    (make-pathname
		     :name name
		     :type type
		     :directory `(,@(pathname-directory
				     (user-homedir-pathname))
				    ".allegrograph")))))
    (cond ((and prefer-current-directory-p current-file)
	   current-file)
	  (t
	   (or home-file current-file)))))

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

