#|
Nice to have undefcategory or the like
|#

(in-package #:common-lisp-user)

(defpackage #:log5
  (:use #:common-lisp)
  (:documentation "A Common Lisp logging library; CLLL? No, it's log5. Log5 is organized around 5 things:

* Categories, 
* Senders,
* Outputs,
* Messages, and
* the Context

")
  (:export #:defcategory		;
	   #:log-message
	   #:log-if
	   #:log-for
	   #:when-logging
	   #:senders			;
	   #:start-sender		;
	   #:start-stream-sender
	   #:stop-sender		;
	   #:stop-all-senders		;
	   #:context
	   #:pop-context
	   #:push-context
	   #:with-context
	   #:with-context-for
	   #:defoutput			;
	   ;; manager
	   #:ignore-errors-p		;
	   #:log-manager		;
	   #:output-specs		;
	   #:category-specs		;
	   #:id->category		;
	   ;; senders
	   #:basic-sender		;
	   #:sender-with-categories
	   #:stream-sender-mixin
	   #:stream-sender
	   #:location
	   #:output-spec
	   #:category-spec
	   #:close-sender
	   #:create-handle-message-context
	   #:start-handling
	   #:finish-handling
	   #:close-stream?
	   #:output-stream
	   #:name
	   #:debug-category-spec
	   #:undebug-category-spec
	   ;; standard 'levels'
	   #:fatal #:error #:warn #:info #:trace #:dribble
	   #:error+ #:warn+ #:info+ #:trace+ #:dribble+
	   #:in-development
	   ;; output
	   #:time #:category #:message
	   ;; configuration
	   #:compile-category-spec
	   #:configuration-file
	   #:configure-from-file))

(in-package #:log5)

(defgeneric update-active-categories (sender maximum-id))

(defgeneric create-handle-message-context (sender))

(defgeneric start-handling (sender))

(defgeneric finish-handling (sender))

(defgeneric separate-properties (sender))

(defgeneric close-sender (sender))

(defgeneric handle-output (sender output))

(defparameter *default-logical-connective* 'or)

(define-condition sender-not-found-warning (warning)
  ((sender-name :initform nil :reader sender-name :initarg :sender-name))
  (:report (lambda (c s)
	     (format s "Sender ~a not found" (sender-name c)))))

(define-condition bad-category-type-error (error)
  ((name :initform nil :initarg :name :reader name))
  (:report (lambda (c s)
	     (format s "Category names must be strings or symbols and ~s is neither." 
		     (name c)))))

(define-condition simple-category-not-found-error (error)
  ((name :initform nil :initarg :name :reader name))
  (:report (lambda (c s)
	     (format s "There is no simple category named ~s. Use defcategory to define simple categories before using it."
		     (name c)))))

(define-condition output-not-found-error (error)
  ((name :initform nil :initarg :name :reader name))
  (:report (lambda (c s)
	     (format s "There is no output-specification named ~s. Use defoutput to define output specifications before using them in a sender."
		     (name c)))))

(defstruct (log-manager (:conc-name log5-))
  senders
  context
  ignore-errors?
  compile-category-spec
  expanded-compile-category-spec
  debug-console)

(defvar *log-manager* nil)

#+(or)
(setf *log-manager* nil)

(defun log-manager ()
  "Returns the component that handles all of log5's bookkeeping."
  (or *log-manager* (setf *log-manager* (make-log-manager))))

(defun compile-category-spec ()
  (log5-compile-category-spec (log-manager)))

(defun (setf compile-category-spec) (category-spec)
  (setf (log5-expanded-compile-category-spec (log-manager))
	(canonize-category-specification category-spec)
	(log5-compile-category-spec (log-manager)) category-spec))

(defun ignore-errors-p ()
  "If true, then log5 will ignore any errors that occur during logging actions. If false, log5 will enter the debugging. This is setfable."
  (log5-ignore-errors? (log-manager)))

(defun (setf ignore-errors-p) (value)
  "If true, then log5 will ignore any errors that occur during logging actions. If false, log5 will enter the debugging. This is setfable."
  (setf (log5-ignore-errors? (log-manager)) (not (null value))))

(defmacro handle-message (id message &rest args)
  ;; needs to be a macro to delay evaluation of args...
  (let ((gid (gensym)) 
	(goutput (gensym))
	(gconsole (gensym))
	(gmanager (gensym))
	(gforce-output (gensym)))
    `(let* ((,goutput nil)
	    (,gid ,id)
	    (,gmanager (log-manager))
	    (,gconsole (log5-debug-console ,gmanager)))
       (labels ((,gforce-output ()
		  (or ,goutput 
		      (setf ,goutput
			    (let ((*print-pretty* nil))
			      ,(if args 
				   `(format nil ,message ,@args) message)))))
		(handle-message-for-sender (sender)
		  (update-active-categories sender ,gid)
		  (when (= (sbit (active-categories sender) ,gid) 1)
		    (funcall (handle-message-fn sender) 
			     ,gid sender (,gforce-output)))))
	 ;; maybe 'map-senders'
	 (dolist (sender (log5-senders ,gmanager))
	   (handle-message-for-sender sender))
	 (when ,gconsole
	   (handle-message-for-sender ,gconsole))
	 ,goutput))))

#+(or)
(defun handle-message-for-sender (sender id message args output)
  (update-active-categories sender id)
  (when (= (sbit (active-categories sender) id) 1)
    (funcall (handle-message-fn sender) id sender
	     (or output
		 (setf output
		       (let ((*print-pretty* nil))
			 (if args 
			     (apply #'format nil message args) message)))))
    output))

;;?? see above, merge / refactor
(defun active-category-p (id)
  (some (lambda (sender)
	  (update-active-categories sender id)
	  (= (sbit (active-categories sender) id) 1))
	(log5-senders (log-manager))))
	
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


#|
(defcategory :error)
(defcategory :task-management)
(defcategory :index)
(defcategory :index-task (:and :index :task-management))
(defcategory :index-trace (and index-task trace))
(defcategory merge nil
  "Anything having to do with merging")
(defcategory index-merge (index merge)
  "Merging or indexing")
(defcategory non-file-index-merge (and (index merge) (not file-system)) 
  "Non-file-system related index and merge activities")
|#

(defvar *category-specs* (make-hash-table :test #'equal))

(defvar *name->category* (make-hash-table :test #'equal))

(defvar *id->category* (make-hash-table :test #'eql))

(defun reset-categories! ()
  "Remove all category definitions; In general, it's not a good idea to use this but it can be handy if you need a clean slate."
  (setf *category-specs* (make-hash-table :test #'equal)
	*name->category* (make-hash-table :test #'equal)
	*id->category* (make-hash-table :test #'eql)))
  
(defmacro defcategory (name &optional category-spec documentation)
  "Define a category named `name`. This can be a simple category like

    (defcategory :error)
    (defcategory prize-allocation)

of a complex category like

    (defcategory :bad-thing (or :error :warning :fatal))
    (defcategory non-file-foo (and (or foo bar biz) (not file-access)))

Specifically, a simple category is just a name whereas a complex category is a boolean combination of other categories (either simple or complex). See `category-specs` if you want a list of defined categories." 
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (update-category-spec 
      ',name ',category-spec
      ,@(when documentation `(:documentation ,documentation)))))

(defstruct (log-category (:conc-name category-)
			 (:print-object print-category)) 
  name
  specification
  documentation
  id
  expanded-specification
  variables
  negated-variables)

(defun print-category (category stream)
  (print-unreadable-object (category stream :type nil :identity nil)
    (format  stream "category ~d: " (category-id category))
    (if (null (category-specification category))
	(format stream "~a" 
		(category-name category))
	(format stream "~@[~a -> ~]~a"
		(category-name category) (category-specification category)))))

(defun update-category-spec (name specification &key 
			     (documentation nil documentation-supplied?))
  (declare (ignore documentation documentation-supplied?))
  (let* ((use-spec (canonize-category-specification specification))
	 (use-name (or (and name (canonize-category-name name :simple? t))
		       use-spec)))
    (multiple-value-bind (value found?) 
	(gethash use-name *category-specs*)
      (unless found?
	(setf value (make-log-category
		     :name use-name
		     :specification specification
		     :id (hash-table-count *category-specs*))
	      (gethash use-name *category-specs*) value))
      (setf (category-specification value) specification
	    (category-expanded-specification value) use-spec
	    (values (category-variables value)
		    (category-negated-variables value))
	    (determine-category-variables (or use-spec (list use-name))))
      (when name
	(setf (gethash use-name *name->category*) value))
      (setf (gethash (category-id value) *id->category*) value)
      value)))

(defun determine-category-variables (spec)
  (let ((positive nil) (negative nil))
    (labels ((doit (x)
	       (cond ((null x))
		     ((atom x)
		      (unless (logical-connective-p x)
			(push x positive)))
		     ((eq (first x) 'not) (push (second x) negative))
		     (t (doit (car x)) (doit (cdr x))))))
    (doit spec)
    (values (nreverse positive) (nreverse negative)))))

(defun id->category (id)
  "Returns the category whose id is id (a number)."
  (gethash id *id->category*))

(defun name->category (name)
  "Returns the category whose name (assumed to be properly **canonized**) is name."
  (gethash name *name->category*))

(defun canonize-category-name (name &key (simple? nil))
  (let ((use-name 
	 (typecase name
	   (string (intern name *package*))
	   (symbol (if (eq (symbol-package name) (find-package :keyword))
		       (intern (symbol-name name) *package*)
		       name))
	   (t (error 'bad-category-type-error :name name)))))
    (when (not (or simple? 
		   (name->category use-name)
		   (logical-connective-p use-name)))
      (error 'simple-category-not-found-error :name name))
    use-name))

(defun canonize-category-specification (specification)
  (labels ((doit (x)
	     (cond ((null x) nil)
		   ((atom x) 
		    (expand-category (canonize-category-name x)))
		   ((listp x)
		    `(,@(unless (logical-connective-p (first x))
			  `(,*default-logical-connective*))
			,@(loop for y in x collect
			       (doit y)))))))
    (doit specification)))

(defun logical-connective-p (x)
  (and (atom x) (member x '(and not or))))

(defun expand-category (name)
  (let ((spec (gethash name *name->category*)))
    (if spec 
	(or (category-expanded-specification spec) name)
	name)))

#|
(defoutput message ...)
(defoutput human-time ...)
(defoutput os-process-id ... :static? t)
(defoutput db-name (or (and *db* (name *db*)) "-none-"))
|#

(defvar *output-specs* (make-hash-table :test #'eq))

(defstruct (log-output (:conc-name output-))
  name form (format "~s") static?)

(defmacro defoutput (name form &key format static?)
  "Define an output specification: a link between a `name` and a `form` that will be called during logging to put something in the log file. 
You can specify a `format` string (as in a call to `format`) with the 
:format keyword argument. You can use the keyword :static? to indicate
that the form's value will not change (e.g., the time of day is _not_
static but the process id of the current Lisp is)."
  `(update-output-spec ',name ',form 
			 ,@(when format `(:format ,format))
			 ,@(when static? `(:static? t))))

(defun update-output-spec (name form &key format static?)
  (multiple-value-bind (value found?) (gethash name *output-specs*)
    (unless found?
      (setf value (setf (gethash name *output-specs*)
			(make-log-output :name name))))
    (setf (output-form value) form)
    (setf (output-format value) (or format "~a"))
    (setf (output-static? value) static?)
    name))

(defun output-specs ()
  "Returns a list of the current output specs in alphatetical order."
  (sort
   (nconc
    (collect-key-value 
     *output-specs*
     :transform (lambda (k v) (declare (ignore v)) k))
    (built-in-output-specs))
   #'string-lessp))

(defun built-in-output-specs ()
  (copy-list '(category message first-context context)))

(defun category-specs ()
  "Returns a list of the defined category specs in alphatetical order."
  (sort
   (collect-key-value
    *category-specs*
    :transform (lambda (k v) (declare (ignore k)) v))
   #'< :key #'category-id))

(defun collect-key-value (ht &key (transform 'cons))
  (let ((result nil))
    (maphash (lambda (k v) 
	       (push (funcall transform k v) result))
	     ht)
    result))

(defoutput time (get-universal-time))

;;?? find duplicates
(defmacro start-sender (name (sender-type &rest args) &key 
			category-spec output-spec)
  "Create a log-message sender and add it to the list of active
senders. Each sender  has a `name`, a category-spec, an output-spec and
a class (with optional initialization arguments). The `name` should be 
symbol and only one sender of any particular name can exist at a time.

The category-spec is a boolean combination of categories (see `defcategory`).
A sender will send any message whose message-category satisfies the sender's
category-spec.

The output-spec is a list of outputs defined with `defoutput`. You can also
include strings and the special, predefined, outputs:

* message - the text of the current log message
* context - the contents of the current context (as a list)
* category - the value of the category of the current message
"
  `(start-sender-fn
    ,name 
    ,category-spec
    ,output-spec
    ',sender-type 
    ,@args))
    
(defmacro start-stream-sender (name location &key output-spec category-spec)
  `(start-sender ,name
		 (stream-sender :location ,location)
		 ,@(when output-spec
			 `(:output-spec ,output-spec))
		 ,@(when category-spec
			 `(:category-spec ,category-spec))))

(defun start-sender-fn (name category-spec output-spec 
			sender-type &rest args)
  ;; stop any current one with the same name... warn?
  (stop-sender-fn name :warn-if-not-found-p nil)
  (let ((sender (apply #'make-instance 
		 sender-type 
		 :name name
		 :category-spec
		 (canonize-category-specification category-spec)
		 :output-spec 
		 (if (consp output-spec) 
		     output-spec (list output-spec))
		 args)))
    (push sender (log5-senders (log-manager)))
    sender))

(defmacro stop-sender (name &key (warn-if-not-found-p t))
  "Find the sender named `name` and stop it. If the sender is not active, then
a `sender-not-found-warning` will be signaled unless `warn-if-not-found-p` is
set to nil."
  `(progn (stop-sender-fn ,name) nil))

(defun stop-sender-fn (name &key (warn-if-not-found-p t))
  (let ((sender (find name (log5-senders (log-manager)) 
		      :key #'name))) 	
    (cond (sender
	   (close-sender sender)
	   (setf (log5-senders (log-manager)) 
		 (remove name (log5-senders (log-manager)) 
			 :key #'name))
	   sender)
	  (t
	   (when warn-if-not-found-p
	     (warn 'sender-not-found-warning :sender-name name))))))

(defun stop-all-senders ()
  "Stops all logging."
  (loop for sender in (log5-senders (log-manager)) do
       (stop-sender-fn (name sender))))

(defun senders ()
  "Returns a list of the current senders."
  (copy-list (log5-senders (log-manager))))

(defclass basic-sender ()
  ((lock :reader lock)
   (name :reader name :initarg :name)
   (output-spec :initarg :output-spec :reader output-spec)
   (handle-message-fn :reader handle-message-fn)
   (active-categories :reader active-categories
		      :initform (make-category-array 0)))
  (:documentation "The root sender class from which all senders ~
should descend."))

(defmethod print-object ((sender basic-sender) stream)
  (print-unreadable-object (sender stream :type t :identity t)
    (print-sender sender stream)))

(defgeneric print-sender (sender stream)
  (:documentation "Print information about sender to stream"))

(defmethod print-sender ((sender basic-sender) stream)
  (format stream "~a" (name sender)))

(defmethod initialize-instance :after ((object basic-sender) &key 
				       )
  (setf (slot-value object 'handle-message-fn)
	(compile-handle-message-fn 
	 (build-handle-message-fn object))))

(defclass sender-with-categories (basic-sender)
  ((category-spec :initarg :category-spec :reader category-spec)
   (expanded-specification :initform nil :reader expanded-specification))
  (:documentation "A sender that responds only to certain log categories."))

(defmethod initialize-instance :after ((object sender-with-categories) &key 
				       )
  (initialize-category-spec object))

(defun initialize-category-spec (sender)
  (setf (slot-value sender 'expanded-specification)
	(canonize-category-specification (slot-value sender 'category-spec))
	(slot-value sender 'active-categories)
	(make-active-category-array sender)))

(defun make-active-category-array (sender)
  (let* ((size (hash-table-count *category-specs*))
	 (array (make-category-array size)))
    (maphash 
     (lambda (name category)
       (declare (ignore name))
       (setf (sbit array (category-id category))
	     (if (sender-responds-to-category-p
		  (expanded-specification sender) category) 1 0)))
     *category-specs*)
    array))

(defun make-category-array (size)
  (make-array size
	      :element-type 'bit
	      :initial-element 0))

(defmethod update-active-categories ((sender basic-sender) max-id)
  (let ((current-max-id (length (active-categories sender))))
    (when (>= max-id current-max-id)
      (setf (slot-value sender 'active-categories)
	    (make-category-array (1+ max-id)))
      sender)))

(defmethod update-active-categories ((sender sender-with-categories) max-id)
  (let ((current-max-id (length (active-categories sender))))
    (when (>= max-id current-max-id)
      (let ((old-array (active-categories sender))
	    (new-array (make-category-array (1+ max-id))))
	(loop for index from 0 below current-max-id do
	     (setf (sbit new-array index) (sbit old-array index)))
	(loop for index from current-max-id to max-id do
	     (setf (sbit new-array index)
		   (if (sender-responds-to-category-p
			(expanded-specification sender) 
			(id->category index)) 1 0)))
	(setf (slot-value sender 'active-categories)
	      new-array)
	sender))))

(defmacro %with-vars (vars default &body body)
  ;; an implementation one-off thing; don't cring when you see it <smile>
  `(progv ',vars ',(make-list (length vars) :initial-element default)
     (locally
	 #+sbcl (declare (sb-ext:disable-package-locks ,@vars))
	 (declare (special ,@vars))
	 ,@body)))

(defun sender-responds-to-category-p (sender-spec category)
  (#+sbcl
   sb-ext:without-package-locks
   #-sbcl
   progn
   (multiple-value-bind (sender-postivies sender-negatives)
       (determine-category-variables sender-spec)
     (let* ((cat-positive (category-variables category))
	    (cat-negative (category-negated-variables category))
	    (sender-variables (nconc sender-postivies sender-negatives))
	    (sender-free (remove-if (lambda (x)
				      (or (member x cat-positive)
					  (member x cat-negative)))
				    sender-variables)))
       (eval
	`(%with-vars ,cat-positive t
	   (%with-vars ,cat-negative nil
	     (%with-vars ,sender-free nil
	       ,sender-spec))))))))

(defun compile-handle-message-fn (fn)
  (compile nil fn))
  
(defun build-handle-message-fn (sender)
  `(lambda (category-id sender message)
     (declare (ignorable category-id sender message))
     (let (,@(create-handle-message-context sender))
       (unwind-protect
	    (progn
	      ,@(start-handling sender) 
	      ,@(loop for name in (output-spec sender) 
		   for output = (valid-output-p name) 
		   for first? = t then nil
		   unless output do (error 'output-not-found-error :name name)
		   unless first? collect (separate-properties sender)
		   when output collect 
		   (handle-output sender output)))
	 ,@(finish-handling sender)))))

(defun valid-output-p (name)
  (or (predefined-output-p name)
      (and (stringp name) name)
      (gethash name *output-specs*)))

(defmethod create-handle-message-context ((sender basic-sender)) nil)

(defmethod start-handling ((sender basic-sender)) nil)

(defmethod finish-handling ((sender basic-sender)) nil)

(defmethod separate-properties ((sender basic-sender)) nil)

(defun predefined-output-p (name)
  (find name '(message context first-context stack category)))

(defmethod close-sender (sender)
  (declare (ignore sender)))

(defclass stream-sender-mixin (basic-sender)
  ((output-stream :reader output-stream)
   (close-stream? :reader close-stream?)
   (location :initarg :location :reader location)))


(defmethod create-handle-message-context ((sender stream-sender-mixin))
  `((stream (make-string-output-stream))))

(defmethod start-handling ((sender stream-sender-mixin)) 
  `())

(defmethod finish-handling ((sender stream-sender-mixin))
  `((let ((output (output-stream sender)))
      (when (open-stream-p output)
	(when (typep output 'file-stream)
	  (file-position output :end))
	(fresh-line output)
	(princ (get-output-stream-string stream) output) 
	(force-output output)))))

#|
(defmethod create-handle-message-context ((sender stream-sender-mixin))
  `((stream (output-stream sender))))

(defmethod start-handling ((sender stream-sender-mixin)) 
  `((when (typep stream 'file-stream)
      (file-position stream :end))
    (fresh-line stream)))

(defmethod finish-handling ((sender stream-sender-mixin))
  `((force-output stream)))
|#

(defmethod separate-properties ((sender stream-sender-mixin))
  `(princ #\Space stream))

(defmethod handle-output ((sender stream-sender-mixin) output)
  (cond ((eq output 'message)
	 `(progn (princ #\" stream) (princ message stream) (princ #\" stream)))
	((eq output 'category)
	 `(progn 
	    (princ #\" stream) 
	    (let ((category (id->category category-id)))
	      (if category
		  (princ (category-specification category) stream)
		  (format stream "\"category ~d not found\"" category-id)))
	    (princ #\" stream)))
	((eq output 'context)
	 `(princ (log5-context (log-manager)) stream))
	((eq output 'first-context)
	 `(princ (first (log5-context (log-manager))) stream))
	((stringp output)
	 `(progn (princ #\" stream) (princ ,output stream) (princ #\" stream)))
	((typep output 'log-output)
	 (if (output-format output)
	     `(format stream ,(output-format output) ,(output-form output))
	     `(princ ,(output-form output) stream)))
	(t
	 (error "don't know how to handle ~a" output))))

#+(or)
;; trying to refactor
(defun output->value-code (output)
  (cond ((eq output 'message)
	 `message)
	((eq output 'category)
	 `(category-specification (id->category category-id)))
	((eq output 'context)
	 `(log5-context (log-manager)))
	((eq output 'first-context)
	 `(first (log5-context (log-manager))))
	((stringp output)
	 output)
	((typep output 'log-output)
	 (if (output-format output)
	     `(format stream ,(output-format output) ,(output-form output))
	     `(princ ,(output-form output) stream)))
	(t
	 (error "don't know how to handle ~a" output))))

(defmethod initialize-instance :after ((object stream-sender-mixin) 
				       &key location)
  (setf (slot-value object 'close-stream?) (not (streamp location))
	(slot-value object 'output-stream)
	(cond ((streamp location) location)
	      ((or (pathnamep location) (stringp location))
	       (ensure-directories-exist location)
	       (open location :direction :output
		     :if-does-not-exist :create
		     :if-exists :append 
		     #+(or) (if reset-log? :supersede :append)))
	      (t (error "don't know how to log to ~a" location)))))

(defmethod close-sender ((sender stream-sender-mixin))
  (when (close-stream? sender)
    (close (output-stream sender))))

(defclass stream-sender (stream-sender-mixin sender-with-categories)
  ())

(defclass debug-console-sender (stream-sender)
  ((specs :initform nil :accessor specs))
  (:default-initargs
   :location *debug-io*
    :name 'debug-console
    :output-spec '(message)
    :category-spec nil))

(defun find-or-create-debug-console ()
  (or (log5-debug-console (log-manager))
      (setf (log5-debug-console (log-manager))
	    (make-instance 'debug-console-sender))))

(defun debug-category-spec (&optional category-spec &key output-spec reset?) 
  (declare (ignore output-spec))
  (let ((console (find-or-create-debug-console)))
    (when reset?
      (setf (specs console) nil))
    (cond (category-spec
	   (let ((current-specs (specs console)))
	     (handler-case
		 (progn
		   (setf (specs console) 
			 (cond ((specs console)
				(if (find category-spec (specs console)
					  :test #'equal)
				    (specs console)
				    `(,@(specs console) ,category-spec)))
			       (t
				`(or ,category-spec)))
			 (slot-value console 'category-spec) (specs console))
		   (initialize-category-spec console))
	       (error (c)
		 ;(declare (ignore c))
		 (setf (specs console) current-specs)
		 (error c))))
	   (specs console))
	  (t
	   (specs console)))))

(defun undebug-category-spec (&optional category-spec)
  (let ((console (find-or-create-debug-console)))
    (cond (category-spec
	   (setf (specs console) 
		 (remove category-spec (specs console) :test #'equal)))
	  (t
	   (setf (specs console) nil)))
    (setf (slot-value console 'category-spec) (specs console))
    (initialize-category-spec console)
    console))
  
;;;;; messages 

(defun %log-p (category-spec)
  (and
   (not (member :no-logging *features*))
   (let ((compile-spec (log5-expanded-compile-category-spec (log-manager))))
     (or (null compile-spec)
	 (sender-responds-to-category-p 
	  compile-spec 
	  (update-category-spec nil category-spec))))))

(defmacro log-for (category-spec message &rest args)
  (if (%log-p category-spec)
      `(let ((category (load-time-value 
			(update-category-spec nil ',category-spec)
			t)))
	 (handle-message
	  (category-id category)
	  ,message
	  ,@args))
      `(values)))

(defmacro when-logging (category-spec &body body)
  (if (%log-p category-spec)
      `(let ((category (load-time-value 
			(update-category-spec nil ',category-spec)
			t)))
	 (when (active-category-p (category-id category))
	   ,@body))
      `(values)))

;;?? if I'm right, it would be faster to call the log-test first and
;; then the predicate only if we're logging...
(defmacro log-if (predicate category-spec message &rest args)
  (declare (dynamic-extent args))
  (if (%log-p category-spec)
      `(when ,predicate 
	(log-for ,category-spec ,message ,@args))
      `(values)))


;;;;; context

(defun pop-context ()
  (pop (log5-context (log-manager))))

(defun push-context (context)
  (push context (log5-context (log-manager))))

(defmacro with-context (context &body body)
  `(unwind-protect
	(progn (push-context ,context)
	       ,@body)
     (pop-context)))

(defmacro with-context-for (category-spec context &body body)
  (if (%log-p category-spec)
      `(flet ((do-it ()
		,@body))
	 (let ((category (load-time-value 
			  (update-category-spec nil ',category-spec)
			  t)))
	   (if (active-category-p (category-id category))
	       (with-context ,context (do-it))
	       (do-it))))
      `(progn ,@body)))


;;;;; utilities

(defun flatten (list)
  "Flattens LIST. Does not handle circular lists but does handle dotted lists."
  (labels ((rec (list)
             (cond ((atom list)
                    (list list))
                   ((dotted-pair-p list)
                    (nconc (rec (car list)) (rec (cdr list))))
                   (t               
                    (mapcan #'rec list)))))
    (declare (dynamic-extent rec))
    (if (atom list)
      list
      (rec list))))

(defun dotted-pair-p (putative-pair)
  "Returns true if and only if `putative-pair` is a dotted-list. I.e., if `putative-pair` is a cons cell with a non-nil cdr."
  (and (consp putative-pair)
       (cdr putative-pair)
       (not (consp (cdr putative-pair)))))

