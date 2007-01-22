(in-package #:common-lisp-user)

(defpackage #:log5
  (:use #:common-lisp)
  (:documentation "A Common Lisp logging librart; CLLL? No, it's log5. Log5 is organized around 5 things:

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
	   #:senders			;
	   #:start-sender		;
	   #:stop-sender		;
	   #:stop-all-senders		;
	   #:context
	   #:pop-context
	   #:push-context
	   #:with-context
	   #:defoutput			;
	   ;; manager
	   #:ignore-errors-p		;
	   #:log-manager		;
	   #:output-specs		;
	   #:category-specs		;
	   #:id->category		;
	   ;; senders
	   #:basic-sender		;
	   #:stream-sender
	   #:location
	   #:output-spec
	   #:category-spec
	   ;; standard 'levels'
	   #:fatal #:error #:warn #:info #:trace #:dribble
	   ;; output
	   #:time #:category #:message))

(in-package #:log5)

(defparameter *default-logical-connective* 'or)

(defvar *log-manager* nil)

(defun log-manager ()
  "Returns the component that handles all of log5's bookkeeping."
  (or *log-manager* (setf *log-manager* (make-log-manager))))

(defstruct (log-manager (:conc-name log5-))
  senders
  context
  ignore-errors?)

(defun ignore-errors-p ()
  "If true, then log5 will ignore any errors that occur during logging actions. If false, log5 will enter the debugging. This is setfable."
  (log5-ignore-errors? (log-manager)))

(defun (setf ignore-errors-p) (value)
  "If true, then log5 will ignore any errors that occur during logging actions. If false, log5 will enter the debugging. This is setfable."
  (setf (log5-ignore-errors? (log-manager)) (not (null value))))

(defun handle-message (id message &rest args)
  (declare (dynamic-extent args))
  (dolist (sender (log5-senders (log-manager)))
    ;; check for new category and fix things up
    (when (>= id (length (active-categories sender)))
      ;;?? how pricy is this?
      (update-active-categories sender id))
    (when (= (aref (active-categories sender) id) 1)
      (funcall (handle-message-fn sender) id sender
	       (if args (apply #'format nil message args) message)))))

(defun update-active-categories (sender max-id)
  (let ((current-max-id (length (active-categories sender)))
	(new-array (adjust-array (active-categories sender) (1+ max-id))))
    (loop for index from current-max-id to max-id do
	 (setf (aref new-array index)
	       (if (sender-responds-to-category-p
		    (category-spec sender) 
		    (id->category index)) 1 0)))
    (setf (slot-value (first (log5:senders)) 'active-categories)
	  new-array)
    sender))


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
  `(update-category-spec 
    ',name ',category-spec
    ,@(when documentation `(:documentation ,documentation))))

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
	 (use-name (or (and name (canonize-category-name name))
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
	(setf (gethash name *name->category*) value))
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

(defun canonize-category-name (name)
  (typecase name
    (string (intern name *package*))
    (symbol (if (eq (symbol-package name) (find-package :keyword))
		(intern (symbol-name name) *package*)
		name))
    (t (error 'bad-category-type-error :name name))))

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

(defcategory :fatal)
(defcategory :error)
(defcategory :warn)
(defcategory :info)
(defcategory :trace)
(defcategory :dribble)

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
   (collect-key-value 
    *output-specs*
    :transform (lambda (k v) (declare (ignore v)) k))
   #'string-lessp))

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
  `(push
    (make-instance 
     ',sender-type 
     :name ',name
     :category-spec ',(canonize-category-specification category-spec)
     :output-spec ',output-spec
     ,@args)
    (log5-senders (log-manager))))

(defmacro stop-sender (name)
  "Find the sender named `name` and stop it."
  `(stop-sending ',name))

(defun stop-sending (name)
  (let ((sender (find name (log5-senders (log-manager)) 
		      :key #'name))) 	
    (cond (sender
	   (close-sender sender)
	   (setf (log5-senders (log-manager)) 
		 (remove name (log5-senders (log-manager)) 
			 :key #'name))))))

(defun stop-all-senders ()
  "Stops all logging."
  (loop for sender in (log5-senders (log-manager)) do
       (stop-sending (name sender))))

(defun senders ()
  "Returns a list of the current senders."
  (copy-list (log5-senders (log-manager))))

(defclass basic-sender ()
  ((lock :reader lock)
   (name :reader name :initarg :name)
   (category-spec :initarg :category-spec :reader category-spec)
   (output-spec :initarg :output-spec :reader output-spec)
   (handle-message-fn :reader handle-message-fn)
   (active-categories :reader active-categories))
  (:documentation "The root sender class from which all senders should descend."))

(defmethod initialize-instance :after ((object basic-sender) &key 
				       )
  (setf (slot-value object 'handle-message-fn)
	(build-handle-message-fn object)
	(slot-value object 'active-categories)
	(make-active-category-array object)))

(defun make-active-category-array (sender)
  (let* ((size (hash-table-count *category-specs*))
	 (array
	  (make-array size
		      :fill-pointer size
		      :element-type 'bit
		      :initial-element 0
		      :adjustable t)))
    (maphash 
     (lambda (name category)
       (declare (ignore name))
       (setf (aref array (category-id category))
	     (if (sender-responds-to-category-p
		  (category-spec sender) category) 1 0)))
     *category-specs*)
    array))

#+(or)
;;?? assume, for today, that sender's spec is AND and 
;; message-spec is OR
(defun sender-responds-to-category-p (sender category-spec)
  (every (lambda (category)
	   (member category category-spec))
	 (category-spec sender)))

(defun sender-responds-to-category-p (sender-spec category-spec)
  (let* ((cat-positive (category-variables category-spec))
	 (cat-negative (category-negated-variables category-spec))
	 (sender-variables (determine-category-variables sender-spec))
	 (sender-free (remove-if (lambda (x)
				   (or (member x cat-positive)
				       (member x cat-negative)))
				 sender-variables)))
    (progv cat-positive (make-list (length cat-positive)
				   :initial-element t) 
      (progv cat-negative (make-list (length cat-negative)
				     :initial-element nil)
	(progv sender-free (make-list (length sender-free)
				      :initial-element nil)
;	  (print sender-spec)
;	  (print cat-positive)
;	  (print cat-negative)
;	  (print sender-free)
	  (eval sender-spec))))))

(defun build-handle-message-fn (sender)
  `(lambda (category-id sender message)
     (let (,@(create-handle-message-context sender))
       (unwind-protect
	    (progn
	      ,@(start-handling sender) 
	      ,@(loop for name in (output-spec sender) 
		   for output = (or (predefined-output-p name)
				      (and (stringp name) name)
				      (gethash name *output-specs*)) 
		   for first? = t then nil
		   unless output do (warn "No output named ~a" name)
		   unless first? collect (separate-properties sender)
		   when output collect 
		   (handle-output sender output)))
	 ,@(finish-handling sender)))))

(defmethod create-handle-message-context ((sender basic-sender)) nil)

(defmethod start-handling ((sender basic-sender)) nil)

(defmethod finish-handling ((sender basic-sender)) nil)

(defmethod separate-properties ((sender basic-sender)) nil)

(defun predefined-output-p (name)
  (find name '(message context first-context stack category)))

(defmethod close-sender (sender)
  (declare (ignore sender)))

(defclass stream-sender (basic-sender)
  ((output-stream :reader output-stream)
   (close-stream? :reader close-stream?)
   (location :initarg :location :reader location)))

(defmethod create-handle-message-context ((sender stream-sender))
  `((stream (output-stream sender))))

(defmethod start-handling ((sender stream-sender)) 
  `((fresh-line stream)))

(defmethod finish-handling ((sender stream-sender))
  `((force-output stream)))

(defmethod separate-properties ((sender stream-sender))
  `(princ #\Space stream))

(defmethod handle-output ((sender stream-sender) output)
  (cond ((eq output 'message)
	 `(progn (princ #\" stream) (princ message stream) (princ #\" stream)))
	((eq output 'category)
	 `(progn 
	    (princ #\" stream) 
	    (princ (category-specification (id->category category-id))
		   stream)
	    (princ #\" stream)))
	((eq output 'context)
	 `(princ (log5-context (log-manager)) stream))
	((eq output 'first-context)
	 `(princ (first (log5-context (log-manager))) stream))
	((stringp output)
	 `(progn (princ #\" stream) (princ ,output) (princ #\" stream)))
	#+(or)
	((eq output 'stack)
	 `(princ message stream))
	((typep output 'log-output)
	 `(princ ,(output-form output) stream))
	(t
	 (error "don't know how to handle ~a" output))))

(defmethod initialize-instance :after ((object stream-sender) &key location)
  (setf (slot-value object 'close-stream?) (not (streamp location))
	(slot-value object 'output-stream)
	(cond ((streamp location) location)
	      ((or (pathnamep location) (stringp location))
	       (open location :direction :output
		     :if-does-not-exist :create
		     :if-exists :append 
		     #+(or) (if reset-log? :supersede :append)))
	      (t (error "don't know how to log to ~a" location)))))

(defmethod close-sender ((sender stream-sender))
  (when (close-stream? sender)
    (close (output-stream sender))))
 
(defmacro log-for (category-spec message &rest args)
  (if (member :no-logging *features*)
      `(values)
      (let ((category (update-category-spec nil category-spec)))
	`(handle-message
	    ,(category-id category)
	    ,message
	    ,@args)
	#+(or)
	`(ignore-errors
	   (handle-message
	    ,(category-id category)
	    ,message
	    ,@args)))))

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

#|
;;;;; with-logging

(defmacro with-logging ((&key name (period 10) reset-log? path)
			       &body body)
  (assert (and (numberp period) (plusp period)))
  (assert (not (null path)))
  (with-gensyms (process)
    `(let ((,process (mp:process-run-function 
	`	     "memory-logger" #'memory-logger
		     ,name ,period ,path ,reset-log?)))
      (unwind-protect
	   (progn ,@body)
	(mp:process-kill ,process)))))

(defun memory-logger (name period path reset-log?)
  (with-logging (path :reset-log? reset-log?)
    (loop do
	 (log-message (memory-log-message name))
	 (sleep period))))

(defun memory-log-message (name)
  (with-output-to-string (out)
    (format out "~%~a " name)
    (format out " ~{~{~s ~} ~}" 
	    (delete-if 
	     (lambda (pair) 
	       (not (member (first pair) 
			    '(:%CPU :%MEM :VSZ :RSS)))) 
	     (os-process-info)))))
|#
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
