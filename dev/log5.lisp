(in-package #:common-lisp-user)

(defpackage #:log5
  (:use #:common-lisp)
  (:export #:defcategory
	   #:log-message
	   #:log-if
	   #:log-for
	   #:senders
	   #:start-sender
	   #:stop-sender
	   #:stop-all-senders
	   #:context
	   #:pop-context
	   #:push-context
	   #:with-context
	   #:defoutput
	   #:log-manager))

(in-package #:log5)

(defparameter *default-logical-connective* 'or)

(defvar *log-manager* nil)

(defun log-manager ()
  (or *log-manager* (setf *log-manager* (make-log-manager))))

(defstruct (log-manager (:conc-name manager-))
  senders
  context)

(defun handle-message (id message)
  (dolist (sender (manager-senders (log-manager)))
    (when (= (aref (active-categories sender) id) 1)
      (funcall (handle-message-fn sender) sender message))))

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

(defun reset-categories! ()
  (setf *category-specs* (make-hash-table :test #'equal)
	*name->category* (make-hash-table :test #'equal)))
  
(defmacro defcategory (name &optional category-spec documentation)
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
	    (determine-category-variables use-spec))
      (when name
	(setf (gethash name *name->category*) value)) 
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

(defun categories ()
  (let ((result nil))
    (maphash (lambda (name spec)
	       (declare (ignore name))
	       (push spec result))
	     *category-specs*)
    result))

(defun canonize-category-name (name)
  (intern (symbol-name name) (find-package :log5)))

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

(defoutput time (get-universal-time))

;;?? find duplicates
(defmacro start-sender (name (sender-type &rest args) &key 
			 category-spec output-spec)
  `(push
    (make-instance 
     ',sender-type 
     :name ',name
     :category-spec ',(canonize-category-specification category-spec)
     :output-spec ',output-spec
     ,@args)
    (manager-senders (log-manager))))

(defmacro stop-sender (name)
  `(stop-sending ',name))

(defun stop-sending (name)
  (let ((sender (find name (manager-senders (log-manager)) 
		      :key #'name))) 	
    (cond (sender
	   (close-sender sender)
	   (setf (manager-senders (log-manager)) 
		 (remove name (manager-senders (log-manager)) 
			 :key #'name))))))

(defun stop-all-senders ()
  (loop for sender in (manager-senders (log-manager)) do
       (stop-sending (name sender))))

(defun senders ()
  (copy-list (manager-senders (log-manager))))

(defclass basic-sender ()
  ((lock :reader lock)
   (name :reader name :initarg :name)
   (category-spec :initarg :category-spec :reader category-spec)
   (output-spec :initarg :output-spec :reader output-spec)
   (handle-message-fn :reader handle-message-fn)
   (active-categories :reader active-categories)))

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
	  ;(print sender-spec)
	  ;(print cat-positive)
	  ;(print cat-negative)
	  ;(print sender-free)
	  (eval sender-spec))))))

#+(or)
(foo '(a b) '(a c d e) '(and (or a c) (or b d) e))

(defun build-handle-message-fn (sender)
  `(lambda (sender message)
     (let (,@(create-context sender))
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

(defmethod create-context ((sender basic-sender)) nil)

(defmethod start-handling ((sender basic-sender)) nil)

(defmethod finish-handling ((sender basic-sender)) nil)

(defmethod separate-properties ((sender basic-sender)) nil)

(defun predefined-output-p (name)
  (find name '(message context first-context stack)))

(defmethod close-sender (sender)
  (delcare (ignore sender)))

(defclass stream-sender (basic-sender)
  ((output-stream :reader output-stream)
   (close-stream? :reader close-stream?)
   (location :initarg :location :reader location)))

(defmethod create-context ((sender stream-sender))
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
	((eq output 'context)
	 `(princ (manager-context (log-manager)) stream))
	((eq output 'first-context)
	 `(princ (first (manager-context (log-manager))) stream))
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
	  ,(if args `(format nil ,message ,@args) message)))))

#+(or)
(defmacro log-for (category-spec message &rest args)
  (update-category-spec nil category-spec)
  (if (member :no-logging *features*)
      `(values)
      `(handle-message
	(load-time-value
	 ,(category-id (update-category-spec nil category-spec))
	 :read-only-p t)
	,(if args `(format nil ,message ,@args) message))))

;;;;; context

(defun pop-context ()
  (pop (manager-context (log-manager))))

(defun push-context (context)
  (push context (manager-context (log-manager))))

(defmacro with-context (context &body body)
  `(unwind-protect
	(progn (push-context ,context)
	       ,@body)
     (pop-context)))

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
