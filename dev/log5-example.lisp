(in-package #:log5)

(reset-categories!)

(defcategory planner)
(defcategory energy)
(defcategory motion)
(defcategory physics (or energy motion))


(categories)

(describe (first (categories)))

(hash-table-count *name->category*)
(hash-table-count *category-specs*)

(defun run-physics ()
  (log-for (and info physics) "starting physics")
  (with-context :energy
    (log-for (debug energy) "energy"))
  (log-for (debug motion) "motion")
  (log-for (info physics) "ending physics"))

(compile 'run-physics)

;; message, category, context, stack
(start-log physics-log
	   (stream-sender :location *standard-output*)
	   :category-spec (physics)
	   :output-spec (time "physics" message context))

(stop-all-senders)

(run-physics)

;; all info messages
(start-log ... :category-spec (info) ...)
  (and info physics)
  (and info error)   !?
  (info planner)

cluster
  task-management
    process-flow
    task
  machine-management
    

(setf x (make-array (* 2 (expt 2 30))
		    :element-type '(unsigned-byte 8)))
      
(stop-log physics-log)

(stop-logs)

(run-physics)

(logs)

PREFIX  rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX   ub: <http://www.lehigh.edu/%7Ezhp2/2004/0401/univ-bench.owl#>
SELECT ?x ?name ?email ?telephone WHERE
{
  ?x ub:worksFor <http://www.Department0.University0.edu> .
  ?x rdf:type ub:FullProfessor .
  ?x ub:name ?name .
  ?x ub:emailAddress ?email .
  ?x ub:telephone ?telephone .
}


#|

(start-log (stream-sender :location *standard-output*) 
	       :category-spec (or error warn info)
	       :output-spec (time os-process-id message current-context))

(start-log (stack-trace-sender) 
	       (error warn)
	       ((time os-process-id message)
		(full-context)
		(stack-trace)))
|#
