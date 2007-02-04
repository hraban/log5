(in-package #:common-lisp-user)

(defpackage #:log5-user-test
  (:use #:common-lisp #:log5 #:lift))

(defpackage #:log5-test
  (:use #:common-lisp #:log5 #:lift)
  (:import-from #:log5
		#:canonize-category-specification
		#:determine-category-variables
		#:canonize-category-name
		#:update-category-spec
		#:start-sender-fn
		#:stop-sender-fn
		))

