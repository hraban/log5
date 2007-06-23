(in-package #:log5)

#|

this 'need' to be in their own file due to changes 
to support compile-time category filtering

|#

(defcategory fatal)
(defcategory error)
(defcategory error+ (or error fatal))
(defcategory warn)
(defcategory warn+ (or warn error+))
(defcategory info)
(defcategory info+ (or info warn+))
(defcategory trace)
(defcategory trace+ (or trace info+))
(defcategory dribble)
(defcategory dribble+ (or dribble trace+))
