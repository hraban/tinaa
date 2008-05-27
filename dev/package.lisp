(in-package #:common-lisp-user)

(defpackage #:tinaa
  (:use #:common-lisp
	#:moptilities
	#:metatilities 
	#:lml2 
	#:metabang.bind 
        #:cl-containers
	#:defsystem-compatibility
	#:anaphora)
  (:nicknames #:doclisp)
  (:documentation "A simple, yet extensible, Common Lisp documentation package.")
  (:import-from metatilities #:fn #:depth #:value #:name)
  (:import-from defsystem-compatibility #:pathname-for-system-file)
  (:export
   #:subpart-kinds
   #:partname-list
   #:display-part
   #:part-documentation
   #:make-part
   
   #:doclisp-part
   #:doclisp-assembly
   
   #:name
   #:parents
   #:instance
   
   #:document-system
   #:*css-file*))

