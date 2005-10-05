(in-package common-lisp-user)

(defpackage "TINAA"
  (:use "COMMON-LISP" "MOPTILITIES" "U" "LML")
  (:nicknames "DOCLISP")
  (:documentation "A simple, yet extensible, Common Lisp documentation package.")
  (:import-from "U" #:fn #:depth #:value #:collect #:name)
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
   
   #:document-system))

