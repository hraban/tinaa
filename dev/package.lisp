(in-package common-lisp-user)

(defpackage "TINAA"
  (:use "COMMON-LISP" "MOPTILITIES" "METATILITIES" "LML2" "METABANG.BIND" 
        "CL-CONTAINERS")
  (:nicknames "DOCLISP")
  (:documentation "A simple, yet extensible, Common Lisp documentation package.")
  (:import-from "METATILITIES" #:fn #:depth #:value #:name)
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

