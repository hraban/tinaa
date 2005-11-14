;;;-*- Mode: Lisp; Package: COMMON-LISP-USER -*-

#| simple-header

Author: Gary King

DISCUSSION

|#

(in-package :common-lisp-user)
(defpackage :asdf-tinaa (:use #:asdf #:cl))
(in-package :asdf-tinaa)

(defsystem tinaa
  :author "Gary Warren King <gwking@metabang.com>"
  :version "0.5"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "Various license"

  :components ((:file "package")
               (:file "class-defs"
                      :depends-on ("package"))
               (:file "macros"
                      :depends-on ("package"))
               (:file "utilities"
                      :depends-on ("package"))
               (:file "tinaa"
                      :depends-on ("package"))
               (:file "doc-package"
                      :depends-on ("package"))
               (:file "doc-class"
                      :depends-on ("package"))
               (:file "doc-variable"
                      :depends-on ("package"))
               (:file "doc-function"
                      :depends-on ("package"))
               (:file "doc-symbol"
                      :depends-on ("package"))
               (:file "doc-glu-system"
                      :depends-on ("package"))
               (:file "build-indexes"
                      :depends-on ("package"))
               (:file "templates"
                      :depends-on ("package"))
               (:file "epilogue"
                      :depends-on ("package")))
  
  #+Ignore
  (("notes"
    "tinaa.css") 
   :associates? t
   :base-dir "tinaa:")
  
  :depends-on (:basic-lift
               :moptilities 
               :cl-containers
               :metatilities-base
               :metatilities
               :lml2
               :metabang.bind
               :metabang.dynamic-classes))
