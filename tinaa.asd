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

  :components ((:module "dev"
                        :components ((:file "package")
                                     (:file "class-defs"
                                            :depends-on ("package"))
                                     (:file "macros"
                                            :depends-on ("package"))
                                     (:file "utilities"
                                            :depends-on ("package" "macros"))
                                     (:file "tinaa"
                                            :depends-on ("package" "macros"
                                                         "class-defs"))
                                     (:file "doc-package"
                                            :depends-on ("package" "tinaa"))
                                     (:file "doc-class"
                                            :depends-on ("package" "tinaa"))
                                     (:file "doc-variable"
                                            :depends-on ("package" "tinaa"))
                                     (:file "doc-function"
                                            :depends-on ("package" "tinaa"))
                                     (:file "doc-symbol"
                                            :depends-on ("package" "tinaa"))
                                     
                                     #+Ignore
                                     (:file "doc-glu-system"
                                            :depends-on ("package" "tinaa"))
                                     
                                     (:file "build-indexes"
                                            :depends-on ("package" "tinaa"))
                                     (:file "templates"
                                            :depends-on ("package" "tinaa"))
                                     (:file "epilogue"
                                            :depends-on ("package" "tinaa"))
                                     
                                     (:static-file "notes.text")
                                     (:static-file "tinaa.css")))
               (:module "website"
                        :components ((:module "source"
                                              :components ((:static-file "index.lml"))))))
  
  :depends-on (lift metatilities lml2 cl-graph))
