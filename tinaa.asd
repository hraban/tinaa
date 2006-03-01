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
                                     (:file "api"
                                            :depends-on ("package"))
                                     (:file "macros"
                                            :depends-on ("api" "package"))
                                     (:file "utilities"
                                            :depends-on ("macros"))
                                     (:file "tinaa"
                                            :depends-on ("api" "macros" "class-defs"))
                                     (:file "doc-package"
                                            :depends-on ("tinaa"))
                                     (:file "doc-class"
                                            :depends-on ("tinaa"))
                                     (:file "doc-variable"
                                            :depends-on ("tinaa"))
                                     (:file "doc-function"
                                            :depends-on ("tinaa"))
                                     (:file "doc-symbol"
                                            :depends-on ("tinaa"))
                                     (:file "doc-asdf-system"
                                            :depends-on ("tinaa"))
                                     
                                     #+Ignore
                                     (:file "doc-glu-system"
                                            :depends-on ("package" "tinaa"))
                                     
                                     (:file "build-indexes"
                                            :depends-on ("tinaa" "doc-symbol"))
                                     (:file "permuted-index"
                                            :depends-on ("build-indexes"))
                                     (:file "templates"
                                            :depends-on ("tinaa"))
                                     (:file "epilogue"
                                            :depends-on ("tinaa"))
                                     
                                     (:static-file "notes.text")
                                     (:static-file "tinaa.css")))
               (:module "website"
                        :components ((:module "source"
                                              :components ((:static-file "index.lml"))))))
  
  :depends-on (defsystem-compatibility
               metatilities
               lml2
               cl-graph))
