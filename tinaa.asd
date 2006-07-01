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
  :version "0.4"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "Various license"

  :components ((:module "dev"
                        :components ((:file "package")
                                     (:file "class-defs"
                                            :depends-on ("package"))
                                     (:file "api"
                                            :depends-on ("package" "class-defs"))
                                     (:file "macros"
                                            :depends-on ("api" "package"))
                                     (:file "utilities"
                                            :depends-on ("macros"))
                                     (:file "definitions"
                                            :depends-on ("package"))
                                     (:file "files-and-urls"
                                            :depends-on ("package" "definitions"))
                                     (:file "tinaa"
                                            :depends-on ("api" "class-defs" "definitions"))
                                     (:file "generate-output"
                                            :depends-on ("tinaa" "macros"))
                                     
                                     (:file "doc-package"
                                            :depends-on ("tinaa" "macros"))
                                     (:file "doc-class"
                                            :depends-on ("tinaa" "macros"))
                                     (:file "doc-variable"
                                            :depends-on ("tinaa" "macros"))
                                     (:file "doc-function"
                                            :depends-on ("tinaa" "macros"))
                                     (:file "doc-symbol"
                                            :depends-on ("tinaa" "macros"))
                                     (:file "doc-asdf-system"
                                            :depends-on ("tinaa""macros"))
                                                                          
                                     (:file "build-indexes"
                                            :depends-on ("tinaa" "doc-symbol" "macros"))
                                     (:file "permuted-index"
                                            :depends-on ("build-indexes")) 
                                     (:file "build-contents"
                                            :depends-on ("package")) 
                                     (:file "graph-parts"
                                            :depends-on ("package")) 
                                     
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
               trivial-shell
               cl-graph))
