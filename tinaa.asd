;;;-*- Mode: Lisp; Package: common-lisp-user -*-

(in-package :common-lisp-user)
(defpackage :asdf-tinaa (:use #:asdf #:cl))
(in-package :asdf-tinaa)

;; try hard
(unless (find-system 'asdf-system-connections nil)
 (when (find-package 'asdf-install)
   (funcall (intern (symbol-name :install) :asdf-install)
   	     'asdf-system-connections)))
;; give up with a useful (?) error message
(unless (find-system 'asdf-system-connections nil)
  (terpri)
  (format t "~&;; Warning: getting Tinaa to work with CL-Markdown requires ASDF-system-connections. See http://www.cliki.net/asdf-system-connections for details and download instructions."))

(when (find-system 'asdf-system-connections nil)
  (asdf:operate 'asdf:load-op 'asdf-system-connections))

(defsystem tinaa
  :author "Gary Warren King <gwking@metabang.com>"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :version "0.5.11"
  :licence "Basically BSD, see file COPYING for details"
  :components
  ((:module
    "dev"
    :components
    ((:file "package")
     (:file "class-defs"
	    :depends-on ("package" "api"))
     (:file "api"
	    :depends-on ("package"))
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
	    :depends-on ("package" "definitions")) 
                                     
     (:file "templates"
	    :depends-on ("tinaa"))
     (:file "epilogue"
	    :depends-on ("tinaa"))
                                     
     (:static-file "notes.text")
     (:static-file "tinaa.css")))
   (:module "website"
	    :components ((:module "source"
				  :components ((:static-file "index.md"))))))
  
  :depends-on ((:version :defsystem-compatibility "0.1.2")
               (:version :metatilities "0.6.6")
               :lml2
	       ;;; ugh, the order matters here. Add more duct tape
	       ;; basically this needs to be before cl-graph
	       #-asdf-system-connections :container-dynamic-classes
               :trivial-shell
               :cl-graph
	       :anaphora
	       :dynamic-classes
	       (:version :cl-containers "0.11.5")
	       ))

#+asdf-system-connections 
(defsystem-connection tinaa-and-cl-markdown 
  :requires (tinaa cl-markdown)
  :components ((:module 
		"dev"
		:components ((:file "cl-markdown-integration")))))

