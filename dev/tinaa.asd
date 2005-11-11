;;;-*- Mode: Lisp; Package: COMMON-LISP-USER -*-

#| simple-header

$Id: tinaa.system,v 1.12 2005/09/13 21:11:40 gwking Exp $

Author: Gary King

DISCUSSION

|#

(in-package :common-lisp-user)

;;; ---------------------------------------------------------------------------
;;; Translations
;;; ---------------------------------------------------------------------------

(glu-define-logical-pathname-translations (tinaa)
   (source)
   (doc (:back "doc"))
   (utils (:back :back "utils" "dev"))
   (containers (:back :back "cl-containers" "dev"))
   (lift (:back :back "lift" "dev"))
   (MOPTILITIES (:back :back "MOPTILITIES" "DEV"))
   (website (:back "website")))

;;; ---------------------------------------------------------------------------
;;; System
;;; ---------------------------------------------------------------------------

(setf (glu-system-source-file :cl-containers)
      "tinaa:containers;cl-containers.system"
      (glu-system-source-file :lift)
      "tinaa:lift;lift.system"
      (glu-system-source-file 'moptilities)
      "tinaa:moptilities;moptilities.system")

;;; ---------------------------------------------------------------------------

#+Ignore "timeit"

(define-glu-system :tinaa
  ((("package"
     "class-defs"
     "macros"
     "utilities"
     "tinaa"
     "doc-package"
     "doc-class"
     "doc-variable"
     "doc-function"
     "doc-symbol"
     "doc-glu-system"
     "build-indexes"
     "templates"))
   
   (("epilogue"))
   
   (("notes"
     "tinaa.css") 
    :associates? t
    :base-dir "tinaa:"))
  
  ;; DEFAULTS
  :base-dir "tinaa:source;"
  :bin-identifiers (:platform :vendor)
  :top-level t
  
  :depends-on (:basic-lift
               :moptilities 
               :cl-containers
               :metatilities-base
               :metatilities
               :lml2
               :metabang.bind
               :metabang.dynamic-classes))

;;; ***************************************************************************
;;; *                              End of File                                *
;;; ***************************************************************************
