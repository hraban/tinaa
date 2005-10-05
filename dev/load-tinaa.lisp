;;;-*- Mode: Lisp; Package: COMMON-LISP-USER -*-

#| simple-header

$Id: load-tinaa.lisp,v 1.2 2004/03/28 18:21:13 gwking Exp $

Copyright 1992 - 2004 Experimental Knowledge Systems Lab, 
University of Massachusetts Amherst MA, 01003-4610
Professor Paul Cohen, Director

Author: Gary King

DISCUSSION

|#

(in-package :common-lisp-user)

;;; ---------------------------------------------------------------------------

(flet ((load-sibling (name &rest args)
         (apply #'load (merge-pathnames name  
                                        #+lucid lcl:*source-pathname*
                                        #+allegro excl:*source-pathname*
                                        #+(or Genera Explorer) sys:fdefine-file-pathname
                                        #+MCL (if *load-pathname* 
                                                *load-pathname*
                                                ;; This makes it work in a fred buffer...
                                                *loading-file-source-file*)
                                        #-(or lucid allegro Genera Explorer MCL)
                                        *load-pathname*)
                :verbose t
                args)))
  (load-sibling "load-glu.lisp")
  (load-sibling "tinaa.system"))

;;; ---------------------------------------------------------------------------

(eksl-load-system 'tinaa)

;;; ***************************************************************************
;;; *                              End of File                                *
;;; ***************************************************************************
