(in-package #:doclisp)

;;; eksl-system

(defclass eksl-system (basic-doclisp-system)
  ())

(defmethod system-part-names ((system eksl-system))
  (list 'file 'package 'class 'variable 'function 'method))

(defmethod part-names ((system eksl-system) (part-name (eql 'class)))
  )