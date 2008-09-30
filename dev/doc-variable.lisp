(in-package #:doclisp)

;;; variable

(defclass* doclisp-variable (doclisp-part)
  ()
  (:default-initargs
    :header "Variable"
    :part-kind "variable"
    :part-type 'variable))


(defmethod make-part (parent (kind (eql 'variable)) name &rest args &key
                              &allow-other-keys)
  (declare (ignore parent))
  (apply #'make-instance 'doclisp-variable
    :name name args))


(defmethod documentation-exists-p ((part doclisp-variable) (mode (eql :detail)))
  (> (length (part-documentation part)) *short-documentation-length*))


(defmethod part-documentation ((part doclisp-variable))
  (documentation (name part) 'variable))


(defmethod display-part ((writer simple-page-writer) (part doclisp-variable)
                         (mode (eql :detail)) &key &allow-other-keys)
  (documenting-page (part)
    (:h2 (lml-format "~A ~:(~A~)" (header part) name))
    
    (maybe-show-documentation part)
    (show-part-parents part)

    (:h3 "Value: " 
         (if (boundp (name part))
           (lml-format "~S" (symbol-value (name part)))
           (lml-princ "&lt;&nbsp; unbound &nbsp;&gt;")))))


;;; constant

(defclass* doclisp-constant (doclisp-variable)
  ()
  (:default-initargs
    :header "Constant"
    :part-kind "constant"
    :part-type 'constant))


(defmethod make-part (parent (kind (eql 'constant)) name &rest args &key
                              &allow-other-keys)
  (declare (ignore parent))
  (apply #'make-instance 'doclisp-constant
    :name name args))

