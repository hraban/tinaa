(in-package doclisp)

;;; ---------------------------------------------------------------------------
;;; variable
;;; ---------------------------------------------------------------------------

(defclass* doclisp-variable (doclisp-part)
  ()
  (:default-initargs
    :header "Variable"
    :part-kind "variable"))

;;; ---------------------------------------------------------------------------

(defmethod make-part (parent (kind (eql 'variable)) name &rest args &key
                              &allow-other-keys)
  (declare (ignore parent))
  (apply #'make-instance 'doclisp-variable
    :name name args))

;;; ---------------------------------------------------------------------------

(defmethod documentation-exists-p ((part doclisp-variable) (mode (eql :detail)))
  (> (length (part-documentation part)) *short-documentation-length*))

;;; ---------------------------------------------------------------------------

(defmethod part-documentation ((part doclisp-variable))
  (documentation (name part) 'variable))

;;; ---------------------------------------------------------------------------

(defmethod display-part ((part doclisp-variable) (mode (eql :detail)))
  (documenting-page (part)
    (:h2 (lml-format "Variable ~:(~A~)" name))
    
    (when documentation? (html (:blockquote (lml-princ documentation))))))


;;; ---------------------------------------------------------------------------
;;; constant
;;; ---------------------------------------------------------------------------

(defclass* doclisp-constant (doclisp-variable)
  ()
  (:default-initargs
    :header "Constant"
    :part-kind "constant"))

;;; ---------------------------------------------------------------------------

(defmethod make-part (parent (kind (eql 'constant)) name &rest args &key
                              &allow-other-keys)
  (declare (ignore parent))
  (apply #'make-instance 'doclisp-constant
    :name name args))

;;; ---------------------------------------------------------------------------

(defmethod display-part ((part doclisp-constant) (mode (eql :detail)))
  (documenting-page (part)
    (:h2 (lml-format "Constant ~:(~A~)" name))
    
    (when documentation? (html (:blockquote (lml-princ documentation))))))
