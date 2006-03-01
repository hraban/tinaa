(in-package doclisp)

(defclass* doclisp-symbol (doclisp-part)
  ()
  (:default-initargs
    :header "symbol"
    :part-kind "symbol"))

;;; ---------------------------------------------------------------------------

(defmethod make-part (parent (kind (eql 'symbol)) name &rest args &key
                              &allow-other-keys)
  (declare (ignore parent))
  (apply #'make-instance 'doclisp-symbol
    :name name args))

;;; ---------------------------------------------------------------------------

(defmethod span-class-for-part-name ((name-holder doclisp-package)
                                     (part doclisp-symbol))
  (symbol-name
   (nth-value 1 (find-symbol (symbol-name (name part)) (instance name-holder)))))