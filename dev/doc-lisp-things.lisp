(in-package #:doclisp)

;;; lisp package

(defclass package-system (doclisp-assembly)
  ())

(defmethod initialize-instance :after ((object package-system) &key)
  (setf (slot-value object 'instance) (find-package (name object))))


(defmethod make-part (parent (kind (eql 'package)) name)
  (make-instance 'package-system
    :parent parent
    :name name))


(defmethod subpart-kinds ((part package-system))
  (list 'class 'variable 'function 'method))


(defmethod partname-list ((part package-system) (part-name (eql 'class)))
  (sort
   (filtered-package-symbols 
    part
    (lambda (symbol access package)
      (declare (ignore access package))
      (aand (find-class symbol nil)
            (typep it 'standard-class))))
   (lambda (a b)
     (let ((ca (find-class a))
           (cb (find-class b)))
       (cond ((subtypep ca cb) nil)
             ((subtypep cb ca) t)
             (t (string-lessp a b)))))))


(defmethod partname-list ((part package-system) (part-name (eql 'variable)))
  (filtered-package-symbols 
   part
   (lambda (symbol access package)
     (declare (ignore access package))
     (boundp symbol))))


(defmethod partname-list ((part package-system) (part-name (eql 'function)))
  (filtered-package-symbols 
   part
   (lambda (symbol access package)
     (declare (ignore access package))
     (and (fboundp symbol)
          (typep (symbol-function symbol) 'function)
          (not (typep (symbol-function symbol) 'standard-generic-function))))))


(defmethod partname-list ((part package-system) (part-name (eql 'method)))
  (filtered-package-symbols 
   part
   (lambda (symbol access package)
     (declare (ignore access package))
     (and (fboundp symbol)
          (typep (symbol-function symbol) 'standard-generic-function)))))


(defun filtered-package-symbols (part filter)
  (let ((result nil))
    (with-package-iterator (symbol-fn (list (name part)) :external :internal)
      (loop
        (multiple-value-bind (more? symbol access package) (symbol-fn)
          (unless more?
            (return))
          (when (funcall filter symbol access package)
            (push #+No (princ-to-string symbol) symbol result))))
      (sort result #'string-lessp))))


(defmethod make-system-part ((part package-system) (part-kind (eql 'class))
                             part-name &key)
  (make-instance 'doclisp-class
    :name (u:form-symbol-in-package (name part) part-name)))


(defmethod display-part ((part package-system) (mode (eql :detail)))
  (documenting part
    (:h2 name)
    (maybe-show-documentation part)
    (loop for kind in (subpart-kinds part) do
          (when (plusp (size (parts kind)))
            (htm
             (:h3 kind)
             (:table
              (iterate-container 
               (parts kind)
               (lambda (thing) 
                 (display-part thing :table-summary)))))))))

