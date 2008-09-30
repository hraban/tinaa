(in-package #:tinaa)

#+Test
(document-system 'tapir-system 'foo "home:tapir;")

(defclass* doclisp-tapir-system (name-holder-mixin doclisp-assembly)
  ((symbol-kinds (list :external) ir))
  (:default-initargs
    :header "Tapir"
    :part-kind "Tapir"
    :document? t))


(defmethod initialize-instance :after ((object doclisp-tapir-system) &key)
  )


(defmethod make-part (parent (kind (eql 'tapir-system)) name &rest args &key
                              &allow-other-keys)
  (declare (ignore parent))
  (apply #'make-instance 'doclisp-tapir-system
    :name name args))


(defmethod subpart-kinds ((part doclisp-tapir-system))
  (list 'action)
  #+Ignore
  (list 'action 'sensor 'goal 'plan))


(defmethod partname-list ((part doclisp-tapir-system) (part-name (eql 'action)))
  (hac:actions-of-type 'hac:tapir-action-mixin))


(defmethod partname-list ((part doclisp-tapir-system) (part-name (eql 'sensor)))
  (sort
   (filtered-package-symbols 
    part
    (lambda (symbol access package)
      (declare (ignore access package))
      (aand (not (hac::internal-class-name-p symbol))
            (find-class symbol nil)
            (typep it 'tapir-sensor-mixin))))
   #'string-lessp))


(defmethod partname-list ((part doclisp-tapir-system) (part-name (eql 'goal)))
  (sort
   (filtered-package-symbols 
    part
    (lambda (symbol access package)
      (declare (ignore access package))
      (aand (not (hac::internal-class-name-p symbol))
            (find-class symbol nil)
            (typep it 'tapir-goal-mixin))))
   #'string-lessp))


(defmethod partname-list ((part doclisp-tapir-system) (part-name (eql 'plan)))
  (sort
   (filtered-package-symbols 
    part
    (lambda (symbol access package)
      (declare (ignore access package))
      (aand (not (hac::internal-class-name-p symbol))
            (find-class symbol nil)
            (typep it 'tapir-plan-mixin))))
   #'string-lessp))

;;; tapir parts

(defclass* tapir-part (doclisp-assembly)
  ())


(defclass* tapir-action (tapir-part)
  ())


(defclass* tapir-sensor (tapir-part)
  ())


(defclass* tapir-goal (tapir-part)
  ())


(defclass* tapir-plan (tapir-part)
  ())


(defmethod make-part ((parent doclisp-tapir-system) (kind (eql 'action)) name
                      &rest args &key &allow-other-keys)
  (apply #'make-instance 'tapir-action :name name args))


(defmethod make-part ((parent doclisp-tapir-system) (kind (eql 'sensor)) name
                      &rest args &key &allow-other-keys)
  (apply #'make-instance 'tapir-sensor :name name args))


(defmethod make-part ((parent doclisp-tapir-system) (kind (eql 'goal)) name
                      &rest args &key &allow-other-keys)
  (apply #'make-instance 'tapir-goal :name name args))


(defmethod make-part ((parent doclisp-tapir-system) (kind (eql 'plan)) name
                      &rest args &key &allow-other-keys)
  (apply #'make-instance 'tapir-plan :name name args))