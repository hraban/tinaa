(in-package doclisp)

;;; ---------------------------------------------------------------------------
;;; class
;;; ---------------------------------------------------------------------------

(defclass doclisp-class (doclisp-assembly)
  ()
  (:default-initargs
    :header "Class"
    :part-kind "class"))

;;; ---------------------------------------------------------------------------

(defmethod initialize-instance :after ((object doclisp-class) &key)
  (setf (slot-value object 'instance) (find-class (name object))))

;;; ---------------------------------------------------------------------------

(defmethod find-part ((parent doclisp-assembly) (kind (eql 'slot)) name)
  (iterate-container
   (mopu-class-precedence-list (instance parent))
   (lambda (class)
     (let ((class-part (make-part (some-parent parent) 'class (class-name class))))
       (return-from find-part (item-at (item-at (subparts class-part) 'slot) name)))))
  (values nil))

;;; ---------------------------------------------------------------------------

(defmethod make-part (parent (kind (eql 'class)) name &rest args &key
                              &allow-other-keys)
  (declare (ignore parent))
  (apply #'make-instance 'doclisp-class
    :name name args))

;;; ---------------------------------------------------------------------------

(defmethod subpart-kinds ((part doclisp-class))
  (list '(class :heading "Direct Superclass") 'slot 'method))

;;; ---------------------------------------------------------------------------

(defmethod partname-list ((part doclisp-class) (part-name (eql 'method)))
  (sort
   (direct-specializers-of 
    (name part) :readers? nil :writers? nil :other? t :short-form? t)
   (lambda (a b)
     (string-lessp (if (consp a) (second a) a)
                   (if (consp b) (second b) b)))))

;;; ---------------------------------------------------------------------------

(defmethod partname-list ((part doclisp-class) (part-name (eql 'slot)))
  (sort
   (mopu-class-slot-names (name part))
   #'string-lessp))

;;; ---------------------------------------------------------------------------

(defmethod partname-list ((part doclisp-class) (part-name (eql 'class)))
  (sort
   (delete-if
    (lambda (class-name)
      (class-uninteresting-p part class-name)) 
    (mapcar #'class-name (mopu-class-direct-superclasses (name part))))
   #'string-lessp))

;;; ---------------------------------------------------------------------------

(defun class-uninteresting-p (part class-name)
  (declare (ignore part))
  (member (symbol-package class-name)
          (list (find-package :common-lisp)
                (find-package :common-lisp-user))))
                              
;;; ---------------------------------------------------------------------------

(defmethod part-documentation ((part doclisp-class))
  (documentation (instance part) 'type))
  
;;; ---------------------------------------------------------------------------

(defmethod display-part ((part doclisp-class) (mode (eql :detail)))
  (documenting-page (part)
    (h2 (lml-format "Class ~:(~A~)" name))
    (when documentation? (blockquote documentation))
    
    #+Ignore
    (when (mopu-class-initargs (instance part))
      (lml-princ "Class ")
      (b (lml-princ name))
      (lml-princ " has the following initargs: ")
      (lml-format "~:(~A~)" 
                  (list->formatted-string 
                   (sort (copy-list (mopu-class-initargs (instance part)))
                         #'string-lessp)))
      (br))
    
    ;; summaries
    (output-table-summary part :table-summary 2)))


;;; ---------------------------------------------------------------------------
;;; doclisp-slot
;;; ---------------------------------------------------------------------------

(defclass doclisp-slot (doclisp-part)
  ()
  (:default-initargs
    :header "Slot"
    :part-kind "slot"))

;;; ---------------------------------------------------------------------------

;;?? Gary King 2004-01-31: this is a hack based on my defclass-patch
(defmethod part-documentation ((part doclisp-slot))
  (documentation (name part) 'ccl::slot))

;;; ---------------------------------------------------------------------------

(defmethod make-part (parent (kind (eql 'slot)) name &rest args &key
                              &allow-other-keys)
  (declare (parent parent))
  (apply #'make-instance 'doclisp-slot
    :name name args))

;;; ---------------------------------------------------------------------------

(defmethod display-part ((part doclisp-slot) (mode (eql :table-summary)))
  (let ((slot-info (mopu-class-slot-information 
                    (instance (some-parent part)) (name part)))
        (add-comma? nil)
        (accessors nil))
    
    ;; merge readers/writers
    (let ((readers (getf slot-info :readers))
          (writers (getf slot-info :writers)))
      (loop for reader in readers do
            (when (find reader writers :key #'second)
              (push reader accessors)))
      (loop for accessor in accessors do
            (setf (getf slot-info :readers) (remove accessor (getf slot-info :readers))
                  (getf slot-info :writers) (remove accessor (getf slot-info :writers)
                                                    :key #'second))))
    
    (documenting part
      (tr :class (if (oddp *current-part-index*) "oddrow" "")
          (td :valign "top" :width 200 (link-for mode))
          (td :valign "top"
              (awhen (getf slot-info :initform) 
                (lml-format "initform ~:(~S~)" it)
                (setf add-comma? t))
              
              (awhen (getf slot-info :initargs)
                (when add-comma? (lml-princ ", "))
                (lml-format "initargs ~:(~A~)" 
                            (list->formatted-string it ", " ""))
                (setf add-comma? t))
              
              (awhen accessors
                (when add-comma? (lml-princ ", "))
                (lml-format "accessors ~:(~A~)" 
                            (list->formatted-string it ", " ""))
                (setf add-comma? t))
              
              (awhen (getf slot-info :readers)
                (when add-comma? (lml-princ ", "))
                (when accessors (lml-princ "additional "))
                (lml-format "reader~P ~:*~:(~A~)" 
                            (list->formatted-string it ", " ""))
                (setf add-comma? t))
              
              (awhen (getf slot-info :writers)
                (when add-comma? (lml-princ ", "))
                (when accessors (lml-princ "additional "))
                (lml-format "writer~P ~:*~:(~A~)" 
                            (list->formatted-string it ", " ""))
                (setf add-comma? t))
              
              (awhen (getf slot-info :allocation)
                (when add-comma? (lml-princ ", "))
                (lml-format "allocation ~:(~A~)" it)
                (setf add-comma? t))
              
              (awhen (getf slot-info :type)
                (when add-comma? (lml-princ ", "))
                (lml-format "type ~:(~A~)" it)
                (setf add-comma? t))
              
              (when add-comma?
                (lml-princ ".")))))))