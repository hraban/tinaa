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

#+Remove
;;??Gary King 2005-12-30: 
(defmethod find-part ((parent doclisp-assembly) (kind (eql 'slot)) name)
  (iterate-container
   (mopu:superclasses (instance parent))
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
  (list '(superclass :heading "Direct Superclass" :part-kind class)
        '(subclass :heading "Direct Subclass" :part-kind class)
        'slot 'method))

(defmethod subpart-kinds ((part doclisp-class))
  (list '(class :heading "Direct Superclass")
        'slot 'method))

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
   (slot-names (name part))
   #'string-lessp))

;;; ---------------------------------------------------------------------------

(defmethod partname-list ((part doclisp-class) (part-name (eql 'class)))
  (sort
   (delete-if
    (lambda (class-name)
      (class-uninteresting-p class-name)) 
    (mapcar #'class-name (direct-superclasses (name part))))
   #'string-lessp))

;;; ---------------------------------------------------------------------------

(defmethod partname-list ((part doclisp-class) (part-name (eql 'superclass)))
  (sort
   (delete-if
    (lambda (class-name)
      (class-uninteresting-p class-name)) 
    (mapcar #'class-name (direct-superclasses (name part))))
   #'string-lessp))

;;; ---------------------------------------------------------------------------

(defmethod partname-list ((part doclisp-class) (part-name (eql 'subclass)))
  (sort
   (delete-if
    (lambda (class-name)
      (class-uninteresting-p class-name)) 
    (mapcar #'class-name (direct-subclasses (name part))))
   #'string-lessp))

;;; ---------------------------------------------------------------------------

#+Remove
(defmethod find-part ((part doclisp-assembly) (kind (eql 'class)) name)
  (or (call-next-method) 
      (find-part part 'superclass name)
      (find-part part 'subclass name)))

;;; ---------------------------------------------------------------------------

(defun class-uninteresting-p (class-name)
  (member (symbol-package (class-name-of (get-class class-name)))
          (list (find-package :common-lisp)
                (find-package :common-lisp-user))))
                              
;;; ---------------------------------------------------------------------------

(defmethod part-documentation ((part doclisp-class))
  (documentation (instance part) 'type))
  
;;; ---------------------------------------------------------------------------

(defmethod display-part ((part doclisp-class) (mode (eql :detail)))
  (documenting-page (part)
    (:h2 (lml-format "Class ~:(~A~)" name))
    (when documentation? (html (:blockquote documentation)))
    
    #+Ignore
    (when (mopu-class-initargs (instance part))
      (lml-princ "Class ")
      (:b (lml-princ name))
      (lml-princ " has the following initargs: ")
      (lml-format "~:(~A~)" 
                  (list->formatted-string 
                   (sort (copy-list (mopu-class-initargs (instance part)))
                         #'string-lessp)))
      (:br))
    
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
  #+MCL
  (documentation (name part) 'ccl::slot)
  #-MCL
  nil)

;;; ---------------------------------------------------------------------------

(defmethod make-part (parent (kind (eql 'slot)) name &rest args &key
                              &allow-other-keys)
  (declare (ignore parent))
  (apply #'make-instance 'doclisp-slot
    :name name args))

;;; ---------------------------------------------------------------------------

(defmethod display-part ((part doclisp-slot) (mode (eql :table-summary)))
  (let ((slot-info (slot-properties 
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
      ((:tr :class (if (oddp *current-part-index*) "oddrow" ""))
          ((:td :valign "top" :width 200) (link-for mode))
          ((:td :valign "top")
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

;;; ---------------------------------------------------------------------------

(defclass* tinaa-part-graph (cl-graph:graph-container)
  ((root-part nil ir))
  (:default-initargs
    :vertex-class 'tinaa-part-vertex))

;;; ---------------------------------------------------------------------------
  
(defclass* tinaa-part-vertex (cl-graph:graph-container-vertex)
  ((part nil ir)))

;;; ---------------------------------------------------------------------------

(defun make-local-class-graph (class-part)
  (bind ((g (cl-graph:make-graph 'tinaa-part-graph
                                 :root-part (root-parent class-part)))
         (instance (class-name (instance class-part)))
         (vertex (cl-graph:add-vertex g instance :part class-part))
         (root-part (root-parent class-part)))
    (iterate-elements
     (direct-superclasses instance)
     (lambda (superclass)
       (unless (class-uninteresting-p superclass)
         (bind ((super-part-name (class-name superclass))
                (super-part (find-part root-part 'class super-part-name))) 
           (cl-graph:add-edge-between-vertexes
            g (cl-graph:add-vertex g super-part-name :part super-part) vertex
            :edge-type :directed)))))
    (iterate-elements
     (direct-subclasses instance)
     (lambda (subclass)
       (unless (class-uninteresting-p subclass)
         (bind ((sub-part-name (class-name subclass))
                (sub-part (find-part root-part 'class sub-part-name))) 
           (cl-graph:add-edge-between-vertexes
            g instance (cl-graph:add-vertex g sub-part-name :part sub-part)
            :edge-type :directed)))))
    g))

;;; ---------------------------------------------------------------------------

(defun class-graph->dot (graph)
  "Returns a string describing graph in DOT format."
  (cl-graph:graph->dot 
   graph
   nil
   :graph-formatter (lambda (g stream)
                      (declare (ignore g))
                      (format stream "rankdir=LR"))
   
   :vertex-labeler
   (lambda (vertex stream)
     (format stream "~(~A~)" (symbol-name (element vertex))))
   
   :vertex-formatter 
   (lambda (vertex stream)
     (let ((part (part vertex))) 
       (when part
         (format stream "URL=\"~A\"," (url-for-part part)))
       (cond ((leaf-class-p (element vertex))
              (format stream "color=\"black\", style=\"filled\", fontcolor=\"white\", ~
                              fillcolor=\"blue\""))
             ((empty-p (direct-superclasses (element vertex)))
              (format stream "color=\"black\", style=\"filled\", fontcolor=\"white\", ~
                              fillcolor=\"green\"")))))))
