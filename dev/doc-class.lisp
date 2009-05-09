(in-package #:doclisp)

(defclass doclisp-class (doclisp-assembly)
  ()
  (:default-initargs
    :header "Class"
    :part-kind "class"
    :part-type 'class))

(defmethod initialize-instance :after ((object doclisp-class) &key)
  (setf (slot-value object 'instance) (find-class (name object))))

(defmethod make-part (parent (kind (eql 'class)) name &rest args &key
                              &allow-other-keys)
  (declare (ignore parent))
  (apply #'make-instance (if (conditionp name) 
                           'doclisp-condition 'doclisp-class)
    :name name args))

(defmethod subpart-kinds ((part doclisp-class))
  (list '(superclass :heading "Direct Superclass" :part-kind class)
        '(subclass :heading "Direct Subclass" :part-kind class)
        'slot 
        '(direct-method :heading "Direct Method" :part-kind method)
        '(other-method :heading "Other Method" :part-kind method)))

(defmethod partname-list ((part doclisp-class) (part-name (eql 'method)))
  (sort
   (direct-specializers-of 
    (name part) :readers? nil :writers? nil :other? t :short-form? t)
   (lambda (a b)
     (string-lessp (if (consp a) (second a) a)
                   (if (consp b) (second b) b)))))

(defmethod partname-list ((part doclisp-class) (part-name (eql 'direct-method)))
  (sort
   (direct-specializers-of 
    (name part) :readers? nil :writers? nil :other? t :short-form? t)
   (lambda (a b)
     (string-lessp (if (consp a) (second a) a)
                   (if (consp b) (second b) b)))))

(defmethod partname-list ((part doclisp-class) (part-name (eql 'other-method)))
  (sort
   (set-difference
    (specializers-of 
     (name part) :readers? nil :writers? nil :other? t :short-form? t)
    (direct-specializers-of 
     (name part) :readers? nil :writers? nil :other? t :short-form? t))
   (lambda (a b)
     (string-lessp (if (consp a) (second a) a)
                   (if (consp b) (second b) b)))))

(defmethod partname-list ((part doclisp-class) (part-name (eql 'slot)))
  (sort
   (slot-names (instance part))
   #'string-lessp))

(defmethod partname-list ((part doclisp-class) (part-name (eql 'class)))
  (sort
   (delete-if
    (lambda (class-name)
      (class-uninteresting-p class-name))
    (mapcar #'class-name (direct-superclasses (name part))))
   #'string-lessp))

(defmethod partname-list ((part doclisp-class) (part-name (eql 'superclass)))
  (interesting-superclasses (name part)))

(defmethod partname-list ((part doclisp-class) (part-name (eql 'subclass)))
  (interesting-subclasses (name part)))

(defun interesting-superclasses (thing)
  (sort
   (delete-if
    (lambda (class-name)
      (class-uninteresting-p class-name)) 
    (mapcar #'class-name (direct-superclasses thing)))
   #'string-lessp))

(defun interesting-subclasses (thing)
  (sort
   (delete-if
    (lambda (class-name)
      (class-uninteresting-p class-name)) 
    (mapcar #'class-name (direct-subclasses thing)))
   #'string-lessp))

(defun class-uninteresting-p (class-name)
  (or (not (find-class class-name))
      (let ((package (canonical-package-id
		       (symbol-package 
			(class-name-of (get-class class-name))))))
	(or (ignore-package-p package)
		  (not (member package (packages-to-document)))))
       ;; make sure we can create it (e.g.., that it isn't just a 
       ;; forward-referenced class
      (null (ignore-errors
	      (allocate-instance (find-class class-name))))))

(defmethod part-documentation ((part doclisp-class))
  (documentation (instance part) 'type))
  
(defmethod display-part ((writer simple-page-writer) (part doclisp-class)
                         (mode (eql :detail)) &key &allow-other-keys)
  (documenting-page (part)
    (:h2 (lml-format "~A ~:(~A~)" (header part) name))
    (maybe-show-documentation part)
    (show-part-parents part)
    (bind (((:values it error) (ignore-errors
				 (default-initargs (instance part)))))
      (cond (error
	     (html 
	      (:div "Unable to introspect class")))
	    (it
	     (html 
	      ((:div :class "table-summary")
	       (:h3 "Default initargs")
	       ((:table :id "default-initargs")
		(let ((iterator (make-iterator it))
		      (count 1))
		  (flet ((one-thing (thing)
			   (bind (((name value nil) thing))
			     (html
			      (:th (lml-format "~(~S~) &rarr; ~S" 
					       name value)))))
			 (no-thing ()
			   (html
			    (:th ""))))
		    (iterate-elements 
		     iterator
		     (lambda (default-initarg)
		       (html
			((:tr :class (if (oddp count) "oddrow" ""))
			 (one-thing default-initarg)
			 (move-forward iterator)
			 (if (current-element-p iterator)
			     (one-thing (current-element iterator))
			     (no-thing))))))))))))))
                
    ;; summaries
    (output-table-summary writer part 2)))


;;; conditions

(defclass doclisp-condition (doclisp-class)
  ()
  (:default-initargs
    :header "Condition"
    :part-kind "condition"
    :part-type 'condition))


(defmethod initialize-instance :after ((object doclisp-condition) &key)
  (setf (slot-value object 'instance) (find-class (name object))))


(defmethod subpart-kinds ((part doclisp-condition))
  (list '(superclass :heading "Direct Superclass" :part-kind condition)
        '(subclass :heading "Direct Subclass" :part-kind condition)
        'slot 
        '(direct-method :heading "Direct Method" :part-kind method)
        '(other-method :heading "Other Method" :part-kind method)))


(defmethod make-part (parent (kind (eql 'condition)) name &rest args &key
                              &allow-other-keys)
  (declare (ignore parent))
  (apply #'make-instance (if (conditionp name) 
                           'doclisp-condition 'doclisp-class)
    :name name args))


(defmethod part-documentation ((part doclisp-condition))
  ;; this works around what may or may not be a glitch in SBCL 
  ;; (see lisppaste 18639 for details
  (documentation (class-name (instance part)) 'type))

;;; doclisp-slot

(defclass* doclisp-slot (doclisp-part)
  ((direct-parent :unbound w))
  (:default-initargs
    :header "Slot"
    :part-kind "slot"
    :part-type 'slot))


;;?? bit of a hack in progress
(defmethod direct-instance ((slot doclisp-slot))
  (let ((parent (direct-parent slot)))
    (if parent
      (instance parent)
      ;; no direct parent found, probably not an exported symbol so fake it...
      (iterate-elements
       (parents slot)
       (lambda (parent)
         (awhen (some-element-p 
                 (superclasses (instance parent) :proper? t)
                 (lambda (superclass)
                   (member (name slot) 
                           (direct-slot-names superclass))))
           (return-from direct-instance it)))))))


(defmethod direct-parent ((slot doclisp-slot))
  (unless (and (slot-boundp slot 'direct-parent)
               (slot-value slot 'direct-parent))
    (setf (direct-parent slot)
          (some-element-p (parents slot)
                          (lambda (parent) 
                            (member (name slot) 
                                    (direct-slot-names (instance parent)))))))
  (slot-value slot 'direct-parent))


(defmethod part-documentation ((part doclisp-slot))
  (documentation (get-slot-definition (instance (direct-parent part)) (name part)) t))


(defmethod make-part (parent (kind (eql 'slot)) name &rest args &key
                              &allow-other-keys)
  (declare (ignore parent))
  (apply #'make-instance 'doclisp-slot
    :name name args))


(defun slot-writer-name (writer)
  (cond ((and (consp writer) (eq (first writer) 'setf))
         (second writer))
        ((atom writer)
         writer)
        (t
         (error "I don't know how to understand the slot writer '~A'" writer))))


(defmethod display-part ((writer simple-page-writer) (part doclisp-slot)
                         (mode (eql :table-summary)) &key &allow-other-keys)
  (let* ((instance (direct-instance part)) 
         (slot-info (slot-properties instance (name part)))
         (add-comma? nil)
         (accessors nil))
    
    ;; merge readers/writers into accessors
    (let ((readers (getf slot-info :readers))
          (writers (getf slot-info :writers)))
      (loop for reader in readers do
            (when (find reader writers :key #'slot-writer-name)
              (push reader accessors)))
      (loop for accessor in accessors do
            (setf (getf slot-info :readers) (remove accessor (getf slot-info :readers))
                  (getf slot-info :writers) (remove accessor (getf slot-info :writers)
                                                    :key #'slot-writer-name))))
    
    (documenting part
      ((:tr :class (if (oddp *current-part-index*) "oddrow" ""))
          (:th (link-for mode))
          (:td
           (awhen (getf slot-info :documentation)
             (html ((:div :class "documentation") (lml-princ it))))
           
           (awhen (getf slot-info :initform) 
             (html ((:span :class "property-heading") "Initform:")
                   ((:span :class "property-value") (lml-format "~(~A~)" it)))
             (setf add-comma? t))
           
           (awhen (getf slot-info :initargs)
             (when add-comma? (lml-princ ", "))
             (html ((:span :class "property-heading") "Initargs:")
                   ((:span :class "property-value") 
                    ;;?? hacky -- wish I could remember format directives!
                    (apply #'lml-format "~(~#[~;~S~:;~@{~#[~;~]~S~^,~}~]~)" it)))
             (setf add-comma? t))
           
           (awhen accessors
             (when add-comma? (lml-princ "; "))
             (html ((:span :class "property-heading") "Accessors:")
                   ((:span :class "property-value") 
                    (lml-format "~(~A~)" (list->formatted-string it ", " ""))))
             (setf add-comma? t))
           
           (awhen (getf slot-info :readers)
             (when add-comma? (lml-princ "; "))
             (html ((:span :class "property-heading")
                    (when accessors (lml-princ "Additional ")) 
                    (lml-format "Reader~P:" (size it)))
                   ((:span :class "property-value") 
                    (lml-format "~(~A~)" (list->formatted-string it ", " ""))))
             (setf add-comma? t))
           
           (awhen (getf slot-info :writers)
             (when add-comma? (lml-princ "; "))
             (html ((:span :class "property-heading")
                    (when accessors (lml-princ "Additional "))
                    (lml-format "Writer~P:" (size it)))
                   ((:span :class "property-value") 
                    (lml-format "~(~A~)" (list->formatted-string it ", " ""))))
             (setf add-comma? t))
           
           (awhen (getf slot-info :allocation)
             (when add-comma? (lml-princ "; "))
             (html ((:span :class "property-heading") "Allocation:")
                   ((:span :class "property-value") 
                    (lml-format "~(~A~)" it)))
             (setf add-comma? t))
           
           (awhen (getf slot-info :type)
             (when add-comma? (lml-princ "; "))
             (html ((:span :class "property-heading") "Type:")
                   ((:span :class "property-value") 
                    (lml-format "~(~A~)" it)))
             (setf add-comma? t))
           
           (when add-comma?
             (lml-princ ".")))))))


(defclass* tinaa-part-graph (cl-graph:graph-container)
  ((root-part nil ir))
  (:default-initargs
    :vertex-class 'tinaa-part-vertex))

  
(defclass* tinaa-part-vertex (cl-graph:graph-container-vertex)
  ((part nil ir)))


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
