(in-package #:doclisp)

(defcondition part-kind-unknown-error ()
  ((parent :unbound ir)
   (kind :unbound ir)
   (name :unbound ir)
   (args :unbound ir))
  (:report (lambda (c s)
             (format s "Part kind ~S unknown. Unable to create part named ~S for parent ~S with arguments ~S."
                     (kind c) (name c) (parent c) (args c)))))


(defclass* basic-doclisp-part ()
  ((name "" ir)
   (parents nil ia)
   (instance nil ir :documentation "An instance of this part.")
   (url nil a)
   (flag? nil a)
   (document? nil ia)
   (header "" ia)
   (part-kind "" ir)
   (name-holder nil ir)
   (part-type nil ir))
  (:default-initargs
    :header ""))


(defclass* basic-page-writer ()
  ((css-file nil ia)))


(defclass* simple-page-writer (basic-page-writer)
  ())


(defmethod index-kinds ((part basic-doclisp-part))
  (index-kinds (name-holder part)))


(defmethod initialize-instance :after ((object basic-doclisp-part) &key)
  (setf (slot-value object 'document?)
        (or (document? object) (document-part-p (name-holder object) object)))
  (when (eq (name-holder object) :self)
    (setf (slot-value object 'name-holder) object))
  (unless (part-type object)
    (error "The part-kind must be specified \(use default-initargs\)")))


(defmethod span-class-for-part-name ((name-holder t) (part basic-doclisp-part))
  "")


(defclass* subpart-kind ()
  ((name nil ir)
   (part-kind :unbound ir)
   (heading :unbound ir)
   (document? t ir)
   (index? t ir)))


(defmethod initialize-instance :after ((object subpart-kind) &key)
  (unless (and (slot-boundp object 'heading) (heading object))
    (setf (slot-value object 'heading) 
          (string-capitalize (symbol-name (name object)))))
  (unless (and (slot-boundp object 'part-kind) (part-kind object))
    (setf (slot-value object 'part-kind)
          (name object))))


(defmethod print-object ((object subpart-kind) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~A" (name object))))


(defclass* doclisp-part (basic-doclisp-part)
  ()
  (:documentation "A part without pieces (a leaf)."))


(defclass* doclisp-assembly (basic-doclisp-part)
  ((subparts (make-container 'alist-container
                             :initial-element-fn 
                             (lambda ()
                               (make-container
                                'alist-container))) r)
   (page-writer-class :unbound ir)
   (page-writer :unbound r))
  (:documentation "A part with sub-parts.")
  (:default-initargs
    :page-writer-class 'simple-page-writer))


(defmethod initialize-instance :after ((object doclisp-assembly) &key)
  (setf (slot-value object 'page-writer)
        (make-instance (page-writer-class object))))


(defclass* name-holder-mixin ()
  ())


(defcondition cannot-make-part (error)
  ((parent nil ir)
   (kind nil ir)
   (name nil ir)
   (reason nil ir))
  (:export-p t)
  (:documentation "This error is signaled when a part cannot be created.")
  (:report (lambda (c s)
             (format s "Unable to make part named ~A of kind ~A because ~A" 
                     (name c) (kind c) (reason c)))))
  
(defmethod partname-list :around (part part-kind)
  (declare (ignore part part-kind))
  (handler-case (call-next-method)
    (error () (princ ".") nil)))

(defmethod display-part
    :around ((writer simple-page-writer) part mode &key &allow-other-keys)
  (when (documentation-exists-p part mode)
    (call-next-method)))

(defmethod find-part (parent kind name)
  (declare (ignore parent kind name))
  (values nil))

(defmethod documentation-exists-p (part mode)
  ;; one around method -- oh vey!
  (length-at-least-p 
   (compute-applicable-methods 
    #'display-part 
    (list (page-writer (root-parent part))  part mode))
   2))
