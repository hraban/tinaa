(in-package doclisp)

(defclass* basic-doclisp-part ()
  ((name "" ir)
   (parents nil ia)
   (instance nil ir :documentation "An instance of this part.")
   (url nil a)
   (flag? nil a)
   (document? nil ir)
   (header "" ia)
   (part-kind "" ir)
   (name-holder nil ir))
  (:default-initargs
    :header ""))

;;; ---------------------------------------------------------------------------

(defmethod initialize-instance :after ((object basic-doclisp-part) &key)
  (setf (slot-value object 'document?)
        (or (document? object) (document-part-p (name-holder object) object)))
  (when (eq (name-holder object) :self)
    (setf (slot-value object 'name-holder) object)))

;;; ---------------------------------------------------------------------------

(defclass* subpart-kind ()
  ((name nil ir)
   (part-kind :unbound ir)
   (heading :unbound ir)
   (document? t ir)
   (index? t ir)))

;;; ---------------------------------------------------------------------------

(defmethod initialize-instance :after ((object subpart-kind) &key)
  (unless (and (slot-boundp object 'heading) (heading object))
    (setf (slot-value object 'heading) 
          (string-capitalize (symbol-name (name object)))))
  (unless (and (slot-boundp object 'part-kind) (part-kind object))
    (setf (slot-value object 'part-kind)
          (name object))))

;;; ---------------------------------------------------------------------------

(defmethod print-object ((object subpart-kind) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~A" (name object))))

;;; ---------------------------------------------------------------------------

(defclass* doclisp-part (basic-doclisp-part)
  ()
  (:documentation "A part without pieces (a leaf)."))

;;; ---------------------------------------------------------------------------

(defclass* doclisp-assembly (basic-doclisp-part)
  ((subparts (make-container 'alist-container
                             :initial-element-fn 
                             (lambda ()
                               (make-container
                                'alist-container))) r))
  (:documentation "A part with sub-parts."))

;;; ---------------------------------------------------------------------------

(defclass* name-holder-mixin ()
  ())

;;; ---------------------------------------------------------------------------

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
  