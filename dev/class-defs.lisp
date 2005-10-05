(in-package doclisp)

(defclass* basic-doclisp-part ()
  ((name "" ir)
   (parents nil ia)
   (instance nil ir :documentation "An instance of this part.")
   (url nil a)
   (flag? nil a)
   (document? nil ir)
   (header "" ia)
   (part-kind "" ir))
  (:default-initargs
    :header ""))

;;; ---------------------------------------------------------------------------

(defmethod initialize-instance :after ((object basic-doclisp-part) &key)
  (setf (slot-value object 'document?)
        (or (document? object) (document-part-p object))))

;;; ---------------------------------------------------------------------------

(defclass* subpart-kind ()
  ((name "" ir)
   (heading :unbound ir)
   (document? t ir)
   (index? t ir)))

;;; ---------------------------------------------------------------------------

(defmethod initialize-instance :after ((object subpart-kind) &key)
  (unless (and (slot-boundp object 'heading) (heading object))
    (setf (slot-value object 'heading) 
          (string-capitalize (symbol-name (name object))))))

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
