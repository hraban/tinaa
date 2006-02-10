(in-package doclisp)

(defclass* doclisp-asdf-system (name-holder-mixin doclisp-assembly)
  ()
  (:default-initargs
    :header "ASDF System"
    :part-kind "asdf-system"
    :document? t))

;;; ---------------------------------------------------------------------------

(defmethod initialize-instance :after ((object doclisp-asdf-system) &key)
  (setf (slot-value object'instance) (dsc:find-system (name object))))

;;; ---------------------------------------------------------------------------

(defmethod make-part (parent (kind (eql 'asdf-system)) name &rest args 
                             &key &allow-other-keys)
  (declare (ignore parent))
  (apply #'make-instance 'doclisp-asdf-system
         :name name args))

;;; ---------------------------------------------------------------------------

(defmethod part-documentation ((part doclisp-asdf-system))
  (or (system-property (name part) 'long-description)
      (system-property (name part) 'description)))
  
;;; ---------------------------------------------------------------------------

(defmethod short-documentation ((part doclisp-asdf-system))
  (or (system-property (name part) 'description)
      (call-next-method)))

;;; ---------------------------------------------------------------------------

(defmethod subpart-kinds ((part doclisp-asdf-system))
  (list 'package 'asdf-system 'file))

;;; ---------------------------------------------------------------------------

(defmethod partname-list ((part doclisp-asdf-system) (part-name (eql 'package)))
  (let ((result nil)
        (files (collect-system-files 
                (name part)
                :include-pathname? t
                :system-closure? t
                :include-associates? nil)))
    (setf result (mapcar (lambda (file) 
                           (aand (ignore-errors (file-package file))
                                 (find-package it)
                                 (package-name it)))
                         files))
    (remove nil
            (remove-if
             #'ignore-package-p
             (remove-duplicates result)))))

#+WEIRD
(defmethod partname-list ((part doclisp-asdf-system) (part-name (eql 'package)))
  (let ((result nil))
    (map-system-files
     (name part)
     (lambda (file)
       (print file)
       (awhen (ignore-errors (file-package file)) 
         (princ it)
         (push (package-name (find-package it)) result)))
     :include-pathname? t
     :system-closure? t
     :include-associates? nil)
    
    (remove-if
     #'ignore-package-p
     (remove-duplicates result))))

;;; ---------------------------------------------------------------------------

(defmethod partname-list ((part doclisp-asdf-system) (part-name (eql 'asdf-system)))
  (collect-system-dependencies (name part)))

;;; ---------------------------------------------------------------------------

(defmethod partname-list ((part doclisp-asdf-system) (part-name (eql 'file)))
  (sort 
   (collect-elements
    (collect-system-files (name part)
                          :include-pathname? t
                          :include-associates? nil)
    :transform #'namestring)
   #'string-lessp))

;;; ---------------------------------------------------------------------------

(defmethod index-kinds ((part doclisp-asdf-system))
  (list '(class) '(variable constant) '(function generic-function macro) '(package)))

;;; ---------------------------------------------------------------------------

(defmethod display-part ((part doclisp-asdf-system) (mode (eql :detail))
                         &key &allow-other-keys)
  (documenting-page (part)
    (:h2 (lml-format "asdf System ~A" name))
    (when documentation (html (:blockquote (lml-princ documentation))))
    
    ;; summaries
    (output-table-summary part :table-summary 1)))


;;; ---------------------------------------------------------------------------
;;; doclisp-file
;;; ---------------------------------------------------------------------------

(defclass* doclisp-file (basic-doclisp-part)
  ((filename nil ir))
  (:default-initargs
    :header "File"
    :part-kind "file"
    :document? t))

;;; ---------------------------------------------------------------------------

(defmethod make-part (parent (kind (eql 'file)) name &rest args 
                             &key &allow-other-keys)
  (declare (ignore parent))
  (apply #'make-instance 'doclisp-file
    :name (string->symbol (substitute #\- #\: name)) 
    :filename name
    args))

;;; ---------------------------------------------------------------------------

(defmethod part-name ((part doclisp-file))
  (string-downcase (filename part)))

;;; ---------------------------------------------------------------------------

(defmethod display-part ((part doclisp-file) (mode (eql :table-summary))
                         &key &allow-other-keys)
  (documenting part
   ((:tr :class (if (oddp *current-part-index*) "oddrow" ""))
    ((:td :valign "top" :width 200) (link-for mode)))))

;;; ---------------------------------------------------------------------------

#+Ignore
(defmethod display-part ((part doclisp-file) (mode (eql :table-summary)))
  (documenting part
    (if (oddp *current-part-index*)
      (td :valign "top" :width 200 (link-for mode))
      (tr (td :valign "top" :width 200 (link-for mode))))))




