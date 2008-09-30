(in-package #:doclisp)

(defclass* doclisp-glu-system (name-holder-mixin doclisp-assembly)
  ()
  (:default-initargs
    :header "glu System"
    :part-kind "glu-system"
    :document? t))


(defmethod make-part (parent (kind (eql 'glu-system)) name &rest args 
                             &key &allow-other-keys)
  (declare (ignore parent))
  (apply #'make-instance 'doclisp-glu-system
    :name name args))


(defclass* doclisp-file (basic-doclisp-part)
  ((filename nil ir))
  (:default-initargs
    :header "File"
    :part-kind "file"
    :document? t))


(defmethod make-part (parent (kind (eql 'file)) name &rest args 
                             &key &allow-other-keys)
  (declare (ignore parent))
  (apply #'make-instance 'doclisp-file
    :name (string->symbol (substitute #\- #\: name)) 
    :filename name
    args))


(defmethod subpart-kinds ((part doclisp-glu-system))
  (list 'package 'glu-system 'file))


(defmethod partname-list ((part doclisp-glu-system) (part-name (eql 'package)))
  (let ((result nil))
    (user:map-glu-system-files
     (name part)
     (lambda (file)
       (awhen (file-package file) 
         (push (intern (package-name it) "KEYWORD") result)))
     :include-pathname? t
     :include-associates? nil)
    
    (remove-if
     #'ignore-package-p
     (remove-duplicates result :test #'string-equal))))


(defmethod partname-list ((part doclisp-glu-system) (part-name (eql 'glu-system)))
  (let ((result nil)
        (canonical-name (user::canonicalize-glu-system-name (name part))))
    (user:map-glu-sub-systems
     (name part)
     (lambda (system)
       (unless (eq system canonical-name)
         (push system result)))
     :include-modules? nil)
    
    result))


(defmethod partname-list ((part doclisp-glu-system) (part-name (eql 'file)))
  (let ((result nil))
    (user:map-glu-system-files
     (name part)
     (lambda (file)
       (push (namestring file) result))
     :include-pathname? t
     :include-associates? nil)
    
    (sort result #'string-lessp)))


(defmethod index-kinds ((part doclisp-glu-system))
  (list '(class) '(variable constant) '(function generic-function macro) '(package)))


(defmethod display-part ((part doclisp-glu-system) (mode (eql :detail)))
  (documenting-page (part)
    (:h2 (lml-format "glu System ~A" name))
    (maybe-show-documentation part)
    
    ;; summaries
    (output-table-summary part :table-summary 1)))


(defmethod display-part ((part doclisp-file) (mode (eql :table-summary)))
  (documenting part
   ((:tr :class (if (oddp *current-part-index*) "oddrow" ""))
    ((:td :valign "top" :width 200) (link-for mode)))))

#+Ignore
(defmethod display-part ((part doclisp-file) (mode (eql :table-summary)))
  (documenting part
    (if (oddp *current-part-index*)
      (td :valign "top" :width 200 (link-for mode))
      (tr (td :valign "top" :width 200 (link-for mode))))))


(defmethod part-name ((part doclisp-file))
  (string-downcase (filename part)))
