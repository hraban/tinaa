(in-package doclisp)

(defclass* doclisp-asdf-system (name-holder-mixin doclisp-assembly)
  ()
  (:default-initargs
    :header "ASDF System"
    :part-kind "asdf-system"
    :document? t
    :part-type 'asdf-system))

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
  (list '(direct-package :heading "Direct Package" :part-kind package)
        '(other-package :heading "Other Package" :part-kind package)
        '(direct-dependency :heading "Direct Dependency" :part-kind asdf-system)
        '(other-dependency :heading "Other Dependency" :part-kind asdf-system)
        '(direct-file :heading "Direct File" :part-kind file)
        '(other-file :heading "Other File" :part-kind file)))

;;; ---------------------------------------------------------------------------

(defmethod partname-list ((part doclisp-asdf-system) (part-name (eql 'direct-package)))
  (sort (system-packages part nil) #'string-lessp))
  
;;; ---------------------------------------------------------------------------

(defmethod partname-list ((part doclisp-asdf-system) (part-name (eql 'other-package)))
  (sort (set-difference (system-packages part t) (system-packages part nil))
         #'string-lessp))

;;; ---------------------------------------------------------------------------

(defmethod partname-list ((part doclisp-asdf-system) (part-name (eql 'direct-file)))
  (let ((system-root (system-source-directory (name part))))
    (sort 
     (mapcar (lambda (file)
               (cons (name part) (enough-namestring file system-root)))
             (system-files part nil))
     #'string-lessp
     :key #'cdr)))

;;; ---------------------------------------------------------------------------

(defmethod partname-list ((part doclisp-asdf-system) (part-name (eql 'other-file)))
  (loop for system in (collect-system-dependencies (name part)) nconc
        (partname-list (make-instance 'doclisp-asdf-system :name system) 'direct-file)))

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

(defmethod system-files ((part symbol) system-closure?)
  (mapcar #'namestring 
          (collect-system-files 
           part
           :include-pathname? t
           :system-closure? system-closure?
           :include-associates? nil)))

;;; ---------------------------------------------------------------------------

(defmethod system-files ((part doclisp-asdf-system) system-closure?)
  (system-files (name part) system-closure?))

;;; ---------------------------------------------------------------------------

(defun system-packages (part system-closure?)
  (let ((result nil)
        (files (system-files part system-closure?)))
    (setf result (mapcar (lambda (file) 
                           (aand (ignore-errors (file-package file))
                                 (find-package it)
                                 (package-name it)))
                         files))
    (remove nil
            (remove-if
             #'ignore-package-p
             (remove-duplicates result)))))

;;; ---------------------------------------------------------------------------

(defmethod partname-list ((part doclisp-asdf-system) (part-name (eql 'direct-dependency)))
  (system-dependencies (name part)))

;;; ---------------------------------------------------------------------------

(defmethod partname-list ((part doclisp-asdf-system) (part-name (eql 'other-dependency)))
  (set-difference (collect-system-dependencies (name part))
                  (system-dependencies (name part))))

;;; ---------------------------------------------------------------------------

(defmethod index-kinds ((part doclisp-asdf-system))
  #+NotYet
  (list '((package)))
  (list '((class))
        '((condition)) 
        '((variable constant))
        '((function generic-function))
        '((macro))
        '((package))
        '((symbol))
        '((symbol) 
          :heading "Permuted"
          :index-name "permuted-symbols"
          :build-using build-permuted-index
          :index-kind 
          permuted)))

;;; ---------------------------------------------------------------------------

(defmethod display-part ((part doclisp-asdf-system) (mode (eql :detail))
                         &key &allow-other-keys)
  (documenting-page (part)
    (:h2 (lml-format "ASDF System ~A" name))
    (when documentation (html (:blockquote (lml-princ documentation))))
    
    ;; summaries
    (output-table-summary part :table-summary 1)))

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
;;; doclisp-file
;;; ---------------------------------------------------------------------------

(defclass* doclisp-file (basic-doclisp-part)
  ((system nil ir)
   (filename nil ir)
   (enough-filename nil ir))
  (:default-initargs
    :header "File"
    :part-kind "file"
    :document? t
    :part-type 'file))

;;; ---------------------------------------------------------------------------

(defmethod make-part (parent (kind (eql 'file)) name &rest args 
                             &key &allow-other-keys)
  ;; we handle files specially: the name is a cons of the form (system . enough-namestring)
  (declare (ignore parent))
  (let* ((system (car name))
         (enough (cdr name))
         (system-root (system-source-directory system)))
    (apply #'make-instance 'doclisp-file
           :name (concatenate 'string 
                              (symbol->string system) 
                              (substitute #\- #\: enough)) 
           :enough-filename enough
           :filename (namestring (merge-pathnames enough system-root))
           :system system
           args)))

;;; ---------------------------------------------------------------------------

(defmethod part-name ((part doclisp-file))
  (string-downcase (filename part)))

;;; ---------------------------------------------------------------------------

(defmethod display-part ((part doclisp-file) (mode (eql :table-summary))
                         &key &allow-other-keys)
  (documenting part
   ((:tr :class (if (oddp *current-part-index*) "oddrow" ""))
    ((:td :valign "top" :width 200) 
     (spy name url (system part) (enough-filename part) (filename part))
     
     (link-for mode)))))






