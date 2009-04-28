(in-package #:doclisp)

#|
;; empirically most of the properties found in ASDF system definitions
(:author :components :depends-on :description :in-order-to :licence
         :long-description :maintainer :name :perform :properties :serial :version)

;; we can about these
:author  :licence :maintainer :version

:description        => short-documentation
:long-description   => documentation 
:components         => files
:depends-on         => dependencies

;; we might case about these
:name  :serial :in-order-to  :perform :properties

|#

(defclass* doclisp-asdf-system (name-holder-mixin doclisp-assembly)
  ((author-name nil r)
   (author-mail nil r)
   (maintainer-name nil ir)
   (maintainer-mail nil ir))
  
  (:default-initargs
    :header "ASDF System"
    :part-kind "asdf-system"
    :document? t
    :part-type 'asdf-system))


(defmethod initialize-instance :after ((object doclisp-asdf-system) &key)
  (let ((system (dsc:find-system (name object))))
    (assert (system-loaded-p (name object)))
    (setf (slot-value object'instance) system)
    (setf (values (slot-value object 'author-name) 
                  (slot-value object 'author-mail))
          (find-name-and-email (system-property (name object) 'author))
          (values (slot-value object 'maintainer-name)
                  (slot-value object 'maintainer-mail))
          (find-name-and-email (system-property (name object) 'maintainer)))))


(defun find-name-and-email (string)
  "Returns \(as multiple values\) a name and e-mail address as parsed from a string. Handles only \"name <mail>\" right now. if the string isn't in this from,then this function assumes that the string contains only a name. Also doesn't handle group projects!"
  (let ((pos-< (position #\< string :test #'char-equal))
        (pos-> (position #\> string :test #'char-equal)))
    (if (and pos-< pos-> (< pos-< pos->))
      (values (subseq string 0 (1- pos-<)) 
              (subseq string (1+ pos-<) pos->))
      (values string nil))))

#+LIFT
(deftestsuite test-find-name-and-email ()
  ()
  (:test ((ensure-same (find-name-and-email "gwking <gwking@foo.com>")
                       (values "gwking" "gwking@foo.com") :test #'string-equal)))
  (:test ((ensure-same (find-name-and-email "gwking gwking@foo.com")
                       (values "gwking gwking@foo.com" nil) :test #'string-equal)))
  (:test ((ensure-same (find-name-and-email "")
                       (values "" nil) :test #'string-equal)))
  (:test ((ensure-same (find-name-and-email "gwking >gwking@foo.com<")
                       (values "gwking >gwking@foo.com<" nil) :test #'string-equal))))


(defmethod make-part (parent (kind (eql 'asdf-system)) name &rest args 
                             &key &allow-other-keys)
  (declare (ignore parent))
  (apply #'make-instance 'doclisp-asdf-system
         :name name args))


(defmethod part-documentation ((part doclisp-asdf-system))
  (or (system-property (name part) 'long-description)
      (system-property (name part) 'description)))
  

(defmethod short-documentation ((part doclisp-asdf-system))
  (or (system-property (name part) 'description)
      (awhen (system-property (name part) 'long-description) 
        (let ((max-length (size it)))
          (subseq it 0 (min max-length *short-documentation-length*)))) 
      (call-next-method)))


(defmethod subpart-kinds ((part doclisp-asdf-system))
  (list '(direct-dependency :heading "Direct Dependency" :part-kind asdf-system)
        '(other-dependency :heading "Other Dependency" :part-kind asdf-system)
        '(direct-package :heading "Direct Package" :part-kind package)
        '(other-package :heading "Other Package" :part-kind package)
        '(direct-file :heading "Direct File" :part-kind file)
        '(other-file :heading "Other File" :part-kind file)))


(defmethod partname-list ((part doclisp-asdf-system) (part-name (eql 'direct-package)))
  (sort (system-packages part nil) #'string-lessp))
  

(defmethod partname-list ((part doclisp-asdf-system) (part-name (eql 'other-package)))
  (sort (set-difference (system-packages part t) (system-packages part nil))
         #'string-lessp))


(defmethod partname-list ((part doclisp-asdf-system) (part-name (eql 'direct-file)))
  (let ((system-root (system-source-directory (name part))))
    (sort 
     (mapcar (lambda (file)
               (cons (name part) (enough-namestring file system-root)))
             (system-files part nil))
     #'string-lessp
     :key #'cdr)))


(defmethod partname-list ((part doclisp-asdf-system) (part-name (eql 'other-file)))
  (sort 
   (loop for system in (collect-system-dependencies (name part)) nconc
         (partname-list (make-instance 'doclisp-asdf-system :name system) 'direct-file))
   #'string-lessp
   :key #'cdr))

  
(defmethod partname-list ((part doclisp-asdf-system) (part-name (eql 'file)))
  (sort 
   (collect-elements
    (collect-system-files (name part)
                          :include-pathname? t)
    :transform #'namestring)
   #'string-lessp))


(defmethod system-files ((part symbol) system-closure?)
  (mapcar #'namestring 
          (collect-system-files 
           part
           :include-pathname? t
           :system-closure? system-closure?
           :include-non-source? nil)))


(defmethod system-files ((part doclisp-asdf-system) system-closure?)
  (system-files (name part) system-closure?))


(defun system-packages (part system-closure?)
  (let ((result nil)
        (files (system-files part system-closure?)))
    (setf result (mapcar (lambda (file) 
                           (aand (ignore-errors (file-package file))
                                 (find-package it)
                                 (canonical-package-id it)))
                         files))
    (remove nil
            (remove-if
             #'ignore-package-p
             (remove-duplicates result)))))


(defmethod partname-list ((part doclisp-asdf-system) (part-name (eql 'direct-dependency)))
  (system-dependencies (name part)))


(defmethod partname-list ((part doclisp-asdf-system) (part-name (eql 'other-dependency)))
  (set-difference (collect-system-dependencies (name part))
                  (system-dependencies (name part))))


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


(defmethod display-part ((writer simple-page-writer) (part doclisp-asdf-system)
                         (mode (eql :detail)) &key &allow-other-keys)
  (documenting-page (part)
    (:h2 (lml-format "ASDF System ~A" name))
    (maybe-show-documentation part)
    
    (when (or (author-name part)
              (maintainer-name part)
              (system-property name 'licence)
              (system-property name 'version))
      (flet ((output-name (heading name mail)
               (html (:tr
                      (:th (lml-princ heading)) 
                      (:td 
                       (cond ((and name mail)
                              (html ((:a :href (format nil "mailto:~A" mail))
                                     (lml-princ name))))
                             (name
                              (html (lml-princ name))))))))
             (output-property (heading value)
               (html (:tr 
                      (:th (lml-princ heading))
                      (:td (lml-princ value))))))
        (html
         ((:div :class "part-summary")
          (:table 
           (when (author-name part)
             (output-name "Author:" (author-name part) (author-mail part)))
           (when (and (maintainer-name part)
                      (not (string-equal (author-name part) (maintainer-name part))))
             (output-name "Maintainer:" (maintainer-name part) (maintainer-mail part)))
           (awhen (system-property name 'version)
             (output-property "Version:" it))
           (awhen (system-property name 'licence)
             (cond ((> (size it) *short-documentation-length*)
                    (let ((url (build-license-page part)))
                      (html 
                       (:tr 
                        (:th "License:")
                        (:td (lml-princ (subseq it 0 *short-documentation-length*))
                             "... [" ((:a :href url) "More") "]")))))
                   (t (output-property "License:" it)))))))))
    
    (show-part-parents part)
                
    ;; summaries
    (output-table-summary writer part 1)))


(defun build-license-page (part)
  (let* ((file (url->file (url part)))
         (*document-file* 
          (make-pathname 
           :name "license"
           :defaults (namestring (translate-logical-pathname file)))))
    (with-new-file (*document-stream* 
                    *document-file*)
      (with-html-output (*document-stream*)
        (html
         (:html
          (:head
           (:title (lml-format "License | ~A ~:(~A~) [Tinaa]"
                               (header part) (name part)))
           ((:link :rel "stylesheet" :href (stylesheet-url part))))
          (:body
           ;;?? this and documenting-page macro should be refactored
           (doclisp-header nil :force-contents-link? t)
           
           ((:div :class "contents")
            (:h2 (lml-format "License for ASDF System ~A" (name part)))
            ((:div :class "license")
             (lml-princ (system-property (name part) 'licence)))
            (:div ((:a :href (relative-url (url part))) "Back")))
           
           (doclisp-footer part :force-contents-link? t)))))
    
      (values (namestring (pathname-name+type *document-file*))))))
       

(defmethod output-table-summary-of-parts (writer part
                                                 (subpart-name (eql 'other-file))     
                                                 heading)
  (let ((parts (item-at (subparts part) subpart-name))
        (count 1)
        (system nil))
    
    (when (some-key-value-p parts
                            (lambda (name part)
                              (declare (ignore name))
                              (and (document? part)
                                   (documentation-exists-p part :table-summary))))
      (html
       ((:div :class "table-summary")
        (:h3 (lml-princ heading)) 
        ((:table :id (substitute-if #\- 'whitespacep (string-downcase heading)))
         (iterate-container 
          parts
          (lambda (thing) 
            (when (document? thing)
              (unless (eq system (system thing))
                (setf system (system thing))
                (let ((sub-system (find-part part 'asdf-system system))) 
                  (html
                   ((:tr :class "subsystem-name")
                    (:th (if sub-system 
                           (display-part writer sub-system :name+type)
                           (lml-princ system)))))))
              
              (let ((*current-part-index* count))
                (display-part writer thing :table-summary))
              (incf count))))))))))


;;; doclisp-file

(defclass* doclisp-file (basic-doclisp-part)
  ((system nil ir)
   (filename nil ir)
   (enough-filename nil ir))
  (:default-initargs
    :header "File"
    :part-kind "file"
    :document? t
    :part-type 'file))


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


(defmethod part-name ((part doclisp-file))
  (string-downcase (filename part)))


(defmethod display-part ((writer simple-page-writer) (part doclisp-file)
                         (mode (eql :table-summary)) &key &allow-other-keys)
  (documenting part
   ((:tr :class (if (oddp *current-part-index*) "oddrow" ""))
    (:td (lml-princ  (enough-filename part))))))





