;;;-*- Mode: Lisp; Package: doclisp -*-

(in-package #:doclisp)

;; from utils
(defmethod name (object)
  (type-of object))

(defmethod name ((object symbol))
  object)

(defparameter *make-part-methods* nil
  "Used to help ensure that we don't use up the stack trying to find a good method... Probably too baroque.")

(defmethod make-part (parent kind name &rest args &key &allow-other-keys)
  (unless *make-part-methods*
    (setf *make-part-methods* 
          (compute-applicable-methods #'make-part (list nil nil nil))))
   
  (let ((tinaa-symbol (form-symbol-in-package :tinaa kind)))
    (if (set-equal 
         *make-part-methods*
         (compute-applicable-methods 
          #'make-part
          (list parent tinaa-symbol name args)))
      ;; we tried this already
      (error 'part-kind-unknown-error :parent parent :kind kind :name name :args args)
      (apply #'make-part parent tinaa-symbol name args))))


;;?? Gary King 2006-04-02:  fowls things up...
#+Ignore
(defmethod make-part :around (parent kind (name string) &rest args 
                                     &key &allow-other-keys)
  (declare (dynamic-extent args))
  (apply #'make-part parent kind (string->symbol name) args))


(defmethod make-part :around (parent kind name &key &allow-other-keys)
  (aprog1
    (aif (find-part (name-holder parent) kind name)
         it
         (call-next-method))
    (pushnew parent (parents it))))


(defun some-parent (part)
  (first (parents part)))

#+(or)
;; what we ought to be able to use (see below)
(defun root-parent (part)
  "Go up the parent chain until there are no parents... that's the root."
  (if (some-parent part)
    (root-parent (some-parent part))
    part))


(defun root-parent (part)
  "Go up the parent chain until there are no parents... that's the root."
  ;; there are cases where tinaa can be confused and create loops. One
  ;; case is when a class has a superclass and the superclass hasn't been
  ;; defined.
  (let ((searched (make-container 'simple-associative-container)))
    (labels ((look-up (it)
	       (cond ((item-at-1 searched it)
		      ;; circularity!?
		      (warn "~a is its own parent!" it)
		      it)
		     (t
		      (setf (item-at-1 searched it) t)
		      (if (some-parent it)
			  (look-up (some-parent it))
			  it)))))
      (look-up part))))


(defun tinaa-home ()
  (system-source-directory 'tinaa))


(defun subpart-info (part kind)
  ;; consy
  (find kind (subpart-kinds part) :key #'name))


(defmethod document-part-p ((name-holder name-holder-mixin) (part basic-doclisp-part))
  (find (canonical-package-id (symbol-package (part-symbol-name part)))
        (packages-to-document)))


(defun part-symbol-name (part)
  (if (and (consp (name part))
           (eq (first (name part)) 'setf))
    (second (name part))
    (name part)))


(defun packages-to-document ()
  *packages-to-document*)

(defun add-package-to-document (package-name)
  (unless (ignore-package-p package-name)
    (pushnew (canonical-package-id package-name) *packages-to-document*)))

(defun ignore-package-p (p)
  (member p (packages-to-ignore)))

(defun packages-to-ignore ()
  ;; hack
  *default-packages-to-ignore*)

(defun canonical-package-id (package)
  (ignore-errors
   (form-keyword (typecase package
                   (package (package-name package))
                   (t (package-name (find-package package)))))))

(defmethod subpart-kinds :around ((assembly doclisp-assembly))
  (mapcar (lambda (kind)
            (let ((name (first (ensure-list kind)))
                  (args (rest (ensure-list kind))))
              (apply #'make-instance 'subpart-kind 
                     :name name args)))
          (call-next-method)))

(defmethod name-holder ((part (eql nil)))
  nil)

(defmethod name-holder ((part name-holder-mixin))
  (or (name-holder (some-parent part)) part))

(defmethod part-name ((part basic-doclisp-part))
  (string-downcase (symbol-name (name part))))

(defmethod print-object ((system basic-doclisp-part) stream)
  (print-unreadable-object (system stream :type t :identity t)
    (format stream "~A" (name system))))

(defmethod part-documentation ((part basic-doclisp-part))
  nil)

(defmethod short-documentation ((part basic-doclisp-part))
  (let ((doc (part-documentation part)))
    (if (> (length doc) (- *short-documentation-length* 3))
      (concatenate 'string (subseq doc 0 (- *short-documentation-length* 3))
                   "...")
      doc)))

(defmethod find-part ((part name-holder-mixin) kind name)
  (or (aand (item-at (subparts part) kind)
            (item-at it name))
      (and (not (eq (name-holder part) part))
           (find-part (name-holder part) kind name))
      (call-next-method)))

(defmethod grovel-part ((part basic-doclisp-part))
  )

(defmethod subpart-kinds ((part basic-doclisp-part))
  (values nil))

(defmethod grovel-part ((main-part doclisp-assembly))
  (let ((seen (make-container 'associative-container)))
    (labels ((do-it (part)
               (unless (item-at seen part)
                 (setf (item-at seen part) t)
                 (start-grovel part)
                 (map-subpart-kinds 
                  part
                  (lambda (subpart-info)
                    (let* (;;?? Gary King 2005-12-30:  bad name here
                           (subpart-kind (part-kind subpart-info))
                           (kind (name subpart-info))
                           (parts-names (partname-list part kind))
                           (name-holder (if (typep part 'name-holder-mixin) 
                                          part (name-holder part))))
                      (loop for part-name in parts-names
                            do
                            (let ((sub-part (make-part
                                             part subpart-kind part-name
                                             :name-holder name-holder)))
                              ;; (print part-name)
                              (when sub-part
                                (unless (eq subpart-kind kind)
                                  (setf (item-at (item-at (subparts part) kind) (name sub-part))
                                        sub-part))
                                #+(or)
                                (format t "~%~20,A ~20,A ~20,A" 
                                        part-name subpart-kind (part-type sub-part)) 
                                
                                ;;?? Gary King 2005-12-30: perhaps a hack
                                ;;?? Gary King 2006-03-31: perhaps another hack <smile>
                                ;;   make-part needn't return a part of the same kind as subpart-kind (cf class / conditions)
                                (when (eq subpart-kind (part-type sub-part)) 
                                  (setf (item-at (item-at (subparts main-part) subpart-kind) (name sub-part))
                                        sub-part))
                                (setf (item-at (item-at (subparts part) subpart-kind) (name sub-part))
                                      sub-part)
                                (when (document? sub-part)
                                  (do-it sub-part))))))))
                 (finish-grovel part))))
      (do-it main-part))))

(defmethod start-grovel ((part basic-doclisp-part))
  (values))

(defmethod finish-grovel ((part basic-doclisp-part))
  (values))

(defmethod finish-grovel ((part doclisp-assembly))
  (map-subpart-kinds
   part
   (lambda (subpart-info)
     (sort-keys (item-at (subparts part) (name subpart-info))
                #'string-lessp
                :key 
                (lambda (key)
                  (string-downcase (atypecase key
                                     (symbol (symbol-name it))
                                     (cons 
                                      (apply #'concatenate 'string 
                                             (mapcar #'symbol-name it))))))))))

(defmethod include-kind-in-index-p ((part basic-doclisp-part) (kind t))
  nil)

(defmethod display-part ((writer simple-page-writer) (part basic-doclisp-part)
                         (mode (eql :name)) &key &allow-other-keys)
  (html 
   (if (documentation-exists-p part :detail)
     (html ((:a :href (relative-url (url part)))
            (lml-princ (part-name part))))
     (lml-princ (part-name part)))))

(defmethod display-part ((writer simple-page-writer) (part basic-doclisp-part)
                         (mode (eql :name+type)) &key &allow-other-keys)
  (html 
   (lml-princ (header part)) " "
   (if (documentation-exists-p part :detail)
     (html ((:a :href (relative-url (url part)))
            (lml-princ (part-name part))))
     (lml-princ (part-name part)))))

(defun parts-with-no-documentation (part)
  (let ((result nil))
    (map-parts-from-leaves
     part
     (lambda (subpart)
       (when 
         ;;?? HACK
         (ignore-errors
              (and (document-part-p part subpart)
                   (part-can-have-documention-p subpart)
                   (or (not (part-documentation subpart))
                       (zerop (size (part-documentation subpart))))))
         (push subpart result))))
    result))

(defun part-can-have-documention-p (part)
  (not (or (typep part 'doclisp-symbol)         ; won't have any
           (typep part 'doclisp-method)         ; can't have any
           )))

;;; ***************************************************************************
;;; *                              End of File                                *
;;; ***************************************************************************