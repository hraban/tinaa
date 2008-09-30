(in-package #:doclisp)

(defclass* doclisp-function (doclisp-part)
  ()
  (:default-initargs
    :header "Function"
    :part-kind "function"
    :part-type 'function))


(defmethod make-part (parent (kind (eql 'function)) name &rest args &key
                              &allow-other-keys)
  (declare (ignore parent))
  (apply #'make-instance 'doclisp-function
    :name name args))


(defmethod part-name ((part doclisp-function))
  (string-downcase (atypecase (name part)
                     (symbol (symbol-name it))
                     (cons 
                      (apply #'concatenate 'string 
                             (mapcar #'symbol-name it))))))


(defmethod part-documentation ((part doclisp-function))
  (documentation (name part) 'function))


(defmethod display-part ((writer simple-page-writer) (part doclisp-function)
                         (mode (eql :detail)) &key &allow-other-keys)
  (documenting-page (part)
    (:h2 (lml-format "~@(~A~) ~:(~A~)" (header part) name))
    (show-part-parents part)
    
    (display-function part)
    
    (maybe-show-documentation part)))


;;; generic functions

(defclass* doclisp-generic-function (doclisp-function)
  ()
  (:default-initargs
    :header "Generic Function"
    :part-kind "generic function"
    :part-type 'generic-function))


(defmethod make-part (parent (kind (eql 'generic-function)) name &rest args &key
                              &allow-other-keys)
  (declare (ignore parent))
  (apply #'make-instance 'doclisp-generic-function
    :name name
    :instance (symbol-function name) args))


#+Ignore
(defun gf-info (symbol)
  (let ((gf (symbol-function symbol)))
    (list 
     :declarations (generic-function-declarations gf)
     :lambda-list (generic-function-lambda-list gf)
     :method-class (generic-function-method-class gf)
     :method-combination (generic-function-method-combination  gf)
     :methods (generic-function-methods  gf) 
     :name (generic-function-name  gf))))


#+Ignore
(defun write-gf-template (symbol stream)
  (let ((info (gf-info symbol)))
    (format stream "\(defgeneric ~(~A~) " (getf info :name))
    (format stream "~(~A~)" (getf info :lambda-list))
    (when (and (getf info :method-combination)
               (not (eq (ccl::method-combination-name
			 (getf info :method-combination)) 
                        'standard)))
      (format stream "~%  \(:method-combination ~(~A~)\)" 
	      (getf info :method-combination)))
    (if (documentation symbol 'function)
      (format stream "~%  \(:documentation ~S\)\)"
	      (documentation symbol 'function))
      (format stream "~%  \(:documentation \"\"\)\)~%")))
  (terpri stream)
  (values))


(defmethod display-part ((writer simple-page-writer) 
			 (part doclisp-generic-function)
                         (mode (eql :detail)) &key &allow-other-keys)
  (let ((method-count (length (generic-function-methods (instance part)))))
    (documenting-page (part)
      (:h2 (lml-format "Generic Function ~:(~A~) \(~D method~:P\)"
                       name method-count))
      (display-function part)
      (show-part-parents part)
      (maybe-show-documentation part)
      #+Ignore
      (when (aand (generic-function-method-combination (instance part))
                  (not (eq it (generic-function-method-combination 
			       #'display-part))))
        (html 
         (:p "Method combination")))
      (:h3 "Method Summary")
      ((:table :class "method-table")
       (iterate-container
        (sort 
         (copy-list (mopu:generic-function-methods (instance part)))
         #'method-sorter)
        (lambda (m)
          (html 
           (:tr
            (:td (lml-format "~(~A ~@[~{~S ~}~]~)"
                             name
                             (method-qualifiers m))
                 (iterate-container
                  (mopu:method-specializers m)
                  (lambda (s)
                    (let* ((name
                            (or (mopu:eql-specializer-p s)
                                (typecase s
                                  (standard-class (class-name s))
                                  (built-in-class (class-name s))
                                  (t (type-of s)))))
                           (part (find-part (name-holder part) 'class name)))
                      (let ((*print-case* :downcase))
                        (cond ((consp name)
                               (lml-format "~S" name))
                              (t
                               (lml-princ "&lt; ")
                               (if part
                                 (display-part writer part :function)
                                 (lml-format "~S" name))
                               (lml-princ " &gt;&nbsp;"))))))))))))))))


(defmethod display-part ((writer simple-page-writer) (part basic-doclisp-part)
                         (mode (eql :function)) &key &allow-other-keys)
  (html 
   ((:span :class "function-parameter")
    ;;?? Gary King 2006-03-31: this is also in display-part-for-index
    (if (documentation-exists-p part :detail)
      (html ((:a :href (relative-url (url part)))
             (lml-princ (part-name part))))
      (lml-princ (part-name part))))))


(defun method-sorter (m1 m2)
  (cond ((string-lessp (method-name m1) (method-name m2))
         (values t))
        ((string-lessp (method-name m2) (method-name m1))
         (values nil))
        (t
         (block sort-specializers
           (let ((s1 (mopu:method-specializers m1))
                 (s2 (mopu:method-specializers m2)))
             (loop for sp1 in s1
                   for sp2 in s2 do
                   ;; the format here is a bit of a hack to handle eql specializers
                   ;; that specialize on things other than symbols
                   (let ((eq1? (awhen (eql-specializer-p sp1) 
                                 (format nil "~S" it)))
                         (eq2? (awhen (eql-specializer-p sp2) 
                                 (format nil "~S" it))))
                     (cond ((and (null eq1?) (null eq2?))
                            (cond ((string-lessp (class-name sp1) (class-name sp2))
                                   (return-from sort-specializers t))
                                  ((string-lessp (class-name sp2) (class-name sp1))
                                   (return-from sort-specializers nil))))
                           ((and (null eq1?))
                            (return-from sort-specializers t))
                           ((and (not (null eq1?)) (not (null eq2?)))
                            (cond ((string-lessp eq1? eq2?)
                                   (return-from sort-specializers t))
                                  ((string-lessp eq2? eq1?)
                                   (return-from sort-specializers nil))))))))
           
           (let ((q1 (method-qualifiers m1))
                 (q2 (method-qualifiers m2)))
             (loop for qp1 in q1
                   for qp2 in q2 do
                   (cond ((string-lessp qp1 qp2)
                          (return-from sort-specializers t))
                         ((string-lessp qp2 qp1)
                          (return-from sort-specializers nil)))))))))
         
;;; method

(defclass* doclisp-method (doclisp-function)
  ()
  (:default-initargs
    :header "Method"
    :part-kind "method"
    :part-type 'method))


(defmethod make-part (parent (kind (eql 'method)) name &rest args &key
                              &allow-other-keys)
  (declare (ignore parent))
  (apply #'make-instance 'doclisp-method
    :name name args))


(defmethod display-part ((writer simple-page-writer) (part doclisp-method)
                         (mode (eql :table-summary)) &key &allow-other-keys)
  (let ((gf (find-part (name-holder part) 'generic-function (name part))))
    (documenting part
      ((:tr :class (if (oddp *current-part-index*) "oddrow" ""))
       (:th
        (if gf
          (make-link-for mode name (url gf))
          (link-for mode)))
       (:td (when documentation? (lml-princ short-documentation)))))))


;;; macro

(defclass* doclisp-macro (doclisp-function)
  ()
  (:default-initargs
    :header "Macro"
    :part-kind "macro"
    :part-type 'macro))


(defmethod make-part (parent (kind (eql 'macro)) name &rest args &key
                              &allow-other-keys)
  (declare (ignore parent))
  (apply #'make-instance 'doclisp-macro
    :name name args))


;;; utilities

;;;; Copyright (C) 2003 Sven Van Caekenberghe.
(defun symbol-name-tree (x)
  (cond ((null x) '())
	((symbolp x) (if (and (null (symbol-package x))
			      (char= #\G (char (symbol-name x) 0)))
			 ;; trying to filter out gensyms
			 ;; hoping there is only one of these per arglist
			 "x"
		       (string-downcase (symbol-name x))))
	((stringp x) (string-downcase x))
	((numberp x) (princ-to-string x))
	((listp x) (mapcar #'symbol-name-tree x))
	(t (error "unknown tree element"))))