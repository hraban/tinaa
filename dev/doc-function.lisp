(in-package doclisp)

(defclass* doclisp-function (doclisp-part)
  ()
  (:default-initargs
    :header "Function"
    :part-kind "function"))

;;; ---------------------------------------------------------------------------

(defmethod make-part (parent (kind (eql 'function)) name &rest args &key
                              &allow-other-keys)
  (declare (ignore parent))
  (apply #'make-instance 'doclisp-function
    :name name args))

;;; ---------------------------------------------------------------------------

(defmethod part-name ((part doclisp-function))
  (string-downcase (atypecase (name part)
                     (symbol (symbol-name it))
                     (cons 
                      (apply #'concatenate 'string 
                             (mapcar #'symbol-name it))))))

;;; ---------------------------------------------------------------------------

(defmethod part-documentation ((part doclisp-function))
  (documentation (name part) 'function))

;;; ---------------------------------------------------------------------------

(defmethod display-part ((part doclisp-function) (mode (eql :detail)))
  (documenting-page (part)
    (:h2 (lml-format "Function ~:(~A~)" name))
    
    (display-function part)
    
    (when documentation? (html (:blockquote (lml-princ documentation))))))


;;; ---------------------------------------------------------------------------
;;; generic functions
;;; ---------------------------------------------------------------------------

(defclass* doclisp-generic-function (doclisp-function)
  ()
  (:default-initargs
    :header "Generic Function"
    :part-kind "generic function"))

;;; ---------------------------------------------------------------------------

(defmethod make-part (parent (kind (eql 'generic-function)) name &rest args &key
                              &allow-other-keys)
  (declare (ignore parent))
  (apply #'make-instance 'doclisp-generic-function
    :name name
    :instance (symbol-function name) args))

;;; ---------------------------------------------------------------------------

(defmethod display-part ((part doclisp-generic-function) (mode (eql :detail)))
  (documenting-page (part)
    (:h2 (lml-format "Generic Function ~:(~A~)" name))
    
    (display-function part)
    (when documentation? (html (:blockquote (lml-princ documentation))))
    
    (:h3 (lml-format "Method Summary \(~D method~:P\)" 
                    (length (mopu:generic-function-methods (instance part)))))
    
    (:table
      (iterate-container
       (sort 
        (copy-list (mopu:generic-function-methods (instance part)))
        #'method-sorter)
       (lambda (m)
         (html 
          ((:tr :valign "top")
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
                          (part (find-part (some-parent part) 'class name)))
                     (let ((*print-case* :downcase))
                       (cond ((consp name)
                              (lml-format "~S" name))
                             (t
                              (lml-princ "&lt; ")
                              (if part
                                (display-part part :index)
                                (lml-format "~S" name))
                              (lml-princ " &gt;&nbsp;")))))))))))))))

;;; ---------------------------------------------------------------------------

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
         
;;; ---------------------------------------------------------------------------
;;; method
;;; ---------------------------------------------------------------------------

(defclass* doclisp-method (doclisp-function)
  ()
  (:default-initargs
    :header "Method"
    :part-kind "method"))

;;; ---------------------------------------------------------------------------

(defmethod make-part (parent (kind (eql 'method)) name &rest args &key
                              &allow-other-keys)
  (declare (ignore parent))
  (apply #'make-instance 'doclisp-method
    :name name args))

;;; ---------------------------------------------------------------------------

(defmethod display-part ((part doclisp-method) (mode (eql :table-summary)))
  (let ((gf (find-part (name-holder part) 'generic-function (name part))))
    (documenting part
      ((:tr :class (if (oddp *current-part-index*) "oddrow" ""))
          ((:td :valign "top" :width 200) 
              (if gf
                (make-link-for mode name (url gf))
                (link-for mode)))
          ((:td :valign "top") (when documentation? (lml-princ short-documentation)))))))


;;; ---------------------------------------------------------------------------
;;; macro
;;; ---------------------------------------------------------------------------

(defclass* doclisp-macro (doclisp-function)
  ()
  (:default-initargs
    :header "Macro"
    :part-kind "macro"))

;;; ---------------------------------------------------------------------------

(defmethod make-part (parent (kind (eql 'macro)) name &rest args &key
                              &allow-other-keys)
  (declare (ignore parent))
  (apply #'make-instance 'doclisp-macro
    :name name args))

;;; ---------------------------------------------------------------------------

(defmethod display-part ((part doclisp-macro) (mode (eql :table-summary)))
  (let ((gf (find-part (some-parent part) 'macro (name part))))
    (documenting part
      ((:tr :class (if (oddp *current-part-index*) "oddrow" ""))
          ((:td :valign "top" :width 200) 
              (if gf
                (make-link-for mode name (url gf))
                (link-for mode)))
          ((:td :valign "top") (when documentation? (lml-princ short-documentation)))))))


;;; ---------------------------------------------------------------------------
;;; utilities
;;; ---------------------------------------------------------------------------

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