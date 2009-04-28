(in-package #:doclisp)

;;; lisp package

(defclass* doclisp-package (name-holder-mixin doclisp-assembly)
  ((symbol-kinds (list :internal :external) ia))
  (:default-initargs
    :header "Package"
    :part-kind "package"
    :document? t
    :part-type 'package))


(defmethod initialize-instance :after ((object doclisp-package) &key)
  (format t "~%Package: ~A" (name object))
  (setf (slot-value object 'instance) (find-package (name object))))


(defmethod document-part-p ((name-holder doclisp-package) (part doclisp-package))
  (values t))


(defmethod document-part-p ((name-holder doclisp-package) (part basic-doclisp-part))
  (and #+Ignore
       ;;?? Gary King 2006-01-30: having this prevents imported exported symbols from 
       ;; appearing in documentation. e.g., process-wait in portable-threads.
       (call-next-method)
       
       ;; Thanks to Todd Mokros
       (symbolp (part-symbol-name part))
       
       (multiple-value-bind (found? status) 
                            (find-symbol (symbol-name (part-symbol-name part))
                                         (instance name-holder))
         (declare (ignore found?))
         (cond ((member status (symbol-kinds name-holder))
                (values t))
               (t
                (values nil))))))


(defmethod make-part (parent (kind (eql 'package)) name &rest args &key
                             &allow-other-keys)
  (if (find-package name)
    (apply #'make-instance 'doclisp-package
           :name (intern (package-name (find-package name)) :keyword) args)
    (error 'cannot-make-part :parent parent :kind kind :name name
           :reason "package not found.")))


(defmethod include-kind-in-index-p ((part doclisp-package) (kind (eql 'symbol)))
  nil)


(defmethod include-kind-in-index-p ((part doclisp-package) (kind (eql 'method)))
  nil)


(defmethod include-kind-in-index-p ((part doclisp-package) (kind t))
  t)


(defmethod subpart-kinds ((part doclisp-package))
  (list 'condition 'class 'variable 'constant 'function 'generic-function
        'macro '(symbol :document? nil)))


(defmethod index-kinds ((part doclisp-package))
  (list '((class))
        '((condition)) 
        '((variable constant))
        '((function generic-function))
        '((macro))
        '((symbol))
        '((symbol) 
          :heading "Permuted"
          :index-name "permuted-symbols"
          :build-using build-permuted-index
          :index-kind 
          permuted)))


(defmethod part-documentation ((part doclisp-package))
  (documentation (instance part) t))


(defmethod partname-list ((part doclisp-package) (part-name (eql 'class)))
  (sort
   (filtered-package-symbols 
    part
    (lambda (symbol access package)
      (declare (ignore access package))
      (let ((class (find-class symbol nil))) 
        (and class 
             (typep class 'standard-class)
             (not (conditionp class))))))
   #'class-sorter))


(defmethod partname-list ((part doclisp-package) (part-name (eql 'condition)))
  (sort
   (filtered-package-symbols 
    part
    (lambda (symbol access package)
      (declare (ignore access package))
      (aand (find-class symbol nil)
            (conditionp it))))
   #'class-sorter))


(defun  conditionp (thing)
  "Returns true if and only if thing is a condition"
  (c2mop:subclassp thing 'condition))


(defun class-sorter (a b)
  (let ((ca (find-class a))
        (cb (find-class b)))
    (cond ((subtypep ca cb) nil)
          ((subtypep cb ca) t)
          (t (string-lessp a b)))))


(defmethod partname-list ((part doclisp-package) (part-name (eql 'variable)))
  (filtered-package-symbols 
   part
   (lambda (symbol access package)
     (declare (ignore access package))
     (and (boundp symbol)
          (not (constantp symbol))))))


(defmethod partname-list ((part doclisp-package) (part-name (eql 'constant)))
  (filtered-package-symbols 
   part
   (lambda (symbol access package)
     (declare (ignore access package))
     (and (boundp symbol) 
          (constantp symbol)))))


(defmethod partname-list ((part doclisp-package) (part-name (eql 'function)))
  (filtered-package-symbols 
   part
   (lambda (symbol access package)
     (declare (ignore access package))
     (and (fboundp symbol)
          (not (macro-function symbol))
          (typep (symbol-function symbol) 'function)
          (not (typep (symbol-function symbol) 'standard-generic-function))))))


(defmethod partname-list ((part doclisp-package) (part-name (eql 'generic-function)))
  (filtered-package-symbols 
   part
   (lambda (symbol access package)
     (declare (ignore access package))
     (and (fboundp symbol)
          (typep (symbol-function symbol) 'standard-generic-function)
          (some (lambda (m)
                  (not (or (reader-method-p m)
                           (writer-method-p m))))
                (mopu:generic-function-methods (symbol-function symbol)))))))


(defmethod partname-list ((part doclisp-package) (part-name (eql 'macro)))
  (filtered-package-symbols 
   part
   (lambda (symbol access package)
     (declare (ignore access package))
     (and (macro-function symbol)))))


(defmethod partname-list ((part doclisp-package) (part-name (eql 'symbol)))
  (filtered-package-symbols 
   part
   (lambda (symbol access package)
     (declare (ignore symbol access package))
     t)))


(defun filtered-package-symbols (part filter)
  (let ((result nil)
        (package (find-package (name part))))
    (cond (package
           (when (member :external (symbol-kinds part)) 
             (with-package-iterator (symbol-fn (list package) :external)
               (loop
                 (multiple-value-bind (more? symbol access package) (symbol-fn)
                   (unless more?
                     (return))
                   (when (and (eq (symbol-package symbol) package)
                              (funcall filter symbol access package))
                     (push symbol result))))))
           (when (member :internal (symbol-kinds part)) 
             (with-package-iterator (symbol-fn (list package) :internal)
               (loop
                 (multiple-value-bind (more? symbol access package) (symbol-fn)
                   (unless more?
                     (return))
                   (when (and (eq (symbol-package symbol) package)
                              (funcall filter symbol access package))
                     #+Old (funcall filter symbol access package)
                     (push symbol result))))))
           (sort result #'string-lessp))
          (t
           (error "Package ~A not found." (name part))))))


(defun nice-package-name (package)
  (string-capitalize (package-name package)))


(defmethod start-grovel :before ((part doclisp-package))
  (add-package-to-document (name part)))


(defun update-document-part-p (package)
  (map-parts-from-leaves 
   package 
   (lambda (subpart)
     (setf (document? subpart) (document-part-p package subpart)))))


(defmethod document-part-to-file ((writer basic-page-writer)
				  (part doclisp-package))
  (fluid-bind (((symbol-kinds part) '(:external)))
    (update-document-part-p part)
    (call-next-method))
  
  (fluid-bind (((symbol-kinds part) '(:internal :external))
               (*document-file* (make-pathname :name "internal-index"
                                               :type "html"
                                               :defaults *document-file*)))
    (update-document-part-p part)
    (call-next-method)))


(defmethod maybe-show-documentation :before ((part doclisp-package))
  ;;?? hacky!!!! - depends too much on other stuff that is going on.
  (if (equal (symbol-kinds part) '(:external))
    (html 
     ((:div :class "package-symbols")
      "See " ((:a :href "internal-index.html") "internal symbols") " too"))
    (html 
     ((:div :class "package-symbols")
      "See only " ((:a :href "index.html") "external symbols")))))


(defmethod display-part ((writer simple-page-writer) (part doclisp-package)
                         (mode (eql :detail)) &key &allow-other-keys)
  (let* ((package (find-package (name part)))
         (package-name (string-capitalize (package-name package)))
         (sentence-starter (format nil "Package ~A " package-name)))
    (documenting-page (part)
      (:h2 (lml-format "Package ~A - " package-name)
           (when (member :internal (symbol-kinds part))
             (lml-princ "internal"))
           (when (member :external (symbol-kinds part))
             (when (member :internal (symbol-kinds part))
               (lml-princ " and "))
             (lml-princ "external"))
           (lml-princ " symbols"))
      (html
       (show-part-parents part)
       (maybe-show-documentation part)
       
       ((:div :class "part-summary")
        (when (package-use-list package)
          (lml-princ sentence-starter)
          (lml-princ "uses the packages ")
          (display-list-of-potential-parts
           part (sort
                 (mapcar #'nice-package-name (package-use-list package))
                 #'string-lessp) 'package)
          (lml-princ ".  ")
          (setf sentence-starter "It "))
        
        #+Ignore
        (when (package-used-by-list package)
          (lml-princ sentence-starter)
          (lml-princ "is used by the packages ")
          (display-list-of-potential-parts
           part (sort (mapcar #'nice-package-name (package-used-by-list package))
                      #'string-lessp) 'package)
          (lml-princ ".  ")
          (setf sentence-starter "It "))
        
        (when (package-nicknames package)
          (lml-princ sentence-starter)
          (lml-princ "is also known as ")
          (display-list-of-potential-parts
           part (sort (mapcar #'string-capitalize (package-nicknames package))
                      #'string-lessp) 'package)
          (lml-princ ".  ")
          (setf sentence-starter "It "))
        
        (lml-princ sentence-starter)
        (lml-format "has ~,,,D total symbols and ~,,,D external ones."
                    (symbol-count package :internal)
                    (symbol-count package :external))))
            
      ;; summaries
      (output-table-summary writer part 1))))


(defun symbol-count (package kind)
  (ecase kind
    (:external
     (loop for x being each external-symbol of package
           when (eq (symbol-package x) package)
           summing 1))
    (:internal
     (loop for x being each symbol of package
           when (eq (symbol-package x) package)
           summing 1))))

#+Temp
(defun symbols (package kind)
  (ecase kind
    (:external
     (loop for x being each external-symbol of package
           when (eq (symbol-package x) package)
           collect x))
    (:internal
     (loop for x being each symbol of package
           when (eq (symbol-package x) package)
           collect x))))



