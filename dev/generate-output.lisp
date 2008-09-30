(in-package #:tinaa)

(defun document-system (system-kind system-name destination &rest args
                                    &key (show-parts-without-documentation? t)
                                    (write-files? t) (page-writer-class nil))
  "Create TINAA documentation for a system. System-kind should be 'package or 
some other value for which it makes sense (e.g., an 'asdf-system if ASDFis
loaded...). System-name is the identifier of the 
system. Destination is the location in the file system where you want the 
documentation to go. Finally, you can pass in other arguments that are specific
to the kind of system you are documenting.

Note that system-kind will be coerced into a symbol interned in the Tinaa 
package because this makes Tinaa easier to use. If you happen to write your 
own system-kind, it will need to be a class defined in the Tinaa package."
  (when write-files?
    (assert (directory-pathname-p destination)))
  (remf args :show-parts-without-documentation?)
  (remf args :write-files?)
  ;;
  ;; remove argument if it's null (so that we get the default)
  (unless page-writer-class (remf args :page-writer-class))
  (setf system-kind (intern (string system-kind) :tinaa))
  (let ((*root-part* (apply #'make-part nil system-kind system-name 
                            :document? t 
                            :name-holder :self 
                            args))
        (*packages-to-document* nil)
        (*output-calls* (make-container 'associative-container)))
    (grovel-part *root-part*)
    (map-parts-from-leaves 
     *root-part*
     (lambda (sub-part)
       (setf (url sub-part) (url-for-part sub-part))))
    (when write-files?
      (format t "~%Writing files")
      (build-documentation (page-writer *root-part*) *root-part* destination)) 
    (when show-parts-without-documentation?
      (format t "~%The following parts appear to have no documentation:")
      (iterate-elements
       (parts-with-no-documentation *root-part*)
       (lambda (part)
         (format t "~%  ~A" (part-name part)))))
    *root-part*))

(defun write-css-file (writer destination &rest args &key (if-exists :supersede)
                                   &allow-other-keys)
  (let* ((css-file (css-file-for-writer writer))
	 (output (merge-pathnames 
		  "tinaa.css"
		  (namestring (translate-logical-pathname destination)))))
    (apply #'copy-file 
	   css-file
           output
           :if-exists if-exists args)))

(defmethod css-file-for-writer ((writer basic-page-writer))
  (or
   (css-file writer)
   (setf (css-file writer)
	 (or 
	  *css-file*
	  (pathname-for-system-file 'tinaa "tinaa.css")
	  (error "can't find CSS file")))))

(defmethod build-documentation :around
           ((writer basic-page-writer) (part doclisp-assembly)
            root &key (erase-first? nil))
  (let ((*document-root* (namestring (translate-logical-pathname root)))
        (*root-part* part)
	(*print-readably* nil))
    (when erase-first?
      (delete-directory *document-root*
                        :if-does-not-exist :ignore))
    (ensure-directories-exist *document-root*)
    (call-next-method)))

(defmethod build-documentation ((writer basic-page-writer) (part doclisp-assembly)
                                root &key &allow-other-keys)
  (set-flags part nil)
  (map-parts-from-leaves 
   part
   (lambda (sub-part)
     (when (not (flag? sub-part))
       (setf (flag? sub-part) t)
       (when (and (document? sub-part) 
                  (documentation-exists-p sub-part :detail))
         (let ((*document-file* (namestring (translate-logical-pathname 
                                             (url->file (url sub-part)))))) 
           (document-part-to-file writer sub-part))))))

  (write-css-file writer root)
  (build-contents-page writer root (content-things-from-part part)))

(defmethod document-part-to-file ((writer basic-page-writer) (part basic-doclisp-part))
  (let ((*current-part* part))
    (ensure-directories-exist *document-file*)
    (with-open-file (*document-stream* *document-file* 
                                       :direction :output
                                       :if-exists :supersede
                                       :if-does-not-exist :create)
      (display-part writer part :detail))
    (build-indexes writer part)))

(defmethod doclisp-header ((part (eql nil)) &key force-contents-link?)
  (declare (ignore force-contents-link?))
  (html ((:div :id "header"))))
   
(defmethod doclisp-header ((part basic-doclisp-part)
                           &key force-contents-link?)
  (html
   ((:div :id "header")
    (add-contents-link part force-contents-link?)
    (build-index-links part *root-part* *current-index*))))

(defun add-tinaa-link ()
  (html 
   ((:a :id "tinaa-logo" :href *tinaa-home-page*
        :title "Go to Tinaa home page")
    ((:img :src (format nil "~Aimages/logo.jpg" *tinaa-home-page*)
           :width 90
           :height 82)))))

;;?? probably still a defun
(defmethod doclisp-footer (part &key force-contents-link?)
  (html
   ((:div :id "footer")
    ((:span :class "date") 
     (lml-format "Generated: ~A"
                 (format-date "%a, %b %e, %Y" (get-universal-time))))
    ((:span :class "version") 
     (lml-format "[Tinaa version ~A]" *tinaa-version*))
    (add-contents-link part force-contents-link?)
    (add-tinaa-link))))


(defun url-name (url)
  (awhen (position #\# url :from-end t)
    (subseq url (1+ it))))


(defmethod make-name-link-for (mode name url)
  (declare (ignore name mode))
  (with-html-output (*document-stream*)
    ;; note the hack
    (html ((:a :name (url-name url))))))


(defmethod make-link-for ((mode (eql :table-summary)) name url)
  (with-html-output (*document-stream*)
    (cond ((not url)
           (lml-princ name))
          ((url-name url)
           (html ((:a :href (concatenate 'string "#" (url-name url))) (lml-princ name))))
          (t
           (html ((:a :href (relative-url url)) (lml-princ name)))))))


(defun output-table-summary (writer part parts-per-row) 
  (declare (ignore parts-per-row))
  (map-subpart-kinds
   part
   (lambda (subpart-info)
     (when (document? subpart-info)
       (output-table-summary-of-parts 
        writer part (name subpart-info) (heading subpart-info))))))


(defmethod output-table-summary-of-parts (writer part subpart-name heading)
  (let ((parts (item-at (subparts part) subpart-name))
        (count 1))
    
    (when (some-key-value-p parts
                            (lambda (name part)
                              (declare (ignore name))
                              (and (document? part)
                                   (documentation-exists-p part :table-summary))))
      (html
       ((:div :class "table-summary")
        (:h3 (lml-princ heading) 
             #+Ignore " Summary")
        ((:table :id (string-downcase heading))
         (iterate-container 
          parts
          (lambda (thing) 
            (when (document? thing)
              (let ((*current-part-index* count))
                (display-part writer thing :table-summary))
              (incf count))))))))))


(defun maybe-display-part (parent name kind mode)
  (declare (ignore mode))
  (let ((part (find-part parent kind name)))
    (cond ((and part (documentation-exists-p part :detail))
           (html
            ((:a :href (relative-url (url part)))
             (lml-princ (part-name part)))))
          (part
           (lml-princ (part-name part)))
          (name
           (lml-princ name))
          (t
           ;; no-op
           nil
           ))))


(defun display-list-of-potential-parts (part subpart-names subpart-kind)
  (let ((index 0)
        (count (1- (size subpart-names))))
    (iterate-container 
     subpart-names
     (lambda (name)
       (cond ((and (plusp count) (= index count))
              (lml-princ " and "))
             ((plusp index)
              (lml-princ ", ")))
       (maybe-display-part part name subpart-kind :index)
       (incf index)))))


(defmethod display-part ((writer simple-page-writer) (part basic-doclisp-part)
                         (mode (eql :table-summary)) &key &allow-other-keys)
  (documenting part
   ((:tr :class (if (oddp *current-part-index*) "oddrow" ""))
       (:th (link-for mode))
       (:td (when documentation (lml-princ short-documentation))))))


(defmethod show-part-parents :around ((part basic-doclisp-part))
  (when (part-has-parents-p part)
    (call-next-method)))


(defun part-has-parents-p (part)
  (and (parents part)
       (not (null (car (parents part))))))

(defmethod show-part-parents ((part basic-doclisp-part))
  (html
   ((:div :class "part-parents") 
    (:h3 "Part of:")
    (let ((count (size (parents part))))
      (flet ((display-parent-name (parent)
               (lml-format "~A ~A" (part-kind parent) (part-name parent))))
        (iterate-elements 
         (parents part)
         (lambda (parent)
           (if (url parent)
             (html ((:a :href (relative-url (url parent))) 
                    (display-parent-name parent)))
             (html (display-parent-name parent)))
           (unless (zerop (decf count))
             (lml-princ ", ")))))))))

(defmethod maybe-show-documentation ((part basic-doclisp-part))
  (let ((documentation (part-documentation part)))
    (when documentation 
      (html ((:div :class "documentation") 
             (lml-princ (string->html documentation)))))))

