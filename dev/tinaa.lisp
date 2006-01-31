;;;-*- Mode: Lisp; Package: DOCLISP -*-

#| simple-header

$Id: tinaa.lisp,v 1.12 2005/02/16 03:06:21 gwking Exp $

Copyright 1992 - 2004 Experimental Knowledge Systems Lab, 
University of Massachusetts Amherst MA, 01003-4610
Professor Paul Cohen, Director

Author: Gary King

DISCUSSION

|#

(in-package doclisp)

(defparameter *tinaa-home-page* "http://common-lisp.net/project/tinaa/")
(defparameter *tinaa-version* "0.3")
(defvar *document-stream* *standard-output*)
(defvar *document-file* nil)
(defvar *document-root* nil)
(defvar *root-part* nil)
(defvar *current-index* nil)
(defvar *current-part-index* "")
(defparameter *short-documentation-length* 100
  "The number of characters of documentation to show in summaries.")
(defvar *packages-to-document* nil)
(defvar *default-packages-to-ignore* nil) 
(defvar *output-calls* nil)

;;; ---------------------------------------------------------------------------

;; from utils
(defmethod name (object)
  (type-of object))

;;; ---------------------------------------------------------------------------

(defmethod name ((object symbol))
  object)

;;; ---------------------------------------------------------------------------

(defmethod make-part (parent kind name &key &allow-other-keys)
  (declare (ignore parent kind name))
  (values nil))

;;; ---------------------------------------------------------------------------

(defmethod make-part :around (parent kind name &key &allow-other-keys)
  (aprog1
    (aif (find-part (name-holder parent) kind name)
         it
         (call-next-method))
    (pushnew parent (parents it))))

;;; ---------------------------------------------------------------------------

(defun some-parent (part)
  (first (parents part)))

;;; ---------------------------------------------------------------------------

(defun root-parent (part)
  (if (some-parent part)
    (root-parent (some-parent part))
    part))

;;; ---------------------------------------------------------------------------

(defun tinaa-home ()
  (system-source-directory 'tinaa))

;;; ---------------------------------------------------------------------------

(defun subpart-info (part kind)
  ;; consy
  (find kind (subpart-kinds part) :key #'name))

;;; ---------------------------------------------------------------------------

(defmethod document-part-p ((name-holder name-holder-mixin) (part basic-doclisp-part))
  (find (canonical-package-id (symbol-package (part-symbol-name part)))
        (packages-to-document)))

;;; ---------------------------------------------------------------------------

(defun part-symbol-name (part)
  (if (and (consp (name part))
           (eq (first (name part)) 'setf))
    (second (name part))
    (name part)))

;;; ---------------------------------------------------------------------------

(defun packages-to-document ()
  *packages-to-document*)

;;; ---------------------------------------------------------------------------

(defun add-package-to-document (package-name)
  (unless (ignore-package-p package-name)
    (pushnew (canonical-package-id package-name) *packages-to-document*)))

;;; ---------------------------------------------------------------------------

(defun ignore-package-p (p)
  (member p (packages-to-ignore) :test #'string-equal))

;;; ---------------------------------------------------------------------------

(defun packages-to-ignore ()
  ;; hack
  *default-packages-to-ignore*)

;;; ---------------------------------------------------------------------------

(defun canonical-package-id (package)
  (ignore-errors
   (form-keyword (typecase package
                   (package (package-name package))
                   (t (package-name (find-package package)))))))

;;; ---------------------------------------------------------------------------
;;; methods
;;; ---------------------------------------------------------------------------

(defmethod subpart-kinds :around ((assembly doclisp-assembly))
  (mapcar (lambda (kind)
            (let ((name (first (ensure-list kind)))
                  (args (rest (ensure-list kind))))
              (apply #'make-instance 'subpart-kind 
                     :name name args)))
          (call-next-method)))

;;; ---------------------------------------------------------------------------

(defmethod name-holder ((part (eql nil)))
  nil)

;;; ---------------------------------------------------------------------------

(defmethod name-holder ((part name-holder-mixin))
  (or (name-holder (some-parent part)) part))

;;; ---------------------------------------------------------------------------

(defmethod part-name ((part basic-doclisp-part))
  (string-downcase (symbol-name (name part))))

;;; ---------------------------------------------------------------------------

(defmethod print-object ((system basic-doclisp-part) stream)
  (print-unreadable-object (system stream :type t :identity t)
    (format stream "~A" (name system))))

;;; ---------------------------------------------------------------------------

(defmethod part-documentation ((part basic-doclisp-part))
  nil)

;;; ---------------------------------------------------------------------------

(defmethod short-documentation ((part basic-doclisp-part))
  (let ((doc (part-documentation part)))
    (if (> (length doc) (- *short-documentation-length* 3))
      (concatenate 'string (subseq doc 0 (- *short-documentation-length* 3))
                   "...")
      doc)))

;;; ---------------------------------------------------------------------------

(defmethod find-part ((part name-holder-mixin) kind name)
  (or (aand (item-at (subparts part) kind)
            (item-at it name))
      (call-next-method)))

;;; ---------------------------------------------------------------------------

(defmethod grovel-part ((part basic-doclisp-part))
  )

;;; ---------------------------------------------------------------------------

(defmethod subpart-kinds ((part basic-doclisp-part))
  (values nil))

;;; ---------------------------------------------------------------------------

(defmethod grovel-part ((main-part doclisp-assembly))
  (let ((seen (make-container 'associative-container)))
    (labels ((do-it (part)
               (unless (item-at seen part)
                 (setf (item-at seen part) t)
                 (start-grovel part)
                 (map-subpart-kinds 
                  part
                  (lambda (subpart-info)
                    (let (;;?? Gary King 2005-12-30:  bad name here
                          (subpart-kind (part-kind subpart-info))
                          (kind (name subpart-info)))
                      ;; we make all parts at this level and then grovel over 'em
                      (loop for sub-part in 
                            (loop for part-name in (partname-list part kind)
                                  collect
                                  (let ((sub-part (make-part
                                                   part subpart-kind part-name
                                                   :name-holder main-part)))
                                    (when sub-part
                                      (unless (eq subpart-kind kind)
                                        (setf (item-at (item-at (subparts part) kind) part-name)
                                              sub-part))
                                      
                                      ;;?? Gary King 2005-12-30: perhaps a hack
                                      (setf (item-at (item-at (subparts main-part) subpart-kind) part-name)
                                            sub-part
                                            (item-at (item-at (subparts part) subpart-kind) part-name)
                                            sub-part))))
                            when (document? sub-part) do
                            (do-it sub-part)))))
                 (finish-grovel part))))
      (do-it main-part))))

;;; ---------------------------------------------------------------------------

(defmethod start-grovel ((part basic-doclisp-part))
  (values))

;;; ---------------------------------------------------------------------------

(defmethod finish-grovel ((part basic-doclisp-part))
  (values))

;;; ---------------------------------------------------------------------------

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

;;; ---------------------------------------------------------------------------

(defun document-system (system-kind system-name destination &rest args)
  "Create TINAA documentation for a system. System-kind should be 'package or 
some other value for which it makes sense (e.g., an EKSL-system or an ASDF
system if you have those loaded...). System-name is the identifier of the 
system. Destination is the location in the file system where you want the 
documentation to go. Finally, you can pass in other arguments that are specific
to the kind of system you are documenting."
  (let ((*root-part* (apply #'make-part nil system-kind system-name 
                            :document? t 
                            :name-holder :self 
                            args))
        (*packages-to-document* nil)
        (*output-calls* (make-container 'associative-container)))
    (grovel-part *root-part*)
    (write-css-file destination)
    (build-documentation *root-part* destination)
    *root-part*))

;;; ---------------------------------------------------------------------------

(defun write-css-file (destination &rest args &key (if-exists :supersede)
                                   &allow-other-keys)
  (let ((output (merge-pathnames "tinaa.css" destination)))
    (ensure-directories-exist output)
    (apply #'copy-file 
           (or 
            (pathname-for-system-file 'tinaa "tinaa.css")
            (error "can't find tinaa.css"))
           output
           :if-exists if-exists args)))

;;; ---------------------------------------------------------------------------

(defmethod build-documentation ((part doclisp-assembly) root)
  (let ((*document-root* root)
        (*root-part* part))
    (map-parts-from-leaves 
     part
     (lambda (sub-part)
       (setf (url sub-part) (url-for-part sub-part))))
    
    (set-flags part nil)
    (map-parts-from-leaves 
     part
     (lambda (sub-part)
       (setf (url sub-part) (url-for-part sub-part))
       (when (not (flag? sub-part))
         (setf (flag? sub-part) t)
         (when (and (document? sub-part) 
                    (documentation-exists-p sub-part :detail))
           (document-part-to-file 
            sub-part
            (when (eq sub-part part)
              (url->file "index.html")))))))
    (build-indexes part)))

;;; ---------------------------------------------------------------------------

(defmethod build-documentation ((part basic-doclisp-part) root)
  (let ((*document-root* root))
    (when (documentation-exists-p part :detail)
      (document-part-to-file part))))

;;; ---------------------------------------------------------------------------

(defun set-flags (part value)
  (map-parts-from-leaves part (lambda (p) (setf (flag? p) value))))

;;; ---------------------------------------------------------------------------

(defmethod document-part-to-file ((part basic-doclisp-part) 
                                  &optional file)
  (let ((*document-file* (or file (url->file (url part)))))
    (ensure-directories-exist *document-file*)
    (with-open-file (*document-stream* *document-file* 
                                       :direction :output
                                       :if-exists :supersede
                                       :if-does-not-exist :create)
      (display-part part :detail))))

;;; ---------------------------------------------------------------------------

(defun url->file (url &optional (extension "html"))
  "Returns a file spec for the url rooted at *document-root*."
  (bind ((full-path (tokenize-string url :delimiter #\/))
         (name (first (last full-path)))
         (path (butlast full-path)))
    (awhen (position #\. name)
      (setf name (subseq name 0 it)))
    (merge-pathnames
     (make-pathname :name name
                    :type extension 
                    :directory `(:relative ,@path))
     *document-root*)))

;;; ---------------------------------------------------------------------------

(defun relative-url (url file)
  (bind ((split-url (tokenize-string url :delimiter #\/))
         (file-name (namestring file))
         (root-pos (search (namestring *document-root*) file-name))
         (file-name (subseq file-name (+ root-pos (length (namestring *document-root*))))) 
         (split-file (tokenize-string file-name :delimiter #\;))
         (file-path (butlast split-file))
         (url-last (first (last split-url)))
         (url-path (butlast split-url)))
    (loop while (and url-path file-path
                     (string-equal (first url-path) (first file-path))) do
          (setf url-path (rest url-path)
                file-path (rest file-path)))
    (format nil "~{~A/~}~A" url-path url-last)))

;;; ---------------------------------------------------------------------------

(defun file-depth-below-root (url)
  (let* ((root (namestring (url->file "" nil)))
         (leaf (namestring (url->file url)))
         (pos 0))
    (setf pos (search root leaf))
    (when pos
      (count #\; (subseq leaf (+ pos (length root)))))))

;;; ---------------------------------------------------------------------------

(defmethod url-for-part ((part basic-doclisp-part))
  (bind ((name-holder (name-holder part))
         (name-holder-name (ensure-filename-safe-for-os
                            (symbol-name (name name-holder))))
         (part-name (ensure-filename-safe-for-os (part-name part)))
         (part-kind (part-kind part)))
    (cond ((eq part name-holder)
           (string-downcase
            (concatenate 'string name-holder-name ".html")))
          ((documentation-exists-p part :detail)
           (string-downcase
            (concatenate 'string name-holder-name "/" part-kind "-" part-name ".html")))
          (t
           nil
           #+Old
           (concatenate 'string name-holder-name "#" part-name)))))

;;; ---------------------------------------------------------------------------

(defmacro documenting (part &body body)
  `(symbol-macrolet ((name (part-name ,part))
                     (documentation? (not (null (part-documentation ,part)))) 
                     (documentation (string->html (part-documentation ,part)))
                     (short-documentation (string->html (short-documentation ,part)))
                     (subparts (subparts ,part))
                     (url (url ,part)))
     (macrolet ((parts (kind)
                  `(item-at subparts ,kind))
                (link-for (mode)
                  `(make-link-for ,mode name url))
                (mark-spot (mode)
                  `(make-name-link-for ,mode name url)))
       (with-html-output (*document-stream*)
         (html ,@body)))))

;;; ---------------------------------------------------------------------------

(defmacro documenting-page ((part &key title force-contents-link?) &body body)
  `(documenting part
     (:html
      (:head
       (:title (if ,title 
                 (lml-princ ,title)
                 (lml-format "~A ~:(~A~) [Tinaa]"
                             (header ,part) (name ,part))))
       ((:link :rel "stylesheet" :href (stylesheet-url ,part))))
      (:body
       (doclisp-header ,part :force-contents-link? ,force-contents-link?)
       
       ,@body
       
       (doclisp-footer ,part :force-contents-link? ,force-contents-link?)))))

;;; ---------------------------------------------------------------------------
  
(defun stylesheet-url (part)
  (make-root-pointing-url part "tinaa.css"))

;;; ---------------------------------------------------------------------------

(defmethod doclisp-header ((part basic-doclisp-part)
                           &key force-contents-link?)
  (html
   ((:div :id "header")
    (add-contents-link part force-contents-link?)
    (build-index-links part *root-part* *current-index*))))

;;; ---------------------------------------------------------------------------

(defun add-tinaa-link (part)
  (declare (ignore part))
  (html 
   ((:a :id "logo" :href *tinaa-home-page*
        :title "Go to Tinaa home page")
    ((:img :src (format nil "~Aimages/logo.jpg" *tinaa-home-page*)
           :width 90
           :height 82)))))

;;; ---------------------------------------------------------------------------

(defun add-contents-link (part force-contents-link?)
  (multiple-value-bind (url root-level?) 
                       (make-root-pointing-url part "index.html")
    (when (or force-contents-link? (not root-level?))
      (html ((:a :class "header-link" :href url :title "Go to contents") "Contents")))))

;;; ---------------------------------------------------------------------------

(defun make-root-pointing-url (part name)
  (let ((depth (file-depth-below-root (url part))))
    (if (plusp depth)
      (values (apply #'concatenate 'string
                     (append (make-list depth :initial-element "../")
                             (list name))) nil)
      (values name t))))

;;; ---------------------------------------------------------------------------

(defmethod doclisp-footer ((part basic-doclisp-part)
                           &key force-contents-link?)
  (html
   ((:div :id "footer")
    (lml-format "Generated: ~A [Tinaa version ~A]"
                (format-date "%a, %b %e, %Y" (get-universal-time))
                *tinaa-version*)
    (add-contents-link part force-contents-link?)
    (add-tinaa-link part))))

;;; ---------------------------------------------------------------------------

(defun url-name (url)
  (awhen (position #\# url :from-end t)
    (subseq url (1+ it))))

;;; ---------------------------------------------------------------------------

(defmethod make-name-link-for (mode name url)
  (declare (ignore name mode))
  (with-html-output (*document-stream*)
    ;; note the hack
    (html ((:a :name (url-name url))))))

;;; ---------------------------------------------------------------------------

(defmethod make-link-for ((mode (eql :table-summary)) name url)
  (with-html-output (*document-stream*)
    (cond ((not url)
           (lml-princ name))
          ((url-name url)
           (html ((:a :href (concatenate 'string "#" (url-name url))) (lml-princ name))))
          (t
           (html ((:a :href (relative-url url *document-file*)) (lml-princ name)))))))

;;; ---------------------------------------------------------------------------

(defun map-subpart-kinds (part fn)
  (loop for subpart-info in (subpart-kinds part) do
        (funcall fn subpart-info)))

;;; ---------------------------------------------------------------------------

(defun map-parts-from-leaves (part fn)
  (let ((seen (make-container 'associative-container)))
    (labels ((do-it (part fn)
               (unless (item-at seen part)
                 (setf (item-at seen part) t)
                 (map-subpart-kinds 
                  part
                  (lambda (subpart-info)
                    (iterate-container (item-at (subparts part) (name subpart-info))
                                       (lambda (sub-part) (do-it sub-part fn))))))
               (funcall fn part)))
      (do-it part fn))))

;;; ---------------------------------------------------------------------------

#|
  (when (item-at *output-calls* 'output-table-summary part mode)
    (return-from output-table-summary))
  (setf (item-at *output-calls* 'output-table-summary part mode) t)
|#

(defun output-table-summary (part mode parts-per-row) 
  (map-subpart-kinds
   part
   (lambda (subpart-info)
     (let ((parts (item-at (subparts part) (name subpart-info)))
           (count 1))
       
       (when (and (document? subpart-info)
                  (some-key-value-p parts
                                    (lambda (name part)
                                      (declare (ignore name))
                                      (and (document? part)
                                           (documentation-exists-p part mode)))))
         (html
          (:h3 (lml-princ (heading subpart-info)) " Summary")
          (:table
           (iterate-container 
            parts
            (lambda (thing) 
              (when (document? thing)
                (let ((*current-part-index* count))
                  (display-part thing mode))
                (incf count)))))))))))

;;; ---------------------------------------------------------------------------

(defun maybe-display-part (parent name kind mode)
  (declare (ignore mode))
  (let ((part (find-part parent kind name)))
    (if (and part (documentation-exists-p part :detail))
      (html
       ((:a :href (relative-url (url part) *document-file*))
        (lml-princ (part-name part))))
      (lml-princ name))))

;;; ---------------------------------------------------------------------------

(defun display-list-of-potential-parts (part subpart-names subpart-kind)
  (let ((first? t))
    (iterate-container 
     subpart-names
     (lambda (name)
       (when (not first?)
         (lml-princ ", "))
       (maybe-display-part part name subpart-kind :index)
       (setf first? nil)))))


;;; ***************************************************************************
;;; *                              End of File                                *
;;; ***************************************************************************