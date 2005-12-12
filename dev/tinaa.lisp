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
(defparameter *tinaa-version* "0.2")
(defvar *document-stream* *standard-output*)
(defvar *document-file* nil)
(defvar *document-root* nil)
(defvar *root-part* nil)
(defvar *current-index* nil)
(defvar *current-part-index* "")
(defconstant +short-documentation-length+ 100
  "The number of characters of documentation to show in summaries.")
(defvar *packages-to-document* nil)
(defvar *default-packages-to-ignore* nil) 
(defvar *output-calls* nil)

;; from utils
(defmethod name (object)
  (type-of object))

;;; ---------------------------------------------------------------------------

(defmethod name ((object symbol))
  object)

;;; ---------------------------------------------------------------------------

(defgeneric subpart-kinds (assembly)
  (:documentation "Returns a list of the kinds of the subparts of assembly.
This is a list of instances of subpart-kind."))

;;; ---------------------------------------------------------------------------

(defgeneric index-kinds (part)
  (:documentation "Returns a list of lists of part kinds that should be
grouped when determining how to link a symbol from its index. That makes
no sense at all.")
  (:method ((part t))
           (subpart-kinds part)))

;;; ---------------------------------------------------------------------------

(defgeneric partname-list (part part-kind)
  (:documentation "Returns a list of the names \(as symbols\) of the subparts of part 
of type 'part-kind'."))

;;; ---------------------------------------------------------------------------

(defgeneric display-part (part mode)
  (:documentation "Output information about a part. Example modes are 
:subpart-list, :detail, :summary.")
  (:method :around (part mode)
           (when (documentation-exists-p part mode)
             (call-next-method)))) 

;;; ---------------------------------------------------------------------------

(defgeneric document-system-part (system part stream)
  (:documentation ""))

;;; ---------------------------------------------------------------------------

(defgeneric make-system-part (system part-kind part-name &key)
  (:documentation ""))

;;; ---------------------------------------------------------------------------

(defgeneric part-name (part)
  (:documentation "Returns the name of the part as a string"))

;;; ---------------------------------------------------------------------------

(defgeneric part-documentation (part)
  (:documentation "Returns whatever documentation is available for part"))

;;; ---------------------------------------------------------------------------

(defgeneric short-documentation (part)
  (:documentation "Returns the first bit of the documentation for part.
Change +short-documentation-length+ to determine how much is returned."))

;;; ---------------------------------------------------------------------------

(defgeneric document-part-to-file (part &optional file)
  (:documentation ""))

;;; ---------------------------------------------------------------------------

(defgeneric url-for-part (part)
  (:documentation "Returns the url for the part, creating it if necessary."))

;;; ---------------------------------------------------------------------------

(defgeneric make-part (parent kind name &key)
  (:documentation "Make a part named 'name' of kind 'kind' whose parent is 'parent'.")
  (:method (parent kind name &key &allow-other-keys)
           (declare (ignore parent kind name))
           (values nil))
  (:method :around (parent kind name &key &allow-other-keys)
           (aprog1
             (aif (find-part parent kind name)
                  it
                  (call-next-method))
             (pushnew parent (parents it)))))

;;; ---------------------------------------------------------------------------

(defgeneric find-part (ancester kind name)
  (:documentation "Returns a existing part if it can be found.")
  (:method (parent kind name)
           (declare (ignore parent kind name))
           (values nil)))

;;; ---------------------------------------------------------------------------

(defgeneric name-holder (part)
  (:documentation ""))

;;; ---------------------------------------------------------------------------

(defgeneric grovel-part (part)
  (:documentation ""))

;;; ---------------------------------------------------------------------------

(defgeneric finish-grovel (part)
  (:documentation ""))

;;; ---------------------------------------------------------------------------

(defun some-parent (part)
  (first (parents part)))

;;; ---------------------------------------------------------------------------

(defun tinaa-home ()
  (or #+ASDF
      (slot-value (asdf:find-system 'tinaa) 'asdf::relative-pathname)
      (error "cannot determine Tinaa's home")))

;;; ---------------------------------------------------------------------------

(defgeneric documentation-exists-p (part mode)
  (:documentation "")
  (:method (part mode)
           ;; one around method -- oh vey!
           (length-at-least-p 
            (compute-applicable-methods #'display-part (list part mode))
            2)
           #+Old
           ;; too strict
           (find-method #'display-part nil 
                        (list (class-of part) 
                              (or #+MCL-COMMON-MOP-SUBSET
                                  (ccl:intern-eql-specializer mode)
                                  #+DIGITOOL
                                  `(eql ,mode)))
                        nil)))

;;; ---------------------------------------------------------------------------

(defun subpart-info (part kind)
  ;; consy
  (find kind (subpart-kinds part) :key #'name))

;;; ---------------------------------------------------------------------------

(defgeneric document-part-p (part)
  (:documentation ""))

;;; ---------------------------------------------------------------------------

(defmethod document-part-p ((part basic-doclisp-part))
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

(defmethod name-holder ((part basic-doclisp-part))
  (awhen (parents part)
    (name-holder (car it))))
    
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
    (if (> (length doc) (- +short-documentation-length+ 3))
      (concatenate 'string (subseq doc 0 (- +short-documentation-length+ 3))
                   "...")
      doc)))

;;; ---------------------------------------------------------------------------

(defmethod find-part ((part name-holder-mixin) kind name)
  (or (aand (item-at (subparts part) kind)
            (item-at it name))
      (call-next-method)))

;;; ---------------------------------------------------------------------------
    
(defmethod find-part ((part doclisp-assembly) kind name)
  (when (parents part)
    ;; any parent will do in a storm
    (find-part (car (parents part)) kind name)))

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
                 (let ((name-holder (name-holder part)))
                   (loop for subpart-info in (subpart-kinds part) do
                         (let ((kind (name subpart-info)))
                           ;; we make all parts at this level and then grovel over 'em
                           (loop for sub-part in 
                                 (loop for part-name in (partname-list part kind) collect
                                       (let ((sub-part (make-part part kind part-name)))
                                         (when sub-part
                                           (setf (item-at (item-at (subparts name-holder) kind) part-name)
                                                 sub-part
                                                 (item-at (item-at (subparts part) kind) part-name)
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
  (loop for subpart-info in (subpart-kinds part) do
        (sort-keys (item-at (subparts part) (name subpart-info))
                   #'string-lessp
                   :key 
                   (lambda (key)
                     (string-downcase (atypecase key
                                        (symbol (symbol-name it))
                                        (cons 
                                         (apply #'concatenate 'string 
                                                (mapcar #'symbol-name it)))))))))

;;; ---------------------------------------------------------------------------

(defun document-system (system-kind system-name destination &rest args)
  "Create TINAA documentation for a system. System-kind should be 'package or 
some other value for which it makes sense (e.g., an EKSL-system or an ASDF
system if you have those loaded...). System-name is the identifier of the 
system. Destination is the location in the file system where you want the 
documentation to go. Finally, you can pass in other arguments that are specific
to the kind of system you are documenting."
  (let ((*root-part* (apply #'make-part nil system-kind system-name 
                            :document? t args))
        (*packages-to-document* nil)
        (*output-calls* (make-container 'associative-container)))
    (grovel-part *root-part*)
    (write-css-file destination)
    (build-documentation *root-part* destination)
    *root-part*))

;;; ---------------------------------------------------------------------------

(defun write-css-file (destination &rest args &key (if-exists :supersede)
                                   &allow-other-keys)
  (apply #'copy-file 
         (or 
          #+ASDF
          (metabang-project-manager:pathname-for-system-file 'tinaa "tinaa.css") 
          #+GLU
          "tinaa:tinaa.css"
          (error "can't find tinaa home"))
         (merge-pathnames "tinaa.css" destination)
         :if-exists if-exists args))

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
         (name-holder-name (make-name-safe-for-filesystem
                            (symbol-name (name name-holder))))
         (part-name (make-name-safe-for-filesystem (part-name part)))
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

(defun make-name-safe-for-filesystem (name)
  (let* ((array (make-array (* 2 (length name)) :fill-pointer 0 :adjustable t)))
    (flet ((add-char (ch)
             (vector-push-extend ch array)))
      (loop for ch across name do
            (cond ((char-equal ch #\/)
                   (add-char #\-)
                   (add-char #\1))
                  ((char-equal ch #\*)
                   (add-char #\-)
                   (add-char #\2))
                  ((char-equal ch #\-)
                   (add-char #\-)
                   (add-char #\-))
                  (t
                   (add-char ch))))
      (coerce array 'string))))

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
    ((:span :id "indexes")
     (build-index-links part *root-part* *current-index*)))))

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

(defun add-back-link (part)
  (when (some-parent part)
    ;; (a :href (relative-url (url (some-parent part)) *document-file*) "back")
    (lml-princ "&nbsp;&nbsp;")))

;;; ---------------------------------------------------------------------------

(defun add-contents-link (part force-contents-link?)
  (multiple-value-bind (url root-level?) (make-root-pointing-url part "index.html")
    (when (or force-contents-link? (not root-level?))
      (html ((:a :class "header-link" :href url :title "Go to contents") "Contents"))
      (lml-princ "&nbsp;&nbsp;"))))

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
    (lml-format "Generated: ~A, version ~A&nbsp;&nbsp;"
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

#|
(pro:with-profiling
  (:timeout (* 60 60))
  (document-system 'eksl-system 'hac "home:hac-system;"))

;;; 31.9360 cpu seconds (31.9360 cpu seconds ignoring GC)
;;; 7,235,480 words consed
Execution time profile from 1001 samples
  Parents
Function
  Children                                   Relative  Absolute Consing       Conses
----
document-system                                            100%    100%    7,091,896
  partname-list <doclisp-eksl-system> <(eql package)>  42%
  make-part (around) <t> <t> <t>                 24%
  filtered-package-symbols                        7%
  sort-list                                       7%
  partname-list <doclisp-package> <(eql class)>   6%
  do-it                                           5%
  map-subpart-kinds                               5%
  %%standard-combined-method-dcode                1%
  finish-grovel <doclisp-assembly>                1%
  item-at! <alist-container> <t>                  1%
----
  document-system                               100%
partname-list <doclisp-eksl-system> <(eql package)>         42%      7%      550,424
  call-function                                 100%
----
  partname-list <doclisp-eksl-system> <(eql package)> 100%
call-function                                               42%      7%      548,512
  file-package                                   98%
  %inited-class-cpl                               1%
----
  call-function                                 100%
file-package                                                41%      7%      527,160
  open                                           87%
  close                                          10%
  map-forms-in-file <t> <t>                       2%
  %%standard-combined-method-dcode                1%
----
  file-package                                  100%
open                                                        36%      7%      502,808
  getf                                           52%
  make-instance <symbol>                         41%
  probe-file-x                                    4%
  read-scriptruns                                 3%
----
  document-system                               100%
make-part (around) <t> <t> <t>                              23%     14%    1,021,864
  find-part <name-holder-mixin> <t> <t>          80%
  make-instance <symbol>                         19%
  %%nth-arg-dcode                                 1%
----
  make-instance <symbol>                         98%
  make-instance <standard-class>                  2%
%make-std-instance                                          20%      7%      537,960
  %%standard-combined-method-dcode               75%
  %%before-and-after-combined-method-dcode       23%
----
  open                                           76%
  make-part (around) <t> <t> <t>                 22%
  map-subpart-kinds                               1%
  %call-next-method                               1%
make-instance <symbol>                                      20%      7%      537,896
  %make-std-instance                             99%
  make-instance <standard-class>                  1%
----
  make-part (around) <t> <t> <t>                100%
find-part <name-holder-mixin> <t> <t>                       19%     12%      824,040
  item-at! <alist-container> <t>                 46%
  assoc                                          40%
  make-initial-element <initial-element-mixin>    5%
  (setf item-at) <t> <t>                          5%
  %%one-arg-dcode                                 1%
----
  open                                          100%
getf                                                        19%      4%      328,328
----
  instance-initialize (after) <file-stream>      80%
  stream-position <file-stream>                  18%
  %fopen                                          1%
  %fpos                                           1%
%fbread                                                     16%      3%      241,232
----
  %make-std-instance                             92%
  document-system                                 6%
  file-package                                    1%
  initialize-instance <instance-initialize-mixin>   1%
%%standard-combined-method-dcode                            16%      5%      393,624
  instance-initialize (after) <file-stream>      90%
  %call-next-method                               6%
  %%before-and-after-combined-method-dcode        2%
----
  %%standard-combined-method-dcode               99%
  %%before-and-after-combined-method-dcode        1%
instance-initialize (after) <file-stream>                   14%      1%       96,680
  %fbread                                        92%
  probe-file                                      7%
----
  document-system                                56%
  map-subpart-kinds                              44%
do-it                                                       10%     64%    4,489,816
  iterate-container <alist-container> <t>        56%
  find-or-create-ht                              35%
  make-initial-element <initial-element-mixin>    4%
  %%one-arg-dcode                                 1%
----
  find-part <name-holder-mixin> <t> <t>          95%
  document-system                                 5%
item-at! <alist-container> <t>                               9%      6%      394,032
  car                                            46%
----
  find-part <name-holder-mixin> <t> <t>          97%
  finish-grovel <doclisp-assembly>                3%
assoc                                                        8%      5%      389,528
----
  document-system                               100%
filtered-package-symbols                                     7%      2%      116,936
  symbol-function                                72%
  #<anonymous function #x346bc8e>                13%
----
  document-system                               100%
partname-list <doclisp-package> <(eql class)>                6%    ---%       17,160
  #<anonymous function #x342c7ce>                52%
  class-cell-typep                               45%
----
  document-system                               100%
sort-list                                                    6%    ---%       34,192
  #<anonymous function #x342c916>                94%
----
  sort-list                                     100%
#<anonymous function #x342c916>                              6%    ---%        2,984
  specifier-type                                 84%
  memq                                            8%
  %inited-class-cpl                               5%
----
  do-it                                          89%
  document-system                                11%
iterate-container <alist-container> <t>                      6%     17%    1,202,000
  url-for-part <basic-doclisp-part>              82%
  map-subpart-kinds                              11%
----
  filtered-package-symbols                      100%
symbol-function                                              5%      1%       43,288
----
  #<anonymous function #x342c916>                98%
  subtypep                                        2%
specifier-type                                               5%    ---%        2,664
  values-specifier-type-internal                 96%
----
  document-system                                86%
  iterate-container <alist-container> <t>        14%
map-subpart-kinds                                            5%     65%    4,579,656
  do-it                                          86%
  make-instance <symbol>                          4%
----
  iterate-container <alist-container> <t>        98%
  #<anonymous function #x34903b6>                 2%
url-for-part <basic-doclisp-part>                            5%      7%      516,584
  get-help-file-entry                            80%
  %compute-applicable-methods*                   18%
  %%one-arg-dcode                                 2%
----
  specifier-type                                 98%
  values-specifier-type                           2%
values-specifier-type-internal                               5%    ---%        2,600
----
  %make-std-instance                             92%
  %%standard-combined-method-dcode                8%
%%before-and-after-combined-method-dcode                     5%      6%      400,208
  document-part-p <basic-doclisp-part>           55%
  %shared-initialize                             16%
  instance-initialize (after) <file-stream>       2%
----
  item-at! <alist-container> <t>                100%
car                                                          4%      2%      175,664
----
  file-package                                  100%
close                                                        4%    ---%        6,440
  %fclose                                       100%
----
  close                                         100%
%fclose                                                      4%    ---%        6,440
----
  url-for-part <basic-doclisp-part>              98%
  documentation-exists-p <doclisp-variable> <(eql detail)>   2%
get-help-file-entry                                          4%      3%      211,016
  #<anonymous function #x22cab06>               100%
----
  get-help-file-entry                            95%
  call-accessing-index-stream                     5%
#<anonymous function #x22cab06>                              4%      3%      211,016
  binary-search                                  80%
  check-index-file-length                        20%
----
  partname-list <doclisp-package> <(eql class)>  83%
  probe-file                                     11%
  %path-from-fsref                                3%
  allocate-resource                               3%
class-cell-typep                                             3%    ---%       26,848
  %inited-class-cpl                               3%
----
  partname-list <doclisp-package> <(eql class)> 100%
#<anonymous function #x342c7ce>                              3%    ---%        5,648
  %%nth-arg-dcode                                 3%
----
  do-it                                          97%
  item-at! <associative-container> <t>            3%
find-or-create-ht                                            3%     42%    2,969,208
  %cons-hash-table                               76%
----
  #<anonymous function #x22cab06>               100%
binary-search                                                3%      2%      168,472
  stream-position <file-stream>                  78%
  skip-past-double-newline                       22%
----
  binary-search                                  78%
  check-index-file-length                        13%
  index-file-p                                    9%
stream-position <file-stream>                                3%      2%      169,816
  %fbread                                        94%
----
  %%before-and-after-combined-method-dcode       93%
  initialize-instance (after) <basic-doclisp-part>   7%
document-part-p <basic-doclisp-part>                         3%      2%      119,192
  form-symbol-in-package                         44%
  find-package                                   23%
----
  find-or-create-ht                              96%
  make-hash-table                                 4%
%cons-hash-table                                             3%     35%    2,460,976
----
  open                                          100%
probe-file-x                                                 2%      1%       47,032
  my-fsref-make-path                             12%
----
  instance-initialize (after) <file-stream>      53%
  read-scriptruns                                47%
probe-file                                                   2%      1%       38,424
  my-fsref-make-path                             35%
  class-cell-typep                               30%
----
Welcome to Macintosh Common Lisp Version 5.1b1!
? 
|#
;;; ***************************************************************************
;;; *                              End of File                                *
;;; ***************************************************************************