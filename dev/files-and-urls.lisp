(in-package #:tinaa)


(defun url->file (url &optional (extension "html"))
  "Returns a file spec for the url rooted at *document-root*. The URL must be delimited using forward slashes \(#\/\). The *document-root* can be a logical pathname or a physical pathname on the current platform."
  (bind ((full-path (tokenize-string url :delimiter #\/))
         (name (first (last full-path)))
         (path (butlast full-path)))
    (awhen (position #\. name)
      (setf name (subseq name 0 it)))
    (namestring
     (merge-pathnames
      (make-pathname :name name
		     :type extension 
		     :directory `(:relative ,@path))
      *document-root*))))


(defun relative-url (url)
  "Returns a URL that points to the same things as `url` but relative to \(url *current-part*\)"
  (make-root-pointing-url *current-part* url))


(defun file-depth-below-root (url)
  (let* ((root (namestring (url->file "" nil)))
         (leaf (namestring (url->file url)))
         (pos 0))
    (setf pos (search root leaf))
    (if pos
      (count 
       ;; assuming single character delimiter
       (aref (physical-pathname-directory-separator) 0)
       (subseq leaf (+ pos (length root))) :test #'char-equal)
      0)))


(defmethod url-for-part ((part basic-doclisp-part))
  (when (documentation-exists-p part :detail)
    (bind ((name-holder (name-holder part))
           (name-holder-name (symbol-name (name name-holder)))
           (name-holder-kind (part-kind name-holder))
           (part-name (ensure-filename-safe-for-os (part-name part)))
           (part-kind (ensure-filename-safe-for-os (part-kind part))))
      (string-downcase
       (concatenate 'string 
                    name-holder-name "-" name-holder-kind
                    "/" part-kind "-" part-name ".html")))))


(defmethod url-for-part ((part name-holder-mixin))
  (bind ((part-name (part-name part))
         (part-kind (part-kind part)))
    (string-downcase
     (concatenate 'string part-name "-" part-kind "/index.html"))))


(defmethod make-root-pointing-url ((part (eql nil)) name)
  (values name t))


(defmethod make-root-pointing-url ((part basic-doclisp-part) name)
  (let ((depth (file-depth-below-root (url part))))
    (if (plusp depth)
      (values (apply #'concatenate 'string
                     (append (make-list depth :initial-element "../")
                             (list name))) nil)
      (values name t))))


(defmethod add-contents-link ((part (eql nil)) force-contents-link?)
  (declare (ignore force-contents-link?)))


(defmethod add-contents-link ((part basic-doclisp-part) force-contents-link?)
  (multiple-value-bind (url root-level?) 
                       (make-root-pointing-url part "index.html")
    (when (or force-contents-link? (not root-level?))
      (html ((:a :class "contents-link" :href url :title "Go to contents") "Contents")))))


(defun stylesheet-url (part)
  (make-root-pointing-url part "tinaa.css"))
