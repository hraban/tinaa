(in-package #:tinaa)

#|
make package and system look different

use clustering?
|#

(defvar *graph-size-in-inches* 3.5)

(defvar *graph-image-format* "png"
  "The image format to use for graph files")

(defvar *graphviz-directory* "/users/gwking/bin/gv/"
  "The location of the various GraphViz files. In particular, dot, neato, circo and twopi should live in this directory.")


(defclass* tinaa-graph (cl-graph:dot-graph-mixin cl-graph:graph-container)
  ()
  (:default-initargs
    :default-edge-class 'tinaa-edge
    :dot-attributes `(:size (,*graph-size-in-inches* ,*graph-size-in-inches*))))


(defclass* tinaa-vertex (cl-graph:dot-vertex cl-graph:graph-container-vertex)
  ())


(defclass* tinaa-asdf-system-vertex (tinaa-vertex)
  ())


(defclass* tinaa-package-vertex (tinaa-vertex)
  ())


(defmethod cl-graph:vertex->dot ((thing tinaa-package-vertex) stream)
  (format stream "shape=\"plaintext\""))


(defclass* tinaa-edge (cl-graph:dot-directed-edge)
  ((label "" ir)))


(defclass* direct-package-edge (tinaa-edge)
  ())


(defmethod cl-graph:edge->dot ((thing direct-package-edge) stream)
  (format stream "weight=\"5\""))


(defclass* other-package-edge (tinaa-edge)
  ())


(defclass* direct-dependency-edge (tinaa-edge)
  ())


(defclass* other-dependency-edge (tinaa-edge)
  ())


(defmethod add-part-vertex ((graph cl-graph:basic-graph) (part basic-doclisp-part))
  (cl-graph:add-vertex graph part :if-duplicate-do :ignore))


(defmethod add-part-vertex ((graph cl-graph:basic-graph) (part doclisp-package))
  (cl-graph:add-vertex graph part 
                       :vertex-class 'tinaa-package-vertex 
                       :if-duplicate-do :ignore
                       :dot-attributes 
                       `(:label ,(part-name part)
                         ,@(when (url part)
                             `(:url ,(relative-url (url part)))))))


(defmethod add-part-vertex ((graph cl-graph:basic-graph) (part doclisp-asdf-system))
  (cl-graph:add-vertex graph part 
                       :vertex-class 'tinaa-asdf-system-vertex
                       :if-duplicate-do :ignore
                       :dot-attributes 
                       `(:label ,(part-name part)
                         ,@(when (url part)
                             `(:url ,(relative-url (url part)))))))


(defun build-part-graph (writer root &key (depth (depth-for-part writer root)))
  ;; vertexes are systems and packages
  ;; edges are 
  (let ((g (cl-graph:make-graph 'tinaa-graph)))
    (labels ((do-part (part depth)
               (when (or (not depth) (plusp depth))
                 (loop for (part-kind edge-class direction label)
                       in (edge-kinds-for-part-graph writer root part)
                       do 
                       (add-edges part depth part-kind edge-class direction label))))
             (add-edges (part depth part-kind edge-class direction &optional label)
               (let ((part-vertex (add-part-vertex g part)))
                 (iterate-elements 
                  (item-at (subparts part) part-kind)
                  (lambda (other-part) 
                    (if (eq direction :source)
                      (cl-graph:add-edge-between-vertexes 
                       g part-vertex (add-part-vertex g other-part) 
                       :edge-class edge-class
                       :if-duplicate-do :ignore
                       :label label)
                      (cl-graph:add-edge-between-vertexes 
                       g (add-part-vertex g other-part) part-vertex 
                       :edge-class edge-class
                       :if-duplicate-do :ignore
                       :label label))
                    (do-part other-part (when depth (1- depth))))))))
      (do-part root depth))
    g))


(defun write-part-graph (writer part)
  (when (graph-part-p writer part)
    (let* ((*current-part* part)
           (graph (make-part-graph writer part))
           (*document-file* (graph-file-name-for-part writer part))) 
      (when graph
        (layout-graph-to-file
         graph (layout-engine-for-part writer part)
         *graph-image-format* *document-file*)))))


(defgeneric graph-part-p (writer part)
  (:method (writer part)
    (declare (ignore writer part)) (values nil)))


(defmethod edge-kinds-for-part-graph (writer root part)
  (declare (ignore writer root part))
  nil)


(defmethod graph-part-p (writer (part doclisp-asdf-system))
  (declare (ignore writer))
  (values t))


(defmethod make-part-graph (writer (part doclisp-asdf-system))
  (build-part-graph writer part))


(defmethod edge-kinds-for-part-graph (writer (root doclisp-asdf-system)
                                             (part doclisp-asdf-system))
  (declare (ignore writer))
  `((direct-package direct-package-edge :target "uses")
    (direct-dependency direct-dependency-edge :source "depends")))


(defmethod graph-part-p (writer (part doclisp-class))
  (declare (ignore writer))
  (values t))


(defmethod make-part-graph (writer (part doclisp-class))
  (build-part-graph writer part))


(defmethod edge-kinds-for-part-graph (writer (root doclisp-asdf-system)
                                             (part doclisp-class))
  (declare (ignore writer))
  `((subclass tinaa-edge :source "")
    (superclass tinaa-edge :target "")))


(defclass* page-writer-with-graphs (simple-page-writer)
  ())


(defmethod build-documentation :before
           ((writer page-writer-with-graphs) (part doclisp-assembly)
            root &key &allow-other-keys)
  (declare (ignore root))
  (set-flags part nil)
  (map-parts-from-leaves 
   part
   (lambda (sub-part)
     (when (not (flag? sub-part))
       (setf (flag? sub-part) t)
       (when (document? sub-part)
         (write-part-graph writer sub-part))))))


(defmethod layout-engine-for-part ((writer t) 
                                   (part basic-doclisp-part))
  :dot)

(defmethod depth-for-part ((writer t) 
                           (part basic-doclisp-part))
  2)
  
(defun graph-file-name-for-part
    (writer part &key (pathname-type *graph-image-format*))
  (declare (ignore writer))
  (namestring (translate-logical-pathname 
               (url->file (url part) pathname-type))))

(defun ns-tlp (pathname)
  (namestring (translate-logical-pathname pathname)))

(defun pathname->unix (pathname)
  #+digitool
  (concatenate
   'string
   "/"
   (substitute #\/ #\: 
               (namestring 
                (merge-pathnames 
                 (make-pathname 
                  :directory `(:absolute ,@(nthcdr 2 (pathname-directory pathname))))
                 pathname))))
  #-digitool
  pathname)

(defmethod layout-graph-to-file (graph layout-engine output-type output-base)
  ;; layout-engine: :dot, :neato, ...
  ;; output-type  : "jpg", "pdf", ...
  ;; (format t "~%~A ~A" graph output-base)
  (let ((out-file (pathname->unix (ns-tlp (make-pathname :type output-type
				                         :defaults output-base))))
        (map-file (pathname->unix (ns-tlp (make-pathname :type "map"
				                         :defaults output-base))))
        (tmp-file (ns-tlp (make-pathname :type "dot"
                                         :defaults output-base))))
    (unwind-protect
      (progn
        (cl-graph:graph->dot 
         graph tmp-file
         :edge-labeler (lambda (e s) (princ (label e) s)))
        
        (metashell:shell-command
         (concatenate 'string
		      *graphviz-directory* (string-downcase (symbol-name layout-engine))
		      " -T" output-type " -o" out-file
                      " < " (pathname->unix tmp-file)))
        (metashell:shell-command
         (concatenate 'string
		      *graphviz-directory* (string-downcase (symbol-name layout-engine))
		      " -Tcmapx -o" map-file
                      " < " (pathname->unix tmp-file))))
      
      ;; clean-up
      (when (probe-file tmp-file)
        (delete-file tmp-file)))
    nil))
