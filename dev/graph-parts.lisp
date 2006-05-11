(in-package #:tinaa)

#|
make package and system look different

use clustering?
|#

(defclass* tinaa-asdf-system-vertex (cl-graph:graph-container-vertex)
  ())

(defclass* tinaa-package-vertex (cl-graph:graph-container-vertex)
  ())

(defmethod cl-graph:vertex->dot ((thing tinaa-package-vertex) stream)
  (format stream "shape=\"plaintext\""))

(defclass* tinaa-edge (cl-graph:directed-edge-mixin cl-graph:graph-container-edge)
  ())

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

(defmethod make-part-graph ((part basic-doclisp-part) &key depth)
  (declare (ignore depth))
  (values))

(defmethod add-part-vertex ((graph cl-graph:basic-graph) (part basic-doclisp-part))
  (cl-graph:add-vertex graph part :if-duplicate-do :ignore))

(defmethod add-part-vertex ((graph cl-graph:basic-graph) (part doclisp-package))
  (cl-graph:add-vertex graph part 
                       :vertex-class 'tinaa-package-vertex :if-duplicate-do :ignore))

(defmethod add-part-vertex ((graph cl-graph:basic-graph) (part doclisp-asdf-system))
  (cl-graph:add-vertex graph part 
                       :vertex-class 'tinaa-asdf-system-vertex
                       :if-duplicate-do :ignore))

(defmethod make-part-graph ((part doclisp-asdf-system) &key depth)
  ;; vertexes are systems and packages
  ;; edges are 
  (let ((g (cl-graph:make-graph 'cl-graph:graph-container)))
    (labels ((do-part (part depth)
               (when (or (not depth) (plusp depth))
                 (add-edges part depth 'direct-package 'direct-package-edge :source)
                 (add-edges part depth 'direct-dependency 'direct-dependency-edge :target)))
             (add-edges (part depth part-kind edge-class direction)
               (let ((part-vertex (add-part-vertex g part)))
                 (iterate-elements 
                  (item-at (subparts part) part-kind)
                  (lambda (other-part) 
                    (if (eq direction :source)
                      (cl-graph:add-edge-between-vertexes 
                       g part-vertex (add-part-vertex g other-part) 
                       :edge-class edge-class
                       :if-duplicate-do :ignore)
                      (cl-graph:add-edge-between-vertexes 
                       g (add-part-vertex g other-part) part-vertex 
                       :edge-class edge-class
                       :if-duplicate-do :ignore))
                    (do-part other-part (when depth (1- depth))))))))
      (do-part part depth))
    g))

;;; ---------------------------------------------------------------------------

(defmethod make-part-graph ((part doclisp-asdf-system) &key depth)
  ;; vertexes are systems and packages
  ;; edges are 
  (let ((g (cl-graph:make-graph 'cl-graph:graph-container)))
    (labels ((do-part (part depth)
               (when (or (not depth) (plusp depth))
                 (add-edges part depth 'direct-package 'direct-package-edge :source)
                 (add-edges part depth 'direct-dependency 'direct-dependency-edge :target)))
             (add-edges (part depth part-kind edge-class direction)
               (let ((part-vertex (add-part-vertex g part)))
                 (iterate-elements 
                  (item-at (subparts part) part-kind)
                  (lambda (other-part) 
                    (if (eq direction :source)
                      (cl-graph:add-edge-between-vertexes 
                       g part-vertex (add-part-vertex g other-part) 
                       :edge-class edge-class
                       :if-duplicate-do :ignore)
                      (cl-graph:add-edge-between-vertexes 
                       g (add-part-vertex g other-part) part-vertex 
                       :edge-class edge-class
                       :if-duplicate-do :ignore))
                    (do-part other-part (when depth (1- depth))))))))
      (do-part part depth))
    g))

;;; ---------------------------------------------------------------------------

(defmethod make-part-graph ((part doclisp-class) &key depth)
  ;; vertexes are classes
  ;; edges are 
  (let ((g (cl-graph:make-graph 'cl-graph:graph-container)))
    (labels ((do-part (part depth)
               (when (or (not depth) (plusp depth))
                 (add-edges part depth 'subclass 'tinaa-edge :source)
                 (add-edges part depth 'superclass 'tinaa-edge :target)))
             (add-edges (part depth part-kind edge-class direction)
               (let ((part-vertex (add-part-vertex g part)))
                 (iterate-elements 
                  (item-at (subparts part) part-kind)
                  (lambda (other-part) 
                    (if (eq direction :source)
                      (cl-graph:add-edge-between-vertexes 
                       g part-vertex (add-part-vertex g other-part) 
                       :edge-class edge-class
                       :if-duplicate-do :ignore)
                      (cl-graph:add-edge-between-vertexes 
                       g (add-part-vertex g other-part) part-vertex 
                       :edge-class edge-class
                       :if-duplicate-do :ignore))
                    (do-part other-part (when depth (1- depth))))))))
      (do-part part depth))

    ;; don't make images for trivial class graphs
    (when (> (size g) 1) g)))

#|
(cl-graph:graph->dot 
 (make-part-graph mopu :depth 2)
 "user-home:temporary;foo.dot"
 :vertex-labeler (lambda (v s) (princ (part-name (element v)) s)))

(cl-graph:graph->dot 
 (make-part-graph 
  (item-at 
   (item-at (subparts (item-at (item-at (subparts mopu) 'package) :tinaa)) 'class)
   'doclisp-condition)
  :depth 1)
 "user-home:temporary;bar.dot"
 :vertex-labeler (lambda (v s) (princ (part-name (element v)) s)))

(cl-graph:graph->dot 
 (make-part-graph ms :depth 3)
 t)
|#


(defclass* page-writer-with-graphs (simple-page-writer)
  ())

(defvar *graph-image-format* "png"
  "The image format to use for graph files")

(defvar *graphviz-directory* "/users/gwking/bin/gv/"
  "The location of the various GraphViz files. In particular, dot, neato, circo and twopi should live in this directory.")

(defmethod build-documentation :before
           ((writer page-writer-with-graphs) (part doclisp-assembly)
            root &key &allow-other-keys)
  (set-flags part nil)
  (let ((graph nil))
    (map-parts-from-leaves 
     part
     (lambda (sub-part)
       (when (not (flag? sub-part))
         (setf (flag? sub-part) t)
	 #+Ignore
         (format t "~%~30,A ~A ~A"
                 (name sub-part)
                 (document? sub-part) 
                 (documentation-exists-p sub-part :detail))
         (when (and (document? sub-part) 
                    ;; (documentation-exists-p sub-part :detail)
                    (setf graph (make-part-graph
                                 sub-part :depth (depth-for-part writer sub-part))))
           (let ((*document-file* (graph-file-name-for-part sub-part))) 
             (layout-graph-to-file
              graph (layout-engine-for-part writer sub-part)
              *graph-image-format* *document-file*))))))))

(defmethod layout-engine-for-part ((writer page-writer-with-graphs) 
                                   (part basic-doclisp-part))
  :dot)

(defmethod depth-for-part ((writer page-writer-with-graphs) 
                                  (part basic-doclisp-part))
  2)
  
(defun graph-file-name-for-part (part)
  (namestring (translate-logical-pathname 
               (url->file (url part) "png"))))

(defun ns-tlp (pathname)
  (namestring (translate-logical-pathname pathname)))

(defmethod layout-graph-to-file (graph layout-engine output-type output-base)
  ;; layout-engine: :dot, :neato, ...
  ;; output-type  : "jpg", "pdf", ...
  ;; (format t "~%~A ~A" graph output-base)
  (let ((out-file (ns-tlp (make-pathname :type output-type
				         :defaults output-base)))
        (map-file (ns-tlp (make-pathname :type "map"
				         :defaults output-base)))
        (tmp-file (ns-tlp (make-pathname :type "dot"
				         :defaults output-base))))
    (unwind-protect
      (progn
        (cl-graph:graph->dot 
         graph tmp-file
         :vertex-labeler (lambda (v s) (princ (part-name (element v)) s)))
        
        (kl:command-output
         (concatenate 'string
		      *graphviz-directory* (string-downcase (symbol-name layout-engine))
		      " -T" output-type " -o" out-file
                      " < " tmp-file))
        (kl:command-output
         (concatenate 'string
		      *graphviz-directory* (string-downcase (symbol-name layout-engine))
		      " -Tcmapx -o" map-file
                      " < " tmp-file)))
      
      ;; clean-up
      (when (probe-file tmp-file)
        (delete-file tmp-file)))
    nil))
