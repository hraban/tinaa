(in-package tinaa)

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
  (format stream "weight=\"1\""))

(defclass* other-package-edge (tinaa-edge)
  ())

(defclass* direct-dependency-edge (tinaa-edge)
  ())

(defclass* other-dependency-edge (tinaa-edge)
  ())

(defmethod make-part-graph ((part basic-doclisp-part) &key)
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
               (spy part depth)
               (when (or (not depth) (plusp depth))
                 (add-edges part depth 'direct-package 'direct-package-edge)
                 ;(add-edges part depth 'other-package 'other-package-edge)
                 (add-edges part depth 'direct-dependency 'direct-dependency-edge)
                 #+Ignore (add-edges part depth 'other-dependency 'other-dependency-edge)))
             (add-edges (part depth part-kind edge-class)
               (let ((part-vertex (add-part-vertex g part)))
                 (iterate-elements 
                  (item-at (subparts part) part-kind)
                  (lambda (other-part) 
                    (cl-graph:add-edge-between-vertexes 
                     g part-vertex (add-part-vertex g other-part) 
                     :edge-class edge-class
                     :if-duplicate-do :ignore)
                    (do-part other-part (when depth (1- depth))))))))
      (do-part part depth))
    g))

#|
(cl-graph:graph->dot 
 (make-part-graph ms :depth 1)
 "user-home:temporary;foo.dot"
 :vertex-labeler (lambda (v s) (princ (part-name (element v)) s)))
(cl-graph:graph->dot 
 (make-part-graph ms :depth 3)
 t)
|#
