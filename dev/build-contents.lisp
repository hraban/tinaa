(in-package #:tinaa)

(defmethod build-contents-page ((writer basic-page-writer) root top-level-things)
  (let ((*document-file* (merge-pathnames 
		          "index.html"
		          (namestring (translate-logical-pathname root)))))
    (with-new-file (*document-stream* 
                    *document-file*)
      (with-html-output (*document-stream*)
        (html
         (:html
          (:head
           (:title (lml-format "Table of Contents [Tinaa]"))
           ((:link :rel "stylesheet" :href (stylesheet-url nil))))
          (:body
           ;;?? this and documenting-page macro should be refactored
           (doclisp-header nil :force-contents-link? nil)
           
           ((:div :class "contents")
            (:h2 "Table of Contents")
            ((:table :class "table-of-contents")
             (let ((current-kind nil))
               (iterate-elements
                top-level-things
                (lambda (thing)
                  (bind (((kind name url) thing))
                    (unless (eq current-kind kind)
                      (setf current-kind kind)
                      (html
                       (:tr (:th (lml-princ kind)))))
                    
                    (html (:tr (:td ((:a :href url) (lml-princ name)))))))))))
                   
           (doclisp-footer nil :force-contents-link? nil))))))))


(defun content-things-from-part (part)
  "Returns a list of < kind name url > triples for the subparts of part that should be displayed in the table of contents."
  (let ((result nil))
    (map-parts-from-leaves 
     part
     (lambda (part)
       (when (include-in-contents-p part)
         (push part result))))
    (collect-elements 
     (sort result (make-sorter `((part-kind string-lessp) (part-name string-lessp))))
     :transform (lambda (part)
                  (list (part-kind part) (part-name part) (url-for-part part))))))
 

(defmethod include-in-contents-p (part) 
  (declare (ignore part))
  (values nil))


(defmethod include-in-contents-p ((part name-holder-mixin))
  (values t))
