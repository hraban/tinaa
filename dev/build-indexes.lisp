;;;-*- Mode: Lisp; Package: doclisp -*-

#| simple-header

$Id: build-indexes.lisp,v 1.4 2005/02/16 03:13:16 gwking Exp $

Author: Gary King

DISCUSSION

|#
(in-package #:doclisp)

(defvar *required-index-contents* 
  '("A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K"
    "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V"
    "W" "X" "Y" "Z"))


#+Remove
;;?? Gary King 2006-04-20: GWK - no longer used
(defun build-index-of-kind (writer part subpart-info)
  (bind ((parts (item-at (subparts part) (name subpart-info)))
         (symbol-list (sort
                       (delete-duplicates
                        (append *required-index-contents*
                                (collect-elements 
                                 parts :filter #'index-part-p
                                 :transform (lambda (part) 
                                              (string-upcase (subseq (part-name part) 0 1)))))
                        :test #'string-equal)
                       #'string-lessp))
         (current-symbol nil))
    (when (and (index? subpart-info)
               (plusp (size parts)))
      (let ((*current-index* (name subpart-info)))
        (documenting-page (part :title (format nil "~:(~A~): Index of ~A"
                                               (part-name part) (heading subpart-info))
                                :force-contents-link? t)
          ((:a :name "top")
           (:h3 (lml-princ (heading subpart-info)) " Index"))
          (build-index-letters symbol-list parts)
          
          ((:div :class "index-contents")
           (iterate-container
            parts
            (lambda (part)
              (when (index-part-p part)
                (let ((part-name (part-name part)))
                  (unless (string-equal current-symbol (subseq part-name 0 1))
                    (setf current-symbol (subseq part-name 0 1))
                    (html ((:div :class "index-letter")
                           ((:a :name (concatenate 
                                       'string current-symbol "-section"))
                            ((:div :class "the-letter") (lml-princ current-symbol))
                            ((:div :class "back-to-top") 
                             ((:a :href "#top") "Back to top"))))))
                  (html (:p (display-part writer part :index)))))))))))))


(defun build-index (writer part parts-to-index heading)
  (bind ((symbol-list (sort
                       (delete-duplicates
                        (append *required-index-contents*
                                (collect-elements 
                                 parts-to-index :filter #'index-part-p
                                 :transform (lambda (part) 
                                              (string-upcase (subseq (part-name part) 0 1)))))
                        :test #'string-equal)
                       #'string-lessp))
        (current-symbol nil))
    (when (plusp (size parts-to-index))
      (documenting-page (part :title (format nil "~:(~A~): Index of ~A"
                                             (part-name part) heading)
                              :force-contents-link? t)
        ((:a :name "top")
         (:h3 (lml-princ heading) " Index"))
        (build-index-letters symbol-list parts-to-index)
        
        ((:div :class "index-contents")
         (iterate-container
          parts-to-index
          (lambda (part)
            (when (index-part-p part)
              (let ((part-name (part-name part)))
                (unless (string-equal current-symbol (subseq part-name 0 1))
                  (setf current-symbol (subseq part-name 0 1))
                  (html ((:div :class "index-letter")
                         ((:a :name (concatenate 
                                     'string current-symbol "-section"))
                          ((:div :class "the-letter") (lml-princ current-symbol))
                          ((:div :class "back-to-top") 
                           ((:a :href "#top") "Back to top"))))))
                (html (:p (display-part writer part :index))))))))))))


(defun build-index-letters (symbol-list parts &key (key 'identity))
  (html
   ((:div :class "index-letters")
    (dolist (letter symbol-list)
      (if (some-element-p 
           parts
           (lambda (part)
             (let ((part (funcall key part)))
               (and 
                (string-equal (subseq (symbol-name (name part)) 0 1) letter)
                (index-part-p part)))))
        
        (html ((:div :class "used-letter")
               ((:a :href (concatenate 'string "#" letter "-section")) 
                (lml-princ letter))))
        (html ((:div :class "dead-letter")
               (lml-format "~A" letter))))))))


(defmethod display-part ((writer simple-page-writer) (part basic-doclisp-part)
                         (mode (eql :index)) &key &allow-other-keys)
  (html 
   ((:div :class "index-name")
    (display-part-for-index writer part (part-name part)))))


(defun display-part-for-index (writer part string)
  (declare (ignore writer))
  (html
   ((:span :class (span-class-for-part-name (name-holder part) part))
    ;;?? Gary King 2006-03-31: this is also in display-part <part> <eql :function>
    (if (documentation-exists-p part :detail)
      (html ((:a :href (relative-url (url part)))
             (lml-princ string)))
      (lml-princ string)))))


(defmethod display-part ((writer simple-page-writer) (part doclisp-symbol)
                         (mode (eql :index)) &key &allow-other-keys)
  (let ((name-holder (name-holder part)))
    (html
     ((:div :class "index-name") (display-part-for-index writer part (part-name part)))
     (iterate-elements
      (sort (collect-keys (subparts (name-holder part))) #'string-lessp)
      (lambda (kind)
        (when (include-kind-in-index-p name-holder kind)
          (bind ((real-part (find-part name-holder kind (name part))))
            (when real-part
              (html
               ((:div :class "index-kind")
                (display-part-for-index 
                 writer real-part (string-downcase (part-kind real-part)))))))))))))


(defmethod build-indexes ((writer basic-page-writer) (part basic-doclisp-part))
  (values))


(defmethod build-indexes ((writer basic-page-writer) (part name-holder-mixin))
  (iterate-elements
   (index-kinds part)
   (lambda (index-description)
     (bind (((part-kinds 
              &key 
              (heading (symbol->string (first part-kinds)))
              (build-using (curry #'build-index writer))
              (index-name heading)
              (index-kind (string->symbol index-name))) index-description))
       (when (some-element-p part-kinds
                             (lambda (part-kind)
                               (index-for-kind-p part part-kind)))
         (let ((*document-file* (index-file-name part index-name))
               (*current-index* index-kind))
           (with-new-file (*document-stream* *document-file*)
             (funcall build-using part
                      (loop for kind in part-kinds nconc
                            (collect-elements
                             (item-at (subparts part) kind)
                             :filter #'index-part-p))
                      heading))))))))


(defun local-index-url (part index-name)
  ;; local-index means the index that will in the _current_ directory
  (namestring (pathname-name+type (index-file-name part index-name))))


(defun index-file-name (part index-name) 
  (namestring
   (make-pathname 
   :name (format nil "index-of-~(~A~)" index-name)
   :type "html"
   :defaults (translate-logical-pathname (url->file (url part))))))


(defmethod build-index-links ((for-part (eql nil)) index-part current-index)
  (declare (ignore index-part current-index))
  (values))


(defmethod build-index-links ((for-part basic-doclisp-part) index-part current-index)
  (when (any-indexes-p index-part)
    (html
     ((:div :class "index-links")
      ((:div :class "index") "Indexes:")
      (iterate-elements
       (index-kinds for-part)
       (lambda (index-description)
         (bind (((part-kinds 
                  &key 
                  (index-name (symbol->string (first part-kinds)))
                  (index-kind (string->symbol index-name))
                  &allow-other-keys) index-description))
           (when (some-element-p part-kinds (lambda (kind) 
                                              (index-for-kind-p for-part kind)))
             (html
              ((:div :class "index")
               (if (eq index-kind current-index)
                 (lml-format "~:(~A~)" index-kind)
                 (html ((:a :href (local-index-url for-part index-name))
                        (lml-format "~:(~A~)" index-kind))))))))))))))


(defun any-indexes-p (part)
  (iterate-elements
   (index-kinds part)
   (lambda (index-description)
     (bind (((part-kinds &key &allow-other-keys) index-description))
       (when (some-element-p part-kinds (lambda (kind) 
                                          (index-for-kind-p part kind)))
         (return-from any-indexes-p t))))))


(defun index-for-kind-p (part kind)
  (and (subpart-info part kind)
       (index? (subpart-info part kind))
       (some-element-p
        (item-at (subparts part) kind)
        #'index-part-p)))


(defun index-part-p (part)
  (and (document? part)
       (or (and (boundp (name part)))
           (and (fboundp (name part)) (symbol-function (name part)))
           (find-class (name part) nil))))
