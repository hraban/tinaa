;;;-*- Mode: Lisp; Package: DOCLISP -*-

#| simple-header

$Id: build-indexes.lisp,v 1.4 2005/02/16 03:13:16 gwking Exp $

Copyright 1992 - 2004 Experimental Knowledge Systems Lab, 
University of Massachusetts Amherst MA, 01003-4610
Professor Paul Cohen, Director

Author: Gary King

DISCUSSION

|#
(in-package doclisp)

(defvar *required-index-contents* 
  '("A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K"
    "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V"
    "W" "X" "Y" "Z"))

;;; ---------------------------------------------------------------------------

(defun build-index-of-kind (part subpart-info)
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
                  (html (:p (display-part part :index)))))))))))))

(defun build-index (part parts-to-index heading)
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
                (html (:p (display-part part :index))))))))))))

;;; ---------------------------------------------------------------------------

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

;;; ---------------------------------------------------------------------------

(defmethod display-part ((part basic-doclisp-part) (mode (eql :index))
                         &key &allow-other-keys)
  (html 
   ((:div :class "index-name")
    (display-part-for-index part (part-name part)))))

;;; ---------------------------------------------------------------------------

(defun display-part-for-index (part string)
  (if (documentation-exists-p part :detail)
    (html ((:a :href (relative-url (url part)))
           (lml-princ string)))
    (lml-princ string)))

;;; ---------------------------------------------------------------------------

(defmethod display-part ((part doclisp-symbol) (mode (eql :index))
                          &key &allow-other-keys)
  (let ((name-holder (name-holder part)))
    (html
     ((:div :class "index-name") (display-part-for-index part (part-name part)))
     (iterate-elements
      (sort (collect-keys (subparts (name-holder part))) #'string-lessp)
      (lambda (kind)
        (when (include-kind-in-index-p name-holder kind)
          (bind ((real-part (find-part name-holder kind (name part))))
            (when real-part
              (html
               ((:div :class "index-kind")
                (display-part-for-index 
                 real-part (string-downcase (part-kind real-part)))))))))))))

;;; ---------------------------------------------------------------------------

(defmethod build-indexes (part)
  (iterate-elements
   (index-kinds part)
   (lambda (index-description)
     (bind (((part-kinds 
              &key 
              (heading (symbol->string (first part-kinds)))
              (build-using #'build-index)
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
#+Ignore
(progn
  (map-subpart-kinds
   part
   (lambda (subpart-info)
     (when (index-for-kind-p part subpart-info)
       
       (let ((*document-file* (index-file-name part subpart-info)))
         (with-open-file (*document-stream* *document-file* 
                                            :direction :output
                                            :if-exists :supersede
                                            :if-does-not-exist :create)
           
           (build-index-of-kind part subpart-info))))))
  
  (let ((*document-file* (make-pathname 
                          :name (format nil "permuted-index-~(~A~)"
                                        (name part))
                          :type "html"
                          :defaults *document-root*))) 
    (with-new-file (*document-stream* *document-file*)
      (build-permuted-index part))))

;;; ---------------------------------------------------------------------------

(defun index-file-name (part subpart-name) 
  (declare (ignore part))
  (make-pathname 
   :name (format nil "index-of-~(~A~)" subpart-name)
   :type "html"
   :defaults *document-root*))

;;; ---------------------------------------------------------------------------

(defun index-url-name (part subpart-name) 
  (make-root-pointing-url 
   part (namestring (pathname-name+type (index-file-name part subpart-name)))))

;;; ---------------------------------------------------------------------------

(defun build-index-links (for-part index-part current-index)
  (when (any-indexes-p index-part)
    (html
     ((:DIV :CLASS "index-links")
      ((:DIV :CLASS "index") "Indexes:")
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
              ((:DIV :CLASS "index")
               (if (eq index-kind current-index)
                 (lml-format "~:(~A~)" index-kind)
                 (html ((:a :href (index-url-name for-part index-name))
                        (lml-format "~:(~A~)" index-kind))))))))))))))

;;; ---------------------------------------------------------------------------

(defun any-indexes-p (part)
  (iterate-elements
   (index-kinds part)
   (lambda (index-description)
     (bind (((part-kinds &key &allow-other-keys) index-description))
       (when (some-element-p part-kinds (lambda (kind) 
                                          (index-for-kind-p part kind)))
         (return-from any-indexes-p t))))))

;;; ---------------------------------------------------------------------------

(defun index-for-kind-p (part kind)
  (and (index? (subpart-info part kind))
       (some-element-p
        (item-at (subparts part) kind)
        #'index-part-p)))

;;; ---------------------------------------------------------------------------

(defun index-part-p (part)
  (and (document? part)
       (or (and (boundp (name part)))
           (and (fboundp (name part)) (symbol-function (name part)))
           (find-class (name part) nil))))
