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
          (:p
            (dolist (letter symbol-list)
              (if (some-key-value-p 
                   parts
                   (lambda (name part)
                     (and 
                      (string-equal (subseq (symbol-name name) 0 1) letter)
                      (index-part-p part))))
                   
                (html ((:a :href (concatenate
                                  'string 
                                  "#" letter "-" (symbol-name (name subpart-info)))) 
                       (lml-princ letter)))
                (lml-format "~A" letter))
              (lml-princ "&nbsp;&nbsp;")))
          
          (iterate-container
           parts
           (lambda (part)
             (when (index-part-p part)
               (let ((part-name (part-name part)))
                 (unless (string-equal current-symbol (subseq part-name 0 1))
                   (setf current-symbol (subseq part-name 0 1))
                   (html ((:a :name (concatenate 
                                     'string current-symbol "-" 
                                     (symbol-name (name subpart-info))))
                          (:table
                           (:tr ((:td :width 75 :valign "top" :align "left")
                                 (:h3 (lml-princ (string-upcase current-symbol))))
                                ((:td :valign "top" :align "right")
                                 ((:a :href "#top") "Back to top")))))))
                 (html (:p (display-part part :index))))))))))))

;;; ---------------------------------------------------------------------------

(defmethod display-part ((part basic-doclisp-part) (mode (eql :index)))
  (display-part-for-index part (part-name part)))

;;; ---------------------------------------------------------------------------

(defun display-part-for-index (part string)
  (if (documentation-exists-p part :detail)
    (html ((:a :href (relative-url (url part) *document-file*))
           (lml-princ string)))
    (lml-princ string)))

;;; ---------------------------------------------------------------------------

(defmethod display-part ((part doclisp-symbol) (mode (eql :index)))
  (let ((name-holder (name-holder part)))
    (html
     (:table
      (:tr
       ((:td :width 200) (display-part-for-index part (part-name part)))
       (loop for kinds in (index-kinds (name-holder part)) do
             (let ((real-part (some (lambda (kind)
                                      (find-part name-holder kind (name part)))
                                    kinds)))
               (when real-part
                 (html
                  ((:td :width "20%")
                   (display-part-for-index 
                    real-part (string-downcase (part-kind real-part)))))))))))))

;;; ---------------------------------------------------------------------------

(defmethod build-indexes (part)
  (map-subpart-kinds
   part
   (lambda (subpart-info)
     (when (index-for-kind-p part subpart-info)
       
       (let ((*document-file* (index-file-name part subpart-info)))
         (with-open-file (*document-stream* *document-file* 
                                            :direction :output
                                            :if-exists :supersede
                                            :if-does-not-exist :create)
           
           (build-index-of-kind part subpart-info)))))))

;;; ---------------------------------------------------------------------------

(defun index-file-name (part subpart-info) 
  (declare (ignore part))
  (make-pathname 
   :name (format nil "index-of-~A"
                 (string-downcase (name subpart-info)))
   :type "html"
   :defaults *document-root*))

;;; ---------------------------------------------------------------------------

(defun index-url-name (part subpart-info) 
  (make-root-pointing-url 
   part 
   (format nil "index-of-~A.html"
           (string-downcase (name subpart-info)))))

;;; ---------------------------------------------------------------------------

(defun build-index-links (for-part index-part current-index)
  (when (any-indexes-p index-part)
    (html
     ((:DIV :CLASS "index-links")
      ((:DIV :CLASS "index") "Indexes:")
      (map-subpart-kinds
       index-part
       (lambda (subpart-info)
         (when (index-for-kind-p index-part subpart-info)
           (html
            ((:DIV :CLASS "index")
             (if (eq (name subpart-info) current-index)
               (lml-format "~:(~A~)" (name subpart-info))
               (html ((:a :href (index-url-name for-part subpart-info))
                      (lml-format "~:(~A~)" (name subpart-info))))))))))))))

;;; ---------------------------------------------------------------------------

(defun any-indexes-p (part)
  (map-subpart-kinds
   part
   (lambda (subpart-info)
     (when (index-for-kind-p part subpart-info)
       (return-from any-indexes-p t)))))

;;; ---------------------------------------------------------------------------

(defun index-for-kind-p (part subpart-info)
  (and (index? subpart-info)
       (some-element-p
        (item-at (subparts part) (name subpart-info))
        #'index-part-p)))

;;; ---------------------------------------------------------------------------

(defun index-part-p (part)
  (and (document? part)
       (or (and (boundp (name part)))
           (and (fboundp (name part)) (symbol-function (name part)))
           (find-class (name part) nil))))
