(in-package #:tinaa)


#+Ignore
;; returns a list of properties found in the defsystems I have...
(let ((props nil))
  (fad:walk-directory
   "Billy-Pilgrim:repository:darcs:asdf-systems"
   (lambda (file)
     (let ((system-def (find-defsystem file)))
       (print file)
       (when system-def
         (loop for key in (cddr system-def) by #'cddr do
               (push key props)))))
   :test (lambda (x) (string-equal (pathname-type x) "asd")))
  (sort (remove-duplicates props) #'string-lessp))


(defun find-defsystem (stream-or-file)
  (let ((*package* (find-package :asdf)))
    (map-forms-in-file 
     (lambda (form)
       (when (and (consp form)
                  (eql (car form) 'asdf:defsystem))
         (return-from find-defsystem form)))
     stream-or-file)))


;;; misc.

#+Ignore
;; returns a list of properties found in the defsystems I have...
(let ((props nil))
  (flet ((find-defsystem (stream-or-file)
           (let ((*package* (find-package :asdf)))
             (map-forms-in-file 
              (lambda (form)
                (when (and (consp form)
                           (eql (car form) 'asdf:defsystem))
                  (return-from find-defsystem form)))
              stream-or-file))))
    (fad:walk-directory
     "Billy-Pilgrim:repository:darcs:asdf-systems"
     (lambda (file)
       (let ((system-def (find-defsystem file)))
         (print file)
         (when system-def
           (loop for key in (cddr system-def) by #'cddr do
                 (push key props)))))
     :test (lambda (x) (string-equal (pathname-type x) "asd"))))
  (sort (remove-duplicates props) #'string-lessp))

(defpackage #:tinaa
  (:use #:common-lisp
	#:moptilities
	#:metatilities 
	))
