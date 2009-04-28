(in-package #:tinaa)

directory-pathname-p

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


add-contents-link

TINAA::STRING->HTML

; caught STYLE-WARNING:
;   undefined function: METABANG.MOPTILITIES:SUBCLASSP

; caught STYLE-WARNING:
;   These functions are undefined:
;     TINAA::ADD-CONTENTS-LINK TINAA::ADD-PART-VERTEX TINAA::BUILD-CONTENTS-PAGE TINAA::BUILD-DOCUMENTATION TINAA::CSS-FILE-FOR-WRITER TINAA::DEPTH-FOR-PART TINAA::DIRECT-INSTANCE TINAA::DIRECT-PARENT TINAA::DOCLISP-FOOTER TINAA::DOCLISP-HEADER TINAA::EDGE-KINDS-FOR-PART-GRAPH TINAA::INCLUDE-IN-CONTENTS-P TINAA::LAYOUT-ENGINE-FOR-PART TINAA::LAYOUT-GRAPH-TO-FILE TINAA::MAKE-LINK-FOR TINAA::MAKE-PART-GRAPH TINAA::MAKE-ROOT-POINTING-URL TINAA::MAYBE-SHOW-DOCUMENTATION TINAA::PART-KIND-ABBREVIATION TINAA::STRING->HTML METABANG.MOPTILITIES:SUBCLASSP TINAA::SYSTEM-FILES
