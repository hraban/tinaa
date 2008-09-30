(in-package #:tinaa)

(defmethod string->html ((string t) &optional (max-length nil))
  (declare (ignore max-length))
  nil)

(let ((result (make-array 50000 :fill-pointer 0 :adjustable t)))
  (defmethod string->html ((string string) &optional (max-length nil))
    (when (and (numberp max-length)
               (> max-length (array-dimension result 0)))
      (setf result (make-array max-length :fill-pointer 0 :adjustable t)))
    (let ((index 0)
          (left-quote? t))
      (labels ((add-char (it)
                 (setf (aref result index) it)
                 (incf index))
               (add-string (it)
                 (loop for ch across it do
                       (add-char ch))))
        (loop for char across string do
              (cond ((char= char #\<)
                     (add-string "&lt;"))
                    ((char= char #\>)
                     (add-string "&gt;"))
                    ((char= char #\&)
                     (add-string "&amp;"))
                    ((char= char #\')
                     (add-string "&#39;"))
                    ((char= char #\newline)
                     (add-string "<br>"))
                    ((char= char #\")
                     (if left-quote? (add-string "&#147;") 
			 (add-string "&#148;"))
                     (setf left-quote? (not left-quote?)))
                    (t
                     (add-char char))))
        (setf (fill-pointer result) index)
        (coerce result 'string)))))

#+Test
(string->html "\"hello,\" he said <briskly>. \"It's a nice day & all\"")


(defun display-arguments (arguments)
  (dolist (argument arguments)
    (cond ((consp argument)
           ;; probably part of a macro
           (lml-format "~( ~A~)" argument))
          ((string-equal (symbol-name argument) "&" :start1 0 :start2 0 :end1 1 :end2 1) 
           (lml-format "~( ~A~)" argument))
          (t
           (lml-format "~( &lt; ~A &gt;~)" argument)))))


(defun display-function (part)
  (lml-princ "\( ")
  (html (:b (lml-princ (part-name part))))
  (display-arguments (function-arglist (name part)))
  (lml-princ " \)"))


(defun map-subpart-kinds (part fn)
  (loop for subpart-info in (subpart-kinds part) do
        (funcall fn subpart-info)))


(defun map-parts-from-leaves (part fn)
  (let ((seen (make-container 'associative-container)))
    (labels ((do-it (part fn)
               (unless (item-at seen part)
                 (setf (item-at seen part) t)
                 (map-subpart-kinds 
                  part
                  (lambda (subpart-info)
                    (iterate-container (item-at (subparts part) (name subpart-info))
                                       (lambda (sub-part) (do-it sub-part fn)))))
                 (funcall fn part))))
      (do-it part fn))))


(defun set-flags (part value)
  (map-parts-from-leaves part (lambda (p) (setf (flag? p) value))))


;;?? also in regenerate-websites
(defun lml-insert-file (file)
  (if (probe-file file)
      (with-open-file (in file :direction :input)
        (do ((line (read-line in nil 'eof) (read-line in nil 'eof)))
	    ((eq line 'eof))
	  (html (lml-princ line))))
    (format *trace-output* "Warning: unable to insert LML file ~S" file)))
