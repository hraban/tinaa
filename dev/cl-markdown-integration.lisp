(in-package #:tinaa)

(defmethod maybe-show-documentation ((part basic-doclisp-part))
  ;; just like the regular show-documentation only we call out to
  ;; markdown 
  (let ((documentation (part-documentation part)))
    (when documentation 
      (html ((:div :class "documentation") 
             (lml-princ 
	      (nth-value 
	       1 
	       (cl-markdown:markdown documentation :stream nil))))))))
