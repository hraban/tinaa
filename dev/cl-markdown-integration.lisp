(in-package #:tinaa)

(defmethod string->html :around ((string string) &optional max-length)
  (declare (ignore max-length))
  (let ((docs (call-next-method)))
    (when docs
      (nth-value 
       1 
       (cl-markdown:markdown docs :stream nil)))))

#+(or)
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
