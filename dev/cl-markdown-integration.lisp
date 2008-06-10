(in-package #:tinaa)

(defmethod string->html :around ((string string) &optional max-length)
  (declare (ignore max-length))
  (let ((docs (call-next-method)))
    (when docs
      ;; if we get an error calling markdown, just use the original string
      ;;?? FIXME -- should probably warn about this. It happens, e.g., 
      ;; in CL-PPCRE which has at least one docstring with a {1} in it.
      (or (ignore-errors
	    (nth-value 
	     1 
	     (cl-markdown:markdown docs :stream nil)))
	  docs))))

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
