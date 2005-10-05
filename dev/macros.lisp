(in-package doclisp)

(defmacro with-html-output ((stream) &body body)
  `(let ((*html-output* ,stream))
      ,@body))