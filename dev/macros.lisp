(in-package #:doclisp)

(defmacro with-html-output ((stream) &body body)
  `(let ((*html-stream* ,stream))
      ,@body))


(defmacro documenting (part &body body)
  `(symbol-macrolet ((name (part-name ,part))
                     (documentation? (not (null (part-documentation ,part)))) 
                     (documentation (string->html (part-documentation ,part)))
                     (short-documentation (string->html (short-documentation ,part)))
                     (subparts (subparts ,part))
                     (url (url ,part)))
     (macrolet ((parts (kind)
                  `(item-at subparts ,kind))
                (link-for (mode)
                  `(make-link-for ,mode name url))
                (mark-spot (mode)
                  `(make-name-link-for ,mode name url)))
       (with-html-output (*document-stream*)
         (html ,@body)))))


(defmacro documenting-page ((part &key title force-contents-link?) &body body)
  `(documenting part
     (:html
      (:head
       (:title (if ,title 
                 (lml-princ ,title)
                 (lml-format "~A ~:(~A~) [Tinaa]"
                             (header ,part) (name ,part))))
       ((:link :rel "stylesheet" :href (stylesheet-url ,part))))
      (:body
       (doclisp-header ,part :force-contents-link? ,force-contents-link?)
       
       ((:div :class "contents")
        ,@body)
       
       (doclisp-footer ,part :force-contents-link? ,force-contents-link?)))))

