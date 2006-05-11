(in-package #:doclisp)
(with-open-file (*document-stream* "ccl:tina.html" 
                                   :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create)
  
  (with-html-output (*document-stream*)
    (html
      (head
        (title "Tinaa is a Lisp Documentation system, not an acronym")
        (link :rel "stylesheet" 
              :href "index.css"))
      (body
        (doclisp-header pp)
        
        (h2 "Tinaa: a Lisp documentation system")
        (p "Tinaa is not an acronym.")
        
        (doclisp-footer pp)))))