(in-package #:doclisp)


#+Test
(progn
  (setf pp (make-part nil 'package 'doclisp))
  (grovel-part pp)
  (build-documentation pp "ccl:dl;")
  )

#+Test
(timeit (:report t)
        (document-system 'package 'tinaa "home:tinaa-ei;"
                         :symbol-kinds '(:internal :external)))

#+Ignore
(document-system 'package 'tinaa "home:tinaa-ei;"
                 :symbol-kinds '(:internal :external))
#+Test
(timeit (:report t)
        (document-system 'package 'clasp "home:clasp-e;"
                         :symbol-kinds '(:external)))

#+Test
(timeit (:report t)
        (document-system 'package 'gbbl-agenda-shell "home:gbbl-agenda;"
                         :symbol-kinds '(:internal :external)))

#+Test
(document-system 'package 'variates "home:variates;")
#+Test
(document-system 'package 'containers "home:containers;")

#+Test
(build-documentation pp "ccl:hats;")

#+Test
(setf pp (document-system 'package 'hats "ccl:hats;" 
                          :symbol-kinds '(:internal :external)))

#+Test
(timeit (:report t)
        (setf pp (document-system 'package 'u "home:utils;" 
                                  :symbol-kinds '(:external))))

#+Test
(timeit (:report t)
        (setf pp (document-system 'package 'containers "home:containers;" 
                                  :symbol-kinds '(:external))))


#+Test
(setf sp (document-system 'eksl-system 'tinaa "home:tinaa-system;"))

#+Test
(progn
  (setf pp (make-part nil 'package 'u))
  (grovel-part pp)
  (build-documentation pp "ccl:u;")
  )
