(in-package tinaa)

;; Build documentation for the package Tinaa and write it to
;; the directory "user-home:docs;package;tinaa;"
;;
;; i.e., /users/gwking/docs/package/tinaa/"
;;
(document-system :package 'tinaa
                 "user-home:docs;package;tinaa;")


;; Build documentation for the ASDF-System Tinaa and write it to
;; the directory "user-home:docs;asdf;tinaa;"
;;
(document-system :asdf-system 'tinaa
                 "user-home:docs;asdf;tinaa;")


(progn
  (setf ms
        (document-system 'asdf-system 'moptilities
                         "user-home:docs;asdf;moptilities;"
                         :write-files? nil)))


(document-system 'asdf-system 'floyd-warshall
                 "user-home:docs;asdf;floyd-warshall;"
                 :write-files? t)

(build-documentation ms "user-home:docs;asdf;moptilities;" :erase-first? t)

(progn
  (fad:delete-directory-and-files "user-home:docs;asdf;tinaa;"
                                  :if-does-not-exist :ignore)
  (setf ms
        (document-system 'asdf-system 'tinaa
                         "user-home:docs;asdf;tinaa;")))

(profile:with-profiling  
  (setf ms
        (document-system 'asdf-system 'tinaa
                         "user-home:docs;asdf;tinaa;"
                         :write-files? nil
                         :show-parts-without-documentation? nil)))