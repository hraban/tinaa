(in-package #:tinaa)

;; Build documentation for the package Tinaa and write it to
;; the directory "user-home:docs;package;tinaa;"
;;
;; i.e., /users/gwking/docs/package/tinaa/"
;;
(tinaa:document-system :package 'tinaa
                       "user-home:docs;package;tinaa;")


;; Build documentation for the ASDF-System Tinaa and write it to
;; the directory "user-home:docs;asdf;tinaa;"
;;
(tinaa:document-system :asdf-system 'tinaa
                       "user-home:docs;asdf;tinaa;")


(progn
  (setf mopu
        (document-system 'asdf-system 'moptilities
                         "user-home:docs;asdf;moptilities;"
                         :write-files? t
                         :page-writer-class 'page-writer-with-graphs)))


(document-system 'asdf-system 'floyd-warshall
                 "user-home:docs;asdf;floyd-warshall;"
                 :write-files? t)

(build-documentation ms "user-home:docs;asdf;moptilities;" :erase-first? t)

(progn
  (setf ms
        (document-system 'asdf-system 'tinaa
                         "user-home:docs;asdf;tinaa;")))

(profile:with-profiling  
  (setf ms
        (document-system 'asdf-system 'tinaa
                         "user-home:docs;asdf;tinaa;"
                         :write-files? nil
                         :show-parts-without-documentation? nil)))