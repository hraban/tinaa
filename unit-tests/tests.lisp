(in-package #:tinaa-tests)

(defun pathnames-same-p (p1 p2)
  (string-equal (namestring (translate-logical-pathname p1))
                (namestring (translate-logical-pathname p2))))

(deftestsuite test-url->file () ()
  :teardown (setf *document-root* nil))

(deftestsuite test-url->file-1 (test-url->file)
  ()
  :setup (setf *document-root* "user-home:docs;foo;"))

(addtest (test-url->file-1)
  (ensure-same (url->file "butter") "user-home:docs;foo;butter.html" 
               :test 'pathnames-same-p))

(addtest (test-url->file-1)
  (ensure-same (url->file "better/butter") "user-home:docs;foo;better;butter.html" 
               :test 'pathnames-same-p))

(addtest (test-url->file-1)
  (ensure-same (url->file "better/butter.text") "user-home:docs;foo;better;butter.html" 
               :test 'pathnames-same-p))

(addtest (test-url->file-1)
  (ensure-same (url->file "better/butter.text" "htm") "user-home:docs;foo;better;butter.htm" 
               :test 'pathnames-same-p))

(addtest (test-url->file-1)
  (ensure-same (url->file "better/butter.text") "user-home:docs;foo;better;butter.html" 
               :test 'pathnames-same-p))

(deftestsuite test-url->file-2 (test-url->file)
  ()
  :setup (setf *document-root* 
               ;; MCL uses MacOS directory separators and adds volume name to namestring
               #+DIGITOOL "Billy-Pilgrim:Users:gwking:docs:foo:"
               #-DIGITOOL "/Users/gwking/docs/foo/"))

(addtest (test-url->file-2)
  (ensure-same (url->file "butter") "user-home:docs;foo;butter.html" 
               :test 'pathnames-same-p))

(addtest (test-url->file-2)
  (ensure-same (url->file "better/butter.text") "user-home:docs;foo;better;butter.html" 
               :test 'pathnames-same-p))

;;; ---------------------------------------------------------------------------
;;; relative-url
;;; ---------------------------------------------------------------------------
