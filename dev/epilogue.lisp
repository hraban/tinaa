(in-package #:tinaa)

(setf *default-packages-to-ignore* 
      (delete nil 
              (mapcar #'canonical-package-id '(:ccl :cl :ae-tools :mach :bundle-sys
                                               :common-lisp-user))))