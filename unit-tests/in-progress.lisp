(in-package tinaa-tests)

#|

Hi Gary,

The error remains, I did some testing to isolate the error. 
It seems that this error occours when a class has a superclass in a different package and defines
default-initargs. 
I can offer the following observations:

1.  In the past, tinaa was sucessfull with my package before I updated
your libraries.

   I did not run Tinaa on that package for quite a while. I believe,
but I'm not 100% sure, that I have not changed my packages since the
last successful documentation. So I'm not sure that the error was
caused by the update of your libraries.

2. This will trigger the Error: NIL is not the name of a class

|#

(when (find-package :package-2)
  (delete-package :package-2))

(when (find-package :package-1)
  (delete-package :package-1))

(cl:defpackage :package-1
 (:use
  #:common-lisp
  #:cl-user
  )
 #+(or)
 (:export #:class-1))

(in-package :package-1)

(defclass class-1 ()
 ((slot-1 :accessor slot-1 :initarg slot-1)))

(cl:defpackage :package-2
 (:use
  #:common-lisp
  #:cl-user
  #:package-1
  ))

(in-package :package-2)

(defclass class-2 (class-1)
 ((slot-2 :accessor slot-2))
 (:default-initargs
  :slot-1 t)
 )

#+(or)
;; ok 
(tinaa:document-system
 'package :package-2
 "/tmp/test-tinaa/"
 :write-files? nil)

#+(or)
;; ok now
(tinaa:document-system
 'package :package-2
 "/tmp/test-tinaa/"
 :write-files? t)


#|
(asdf:operate 'asdf:load-op :tinaa :serial t)

(tinaa:document-system 'package :package-2 (ensure-directories-exist "c:/temp/test-tinaa/"))

4. If you comment out   (:default-initargs  :slot-1 t), no problem.

5. If you comment out (export 'class-1) tinaa hangs at 100% cpu usage,
may be because class-1 exists in package-2 and is not a class.


I hope that helps.

Frank


|#