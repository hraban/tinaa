;;;-*- Mode: Lisp -*-

#| copyright

See the file COPYING for details

|#

(defpackage #:tinaa-test-system-test (:use #:asdf #:cl))
(in-package #:tinaa-test-system-test)

(defsystem tinaa-test
  :author "Gary Warren King <gwking@metabang.com>"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style license"
  :description "Test for tinaa"
  :components ((:module 
		"setup"
		:pathname "unit-tests/"
		:components ((:file "packages")))
		(:module 
		"unit-tests"
		:components ((:file "tests"))
		:depends-on ("setup"))
               )
  :depends-on (:tinaa :lift))


