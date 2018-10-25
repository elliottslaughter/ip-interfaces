;;;   Copyright (C) 2018 Elliott Slaughter <elliottslaughter@gmail.com>
;;;
;;;   This file was *NOT* derived from Clozure CL source code.
;;;   However, it is licensed under the LLGPL to maintain license
;;;   compatibility with the rest of the library.

(defpackage :ip-interfaces-test-asd
  (:use :cl :asdf))

(in-package :ip-interfaces-test-asd)

(defsystem ip-interfaces-test
  :name "ip-interfaces-test"
  :description "Test suite for ip-interfaces."
  :author "Elliott Slaughter <elliottslaughter@gmail.com>"
  :version "0.0"
  :license "LLGPL"
  :components ((:test-file "test"))
  :depends-on (:ip-interfaces :prove)
  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c)))
