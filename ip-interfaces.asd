;;;   Copyright (C) 2010 Elliott Slaughter <elliottslaughter@gmail.com>
;;;
;;;   This file was *NOT* derived from Clozure CL source code.
;;;   However, it is licensed under the LLGPL to maintain license
;;;   compatibility with the rest of the library.

(defpackage :ip-interfaces-asd
  (:use :cl :asdf))

(in-package :ip-interfaces-asd)

(defsystem ip-interfaces
  :name "ip-interfaces"
  :author "Elliott Slaughter <elliottslaughter@gmail.com>"
  :version "0.0"
  :components ((:file "package")
               (:file "sockets")
               (:file "ip-interfaces"))
  :serial t
  :depends-on (:cffi))
