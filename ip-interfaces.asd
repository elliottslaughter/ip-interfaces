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
