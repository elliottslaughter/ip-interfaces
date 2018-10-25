;;;   Copyright (C) 2018 Elliott Slaughter <elliottslaughter@gmail.com>
;;;
;;;   This file was *NOT* derived from Clozure CL source code.
;;;   However, it is licensed under the LLGPL to maintain license
;;;   compatibility with the rest of the library.

(cl:defpackage :ip-interfaces-test
  (:use :cl :ip-interfaces :prove))

(in-package :ip-interfaces-test)

(plan 1)

(let ((ifaces (get-ip-interfaces)))
  (ok (> (length ifaces) 0)))

(finalize)
