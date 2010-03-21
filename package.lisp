;;;   Copyright (C) 2010 Elliott Slaughter <elliottslaughter@gmail.com>
;;;
;;;   This file was *NOT* derived from Clozure CL source code.
;;;   However, it is licensed under the LLGPL to maintain license
;;;   compatibility with the rest of the library.

(cl:defpackage :ip-interfaces
  (:use :cl :cffi)
  (:export

   :get-ip-interfaces
   :name
   :address
   :netmask
   :broadcast-address
   :flags
   :address-family

   ))