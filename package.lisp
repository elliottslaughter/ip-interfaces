;;;   Copyright (C) 2010 Elliott Slaughter <elliottslaughter@gmail.com>
;;;
;;;   This file was *NOT* derived from Clozure CL source code.
;;;   However, it is licensed under the LLGPL to maintain license
;;;   compatibility with the rest of the library.

(cl:defpackage :ip-interfaces
  (:use :cl :cffi)
  (:export

   :get-ip-interfaces
   :get-ip-interfaces-by-flags

   :filter-ip-interfaces-by-flags

   :ip-interface-name
   :ip-interface-address
   :ip-interface-netmask
   :ip-interface-broadcast-address
   :ip-interface-flags
   :ip-interface-address-family

   ))