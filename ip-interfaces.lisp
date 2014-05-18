;;;   Copyright (C) 2010 Elliott Slaughter <elliottslaughter@gmail.com>
;;;
;;;   The source code in this file was adapted from Clozure CL, from
;;;   version 1.4 file ccl/level-1/l1-sockets.lisp. The original license
;;;   for that file is included below for your convenience. This
;;;   derivative work is also licensed under the LLGPL.
;;;
;;;   Copyright (C) 2001-2009 Clozure Associates
;;;   This file is part of Clozure CL.  
;;;
;;;   Clozure CL is licensed under the terms of the Lisp Lesser GNU Public
;;;   License , known as the LLGPL and distributed with Clozure CL as the
;;;   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
;;;   which is distributed with Clozure CL as the file "LGPL".  Where these
;;;   conflict, the preamble takes precedence.  
;;;
;;;   Clozure CL is referenced in the preamble as the "LIBRARY."
;;;
;;;   The LLGPL is also available online at
;;;   http://opensource.franz.com/preamble.html

(in-package :ip-interfaces)

(defstruct ip-interface
  name
  address
  netmask
  broadcast-address
  flags
  address-family)

(defun bytes (pointer count)
  (apply #'vector (loop for i from 0 below count
                     collect (mem-aref pointer :uchar i))))

#+unix
(defun get-ip-interfaces ()
  (with-foreign-object (p :pointer)
    (when (zerop (getifaddrs p))
      (unwind-protect
           (do ((q (mem-ref p :pointer)
                   (foreign-slot-value q '(:struct ifaddrs) 'ifa-next))
                (res ()))
               ((null-pointer-p q) (nreverse res))
             (let ((addr (foreign-slot-value q '(:struct ifaddrs) 'ifa-addr)))
               (when (and (not (null-pointer-p addr))
                          (eql (foreign-slot-value addr '(:struct sockaddr) 'sa-family)
                               (foreign-enum-value 'address-family :af-inet)))
                 (push (make-ip-interface
                        :name (foreign-slot-value q '(:struct ifaddrs) 'ifa-name)
                        :address
                        (bytes (foreign-slot-value addr '(:struct sockaddr) 'sa-data) 4)
                        :netmask
                        (bytes (foreign-slot-value
                                (foreign-slot-value q '(:struct ifaddrs) 'ifa-netmask)
                                '(:struct sockaddr) 'sa-data)
                               4)
                        :broadcast-address
                        (bytes (foreign-slot-value
                                (foreign-slot-value q '(:struct ifaddrs) 'ifa-broadaddr)
                                '(:struct sockaddr) 'sa-data)
                               4)
                        :flags (foreign-slot-value q '(:struct ifaddrs) 'ifa-flags)
                        :address-family :af-inet)
                       res))))
        (freeifaddrs (mem-ref p :pointer))))))

#+windows
(defun get-ip-interfaces ()
  (with-foreign-object (wsadata 'wsadata)
    (unless (zerop (wsastartup #x0202 wsadata))
      (return-from get-ip-interfaces)))
  (let ((socket (socket :af-inet :sock-dgram :ipproto-ip)))
    (unwind-protect
    (with-foreign-object (realoutlen 'dword)
      (do* ((i 128 (* i 2))
	    (reservedlen (* i (foreign-type-size 'interface-info))
			 (* i (foreign-type-size 'interface-info))))
	   ((> i 1024))
	(with-foreign-object (buf :uchar reservedlen)
	  (unless (zerop (wsaioctl
			  socket
			  #x4004747F	; SIO_GET_INTERFACE_LIST
			  (null-pointer)
			  0
			  buf
			  reservedlen
			  realoutlen
			  (null-pointer)
			  (null-pointer)))
	    (return nil))
	  (let ((noutbytes (mem-ref realoutlen 'dword)))
	    (when (< noutbytes reservedlen)
	      (let ((interfaces nil))
		(do* ((offset 0 (+ offset
				   (foreign-type-size 'interface-info)))
		      (nameidx 0 (1+ nameidx)))
		     ((>= offset noutbytes))
		     (let ((p (inc-pointer buf offset)))
		       (push (make-ip-interface
			      :name (format nil "ip~d" nameidx)
			      :address
			      (bytes (foreign-slot-value
				      (foreign-slot-value
				       (foreign-slot-value
					p 'interface-info 'ii-address)
				       'sockaddr-gen
				       'address-in)
				      'sockaddr-in
				      'sin-addr)
				     4)
			      :netmask
			      (bytes (foreign-slot-value
				      (foreign-slot-value
				       (foreign-slot-value
					p 'interface-info 'ii-netmask)
				       'sockaddr-gen
				       'address-in)
				      'sockaddr-in
				      'sin-addr)
				     4)
			      :broadcast-address
			      (bytes (foreign-slot-value
				      (foreign-slot-value
				       (foreign-slot-value
					p 'interface-info
					'ii-broadcast-address)
				       'sockaddr-gen
				       'address-in)
				      'sockaddr-in
				      'sin-addr)
				     4)
			      :flags (foreign-slot-value
				      p 'interface-info 'ii-flags)
			      :address-family :af-inet)
			     interfaces)))
		(return interfaces)))))))
    (closesocket socket))))