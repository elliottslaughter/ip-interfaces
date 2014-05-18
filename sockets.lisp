;;;   Copyright (C) 2010 Elliott Slaughter <elliottslaughter@gmail.com>
;;;
;;;   This file was *NOT* derived from Clozure CL source code.
;;;   However, it is licensed under the LLGPL to maintain license
;;;   compatibility with the rest of the library.

(in-package :ip-interfaces)

;; CFFI interface to sockets

(defcenum address-family
  (:af-unspec 0)
  (:af-inet 2)
  (:af-inet6 23))

(defcenum socket-type
  (:sock-stream 1)
  (:sock-dgram 2)
  (:sock-raw 3))

(defcenum socket-protocol
  (:ipproto-ip 0)
  (:ipproto-tcp 6)
  (:ipproto-udp 17)
  (:ipproto-ipv6 41))

#+unix
(progn
  (defcstruct sockaddr
    #-(or darwin openbsd) (sa-family :ushort)
    #+(or darwin openbsd) (sa-len :uchar)
    #+(or darwin openbsd) (sa-family :uchar)
    (sa-data :uchar :offset 4 :count 14))

  (defcstruct ifaddrs
    (ifa-next :pointer) ; to ifaddrs
    (ifa-name :string)
    (ifa-flags :uint)
    (ifa-addr (:pointer (:struct sockaddr)))
    (ifa-netmask (:pointer (:struct sockaddr)))
    (ifa-broadaddr (:pointer (:struct sockaddr)))
    (ifa-data :pointer))

  (defcfun "getifaddrs" :int
    (ifaddrs (:pointer (:pointer ifaddrs))))

  (defcfun "freeifaddrs" :void
    (ifaddrs (:pointer ifaddrs))))

#+windows
(progn
  ;; TODO: is this required on all implementations?
  (define-foreign-library ws2-32
    (:windows "Ws2_32.dll"))
  (use-foreign-library ws2-32)

  (defctype word :uint16)
  (defctype dword :uint32)
  (defctype lpvoid :pointer)
  (defctype lpdword :pointer)
  (defctype socket :pointer)

  (defcstruct sockaddr
    (sa-family :ushort)
    (sa-data :char :count 14))

  (defcstruct sockaddr-in
    (sin-family :short)
    (sin-port :ushort)
    (sin-addr :uchar :count 4)
    (sin-zero :uchar :count 8))

  (defcstruct sockaddr-in6-old
    (sin6-family :short)
    (sin6-port :ushort)
    (sin6-flowinfo :ulong)
    (sin6-addr :uchar :count 16))

  (defcunion sockaddr-gen
    (address sockaddr)
    (address-in sockaddr-in)
    (address-in6 sockaddr-in6-old))

  (defcstruct interface-info
    (ii-flags :ulong)
    (ii-address sockaddr-gen)
    (ii-broadcast-address sockaddr-gen)
    (ii-netmask sockaddr-gen))

  (defcstruct wsadata
    (w-version word)
    (w-high-version word)
    (sz-description :char :count 256)
    (sz-system-status :char :count 128)
    (i-max-sockets :ushort)
    (i-max-udp-dg :ushort)
    (lp-vendor-info :string))

  (defcfun "socket" socket
    (af address-family)
    (type socket-type)
    (protocol socket-protocol))

  (defcfun "closesocket" :void
    (socket socket))

  (defcfun "WSAStartup" :int
    (w-version-requested word)
    (lp-wsadata (:pointer wsadata)))

  (defcfun "WSAIoctl" :int
    (socket socket)
    (dw-io-control-code dword)
    (lpv-in-buffer lpvoid)
    (cb-in-buffer dword)
    (lpv-out-buffer lpvoid)
    (cb-out-buffer dword)
    (lpcb-bytes-returned lpdword)
    (lp-overlapped :pointer)
    (lp-completion-routine :pointer))

  (defcfun "WSAGetLastError" :int))
