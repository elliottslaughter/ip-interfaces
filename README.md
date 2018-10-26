# ip-interfaces [![Build Status](https://travis-ci.org/elliottslaughter/ip-interfaces.svg?branch=master)](https://travis-ci.org/elliottslaughter/ip-interfaces) [![Build status](https://ci.appveyor.com/api/projects/status/l2d355fbg0euv0nd/branch/master?svg=true)](https://ci.appveyor.com/project/elliottslaughter/ip-interfaces/branch/master)

## Introduction

The ip-interfaces library provides a convenient way to obtain a list of
available interfaces on a machine.

IP-Interfaces is a port of Clozure CL's %get-ip-interfaces function to
CFFI so that it can run on other lisp implementations. So far
ip-interfaces has been tested successfully on the following platforms:

| Implementation | Windows | Mac OS X | Linux | OpenBSD |
| :------------- | :------ | :------- | :---- | :------ |
| SBCL           | Yes     | Yes      | Yes   | Yes     |
| CMUCL          |         | Yes      | Yes   |         |
| CLISP          | Yes     | Yes      | Yes   | Yes     |
| Clozure        | Yes     | Yes      | Yes   |         |
| Allegro        | Yes     | Yes      | Yes   |         |
| ECL            |         | Yes      | Yes   | Yes     |
| LispWorks      | Yes     |          |       |         |

IP-Interfaces is released under the
[LLGPL](http://opensource.franz.com/preamble.html) to maintain license
compatibility with Clozure CL.

Note: ip-interfaces is not a socket library. If you are looking for a
portable socket library, check out
[usocket](http://common-lisp.net/project/usocket/).

## Usage

The API for ip-interfaces consists of a single function,
get-ip-interfaces, which returns a list of ip-interface structs
containing information about the interfaces on the machine:

    * (ql:quickload :ip-interfaces)

    NIL
    * (use-package :ip-interfaces)

    T
    * (get-ip-interfaces)

    (#S(IP-INTERFACES::IP-INTERFACE
        :NAME "lo0"
        :ADDRESS #(127 0 0 1)
        :NETMASK #(255 0 0 0)
        :BROADCAST-ADDRESS #(127 0 0 1)
        :FLAGS 32841
        :ADDRESS-FAMILY :AF-INET)
     ...)
    * (ip-interface-address (first *))

    #(127 0 0 1)
