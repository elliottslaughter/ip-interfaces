#!/bin/bash

set -e
set -x

curl -L https://raw.githubusercontent.com/roswell/roswell/release/scripts/install-for-ci.sh | sh

ros run -e '(format t "~a ~a~%" (lisp-implementation-type) (lisp-implementation-version))' -q

# Run a basic sanity test for human-readable output.
ros run -e '(ql:quickload :ip-interfaces)' -e '(format t "~a~%" (ip-interfaces:get-ip-interfaces))' -q

# Run test suite.
ros run -e '(ql:quickload :prove)' -e '(ql:quickload :ip-interfaces)' -e '(asdf:test-system :ip-interfaces)' -q
