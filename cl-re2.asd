;;;; SPDX-FileCopyrightText: 2026 Anthony Green <green@moxielogic.com>
;;;; SPDX-License-Identifier: MIT

(asdf:defsystem "cl-re2"
  :description "A linear-time RE2-compatible regular expression engine"
  :author "Anthony Green <green@moxielogic.com>"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("cl-ppcre" #-sbcl "cl-unicode")
  :serial t
  :components ((:module "src" :components ((:file "package") (:file "re2"))))
  :in-order-to ((test-op (test-op "cl-re2/tests"))))

(asdf:defsystem "cl-re2/tests"
  :depends-on ("cl-re2" "fiveam")
  :serial t
  :components ((:module "tests" :components ((:file "package") (:file "re2-test"))))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call :fiveam :run!
                                       (find-symbol "RE2" "CL-RE2/TESTS"))
               (error "cl-re2 tests failed"))))
