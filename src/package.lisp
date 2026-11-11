;;;; SPDX-FileCopyrightText: 2026 Anthony Green <green@moxielogic.com>
;;;; SPDX-License-Identifier: MIT

(defpackage #:cl-re2
  (:use #:cl)
  (:export #:re2-compile #:re2-search #:re2-match-p
           #:re2-syntax-error #:re2-syntax-error-message))
