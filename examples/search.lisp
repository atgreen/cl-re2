;;;; SPDX-FileCopyrightText: 2026 Anthony Green <green@moxielogic.com>
;;;; SPDX-License-Identifier: MIT

(require :asdf)
(let ((root (uiop:pathname-parent-directory-pathname
             (uiop:pathname-directory-pathname *load-pathname*))))
  (pushnew root asdf:*central-registry* :test #'equal))
(asdf:load-system :cl-re2)

(let ((text "Say HELLO world")
      (program (cl-re2:re2-compile "(?i)hello,? [[:alpha:]]+")))
  (multiple-value-bind (start end) (cl-re2:re2-search program text)
    (format t "~A~%" (subseq text start end))))
