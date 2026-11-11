;;;; SPDX-FileCopyrightText: 2026 Anthony Green <green@moxielogic.com>
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Purpose:
;;;; Verify RE2 syntax and semantics: what Go's regexp accepts and refuses,
;;;; leftmost-first matching, flags, Unicode classes and linear running time.

(in-package #:cl-re2/tests)
(def-suite re2)
(in-suite re2)

(defun re2-test-span (pattern text)
  "Return the span of PATTERN's match in TEXT as a list, or NIL."
  (multiple-value-bind (start end) (cl-re2:re2-search (cl-re2:re2-compile pattern) text)
    (and start (list start end))))

(test re2-matches-as-go-regexp-does
  (is (equal '(0 3) (re2-test-span "abc" "abcd")))
  (is (equal '(1 3) (re2-test-span "b+" "abbc")))
  (is (null (re2-test-span "^b" "abc")))
  (is (equal '(0 0) (re2-test-span "x*" "abc")))
  ;; Leftmost-first, not leftmost-longest.
  (is (equal '(0 1) (re2-test-span "a|ab" "ab")))
  (is (equal '(0 2) (re2-test-span "ab|a" "ab")))
  (is (equal '(0 3) (re2-test-span "a+" "aaa")))
  (is (equal '(0 1) (re2-test-span "a+?" "aaa")))
  (is (equal '(0 1) (re2-test-span "(?U)a+" "aaa")))
  (is (equal '(0 3) (re2-test-span "(?U)a+?" "aaa")))
  (is (equal '(0 2) (re2-test-span "a{2}" "aaa")))
  (is (equal '(0 3) (re2-test-span "a{2,}" "aaa")))
  (is (equal '(0 3) (re2-test-span "a{1,3}" "aaaa")))
  (is (equal '(0 1) (re2-test-span "a{1,3}?" "aaaa")))
  (is (equal '(0 4) (re2-test-span "(ab){2}" "abab")))
  (is (equal '(0 3) (re2-test-span "[a-c]+" "abcd")))
  (is (equal '(3 4) (re2-test-span "[^a-c]" "abcd")))
  (is (equal '(0 2) (re2-test-span "[[:alpha:]]+" "ab1")))
  (is (equal '(2 3) (re2-test-span "[[:^alpha:]]" "ab1")))
  (is (equal '(0 1) (re2-test-span "[\\d]" "1a")))
  (is (equal '(0 1) (re2-test-span "[\\D]" "a1")))
  (is (equal '(1 2) (re2-test-span "\\w" " a ")))
  (is (equal '(0 1) (re2-test-span "\\W" " a ")))
  (is (equal '(0 2) (re2-test-span "\\Qa.\\E" "a.")))
  (is (null (re2-test-span "\\Qa.\\E" "ab")))
  (is (equal '(0 1) (re2-test-span "\\x41" "A")))
  (is (equal '(0 1) (re2-test-span "\\x{263a}" (string (code-char #x263a)))))
  (is (equal '(0 1) (re2-test-span "\\101" "A"))))

(test re2-anchors-and-flags
  ;; $ without m is the end of text, never before a trailing newline.
  (is (null (re2-test-span "a$" (format nil "a~%"))))
  (is (equal '(0 1) (re2-test-span "(?m)a$" (format nil "a~%"))))
  (is (equal '(2 3) (re2-test-span "(?m)^b" (format nil "a~%b"))))
  (is (null (re2-test-span "^b" (format nil "a~%b"))))
  (is (equal '(0 1) (re2-test-span "\\Aa" "ab")))
  (is (equal '(1 2) (re2-test-span "b\\z" "ab")))
  (is (null (re2-test-span "a.b" (format nil "a~%b"))))
  (is (equal '(0 3) (re2-test-span "(?s)a.b" (format nil "a~%b"))))
  (is (equal '(0 3) (re2-test-span "(?i)ABC" "abc")))
  (is (equal '(0 3) (re2-test-span "(?i)[a-c]+" "ABC")))
  (is (null (re2-test-span "(?i:a)B" "ab")))
  (is (equal '(0 2) (re2-test-span "(?i:a)b" "Ab")))
  (is (equal '(0 3) (re2-test-span "\\bfoo\\b" "foo")))
  (is (null (re2-test-span "\\bfoo\\b" "food")))
  (is (equal '(1 2) (re2-test-span "\\Ba" "aa"))))

(test re2-unicode-classes
  (let ((alpha (string (code-char #x3b1))) (latin "a"))
    (is (equal '(0 1) (re2-test-span "\\p{Greek}" alpha)))
    (is (null (re2-test-span "\\p{Greek}" latin)))
    (is (equal '(0 1) (re2-test-span "\\P{Greek}" latin)))
    (is (equal '(0 1) (re2-test-span "\\p{^Greek}" latin)))
    (is (equal '(0 1) (re2-test-span "\\pL" alpha)))
    (is (equal '(0 1) (re2-test-span "\\p{Lu}" "A")))
    (is (null (re2-test-span "\\p{Lu}" "a")))
    (is (equal '(0 1) (re2-test-span "[\\p{Greek}x]" "x")))
    (is (equal '(0 1) (re2-test-span "." alpha)))
    ;; \\d and \\w stay ASCII, as RE2's do.
    (is (null (re2-test-span "\\w" alpha)))))

(test re2-refuses-what-go-refuses
  (dolist (pattern '("(" ")" "[" "a**" "a*+" "a++" "a{2}{3}" "*" "+a" "?" "(?=a)" "(?!a)" "(?<=a)" "(?<!a)"
                     "\\1" "\\8" "\\C" "\\y" "\\" "a{1001}" "a{2,1}" "[b-a]" "[[:nope:]]" "\\p{Nope}"
                     "(?P<n" "(?z)" "\\x{110000}" "\\xg"))
    (signals cl-re2:re2-syntax-error (cl-re2:re2-compile pattern) "accepted ~S" pattern))
  (dolist (pattern '("a{,2}" "a{" "a{x}" "{" "}" "]" "a|" "|" "()" "(|a)" "(?i)" "(?P<name>a)" "(?<name>a)" "a{1000}"))
    (finishes (cl-re2:re2-compile pattern)))
  ;; Text that is not a repeat count stays literal.
  (is (equal '(0 5) (re2-test-span "a{,2}" "a{,2}")))
  (signals cl-re2:re2-syntax-error (cl-re2:re2-compile "((a{1000}){1000}){1000}"))
  (signals cl-re2:re2-syntax-error (cl-re2:re2-compile "(a{100}){100}"))
  (finishes (cl-re2:re2-compile "(a{10}){100}")))

(test re2-runs-in-linear-time
  (let* ((text (make-string 5000 :initial-element #\a))
         (program (cl-re2:re2-compile "(a|a)*(a*)*(x+x+)+y"))
         (start (get-internal-real-time)))
    (is (null (cl-re2:re2-search program text)))
    (is (< (- (get-internal-real-time) start) (* 20 internal-time-units-per-second)))))
