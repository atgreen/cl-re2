;;;; SPDX-FileCopyrightText: 2026 Anthony Green <green@moxielogic.com>
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Purpose:
;;;; Compile and run RE2 syntax regular expressions, as Go's regexp package does
;;;;. The syntax refuses backreferences and lookaround,
;;;; and a Pike virtual machine runs the program in time linear in the text, so a
;;;; hostile pattern cannot stall the application the way a backtracking engine can.

(in-package #:cl-re2)

(define-condition re2-syntax-error (error)
  ((message :initarg :message :reader re2-syntax-error-message))
  (:report (lambda (condition stream) (write-string (re2-syntax-error-message condition) stream)))
  (:documentation "A pattern outside the RE2 syntax."))

(defparameter +re2-maximum-repeat+ 1000 "Largest counted repetition RE2 accepts.")
(defparameter +re2-maximum-program+ 100000 "Most instructions a pattern may expand to.")
(defparameter +re2-maximum-nesting+ 1000 "Deepest group nesting a pattern may use.")
(defparameter +re2-maximum-compiled-program+ 64
  "Largest program transpiled to native code. A bigger one runs on the interpreter instead:
the emitted jump table over program counters costs the host compiler super-linear time, so
beyond a few dozen instructions compiling it outweighs the matching it saves. Both paths are
linear time in the text, so the cap trades throughput for a bounded, predictable compile --
which matters when the pattern itself is attacker supplied.")

(defun re2-error (pattern control &rest arguments)
  "Refuse PATTERN, phrased as Go's regexp/syntax error is."
  (error 're2-syntax-error
         :message (format nil "error parsing regexp: ~?: `~A`" control arguments pattern)))

;;; Character classes are lists of inclusive code ranges, predicates and a negation flag.

(defstruct (re2-class (:conc-name class-)) (ranges nil) (predicates nil) (negated nil))

(defparameter +re2-digit-ranges+ '((48 . 57)))
(defparameter +re2-word-ranges+ '((48 . 57) (65 . 90) (95 . 95) (97 . 122)))
(defparameter +re2-space-ranges+ '((9 . 10) (12 . 13) (32 . 32)))

(defparameter +re2-posix-classes+
  '(("alnum" . ((48 . 57) (65 . 90) (97 . 122)))
    ("alpha" . ((65 . 90) (97 . 122)))
    ("ascii" . ((0 . 127)))
    ("blank" . ((9 . 9) (32 . 32)))
    ("cntrl" . ((0 . 31) (127 . 127)))
    ("digit" . ((48 . 57)))
    ("graph" . ((33 . 126)))
    ("lower" . ((97 . 122)))
    ("print" . ((32 . 126)))
    ("punct" . ((33 . 47) (58 . 64) (91 . 96) (123 . 126)))
    ("space" . ((9 . 13) (32 . 32)))
    ("upper" . ((65 . 90)))
    ("word" . ((48 . 57) (65 . 90) (95 . 95) (97 . 122)))
    ("xdigit" . ((48 . 57) (65 . 70) (97 . 102)))))

(defun re2-class-member-p (class code)
  "Test whether CODE is in CLASS, before any negation."
  (or (loop for (low . high) in (class-ranges class) thereis (<= low code high))
      (loop for predicate in (class-predicates class) thereis (funcall predicate code))))

(defun re2-class-matcher (class fold)
  "Build the function from a code point to whether CLASS accepts it, folding case when FOLD."
  (let ((negated (class-negated class)))
    (lambda (code)
      (let ((found (or (re2-class-member-p class code)
                       (and fold
                            (let ((character (code-char code)))
                              (or (re2-class-member-p class (char-code (char-upcase character)))
                                  (re2-class-member-p class (char-code (char-downcase character)))))))))
        (if negated (not found) (and found t))))))

(defvar *re2-script-names* nil "Lazily collected names of the Unicode scripts.")

(defun re2-script-names ()
  "List every Unicode script SBCL knows, collected on first use."
  (or *re2-script-names*
      (setf *re2-script-names*
            (let ((names (make-hash-table :test #'equal)))
              (loop for code from 0 below char-code-limit by 16
                    for char = (code-char code)
                    #-sbcl when #-sbcl char
                    do (setf (gethash (unicode-script char) names) t))
              names))))

(defun unicode-script (char)
  #+sbcl (sb-unicode:script char)
  #-sbcl (string-upcase (cl-unicode:script char))
  )

(defun unicode-general-category (char)
  #+sbcl (sb-unicode:general-category char)
  #-sbcl (string-upcase (cl-unicode:general-category char))
  )

(defun re2-unicode-predicate (pattern name)
  "Return the membership test of the Unicode category or script NAME, as \\p{NAME} means."
  (let ((upper (string-upcase name)))
    (cond
      ((equal name "Any") (constantly t))
      ((and (<= 1 (length name) 2) (upper-case-p (char name 0))
            (or (= 1 (length name)) (lower-case-p (char name 1)))
            (member name '("C" "Cc" "Cf" "Co" "Cs" "L" "Ll" "Lm" "Lo" "Lt" "Lu" "M" "Mc" "Me" "Mn"
                           "N" "Nd" "Nl" "No" "P" "Pc" "Pd" "Pe" "Pf" "Pi" "Po" "Ps"
                           "S" "Sc" "Sk" "Sm" "So" "Z" "Zl" "Zp" "Zs")
                    :test #'equal))
       (if (= 1 (length name))
           (let ((initial (char upper 0)))
             (lambda (code)
               (char= initial (char (unicode-general-category (code-char code)) 0))))
           (lambda (code) (equal upper (unicode-general-category (code-char code))))))
      ((gethash upper (re2-script-names))
       (lambda (code) (equal upper (unicode-script (code-char code)))))
      (t (re2-error pattern "invalid character class range: `\\p{~A}`" name)))))

;;; Parsing produces a tree of (:char code fold), (:any newline), (:class class fold),
;;; (:assert kind), (:concat nodes), (:alt nodes), (:repeat node min max greedy) and (:empty).

(defstruct (re2-parser (:conc-name rp-))
  pattern (position 0) (fold nil) (multiline nil) (dotall nil) (ungreedy nil) (depth 0))

(defun rp-peek (parser &optional (offset 0))
  "Return the character OFFSET ahead, or NIL past the end."
  (let ((position (+ (rp-position parser) offset)) (pattern (rp-pattern parser)))
    (and (< position (length pattern)) (char pattern position))))

(defun rp-next (parser)
  "Consume and return one character."
  (prog1 (rp-peek parser) (incf (rp-position parser))))

(defun rp-accept (parser text)
  "Consume TEXT when it comes next."
  (let* ((start (rp-position parser)) (end (+ start (length text))) (pattern (rp-pattern parser)))
    (when (and (<= end (length pattern)) (string= text pattern :start2 start :end2 end))
      (setf (rp-position parser) end)
      t)))

(defun re2-parse-alternation (parser)
  "Parse branches separated by |."
  (when (> (incf (rp-depth parser)) +re2-maximum-nesting+)
    (re2-error (rp-pattern parser) "expression nests too deeply"))
  (let ((branches (list (re2-parse-concatenation parser))))
    (loop while (eql #\| (rp-peek parser))
          do (rp-next parser)
             (push (re2-parse-concatenation parser) branches))
    (decf (rp-depth parser))
    (if (rest branches) (list :alt (nreverse branches)) (first branches))))

(defun re2-parse-concatenation (parser)
  "Parse repeated atoms up to | or )."
  (let ((items nil))
    (loop
      (let ((character (rp-peek parser)))
        (when (or (null character) (char= character #\|) (char= character #\)))
          (return))
        (let ((node (re2-parse-atom parser)))
          (when node (push (re2-parse-repetition parser node) items)))))
    (cond ((null items) (list :empty))
          ((rest items) (list :concat (nreverse items)))
          (t (first items)))))

(defun re2-parse-count (parser)
  "Parse {n}, {n,} or {n,m} when one comes next, returning min, max and success."
  (let ((pattern (rp-pattern parser)) (start (rp-position parser)))
    (multiple-value-bind (match registers)
        (cl-ppcre:scan-to-strings "^\\{([0-9]+)(,([0-9]*))?\\}" pattern :start start)
      (when (and match (<= (length (aref registers 0)) 9)
                 (<= (length (or (aref registers 2) "")) 9))
        (let* ((min (parse-integer (aref registers 0)))
               (max (cond ((null (aref registers 1)) min)
                          ((zerop (length (aref registers 2))) nil)
                          (t (parse-integer (aref registers 2))))))
          (when (or (> min +re2-maximum-repeat+) (and max (> max +re2-maximum-repeat+)) (and max (< max min)))
            (re2-error pattern "invalid repeat count: `~A`" match))
          (setf (rp-position parser) (+ start (length match)))
          (values min max t))))))

(defun re2-parse-repetition (parser node)
  "Apply a repetition operator after NODE when one comes next, refusing a doubled one."
  (let ((character (rp-peek parser)) (min nil) (max nil) (matched nil))
    (case character
      (#\* (rp-next parser) (setf min 0 max nil matched t))
      (#\+ (rp-next parser) (setf min 1 max nil matched t))
      (#\? (rp-next parser) (setf min 0 max 1 matched t))
      (#\{ (multiple-value-setq (min max matched) (re2-parse-count parser))))
    (if (not matched)
        node
        (let ((lazy (when (eql #\? (rp-peek parser)) (rp-next parser) t)))
          (when (or (member (rp-peek parser) '(#\* #\+ #\?))
                    (and (eql #\{ (rp-peek parser))
                         (cl-ppcre:scan "^\\{[0-9]+(,[0-9]*)?\\}" (rp-pattern parser) :start (rp-position parser))))
            (re2-error (rp-pattern parser) "invalid nested repetition operator: `~A`" (rp-peek parser)))
          (list :repeat node min max (if (rp-ungreedy parser) lazy (not lazy)))))))

(defun re2-parse-flags (parser)
  "Parse the (?flags) or (?flags:...) group after its (? prefix."
  (let ((pattern (rp-pattern parser)) (negate nil)
        (fold (rp-fold parser)) (multiline (rp-multiline parser))
        (dotall (rp-dotall parser)) (ungreedy (rp-ungreedy parser)) (seen nil))
    (loop
      (let ((character (rp-next parser)))
        (case character
          (#\i (setf fold (not negate) seen t))
          (#\m (setf multiline (not negate) seen t))
          (#\s (setf dotall (not negate) seen t))
          (#\U (setf ungreedy (not negate) seen t))
          (#\- (when negate (re2-error pattern "invalid or unsupported Perl syntax: `(?-`"))
           (setf negate t seen nil))
          ((#\: #\))
           (when (and negate (not seen)) (re2-error pattern "invalid or unsupported Perl syntax: `(?-`"))
           (if (char= character #\))
               (progn
                 ;; Flags hold for the rest of the enclosing group.
                 (setf (rp-fold parser) fold (rp-multiline parser) multiline
                       (rp-dotall parser) dotall (rp-ungreedy parser) ungreedy)
                 (return nil))
               (let ((saved (list (rp-fold parser) (rp-multiline parser) (rp-dotall parser) (rp-ungreedy parser))))
                 (setf (rp-fold parser) fold (rp-multiline parser) multiline
                       (rp-dotall parser) dotall (rp-ungreedy parser) ungreedy)
                 (let ((inner (re2-parse-alternation parser)))
                   (unless (eql #\) (rp-next parser)) (re2-error pattern "missing closing )"))
                   (destructuring-bind (a b c d) saved
                     (setf (rp-fold parser) a (rp-multiline parser) b (rp-dotall parser) c (rp-ungreedy parser) d))
                   (return inner)))))
          (t (re2-error pattern "invalid or unsupported Perl syntax: `(?~A`" (or character ""))))))))

(defun re2-parse-group (parser)
  "Parse a group after its opening parenthesis."
  (let ((pattern (rp-pattern parser)))
    (flet ((capturing ()
             (let ((saved (list (rp-fold parser) (rp-multiline parser) (rp-dotall parser) (rp-ungreedy parser)))
                   (inner (re2-parse-alternation parser)))
               (unless (eql #\) (rp-next parser)) (re2-error pattern "missing closing )"))
               (destructuring-bind (a b c d) saved
                 (setf (rp-fold parser) a (rp-multiline parser) b (rp-dotall parser) c (rp-ungreedy parser) d))
               inner)))
      (if (rp-accept parser "?")
          (cond
            ((or (rp-accept parser "P<") (rp-accept parser "<"))
             (let ((end (position #\> pattern :start (rp-position parser))))
               (unless (and end (cl-ppcre:scan "^[A-Za-z0-9_]+$" (subseq pattern (rp-position parser) end)))
                 (re2-error pattern "invalid named capture"))
               (setf (rp-position parser) (1+ end))
               (capturing)))
            (t (re2-parse-flags parser)))
          (capturing)))))

(defun re2-parse-hex (parser)
  "Parse the digits of \\xHH or \\x{H...}."
  (let ((pattern (rp-pattern parser)))
    (if (rp-accept parser "{")
        (let* ((end (or (position #\} pattern :start (rp-position parser))
                        (re2-error pattern "invalid escape sequence")))
               (digits (subseq pattern (rp-position parser) end)))
          (unless (and (plusp (length digits)) (every (lambda (digit) (digit-char-p digit 16)) digits))
            (re2-error pattern "invalid escape sequence"))
          (setf (rp-position parser) (1+ end))
          (let ((code (parse-integer digits :radix 16)))
            (when (> code #x10FFFF) (re2-error pattern "invalid escape sequence"))
            code))
        (let ((first-digit (rp-next parser)) (second-digit (rp-next parser)))
          (unless (and first-digit second-digit (digit-char-p first-digit 16) (digit-char-p second-digit 16))
            (re2-error pattern "invalid escape sequence"))
          (+ (* 16 (digit-char-p first-digit 16)) (digit-char-p second-digit 16))))))

(defun re2-parse-unicode-class (parser negated)
  "Parse the rest of \\p or \\P, returning a class."
  (let* ((pattern (rp-pattern parser))
         (name (if (rp-accept parser "{")
                   (let ((end (or (position #\} pattern :start (rp-position parser))
                                  (re2-error pattern "invalid character class range"))))
                     (prog1 (subseq pattern (rp-position parser) end)
                       (setf (rp-position parser) (1+ end))))
                   (string (or (rp-next parser) (re2-error pattern "invalid character class range")))))
         (inverted (and (plusp (length name)) (char= #\^ (char name 0))))
         (name (if inverted (subseq name 1) name)))
    (make-re2-class :predicates (list (re2-unicode-predicate pattern name))
                    :negated (if inverted (not negated) negated))))

(defun re2-parse-escape-class (parser character)
  "Return the class of the shorthand escape CHARACTER, consuming any operand, or NIL."
  (case character
    (#\d (make-re2-class :ranges +re2-digit-ranges+))
    (#\D (make-re2-class :ranges +re2-digit-ranges+ :negated t))
    (#\s (make-re2-class :ranges +re2-space-ranges+))
    (#\S (make-re2-class :ranges +re2-space-ranges+ :negated t))
    (#\w (make-re2-class :ranges +re2-word-ranges+))
    (#\W (make-re2-class :ranges +re2-word-ranges+ :negated t))
    (#\p (re2-parse-unicode-class parser nil))
    (#\P (re2-parse-unicode-class parser t))))

(defun re2-parse-literal-escape (parser character)
  "Return the code point the escape CHARACTER denotes, consuming any operand."
  (let ((pattern (rp-pattern parser)))
    (case character
      (#\a 7) (#\f 12) (#\t 9) (#\n 10) (#\r 13) (#\v 11)
      (#\x (re2-parse-hex parser))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)
       (let ((next (rp-peek parser)))
         ;; A lone digit other than 0 would be a backreference, which RE2 refuses.
         (when (and (char/= character #\0) (not (and next (digit-char-p next 8))))
           (re2-error pattern "invalid escape sequence: `\\~A`" character))
         (let ((value (digit-char-p character 8)))
           (loop repeat 2
                 for digit = (rp-peek parser)
                 while (and digit (digit-char-p digit 8))
                 do (setf value (+ (* 8 value) (digit-char-p digit 8)))
                    (rp-next parser))
           value)))
      (t (when (or (null character) (alphanumericp character) (char= character #\_))
           (if (and character (> (char-code character) 127))
               (return-from re2-parse-literal-escape (char-code character))
               (re2-error pattern "invalid escape sequence: `\\~A`" (or character ""))))
         (char-code character)))))

(defun re2-merge-class (target class)
  "Add CLASS, possibly negated, to the set TARGET."
  (if (class-negated class)
      (push (let ((inner (make-re2-class :ranges (class-ranges class) :predicates (class-predicates class))))
              (lambda (code) (not (re2-class-member-p inner code))))
            (class-predicates target))
      (setf (class-ranges target) (append (class-ranges class) (class-ranges target))
            (class-predicates target) (append (class-predicates class) (class-predicates target)))))

(defun re2-parse-class (parser)
  "Parse a bracket expression after its opening bracket."
  (let* ((pattern (rp-pattern parser))
         (class (make-re2-class :negated (rp-accept parser "^")))
         (first-item t))
    (loop
      (let ((character (rp-peek parser)))
        (cond
          ((null character) (re2-error pattern "missing closing ]"))
          ((and (char= character #\]) (not first-item)) (rp-next parser) (return))
          ((rp-accept parser "[:")
           (let* ((end (or (search ":]" pattern :start2 (rp-position parser))
                           (re2-error pattern "invalid character class range")))
                  (name (subseq pattern (rp-position parser) end))
                  (inverted (and (plusp (length name)) (char= #\^ (char name 0))))
                  (ranges (cdr (assoc (if inverted (subseq name 1) name) +re2-posix-classes+ :test #'equal))))
             (unless ranges (re2-error pattern "invalid character class range: `[:~A:]`" name))
             (setf (rp-position parser) (+ end 2))
             (re2-merge-class class (make-re2-class :ranges ranges :negated inverted))))
          (t
           (let ((low (re2-parse-class-item parser class)))
             (when low
               (if (and (eql #\- (rp-peek parser)) (rp-peek parser 1) (char/= #\] (rp-peek parser 1)))
                   (progn
                     (rp-next parser)
                     (let ((high (re2-parse-class-item parser nil)))
                       (unless (and high (<= low high))
                         (re2-error pattern "invalid character class range"))
                       (push (cons low high) (class-ranges class))))
                   (push (cons low low) (class-ranges class)))))))
        (setf first-item nil)))
    class))

(defun re2-parse-class-item (parser class)
  "Parse one bracket item, returning its code, or merging a shorthand into CLASS and returning NIL."
  (let ((character (rp-next parser)))
    (if (char= character #\\)
        (let* ((escape (rp-next parser))
               (shorthand (and class escape (re2-parse-escape-class parser escape))))
          (cond (shorthand (re2-merge-class class shorthand) nil)
                ((null escape) (re2-error (rp-pattern parser) "trailing backslash at end of expression"))
                (t (re2-parse-literal-escape parser escape))))
        (char-code character))))

(defun re2-parse-atom (parser)
  "Parse one atom, or NIL for a construct that only changes flags."
  (let* ((pattern (rp-pattern parser)) (character (rp-next parser)))
    (case character
      (#\( (re2-parse-group parser))
      (#\[ (list :class (re2-parse-class parser) (rp-fold parser)))
      (#\. (list :any (rp-dotall parser)))
      (#\^ (list :assert (if (rp-multiline parser) :line-start :text-start)))
      (#\$ (list :assert (if (rp-multiline parser) :line-end :text-end)))
      ((#\* #\+ #\?) (re2-error pattern "missing argument to repetition operator: `~A`" character))
      (#\\
       (let ((escape (rp-next parser)))
         (case escape
           ((nil) (re2-error pattern "trailing backslash at end of expression"))
           (#\A (list :assert :text-start))
           (#\z (list :assert :text-end))
           (#\b (list :assert :word))
           (#\B (list :assert :not-word))
           (#\Q (let* ((start (rp-position parser))
                       (end (search "\\E" pattern :start2 start))
                       (text (subseq pattern start end)))
                  (setf (rp-position parser) (if end (+ end 2) (length pattern)))
                  (if (zerop (length text))
                      nil
                      (list :concat (map 'list (lambda (literal) (list :char (char-code literal) (rp-fold parser))) text)))))
           (t (let ((class (re2-parse-escape-class parser escape)))
                (if class
                    (list :class class (rp-fold parser))
                    (list :char (re2-parse-literal-escape parser escape) (rp-fold parser))))))))
      (t (list :char (char-code character) (rp-fold parser))))))

;;; Program: a vector of instructions, each a list whose first element is its opcode.
;;; Consuming opcodes advance one code point: (:lit codes) a literal or case-folded
;;; literal, (:dot any) the . metacharacter, (:ranges rangelist negated) a predicate-free
;;; class, (:matcher function) a class needing case folding or Unicode predicates.
;;; Control opcodes are empty width: (:split a b) (:jump a) (:assert kind) (:match).

(defstruct (re2-program (:conc-name program-)) instructions pattern matcher)

(defun re2-consuming-p (instruction)
  "Test whether INSTRUCTION advances over one code point."
  (member (first instruction) '(:lit :dot :ranges :matcher)))

(defun re2-emit-tree (node emit patch here)
  "Emit instructions for NODE through the closures EMIT, PATCH and HERE."
  (ecase (first node)
    (:empty nil)
    (:char
     (destructuring-bind (code fold) (rest node)
       (funcall emit
                (list :lit (if fold
                               (remove-duplicates
                                (list code
                                      (char-code (char-upcase (code-char code)))
                                      (char-code (char-downcase (code-char code)))))
                               (list code))))))
    (:any (funcall emit (list :dot (and (second node) t))))
    (:class
     (destructuring-bind (class fold) (rest node)
       (funcall emit
                (if (and (null (class-predicates class)) (not fold))
                    (list :ranges (class-ranges class) (class-negated class))
                    (list :matcher (re2-class-matcher class fold))))))
    (:assert (funcall emit (list :assert (second node))))
    (:concat (dolist (child (second node)) (re2-emit-tree child emit patch here)))
    (:alt
     (let ((jumps nil))
       (loop for (branch . more) on (second node)
             do (if more
                    (let ((split (funcall emit (list :split 0 0))))
                      (funcall patch split :first (funcall here))
                      (re2-emit-tree branch emit patch here)
                      (push (funcall emit (list :jump 0)) jumps)
                      (funcall patch split :second (funcall here)))
                    (re2-emit-tree branch emit patch here)))
       (dolist (jump jumps) (funcall patch jump :jump (funcall here)))))
    (:repeat
     (destructuring-bind (child min max greedy) (rest node)
       (loop repeat min do (re2-emit-tree child emit patch here))
       (flet ((split-order (split body out)
                (funcall patch split :first (if greedy body out))
                (funcall patch split :second (if greedy out body))))
         (if (null max)
             (let* ((start (funcall here)) (split (funcall emit (list :split 0 0))))
               (let ((body (funcall here)))
                 (re2-emit-tree child emit patch here)
                 (funcall emit (list :jump start))
                 (split-order split body (funcall here))))
             (let ((splits nil))
               (loop repeat (- max min)
                     do (let ((split (funcall emit (list :split 0 0))) (body (funcall here)))
                          (push (cons split body) splits)
                          (re2-emit-tree child emit patch here)))
               (let ((out (funcall here)))
                 (loop for (split . body) in splits do (split-order split body out))))))))))

(defun re2-repeat-product (node)
  "Return the largest product of nested repeat counts in NODE, which Go limits to 1000."
  (case (first node)
    (:repeat (* (max 1 (or (fourth node) (third node) 1)) (re2-repeat-product (second node))))
    ((:concat :alt) (reduce #'max (mapcar #'re2-repeat-product (second node)) :initial-value 1))
    (t 1)))

(defun re2-compile (pattern)
  "Compile the RE2 syntax PATTERN, signaling RE2-SYNTAX-ERROR when it is outside it."
  (let* ((parser (make-re2-parser :pattern pattern))
         (tree (re2-parse-alternation parser)))
    (when (rp-peek parser)
      (re2-error pattern "unexpected ): `~A`" (subseq pattern (rp-position parser))))
    (when (> (re2-repeat-product tree) +re2-maximum-repeat+)
      (re2-error pattern "invalid repeat count"))
    (let* ((code (make-array 64 :adjustable t :fill-pointer 0))
           (emit (lambda (instruction)
                   (when (>= (fill-pointer code) +re2-maximum-program+)
                     (re2-error pattern "expression too large"))
                   (vector-push-extend instruction code)
                   (1- (fill-pointer code))))
           (here (lambda () (fill-pointer code)))
           (patch (lambda (index slot target)
                    (let ((instruction (aref code index)))
                      (ecase slot
                        (:first (setf (second instruction) target))
                        (:second (setf (third instruction) target))
                        (:jump (setf (second instruction) target)))))))
      (re2-emit-tree tree emit patch here)
      (funcall emit (list :match))
      (let ((program (make-re2-program :instructions (coerce code 'simple-vector)
                                       :pattern pattern)))
        (re2-attach-matcher program)
        program))))

(defun re2-attach-matcher (program)
  "Give PROGRAM a matcher of (text start) -> (values start end): a closure compiled from the
program when it is small enough, otherwise the interpreter, which needs no code generation."
  (let ((instructions (program-instructions program)))
    (setf (program-matcher program)
          (if (<= (length instructions) +re2-maximum-compiled-program+)
              (multiple-value-bind (form matchers) (re2-transpile instructions)
                (let ((*compile-verbose* nil) (*compile-print* nil))
                  (handler-bind ((warning #'muffle-warning))
                    (funcall (compile nil form) matchers))))
              (lambda (text start) (re2-search/interpreted program text :start start))))))

;;; Execution

(defun re2-word-at-p (text position)
  "Test whether an ASCII word character is at POSITION of TEXT."
  (and (<= 0 position) (< position (length text))
       (let ((code (char-code (char text position))))
         (or (<= 48 code 57) (<= 65 code 90) (= code 95) (<= 97 code 122)))))

(defun re2-assertion-holds-p (kind text position)
  "Test an empty-width assertion at POSITION of TEXT."
  (ecase kind
    (:text-start (zerop position))
    (:text-end (= position (length text)))
    (:line-start (or (zerop position) (char= #\Newline (char text (1- position)))))
    (:line-end (or (= position (length text)) (char= #\Newline (char text position))))
    (:word (not (eq (re2-word-at-p text (1- position)) (re2-word-at-p text position))))
    (:not-word (eq (re2-word-at-p text (1- position)) (re2-word-at-p text position)))))

(defun re2-spec-matches (instruction code)
  "Test whether the consuming INSTRUCTION accepts the code point CODE."
  (ecase (first instruction)
    (:lit (member code (second instruction)))
    (:dot (or (second instruction) (/= code 10)))
    (:ranges (let ((found (loop for (low . high) in (second instruction) thereis (<= low code high))))
               (if (third instruction) (not found) found)))
    (:matcher (funcall (second instruction) code))))

(defun re2-search/interpreted (program text &key (start 0))
  "Interpret PROGRAM over TEXT, the reference against which the compiled matcher is checked."
  (let* ((code (program-instructions program))
         (size (length code))
         (visited (make-array size :element-type 'fixnum :initial-element -1))
         (current (make-array size :fill-pointer 0))
         (current-starts (make-array size :fill-pointer 0))
         (next (make-array size :fill-pointer 0))
         (next-starts (make-array size :fill-pointer 0))
         (matched-start nil) (matched-end nil)
         (length (length text)))
    (labels ((add (list starts pc thread-start position)
               ;; Priority order is depth first, so an earlier thread shadows a later one.
               (unless (= (aref visited pc) position)
                 (setf (aref visited pc) position)
                 (let ((instruction (aref code pc)))
                   (if (re2-consuming-p instruction)
                       (progn (vector-push pc list) (vector-push thread-start starts))
                       (ecase (first instruction)
                         (:jump (add list starts (second instruction) thread-start position))
                         (:split (add list starts (second instruction) thread-start position)
                          (add list starts (third instruction) thread-start position))
                         (:assert
                          (when (re2-assertion-holds-p (second instruction) text position)
                            (add list starts (1+ pc) thread-start position)))
                         (:match (vector-push pc list) (vector-push thread-start starts))))))))
      (loop for position from start to length
            do (when (and (null matched-start))
                 ;; Only a thread from the current position can start while none has matched.
                 (add current current-starts 0 position position))
               (setf (fill-pointer next) 0 (fill-pointer next-starts) 0)
               (when (zerop (length current)) (when matched-start (return)))
               (let ((code-point (and (< position length) (char-code (char text position)))))
                 (loop for index from 0 below (length current)
                       for pc = (aref current index)
                       for instruction = (aref code pc)
                       do (if (re2-consuming-p instruction)
                              (when (and code-point (re2-spec-matches instruction code-point))
                                (add next next-starts (1+ pc) (aref current-starts index) (1+ position)))
                              ;; A match cuts off every lower priority thread at this position.
                              (progn (setf matched-start (aref current-starts index) matched-end position)
                                     (return)))))
               (rotatef current next)
               (rotatef current-starts next-starts)
            finally (return nil))
      (and matched-start (values matched-start matched-end)))))

;;; Transpilation: rather than dispatch on each instruction at run time, emit a function
;;; specialized to one program. Program counters become CASE keys the compiler turns into a
;;; jump table, and literals, ranges and the . metacharacter inline to integer comparisons.
;;; The Pike thread-set machinery is unchanged, so matching stays linear in the text and the
;;; emitted program linear in the pattern -- no backtracking, no exponential state blow-up.

(defun re2-test-expression (instruction code-var matcher-index)
  "Return a form, in terms of CODE-VAR, that tests the consuming INSTRUCTION.
MATCHER-INDEX interns a fallback closure and returns its slot in the matcher vector."
  (ecase (first instruction)
    (:lit (let ((codes (second instruction)))
            (if (rest codes)
                `(or ,@(mapcar (lambda (c) `(= ,code-var ,c)) codes))
                `(= ,code-var ,(first codes)))))
    (:dot (if (second instruction) t `(/= ,code-var 10)))
    (:ranges (let ((test `(or ,@(mapcar (lambda (range) `(<= ,(car range) ,code-var ,(cdr range)))
                                        (second instruction)))))
               (if (third instruction) `(not ,test) test)))
    (:matcher `(funcall (svref matchers ,(funcall matcher-index (second instruction))) ,code-var))))

(defun re2-transpile (code)
  "Return a form and a matcher vector; the form compiles to a one-argument function that,
given that vector, returns the leftmost-first matcher (text start) -> (values start end).
The scratch arrays are declared dynamic extent, so a match allocates nothing on the heap,
and ADD threads the thread count through its return value in place of a fill pointer, which
keeps the matcher reentrant -- many threads may run the same program at once."
  (let* ((size (length code))
         (matchers nil) (matcher-count 0)
         (add-arms nil) (step-arms nil))
    (flet ((matcher-index (function)
             (prog1 matcher-count (push function matchers) (incf matcher-count))))
      (dotimes (pc size)
        (let ((instruction (aref code pc)))
          (cond
            ((re2-consuming-p instruction)
             ;; In the closure a consuming instruction is enqueued; in the step it is tested.
             (push `(,pc (setf (aref lst count) pc (aref starts count) ts) (1+ count)) add-arms)
             (push `(,pc (when (and (>= code 0) ,(re2-test-expression instruction 'code #'matcher-index))
                           (setf next-count (add next next-starts next-count ,(1+ pc) ts (1+ pos)))))
                   step-arms))
            (t (ecase (first instruction)
                 (:jump (push `(,pc (add lst starts count ,(second instruction) ts pos)) add-arms))
                 (:split (push `(,pc (add lst starts (add lst starts count ,(second instruction) ts pos)
                                          ,(third instruction) ts pos))
                               add-arms))
                 (:assert (push `(,pc (if (re2-assertion-holds-p ,(second instruction) text pos)
                                          (add lst starts count ,(1+ pc) ts pos)
                                          count))
                                add-arms))
                 (:match (push `(,pc (setf (aref lst count) pc (aref starts count) ts) (1+ count)) add-arms)
                         (push `(,pc (setf matched-start ts matched-end pos matched t)
                                     (return-from stepped))
                               step-arms)))))))
      (values
       `(lambda (matchers)
          (declare (ignorable matchers))
          (lambda (text start)
            (declare (optimize (speed 3) (safety 0) (debug 0))
                     #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note style-warning)
                     (type simple-string text) (type fixnum start))
            (let* ((length (length text))
                   (visited (make-array ,size :element-type 'fixnum :initial-element -1))
                   (current (make-array ,size :element-type 'fixnum))
                   (current-starts (make-array ,size :element-type 'fixnum))
                   (next (make-array ,size :element-type 'fixnum))
                   (next-starts (make-array ,size :element-type 'fixnum))
                   (current-count 0) (next-count 0)
                   (matched-start -1) (matched-end -1) (matched nil))
              (declare (type fixnum length current-count next-count matched-start matched-end)
                       (type (simple-array fixnum (*)) visited current current-starts next next-starts)
                       (dynamic-extent visited current current-starts next next-starts))
              (labels ((add (lst starts count pc ts pos)
                         (declare (type (simple-array fixnum (*)) lst starts)
                                  (type fixnum count pc ts pos))
                         (if (= (aref visited pc) pos)
                             count
                             (progn (setf (aref visited pc) pos)
                                    (the fixnum (case pc ,@(nreverse add-arms) (t count)))))))
                (declare (ignorable #'add))
                (loop for pos of-type fixnum from start to length do
                  ;; Only a thread from the current position may start while none has matched.
                  (unless matched (setf current-count (add current current-starts current-count 0 pos pos)))
                  (setf next-count 0)
                  (when (and (zerop current-count) matched) (return))
                  (let ((code (if (< pos length) (char-code (schar text pos)) -1)))
                    (declare (type fixnum code))
                    (block stepped
                      (dotimes (index current-count)
                        (let ((pc (aref current index)) (ts (aref current-starts index)))
                          (declare (type fixnum pc ts))
                          (case pc ,@(nreverse step-arms))))))
                  (rotatef current next)
                  (rotatef current-starts next-starts)
                  (rotatef current-count next-count))
                (if (>= matched-start 0) (values matched-start matched-end) nil)))))
       (coerce (nreverse matchers) 'simple-vector)))))

(defun re2-search (program text &key (start 0))
  "Find PROGRAM's leftmost-first match in TEXT, returning its start and end, or NIL."
  (funcall (program-matcher program)
           (if (typep text 'simple-string) text (coerce text 'simple-string))
           start))

(defun re2-match-p (program text)
  "Test whether PROGRAM matches anywhere in TEXT."
  (and (re2-search program text) t))

(defvar *re2-compiled* (make-hash-table :test #'equal . #+sbcl (:synchronized t) #-sbcl ())
  "Patterns already compiled, as validation reads the same few over and over.")

(defun re2-compiled (pattern)
  "Return PATTERN's program, remembering it."
  (or (gethash pattern *re2-compiled*)
      (progn
        (when (> (hash-table-count *re2-compiled*) 4096) (clrhash *re2-compiled*))
        (setf (gethash pattern *re2-compiled*) (re2-compile pattern)))))
