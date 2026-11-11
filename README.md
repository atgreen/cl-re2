# cl-re2

`cl-re2` is a Common Lisp regular-expression engine for RE2 syntax. It
compiles a pattern to a Pike virtual machine program and matches in time
linear in the input. A small program is transpiled to a native closure and
compiled; a large one runs on the interpreter. Both paths are linear in the
text, so cl-re2 stays safe for patterns and text supplied by untrusted users.

## Usage

```lisp
(let ((program (cl-re2:re2-compile "(?i)hello,? [[:alpha:]]+")))
  (multiple-value-bind (start end)
      (cl-re2:re2-search program "Say HELLO world")
    (subseq "Say HELLO world" start end)))
```

`re2-search` returns the start and end offsets of the leftmost-first match.
`re2-match-p` returns a boolean. Unsupported constructs such as backreferences
and lookaround signal `re2-syntax-error` during compilation.

## Why not a wrapper around cl-ppcre?

[cl-ppcre](https://edicl.github.io/cl-ppcre/) is an excellent engine, and for
patterns you write yourself it is the right tool. cl-re2 exists for a different
job: the pattern itself can come from an attacker, not only the text.

cl-ppcre, like PCRE, Perl and Python's `re`, is a backtracking matcher. On some
pattern/text pairs a backtracker runs in time exponential in the input. An
attacker who controls the pattern, the text, or both can use this to hang the
process. The failure is well known as catastrophic backtracking, or a regular
expression denial-of-service (ReDoS) attack.

RE2 was built to remove this whole class of risk. It matches with an automaton
in time linear in the text, and it refuses the features — backreferences and
lookaround — that force a backtracker. Go's `regexp` package shares this design
and states the linear-time guarantee outright. cl-re2 follows the same rules: a
pattern expands to a program whose size is linear in the pattern, and the Pike
VM runs it in time linear in the text. Neither a hostile pattern nor hostile
text can make it blow up.

This matters wherever a service compiles patterns it did not write: schema and
config validation, rule engines, search filters, and any multi-tenant system
that lets a user supply a regular expression. Validating those patterns with
cl-ppcre would reintroduce the exact denial of service RE2 was chosen to
prevent. It would also drift from RE2 on details that change which inputs are
accepted — for example `$` (end of text in RE2, but also before a trailing
newline in cl-ppcre) and the ASCII meaning of `\d` and `\w`. cl-re2 instead
parses the RE2 dialect, rejects what RE2 rejects, and matches with RE2
semantics.

### References

- Russ Cox, [*Regular Expression Matching Can Be Simple And Fast*](https://swtch.com/~rsc/regexp/regexp1.html) — why backtrackers go exponential and automata do not.
- Go [`regexp`](https://pkg.go.dev/regexp) package — the linear-time guarantee and RE2 lineage.
- RE2 [syntax reference](https://github.com/google/re2/wiki/Syntax) — the dialect cl-re2 targets.
- OWASP, [*Regular expression Denial of Service (ReDoS)*](https://owasp.org/www-community/attacks/Regular_expression_Denial_of_Service_-_ReDoS) — the attack this design removes.
- CEL [specification](https://github.com/google/cel-spec/blob/master/doc/langdef.md) — a validation language whose `matches()` function uses RE2.

## Compatibility

The test suite covers alternation priority, greedy and lazy repetition,
anchors, inline flags, ASCII and Unicode classes, quoting, counted repetition,
syntax rejection, and an adversarial non-match that would be pathological for
a backtracking engine.

## Testing

```sh
/usr/bin/sbcl --noinform --non-interactive \
  --eval '(require :asdf)' \
  --eval '(asdf:load-asd (truename "cl-re2.asd"))' \
  --eval '(asdf:test-system :cl-re2)'
```

## Author

Anthony Green <green@moxielogic.com>

## License

MIT. See `LICENSE`.
