# TOPLIS

TOPLIS is a TOy Prolog LISp implementation.

This is an exercise in minimal lisp implementation and should not be used for anything
other than learning purposes!

## Running

Run with SWI-Prolog and invoke the goal `repl.` to start a REPL session.

```
% swipl toplis.pl
...
?- repl.
|: (def plus (a b) (+ a b))
=> plus
|: (plus (* 4 10) 2)
=> 42
```
