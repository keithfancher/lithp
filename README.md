# lithp

[Write myself a
Scheme](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours) in
48 ~~hours~~ days.

## Usage

First, [build it](https://docs.haskellstack.org/en/stable/#how-to-install-stack):

```
$ stack build
```

Pass it an expression on the command line:

```
# Do some math:
$ stack exec lithp "(- (+ 4 6 3) 3 5 2)"
3

# Play with lists:
$ stack exec lithp "(car (cdr '(a simple test)))"
simple
$ stack exec lithp "(cons '(this is) '())"
((this is))

# Answer the age-old questions:
$ stack exec lithp "(eqv? 1 3)"
#f
$ stack exec lithp "(eqv? 'atom 'atom)"
#t
```

Or run without arguments to play with the REPL:

```
$ stack exec lithp
Lithp>>> (eqv? '(6 6 6) '(6 6 6))
#t
Lithp>>> (eqv? 3 "3")
#f
Lithp>>> (equal? 3 "3")
#t
Lithp>>> (define (f x y) (+ x y))
(lambda ("x" "y") ...)
Lithp>>> (f 40 2)
42
Lithp>>> (define (factorial x) (if (= x 1) 1 (* x (factorial (- x 1)))))
(lambda ("x") ...)
Lithp>>> (factorial 4)
24
Lithp>>> (- (+ (factorial 4) (factorial 4)) 6)
42
Lithp>>> quit
```

Check out [the available primitives](https://github.com/keithfancher/lithp/blob/master/src/Primitives.hs)
to get a better idea of what you can do so far. But don't try nothin' fancy --
it's still a work in progress.
