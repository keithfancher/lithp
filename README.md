# lithp

[Write myself a
Scheme](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours) in
48 ~~hours~~ days.

## Usage

```
# Build it!
$ stack build

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

Don't try nothin' fancy -- it's still a work in progress.
