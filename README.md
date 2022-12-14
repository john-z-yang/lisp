# Lisp Interpreter &middot; [![build](https://github.com/john-z-yang/lisp/actions/workflows/ci.yml/badge.svg?branch=master)](https://github.com/john-z-yang/lisp/actions/workflows/ci.yml) [![ASan](https://github.com/john-z-yang/lisp/actions/workflows/asan.yml/badge.svg?branch=master)](https://github.com/john-z-yang/lisp/actions/workflows/asan.yml)

Lisp interpreter written in [continuation passing style](https://en.wikipedia.org/wiki/Continuation-passing_style) and [trampolined through thunk-returning functions](https://en.wikipedia.org/wiki/Trampoline_(computing)#:~:text=As%20used%20in,programming%20languages.), allowing the interpreter to maintain its own call stack on the heap and execute recursive calls of arbitrary depth.

This project started out as a solution to leetcode problem [736](https://leetcode.com/problems/parse-lisp-expression/) and was further inspired by Peter Norvig's blog where he [creates a lisp interpreter in 90 lines of python code](http://www.norvig.com/lispy.html).

<p align="center">
    <img src="https://raw.githubusercontent.com/john-z-yang/lisp/master/docs/assets/fib_seq_gen.gif" width="100%">
</p>


## Getting Started

These instructions will give you a copy of the interpreter up and running on
your local machine.


### Prerequisites

- [g++](https://gcc.gnu.org/)
- [make](https://www.gnu.org/software/make/)
- [GNU Readline Library](https://tiswww.case.edu/php/chet/readline/rltop.html)


### Installing

Clone this repository.

```bash
git clone https://github.com/john-z-yang/lisp
```

Build the project.
```bash
make
```

The executable (`lisp`) will be in the `bin` directory.

Execute without argument to run in interactive mode.
```console
foo@bar:~$ bin/lisp
Lisp (C++ std: 202002, Nov 30 2022, 13:49:06)
Type "(quit)" or trigger EOF to exit the session.
lisp> (quote (Hello World!))
(Hello World!)
lisp> (quit)
Farewell.
foo@bar:~$ 
```

Supply the file name of a lisp script as the argument to run them.
```console
foo@bar:~$ echo '(display (quote (Hello World!)))' > hello_world.lisp 
foo@bar:~$ bin/lisp hello_world.lisp
(Hello World!)
foo@bar:~$ 
```
_Happy hacking!_


## Special Forms

### Symbols

| Syntax                | Description                                                                                    |
| --------------------- | ---------------------------------------------------------------------------------------------- |
| *sym*                 | Return the value that *sym* is bound to in the closest lexical scope.                          |
| *(`define` sym expr)* | Evaluate *expr*, bind the result to *sym* in current lexical scope.                            |
| *(`set!` sym expr)*   | Evaluate *expr*, find the closest lexical scope where *sym* is bound, re-bind result to *sym*. |


### Macros

| Syntax                                | Description                                                                                                                                                                                                                                                                                       |
| ------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| *(`quote` expr)*                      | Return *expr* without evaluating it.                                                                                                                                                                                                                                                              |
| *(`quasiquote` expr)*                 | Increase the level of quasiquotation of *expr* by 1.                                                                                                                                                                                                                                              |
| *(`unquote` expr)*                    | Decrease the level of quasiquotation of *expr* by 1.<br>If the level of quasiquotation reaches 0, evaluate *expr*; otherwise, *expr* is not evaluated.                                                                                                                                            |
| *(`unquote-splicing` expr)*           | Decrease the level of quasiquotation of *expr* by 1.<br>If the level of quasiquotation reaches 0, evaluate *expr* and splice the resulting as multiple values; otherwise, *expr* is not evaluated. (**Note:** if in an escaping position, the result of evaluating *expresson* must be a *list*.) |
| *(`define-macro` sym `(lambda ...)`)* | Evaluate the `lambda` expression as a *macro*, bind the *macro* to *sym* the in current lexical scope.                                                                                                                                                                                            |
| *(macro expr\*)*                      | Invoke *macro* with the *exprs* as arguments, evaluate the result.                                                                                                                                                                                                                                |


### Procedures

| Syntax                    | Description                                                                                                                                               |
| ------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------- |
| *(`lambda` (sym\*) expr)* | Return a *procedure* that accepts *n* arguments. When invoked, bind each argument to a *sym*, evalulate *expr* and return the result.                     |
| *(`lambda` sym expr)*     | Return a *procedure* that accepts arbitary number of arguments. When invoked, bind parameters to *sym* as a list, evalulate *expr* and return the result. |
| *(procedure expr\*)*      | Evaluates each *expr*, invoke *procedure* with the results as arguments.                                                                                  |


### Logic

| Syntax                                                      | Description                                                                                                                                                                |
| ----------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| *(`if` expr<sub>1</sub> expr<sub>2</sub> expr<sub>3</sub>)* | Evaluate *expr<sub>1</sub>*, if the result is *truthy*, evalulate *expr<sub>2</sub>* and return the result; otherwise, evalulate *expr<sub>3</sub>* and return the result. |
| *(`and` expr\*)*                                            | Evaluate *expr* from left to right. When one of them evaluates to `#f`, immediately return `#f`. If all of them are *truthy*, return the result of the last *expr*.        |
| *(`or` expr\*)*                                             | Evaluate *expr* from left to right. When one of them evaluates to a *truthy* value, immediately return the result. If all of them are `#f`, return `#f`.                   |


### Syntactic Sugar

| Syntax        | Equivalence                 |
| ------------- | --------------------------- |
| `'`*expr*     | *(`quote` expr)*            |
| `` ` ``*expr* | *(`quasiquote` expr)*       |
| `,`*expr*     | *(`unquote` expr)*          |
| `,@`*expr*    | *(`unquote-splicing` expr)* |


## Built-In Procedures

### Symbols

| Name     | Arguments | Description                                                |
| -------- | --------- | ---------------------------------------------------------- |
| `sym?`   | `arg`     | Return `#t` if `arg` is a *symbol*; otherwise return `#f`. |
| `gensym` |           | Return a unique, never defined *symbol*.                   |



### Intergers

| Name   | Arguments    | Description                                              |
| ------ | ------------ | -------------------------------------------------------- |
| `num?` | `arg`        | Return `#t` if `arg` is *number*; otherwise return `#f`. |
| `=`    | `lhs`, `rhs` | Return `#t` if `lhs` = `rhs`; otherwise return `#f`.     |
| `>`    | `lhs`, `rhs` | Return `#t` if `lhs` > `rhs`; otherwise return `#f`.     |
| `>=`   | `lhs`, `rhs` | Return `#t` if `lhs` ??? `rhs`; otherwise return `#f`.     |
| `<`    | `lhs`, `rhs` | Return `#t` if `lhs` < `rhs`; otherwise return `#f`.     |
| `<=`   | `lhs`, `rhs` | Return `#t` if `lhs` ??? `rhs`; otherwise return `#f`.     |
| `abs`  | `arg`        | Return \|`arg`\|.                                        |
| `+`    | `lhs`, `rhs` | Return `lhs` + `rhs`.                                    |
| `-`    | `lhs`, `rhs` | Return `lhs` - `rhs`.                                    |
| `*`    | `lhs`, `rhs` | Return `lhs` ?? `rhs`.                                    |
| `/`    | `lhs`, `rhs` | Return `lhs` ?? `rhs`.                                    |
| `%`    | `lhs`, `rhs` | Return `lhs` % `rhs`.                                    |


### Strings
| Name      | Arguments           | Description                                                                                                                                |
| --------- | ------------------- | ------------------------------------------------------------------------------------------------------------------------------------------ |
| `str?`    | `arg`               | Return `#t` if `arg` is *string*; otherwise return `#f`.                                                                                   |
| `str-len` | `arg`               | Return the length of `arg`.                                                                                                                |
| `str-sub` | `str`, `pos`, `len` | Return a new string that starts at character position `pos` and spans `len` characters (or until the end of `str`, whichever comes first). |
| `str-con` | `lhs`, `rhs`        | Return a new string that is the concatenation of `lhs` `rhs`.                                                                              |
| `->str`   | `arg`               | Return `arg` in string representation.                                                                                                     |


### Pairs and Lists

| Name      | Arguments                                 | Description                                                                                                                                |
| --------- | ----------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------ |
| `null?`   | `arg`                                     | Return `#t` if `arg` is `(quote ())`; otherwise return `#f`.                                                                               |
| `cons?`   | `arg`                                     | Return `#t` if `arg` is a `cons` pair; otherwise return `#f`.                                                                              |
| `cons`    | `lhs`, `rhs`                              | Return a pair where first element is `lhs` and second element is `rhs`.                                                                    |
| `car`     | `pair`                                    | Return the first element of `pair`.                                                                                                        |
| `cdr`     | `pair`                                    | Return the second element of `pair`.                                                                                                       |
| `list`    | `arg`<sub>1</sub>, ..., `arg`<sub>n</sub> | Return `arg`<sub>1</sub>, ..., `arg`<sub>n</sub> as a list (i.e. `(cons arg`<sub>1</sub> ... `(cons arg`<sub>n</sub>` (quote ()))`...`))`. |
| `first`   | `list`                                    | Return the first element if `list` is not empty, otherwise `(quote ())`. (**Note:** `list` must be a properly formed list.)                |
| `last`    | `list`                                    | Return the last element if `list` is not empty, otherwise `(quote ())`. (**Note:** `list` must be a properly formed list.)                 |
| `foldl`   | `fn`, `cur`, `list`                       | For each element `e` in `list`, update `cur` as `(fn e cur)`, return `cur` when no element remains.                                        |
| `map`     | `fn`, `list`                              | Return a new list where `fn` is applied to each element of `list`.                                                                         |
| `reverse` | `list`                                    | Return `list` but with its order of elements reversed.                                                                                     |


### Predicates

| Name    | Arguments    | Description                                                                    |
| ------- | ------------ | ------------------------------------------------------------------------------ |
| `proc?` | `arg`        | Return `#t` if `arg` is *procedure*; otherwise return `#f`.                    |
| `eq?`   | `lhs`, `rhs` | Return `#t` if `lhs` and `rhs` are equivalent in value; otherwise return `#f`. |


### Miscellaneous

| Name      | Arguments                                 | Description                                       |
| --------- | ----------------------------------------- | ------------------------------------------------- |
| `quit`    |                                           | Quit the session.                                 |
| `load`    | `path`                                    | Execute the file at `path` in the current context |
| `display` | `arg`                                     | Print `arg` to `std::cout`, returns `(quote ())`. |
| `not`     | `arg`                                     | Return ??`arg`.                                    |
| `progn`   | `arg`<sub>1</sub>, ..., `arg`<sub>n</sub> | Return `arg`<sub>n</sub>.                         |


## Running the tests

```bash
make test
```
Tests are defined in the `tests` directory. Each test suite is a pair of lisp code (`.lisp`) and its expected output (`.expect`). The `test` command runs the `.lisp` file and generates a `.out` file by redirecting `stdout`. Finally, it `diff`s the `.out` file against the `.expect` file.


### Sample test suite (`combine`)

Lisp code (`combine.lisp`)
```lisp
(define combine
  (lambda (f)
    (lambda (x y) 
      (if (null? x) (quote ())
        (f (list (car x) (car y))
           ((combine f) (cdr x) (cdr y)))))))

(define zip (combine cons))

(display (zip (list 1 2 3 4) (list 5 6 7 8)))
```

When executed, it should behave like this
```console
foo@bar:~$ bin/lisp combine.lisp
((1 5) (2 6) (3 7) (4 8))
foo@bar:~$ 
```

So we create the `.expect` file for expected output (`tests/combine.expect`).
```lisp
((1 5) (2 6) (3 7) (4 8))
```
Add the new test to the `TESTS` variable in `makefile`.
```make
TESTS = $(TESTDIR)/combine # Along with other tests.
```
Tests will be executed from `make test`.
```make
test: $(TESTS)

$(TESTDIR)/%: $(TESTDIR)/%.lisp $(TESTDIR)/%.expect $(OUTDIR)/lisp
	$(OUTDIR)/lisp $@.lisp >> $@.out 2>&1
	diff $@.expect $@.out
	rm $@.out
```


### Style test
Use [`clang-format`](https://clang.llvm.org/docs/ClangFormat.html) for formatting.


## Authors
  - **John Yang** - [john-z-yang](https://github.com/john-z-yang)
  - **Chet Ramey** - *Current maintainer for the GNU Readline library* - [Chet Ramey](https://tiswww.case.edu/php/chet/)
  - **Billie Thompson** - *README Template* -
    [PurpleBooth](https://github.com/PurpleBooth)

See also the list of
[contributors](https://github.com/john-z-yang/lisp/contributors)
who participated in this project.


## Acknowledgments
  - [(How to Write a (Lisp) Interpreter (in Python))](http://www.norvig.com/lispy.html) by [Peter Norvig](https://norvig.com/)
  - Special thanks to [Sophie](https://github.com/yqstan) for pointing out that parameter eval order is different across C++ compiler implementations.
  - [Stackoverflow: What is a trampoline function?](https://stackoverflow.com/questions/189725/what-is-a-trampoline-function) Question by [Benoit](https://stackoverflow.com/users/10703/benoit), Solutions by toyvo (no longer active) and [Piotr Kukielka](https://stackoverflow.com/users/704905/piotr-kukielka).
  - [By example: Continuation-passing style in JavaScript](https://matt.might.net/articles/by-example-continuation-passing-style/) by [Matt Might](https://matt.might.net/)
  - [How to compile with continuations](https://matt.might.net/articles/cps-conversion/) by [Matt Might](https://matt.might.net/)
<br>

---
<p align=center>
<img src="https://imgs.xkcd.com/comics/lisp_cycles.png">
</p>
