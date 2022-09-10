# Lisp Interpreter &middot; [![build](https://github.com/john-z-yang/lisp/actions/workflows/ci.yml/badge.svg?branch=master)](https://github.com/john-z-yang/lisp/actions/workflows/ci.yml)

Interpreter for lisp written in C++. This project started out as a solution to leetcode problem [736](https://leetcode.com/problems/parse-lisp-expression/) and was further inspired by Peter Norvig's blog where he [creates a lisp interpreter in 90 lines of python code](http://www.norvig.com/lispy.html).

<p align="center">
    <img src="https://raw.githubusercontent.com/john-z-yang/lisp/master/docs/assets/fib_seq_gen.gif" width="100%">
</p>


## Getting Started

These instructions will give you a copy of the interpreter up and running on
your local machine.


### Prerequisites

- [g++](https://gcc.gnu.org/)
- [make](https://www.gnu.org/software/make/)


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

| Syntax                         | Description                                                                                                |
| ------------------------------ | ---------------------------------------------------------------------------------------------------------- |
| *symbol*                       | Return the value that *symbol* is bound to in the closest lexical scope.                                   |
| *(`define` symbol expression)* | Evaluate *expression*, bind the result to *symbol* in current lexical scope.                               |
| *(`set!` symbol expression)*   | Evaluate *expression*, find the closest lexical scope where *symbol* is bound, re-bind result to *symbol*. |


### Macros

| Syntax                                   | Description                                                                                                                                                                                                                                                                                                           |
| ---------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| *(`quote` expression)*                   | Return *expression* without evaluating it.                                                                                                                                                                                                                                                                            |
| *(`quasiquote` expression)*              | Increase the level of quasiquotation of *expression* by 1.                                                                                                                                                                                                                                                            |
| *(`unquote` expression)*                 | Decrease the level of quasiquotation of *expression* by 1.<br>If the level of quasiquotation reaches 0, evaluate *expression*; otherwise, the expression is not evaluated.                                                                                                                                            |
| *(`unquote-splicing` expression)*        | Decrease the level of quasiquotation of *expression* by 1.<br>If the level of quasiquotation reaches 0, evaluate *expression* and splice the resulting as multiple values; otherwise, the expression is not evaluated. (**Note:** if in an escaping position, the result of evaluating *expresson* must be a *list*.) |
| *(`define-macro` symbol `(lambda ...)`)* | Evaluate the `lambda` expression as a *macro*, bind the *macro* to *symbol* the in current lexical scope.                                                                                                                                                                                                             |
| *(macro expression\*)*                   | Invoke *macro* with the *expressions* as arguments, evaluate the result.                                                                                                                                                                                                                                              |


### Procedures

| Syntax                             | Description                                                                                                                                                        |
| ---------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| *(`lambda` (symbol\*) expression)* | Return a *procedure* that accepts *n* arguments. When invoked, bind each argument to a *symbol*, evalulate *expression* and return the result.                     |
| *(`lambda` symbol expression)*     | Return a *procedure* that accepts arbitary number of arguments. When invoked, bind parameters to *symbol* as a list, evalulate *expression* and return the result. |
| *(procedure expression\*)*         | Evaluates each *expression*, invoke *procedure* with the results as arguments.                                                                                     |


### Logic

| Syntax                                                                        | Description                                                                                                                                                                                  |
| ----------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| *(`if` expression<sub>1</sub> expression<sub>2</sub> expression<sub>3</sub>)* | Evaluate *expression<sub>1</sub>*, if the result is *truthy*, evalulate *expression<sub>2</sub>* and return the result; otherwise, evalulate *expression<sub>3</sub>* and return the result. |
| *(`and` expression\*)*                                                        | Evaluate *expression* from left to right. When one of them evaluates to `#f`, immediately return `#f`. If all of them are *truthy*, return the result of the last expression.                |
| *(`or` expression\*)*                                                         | Evaluate *expression* from left to right. When one of them evaluates to a *truthy* value, immediately return the result. If all of them are `#f`, return `#f`.                               |


### Syntactic Sugar

| Syntax              | Equivalence                       |
| ------------------- | --------------------------------- |
| `'`*expression*     | *(`quote` expression)*            |
| `` ` ``*expression* | *(`quasiquote` expression)*       |
| `,`*expression*     | *(`unquote` expression)*          |
| `,@`*expression*    | *(`unquote-splicing` expression)* |


## Built-In Procedures

### Arithmetic

| Name  | Arguments    | Description           |
| ----- | ------------ | --------------------- |
| `abs` | `arg`        | Return \|`arg`\|.     |
| `+`   | `lhs`, `rhs` | Return `lhs` + `rhs`. |
| `-`   | `lhs`, `rhs` | Return `lhs` - `rhs`. |
| `*`   | `lhs`, `rhs` | Return `lhs` × `rhs`. |
| `/`   | `lhs`, `rhs` | Return `lhs` ÷ `rhs`. |
| `%`   | `lhs`, `rhs` | Return `lhs` % `rhs`. |


### Predicates

| Name      | Arguments    | Description                                                                    |
| --------- | ------------ | ------------------------------------------------------------------------------ |
| `=`       | `lhs`, `rhs` | Return `#t` if `lhs` = `rhs`; otherwise return `#f`.                           |
| `>`       | `lhs`, `rhs` | Return `#t` if `lhs` > `rhs`; otherwise return `#f`.                           |
| `>=`      | `lhs`, `rhs` | Return `#t` if `lhs` ≥ `rhs`; otherwise return `#f`.                           |
| `<`       | `lhs`, `rhs` | Return `#t` if `lhs` < `rhs`; otherwise return `#f`.                           |
| `<=`      | `lhs`, `rhs` | Return `#t` if `lhs` ≤ `rhs`; otherwise return `#f`.                           |
| `null?`   | `arg`        | Return `#t` if `arg` is `(quote ())`; otherwise return `#f`.                   |
| `cons?`   | `arg`        | Return `#t` if `arg` is a `cons` pair; otherwise return `#f`.                  |
| `sym?`    | `arg`        | Return `#t` if `arg` is *symbol*; otherwise return `#f`.                       |
| `string?` | `arg`        | Return `#t` if `arg` is *string*; otherwise return `#f`.                       |
| `num?`    | `arg`        | Return `#t` if `arg` is *number*; otherwise return `#f`.                       |
| `proc?`   | `arg`        | Return `#t` if `arg` is *procedure*; otherwise return `#f`.                    |
| `eq?`     | `lhs`, `rhs` | Return `#t` if `lhs` and `rhs` are equivalent in value; otherwise return `#f`. |


### Pairs and Lists

| Name      | Arguments                                 | Description                                                                                                                                |
| --------- | ----------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------ |
| `cons`    | `lhs`, `rhs`                              | Return a pair where first element is `lhs` and second element is `rhs`.                                                                    |
| `car`     | `pair`                                    | Return the first element of `pair`.                                                                                                        |
| `cdr`     | `pair`                                    | Return the second element of `pair`.                                                                                                       |
| `list`    | `arg`<sub>1</sub>, ..., `arg`<sub>n</sub> | Return `arg`<sub>1</sub>, ..., `arg`<sub>n</sub> as a list (i.e. `(cons arg`<sub>1</sub> ... `(cons arg`<sub>n</sub>` (quote ()))`...`))`. |
| `first`   | `list`                                    | Return the first element if `list` is not empty, otherwise `(quote ())`. (**Note:** `list` must be a properly formed list.)                |
| `last`    | `list`                                    | Return the last element if `list` is not empty, otherwise `(quote ())`. (**Note:** `list` must be a properly formed list.)                 |
| `foldl`   | `fn`, `cur`, `list`                       | For each element `e` in `list`, update `cur` as `(fn e cur)`, return `cur` when no element remains.                                        |
| `map`     | `fn`, `list`                              | Return a new list where `fn` is applied to each element of `list`.                                                                         |
| `reverse` | `list`                                    | Return `list` but with its order of elements reversed.                                                                                     |


### Miscellaneous

| Name      | Arguments                                 | Description                                       |
| --------- | ----------------------------------------- | ------------------------------------------------- |
| `quit`    |                                           | Quit the session.                                 |
| `display` | `arg`                                     | Print `arg` to `std::cout`, returns `(quote ())`. |
| `not`     | `arg`                                     | Return ¬`arg`.                                    |
| `progn`   | `arg`<sub>1</sub>, ..., `arg`<sub>n</sub> | Return `arg`<sub>n</sub>.                         |
| `gensym`  |                                           | Return a unique, never defined *symbol*.          |


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

  - **Billie Thompson** - *Provided README Template* -
    [PurpleBooth](https://github.com/PurpleBooth)

See also the list of
[contributors](https://github.com/john-z-yang/lisp/contributors)
who participated in this project.


## Acknowledgments
  - [(How to Write a (Lisp) Interpreter (in Python))](http://www.norvig.com/lispy.html) by Peter Norvig
  - Special thanks to [Sophie](https://github.com/yqstan) for pointing out that parameter eval order is different across C++ compiler implementations.
