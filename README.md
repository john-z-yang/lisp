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
foo@bar:~$ out/lisp
lisp> (quote (Hello World!))
(Hello World!)
lisp> (quit)
Farewell.
foo@bar:~$ 
```

Supply the file name of a lisp script as the argument to run them.
```console
foo@bar:~$ echo '(display (quote (Hello World!)))' > hello_world.lsp 
foo@bar:~$ out/lisp hello_world.lsp
(Hello World!)
foo@bar:~$ 
```

_Happy hacking!_
## Supported Syntax

| Syntax                                                                        | Description                                                                                                                                                                                                 |
| ----------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| *symbol*                                                                      | Return the value that *sybmol* is bound to in the closest lexical scope.                                                                                                                                    |
| *number*                                                                      | Return the *number*.                                                                                                                                                                                        |
| *(`define` symbol expression)*                                                | Evaluate *expression*, bind *symbol* to the result in current lexical scope.                                                                                                                                |
| *(`set!` symbol expression)*                                                  | Evaluate *expression*, find the closest lexical scope where *symbol* is bound, re-bind *symbol* to the result.                                                                                              |
| *(`quote` expression)*                                                        | Return *expression* without evaluating it.                                                                                                                                                                  |
| *(`if` expression<sub>1</sub> expression<sub>2</sub> expression<sub>3</sub>)* | Evaluate *expression<sub>1</sub>*, if the result is *truthy* (aka not `#t`), evalulate *expression<sub>2</sub>* and return the result; otherwise, evalulate *expression<sub>3</sub>* and return the result. |
| *(`lambda` (symbol<sub>1</sub> ... symbol<sub>n</sub>) expression)*           | Return a *procedure* that accepts *n* arguments. When invoked, bind parameters to *Symbol<sub>1</sub> ... Symbol<sub>n</sub>*, evalulate *expression* and return the result.                                |
| *(`lambda` symbol expression)*                                                | Return a *procedure* that accepts arbitary number of arguments. When invoked, bind parameters to *Symbol* as a list, evalulate *expression* and return the result.                                          |
| *(procedure expression<sub>1</sub> ... expression<sub>n</sub>)*               | Evaluates *expression<sub>1</sub> ... expression<sub>n</sub>*, invoke *procedure* with the results.                                                                                                         |

## Built-In Functions and Operators
| Name      | Arguments    | Description                                                             |
| --------- | ------------ | ----------------------------------------------------------------------- |
| `quit`    |              | Quit the session                                                        |
| `display` | `arg`        | Print `arg` to `std::cout`, returns `(quote ())`                        |
| `abs`     | `arg`        | Returns \|`arg`\|                                                       |
| `+`       | `lhs`, `rhs` | Returns `lhs` + `rhs`                                                   |
| `-`       | `lhs`, `rhs` | Returns `lhs` - `rhs`                                                   |
| `*`       | `lhs`, `rhs` | Returns `lhs` × `rhs`                                                   |
| `/`       | `lhs`, `rhs` | Returns `lhs` ÷ `rhs`                                                   |
| `%`       | `lhs`, `rhs` | Returns `lhs` % `rhs`                                                   |
| `=`       | `lhs`, `rhs` | Returns `lhs` = `rhs`                                                   |
| `>`       | `lhs`, `rhs` | Returns `lhs` > `rhs`                                                   |
| `>=`      | `lhs`, `rhs` | Returns `lhs` ≥ `rhs`                                                   |
| `<`       | `lhs`, `rhs` | Returns `lhs` < `rhs`                                                   |
| `<=`      | `lhs`, `rhs` | Returns `lhs` ≤ `rhs`                                                   |
| `not`     | `arg`        | Returns ¬`arg`                                                          |
| `and`     | `lhs`, `rhs` | Returns `lhs` ∧ `rhs`                                                   |
| `or`      | `lhs`, `rhs` | Returns `lhs` ∨ `rhs`                                                   |
| `cons`    | `lhs`, `rhs` | Returns a pair where first element is `lhs` and second element is `rhs` |
| `car`     | `pair`       | Returns the first element of `pair`                                     |
| `cdr`     | `pair`       | Returns the second element of `pair`                                    |
| `null?`   | `arg`        | Returns `#t` if `arg` is `(quote ())`; otherwise `#f`                   |
| `cons?`   | `arg`        | Returns `#t` if `arg` is a `cons` pair; otherwise `#f`                  |
| `sym?`    | `arg`        | Returns `#t` if `arg` is *symbol*; otherwise `#f`                       |
| `num?`    | `arg`        | Returns `#t` if `arg` is *number*; otherwise `#f`                       |
| `proc?`   | `arg`        | Returns `#t` if `arg` is *procedure*; otherwise `#f`                    |
| `eq?`     | `lhs`, `rhs` | Returns `#t` if `lhs` and `rhs` are equivalent in value; otherwise `#f` |

## Running the tests

```bash
make test
```
Tests are defined in the `tests` directory. Each test suite is a pair of `.in` (input) and `.expect` (expected output) files. The `test` command in makefile generates a `.out` file with the `.in` file and `diff` it against the `.expect` file.

### Sample test suite (`combine`)

Say we want to reproduce the following behaviours.
```lisp
lisp> (define list (lambda lis lis))
<closure>
lisp> (define combine (lambda (f) (lambda (x y) (if (null? x) (quote ()) (f (list (car x) (car y)) ((combine f) (cdr x) (cdr y)))))))
<closure>
lisp> (define zip (combine cons))
<closure>
lisp> (zip (list 1 2 3 4) (list 5 6 7 8))
((1 5) (2 6) (3 7) (4 8))
lisp> (quit)
Farewell.
```
Create `.in` file for input (`tests/combine.in`).
```lisp
(define list (lambda lis lis))
(define combine (lambda (f) (lambda (x y) (if (null? x) (quote ()) (f (list (car x) (car y)) ((combine f) (cdr x) (cdr y)))))))
(define zip (combine cons))
(zip (list 1 2 3 4) (list 5 6 7 8))
(quit)
```
Create `.expect` file for expected output (`tests/combine.expect`).
```lisp
lisp> <closure>
lisp> <closure>
lisp> <closure>
lisp> ((1 5) (2 6) (3 7) (4 8))
lisp> Farewell.
```
Add the new test to the `TESTS` variable in `makefile`.
```make
TESTS = $(TESTDIR)/combine # Along with other tests.
```
Tests will be executed from `make test`.
```make
test: $(TESTS)

$(TESTDIR)/%: $(TESTDIR)/%.in $(TESTDIR)/%.expect $(OUTDIR)/lisp
	$(OUTDIR)/lisp < $@.in > $@.out
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

  - [(How to Write a (Lisp) Interpreter (in Python))](http://www.norvig.com/lispy.html)
