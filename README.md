# Lisp Interpreter

Interpreter for lisp written in C++. This project started out as a solution to leetcode problem [736](https://leetcode.com/problems/parse-lisp-expression/) and was further inspired by Peter Norvig's blog [(How to Write a (Lisp) Interpreter (in Python))](http://www.norvig.com/lispy.html).

<p align="center">
    <img src="https://raw.githubusercontent.com/john-z-yang/lisp/master/docs/assets/fib_seq_gen.gif" width="100%">
</p>

## Getting Started

Make sure you have [g++](https://gcc.gnu.org/) and [make](https://www.gnu.org/software/make/).

### Installing

Clone this repository.

```bash
git clone https://github.com/john-z-yang/lisp
```

Build the project.
```bash
make
```

The final executable (`lisp`) will be in the `out` directory.

Run the program.

```shell-session
foo@bar:~$ out/lisp
lisp> (quote (Hello World!))
(Hello, (World!, ()))
lisp> (quit)
Farewell.
```
## Supported Syntax

| Syntax                                                        | Description                                                                                                                                                                         |
| :------------------------------------------------------------ | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| *(`define` sym expr)*                                         | Evaluate *expr*, associate symbol *sym* to the result in current lexical scope                                                                                                      |
| *(`set!` sym expr)*                                           | Evaluate *expr*, find the closest lexical scope where *sym* is defined, updates symbol *sym* to the result.                                                                         |
| *(`quote` expr)*                                              | Returns *expr*.                                                                                                                                                                     |
| *(`if` (expr<sub>1</sub>) expr<sub>2</sub> expr<sub>3</sub>)* | Evaluate *expr<sub>1</sub>*, if the result is *truthy* (aka not `#t`), returns *expr<sub>1</sub>*; otherwise returns *expr<sub>3</sub>*.                                            |
| *(`lambda` (Sym<sub>1</sub> ... Sym<sub>n</sub>) expr)*       | Returns a `ClosureAtom`, it accepts Sym<sub>1</sub> ... Sym<sub>n</sub> as arguments and *expr* as body. When invoked, parameters are bound to Sym<sub>1</sub> ... Sym<sub>n</sub>. |
| *(`lambda` sym expr)*                                         | Returns a `ClosureAtom`, it accepts arbitary number of arguments and *expr* as body.                                                                                                |
| *(`closure` expr<sub>1</sub> ... expr<sub>n</sub>)*           | Evaluates *expr<sub>1</sub> ... expr<sub>n</sub>*, invoke `closure` with the results bound to its parameter                                                                         |


## Running the tests

```bash
make test
```
Tests are defined in the `tests` directory. Each test suite is a pair of `.in` (input) and `.expect` (expected output) files. The test definition in makefile generates a `.out` file with the `.in` file and diff it against the `.expect` file.

### Sample test suite (`combine`)

Expected behaviour
```lisp
lisp> (define list (lambda lis lis))
<closure>
lisp> (define combine (lambda (f) (lambda (x y) (if (null? x) (quote ()) (f (list (car x) (car y)) ((combine f) (cdr x) (cdr y)))))))
<closure>
lisp> (define zip (combine cons))
<closure>
lisp> (zip (list 1 2 3 4) (list 5 6 7 8))
((1, (5, ())), ((2, (6, ())), ((3, (7, ())), ((4, (8, ())), ()))))
lisp> (quit)
Farewell.
```
`combine.in`
```lisp
(define list (lambda lis lis))
(define combine (lambda (f) (lambda (x y) (if (null? x) (quote ()) (f (list (car x) (car y)) ((combine f) (cdr x) (cdr y)))))))
(define zip (combine cons))
(zip (list 1 2 3 4) (list 5 6 7 8))
(quit)
```
`combine.expect`
```lisp
lisp> <closure>
lisp> <closure>
lisp> <closure>
lisp> ((1, (5, ())), ((2, (6, ())), ((3, (7, ())), ((4, (8, ())), ()))))
lisp> Farewell.
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
