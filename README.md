# Lisp Interpreter

Interpreter for lisp written in C++. This project started out as a solution to leetcode problem [736](https://leetcode.com/problems/parse-lisp-expression/) and further inspired by Peter Norvig's blog [(How to Write a (Lisp) Interpreter (in Python))](http://www.norvig.com/lispy.html).

![Alt Text](https://media.giphy.com/media/MiHELCn0niMPxU9egY/giphy.gif)

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
lisp> (quote (Hello Word!))
(Hello, (Word!, ()))
lisp> (quit)
Farewell.
```

## Running the tests

```bash
make test
```
Tests are defined in the `tests` directory. Each test suite is a pair of `.in` (input) and `.expect` (expected output) files. The test definition in makefile generates a `.out` file with the `.in` file and diff it against the `.expect` file.

### Sample test suite (`cons`)

Expected behaviour
```lisp
lisp> (define list (lambda lis lis))
<closure>
lisp> (list 1 2 3)
(1, (2, (3, ())))
lisp> (list (+ 1 2) 2 3)
(3, (2, (3, ())))
lisp> (define map (lambda (fn lis) (if (null? lis) () (cons (fn (car lis)) (map fn (cdr lis))))))
<closure>
lisp> (define range (lambda (a b) (if (= a b) (quote ()) (cons a (range (+ a 1) b)))))
<closure>
lisp> (define fib (lambda (n) (if (= n 1) 1 (if (= n 2) 1 (+ (fib (- n 1)) (fib (- n 2)))))))
<closure>
lisp> (map fib (range 1 13))
(1, (1, (2, (3, (5, (8, (13, (21, (34, (55, (89, (144, ()))))))))))))
lisp> (quit)
Farewell.
```
`cons.in`
```lisp
(define list (lambda lis lis))
(list 1 2 3)
(list (+ 1 2) 2 3)
(define map (lambda (fn lis) (if (null? lis) () (cons (fn (car lis)) (map fn (cdr lis))))))
(define range (lambda (a b) (if (= a b) (quote ()) (cons a (range (+ a 1) b)))))
(define fib (lambda (n) (if (= n 1) 1 (if (= n 2) 1 (+ (fib (- n 1)) (fib (- n 2)))))))
(map fib (range 1 13))
(quit)
```
`cons.expect`
```lisp
lisp> <closure>
lisp> (1, (2, (3, ())))
lisp> (3, (2, (3, ())))
lisp> <closure>
lisp> <closure>
lisp> <closure>
lisp> (1, (1, (2, (3, (5, (8, (13, (21, (34, (55, (89, (144, ()))))))))))))
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