# Lisp Interpreter &middot; [![build](https://github.com/john-z-yang/lisp/actions/workflows/ci.yml/badge.svg?branch=master)](https://github.com/john-z-yang/lisp/actions/workflows/ci.yml)

Interpreter for a subset of Scheme written in C++. Implemented through a compiler and a virtual machine. (See [Technical Details section](#Technical-Details) for more details.)

<p align="center">
    <img src="https://raw.githubusercontent.com/john-z-yang/lisp/master/docs/assets/fib_seq_gen.gif" style="max-width:768px;  width:100%;">
</p>

## Getting Started

These instructions will give you a copy of the interpreter up and running on
your machine.

### Prerequisites

- [g++](https://gcc.gnu.org/), or any C++ compiler that supports [C++20](https://en.cppreference.com/w/cpp/20) and [computed goto](https://gcc.gnu.org/onlinedocs/gcc/Labels-as-Values.html)
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

Note: if you like to use the functions defined in `lisp/lib/`. You will need to set the `LISP_LIB_ENV` environment variable to its absolute path.

```console
foo@bar:~$ export LISP_LIB_ENV=/PATH/TO/lisp/lib
```

_Happy hacking!_

## Special Forms

### Procedures

| Syntax                                                      | Description                                                                                                                                                 |
| ----------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------- |
| (**lambda** (_sym<sub>1</sub> ... sym<sub>n</sub>_) _expr_) | Produces a _procedure_ that accepts _n_ arguments. When invoked, bind each argument to a _sym_, evalulate _expr_ and return the result.                     |
| (**lambda** _sym expr_)                                     | Produces a _procedure_ that accepts arbitary number of arguments. When invoked, bind parameters to _sym_ as a list, evalulate _expr_ and return the result. |
| (**procedure** _expr\*_)                                    | Evaluates each _expr_, invoke _procedure_ with the results as arguments.                                                                                    |

### Macros

| Syntax                                                               | Description                                                                                                                   |
| -------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------- |
| (**defmacro** _form_ (_sym<sub>1</sub> ... sym<sub>n</sub>_) _expr_) | Register a _procedure_ equivalent to (**lambda** (_sym<sub>1</sub> ... sym<sub>n</sub>_) _expr_) as a _macro_.                |
| (**defmacro** _form expr_)                                           | Register a _procedure_ equivalent to (**lambda** _sym expr_) as a _macro_.                                                    |
| (_macro expr*_)                                                      | Invoke the procedure _macro_ is bound to with the unevaluated _expr_ as arugment **during compile time**. Compile the output. |
| (**quote** _expr_)                                                   | Returns _expr_ without evaluating it.                                                                                         |
| **'**_expr_                                                          | Equivalent to (**quote** _expr_)                                                                                              |

### Symbols

| Syntax                                                                                         | Description                                                                                                               |
| ---------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------- |
| **sym**                                                                                        | Returns the value that _sym_ is bound to in the closest lexical scope.                                                    |
| (**define** _sym expr_)                                                                        | Evaluates _expr_, bind the result to _sym_ in current lexical scope.                                                      |
| (**set!** _sym expr_)                                                                          | Evaluates _expr_, find the closest lexical scope where _sym_ is bound, re-bind result to _sym_.                           |
| (**let** _((sym<sub>1</sub> expr<sub>1</sub>) ... (sym<sub>n</sub> expr<sub>n</sub>)) expr_)   | Equivalent to ((**lambda** (_sym<sub>1</sub> ... sym<sub>n</sub>_) expr) expr<sub>1</sub> ... expr<sub>n</sub>)           |
| (**let\*** _((sym<sub>1</sub> expr<sub>1</sub>) ... (sym<sub>n</sub> expr<sub>n</sub>)) expr_) | Equivalent to (**let** ((_sym<sub>1</sub> expr<sub>1</sub>)) ... (**let** ((_sym<sub>n</sub> expr<sub>n</sub>)) expr)...) |

### Logic

| Syntax                                 | Description                                                                                                                                                   |
| -------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| (**if** _test conseq alt_)             | Evaluates _test_, if the result is _truthy_, evalulate _conseq_, evalulate _alt_ otherwise.                                                                   |
| (**cond** _(test expr)* (else expr)?_) | Evaluates each pair of _(test expr)_ in sequence, if the result of _test_ is _truthy_, evalulate its _expr_ (_else_ always evaluates to #t).                  |
| (**and** _expr*_)                      | Evaluate _expression_ from left to right. When one of them evaluates to `#f`, return `#f`. If all of them are _truthy_, return the result of the last _expr_. |
| (**or** _expr*_)                       | Evaluate _expression_ from left to right. When one of them evaluates to a _truthy_ value, return the result. If all of them are `#f`, return `#f`.            |

## Built-In Functions

### Symbols

| Function         | Description                                          |
| ---------------- | ---------------------------------------------------- |
| (**sym?** _arg_) | Returns `#t` if _arg_ is a _symbol_, `#f` otherwise. |

### Intergers

| Function                                       | Description                                                                                            |
| ---------------------------------------------- | ------------------------------------------------------------------------------------------------------ |
| (**num?** _arg_)                               | Returns `#t` if _arg_ is a _number_, `#f` otherwise.                                                   |
| (**=** _num<sub>1</sub> ... num<sub>n</sub>_)  | Returns `#t` if all of the arguments are numerically equal, `#f` otherwise.                            |
| (**>** _num<sub>1</sub> ... num<sub>n</sub>_)  | Returns `#t` if all of the arguments in the given order are strictly increasing, `#f` otherwise.       |
| (**>=** _num<sub>1</sub> ... num<sub>n</sub>_) | Returns `#t` if all of the arguments in the given order are non-decreasing, `#f` otherwise.            |
| (**<** _num<sub>1</sub> ... num<sub>n</sub>_)  | Returns `#t` if all of the arguments in the given order are strictly decreasing, `#f` otherwise.       |
| (**<=** _num<sub>1</sub> ... num<sub>n</sub>_) | Returns `#t` if all of the arguments in the given order are non-increasing, `#f` otherwise.            |
| (**abs** arg)                                  | Returns \|arg\|.                                                                                       |
| (**+** _num<sub>1</sub> ... num<sub>n</sub>_)  | Returns the sum of all of the arguments, `0` if no arguments are provided.                             |
| (**-** _num<sub>1</sub> ... num<sub>n</sub>_)  | Returns num<sub>1</sub> - ... - num<sub>n</sub>, `0` - num<sub>1</sub> if no argument are provided.    |
| (**\*** _num<sub>1</sub> ... num<sub>n</sub>_) | Returns the product of all of the arguments, `1` if no arguments are provided.                         |
| (**/** _num<sub>1</sub> ... num<sub>n</sub>_)  | Returns num<sub>1</sub> ÷ ... ÷ num<sub>n</sub>, `1` ÷ num<sub>n</sub> if only 1 argument is supplied. |
| (**%** _lhs rhs_)                              | Returns lhs % rhs.                                                                                     |

### Strings

| Function                                              | Description                                                                                                                                 |
| ----------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------- |
| (**str?** _arg_)                                      | Returns `#t` if _arg_ is _string_; otherwise return _#f_.                                                                                   |
| (**str-len** _arg_)                                   | Returns the length of _arg_.                                                                                                                |
| (**str-sub** _str_ _pos_ _len_)                       | Returns a new string that starts at character position _pos_ and spans _len_ characters (or until the end of _str_, whichever comes first). |
| (**str-con** _str<sub>1</sub>_ ... _str<sub>n</sub>_) | Returns a new string that is the concatenation of _str<sub>1</sub>_, ..., _str<sub>n</sub>_.                                                |
| (**->str** _arg_)                                     | Returns _arg_ in string representation.                                                                                                     |

### Pairs and Lists

| Function                                         | Description                                                                                                             |
| ------------------------------------------------ | ----------------------------------------------------------------------------------------------------------------------- |
| (**null?** _arg_)                                | Returns `#t` if arg is `'()`, `#f` otherwise.                                                                           |
| (**cons?** _arg_)                                | Returns `#t` if arg is a cons pair, `#f` otherwise.                                                                     |
| (**cons** _lhs rhs_)                             | Returns a pair where first element is lhs and second element is rhs.                                                    |
| (**car** _pair_)                                 | Returns the first element of pair.                                                                                      |
| (**cdr** _pair_)                                 | Returns the second element of pair.                                                                                     |
| (**list** _arg<sub>1</sub> ... arg<sub>n</sub>_) | Returns arg<sub>1</sub>, ..., arg<sub>n</sub> as a list (i.e. (cons arg<sub>1</sub> ... (cons arg<sub>n</sub>'())...)). |
| (**first** _list_)                               | Returns the first element if list is not empty, `'()` otherwise. (**Note:** list must be a properly formed list.)       |
| (**last** _list_)                                | Returns the last element if list is not empty, `'()` otherwise. (**Note:** list must be a properly formed list.)        |
| (**foldl** _fn cur list_)                        | For each element e in list, update cur as (fn e cur), returns cur when no element remains.                              |
| (**map** _fn list_)                              | Returns a new list where fn is applied to each element of list.                                                         |
| (**reverse** _list_)                             | Returns list but with its order of elements reversed.                                                                   |

### Predicates

| Function             | Description                                                                      |
| -------------------- | -------------------------------------------------------------------------------- |
| (**proc?** _arg_)    | Returns `#t` if arg is _closure_ or _native function_, `#f` otherwise.           |
| (**eq?** _lhs rhs_)  | Returns `#t` if lhs and rhs represent the same object in memory, `#f` otherwise. |
| (**eqv?** _lhs rhs_) | Returns `#t` if lhs and rhs are equivalent in value, `#f` otherwise.             |

### Miscellaneous

| Function            | Description                                                   |
| ------------------- | ------------------------------------------------------------- |
| (**quit**)          | Quits the session.                                            |
| (**error** _msg_)   | Throws a runtime error with _msg_ as the message.             |
| (**dis** _closure_) | Prints the disassembled bytecode for _closure_, returns `'()` |
| (**display** _arg_) | Prints arg to std::cout, returns `'()`.                       |

## Technical Details

### Overview

The interpreter is implemented through a [bytecode](https://en.wikipedia.org/wiki/Bytecode) compiler and [stack-based virtual machine](https://en.wikipedia.org/wiki/Stack_machine).

```mermaid
flowchart LR
  subgraph Compiler
    direction TB
    B[Tokenization] -- Token --> C[Parsing]
    C -- S-expression --> D[Bytecode generation]
  end
  A[REPL] -- std::string --> Compiler
  Compiler -- FnAtom --> E[Virtual Machine]
```

### File structure

```text
.
├── bin                # Compiled files
├── docss              # Supporting documentation
├── lib                # Pre-defined functions
├── src/
│   ├── common         # Source for shared data structures (Opcodes, S-expressions, etc.)
│   ├── compile        # Source for compiler (grammer, parser, etc.)
│   ├── repl           # Source for REPL
│   └── runtime        # Source for rumtime (vm, runtime errors, etc.)
└── tests
```

### Opcodes

| Opcode                           | Description                                                                                                                                                                                           |
| -------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `MAKE_CLOSURE (i) (IS_LOCAL j)*` | Push a ClosureAtom onto the stack, created from the FnAtom at `const[i]`. For each upvalue in the ClosureAtom, capture it from `stack[j]` if `IS_LOCAL` is `1`, capture from `upvalues[j]` otherwise. |
| `CALL (argc)`                    | Call the function at **TOS** - `argc` with `argc` parameters.                                                                                                                                         |
| `RETURN`                         | Return **TOS**.                                                                                                                                                                                       |
| `POP_TOP`                        | Pop **TOS**.                                                                                                                                                                                          |
| `CLOSE_UPVALUE`                  | Pop **TOS**, and close the free variables it's associated with.                                                                                                                                       |
| `LOAD_CONST (idx)`               | Push `const[idx]` onto the stack.                                                                                                                                                                     |
| `LOAD_SYM (idx)`                 | Push `globals[const[idx]]` onto the stack.                                                                                                                                                            |
| `DEF_SYM (idx)`                  | Define `globals[const[idx]]` as **TOS**.                                                                                                                                                              |
| `SET_SYM (idx)`                  | Set `globals[const[idx]]` as **TOS**.                                                                                                                                                                 |
| `LOAD_UPVALUE (idx)`             | Push `upvalues[idx]` onto the stack.                                                                                                                                                                  |
| `SET_UPVALUE (idx)`              | Set `upvalues[idx]` as **TOS**.                                                                                                                                                                       |
| `LOAD_STACK (idx)`               | Push `upvalues[idx]` onto the stack.                                                                                                                                                                  |
| `SET_STACK (idx)`                | Set `upvalues[idx]` to **TOS**.                                                                                                                                                                       |
| `JUMP (offset)`                  | Set `ip` of current frame to `offset`.                                                                                                                                                                |
| `POP_JUMP_IF_FALSE (offset)`     | Set `ip` of current frame to `offset` if **TOS** is not _truthy_.                                                                                                                                     |
| `MAKE_LIST (argc)`               | Pop `argc` elements from **TOS**, push those elements as cons list onto the stack.                                                                                                                    |

#### Example

```lisp
lisp> (define fac
  ...   (lambda (n)
  ...     (if (= n 0)
  ...       1
  ...     (* n (fac (- n 1))))))
<Closure at 0x600000980888>

lisp> (dis fac)
<Closure at 0x60000186c748>, instance of:
    <Function at 0x600001f6c000, arity: 1, upvalues: 0>
Constants:
    =, 0, 1, *, fac, -, 1
Bytecodes (raw):
    05 00 0a 01 04 01 01 02 0d 00 05 04 02 0c 00 12
    05 03 0a 01 05 04 05 05 0a 01 04 06 01 02 01 01
    01 02 02
Bytecodes:
    3                  0 LOAD_SYM                =
    3                  2 LOAD_STACK              1
    3                  4 LOAD_CONST              0
    3                  6 CALL                    2
    3                  8 POP_JUMP_IF_FALSE       5
    4                 11 LOAD_CONST              1
    4                 13 JUMP                    18
    5                 16 LOAD_SYM                *
    5                 18 LOAD_STACK              1
    5                 20 LOAD_SYM                fac
    5                 22 LOAD_SYM                -
    5                 24 LOAD_STACK              1
    5                 26 LOAD_CONST              1
    5                 28 CALL                    2
    5                 30 CALL                    1
    5                 32 CALL                    2
    ?                 34 RETURN
```

## Running the tests

```bash
make test
```

Tests are defined in the `tests` directory. Each test suite is a pair of lisp code (`.lisp`) and its expected output (`.expect`).

The `test` command runs the `.lisp` file and generates a `.out` file by redirecting `stdout`. Finally, it `diff`s the `.out` file against the `.expect` file.

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
	(export LISP_LIB_ENV=$(LIBDIR); $(OUTDIR)/lisp $@.lisp >> $@.out 2>&1)
	diff $@.expect $@.out
	rm $@.out
```

## Author

- **John Yang** - [john-z-yang](https://github.com/john-z-yang)

See also the list of
[contributors](https://github.com/john-z-yang/lisp/contributors)
who participated in this project.

## Acknowledgments

- [(How to Write a (Lisp) Interpreter (in Python))](http://www.norvig.com/lispy.html) by [Peter Norvig](https://norvig.com/)
- [Crafting Interpreters](https://craftinginterpreters.com) by [Bob Nystrom](https://journal.stuffwithstuff.com/)
- Special thanks to [Sophie](https://github.com/yqstan) for pointing out that parameter eval order is different across C++ compiler implementations.
- [Stackoverflow: What is a trampoline function?](https://stackoverflow.com/questions/189725/what-is-a-trampoline-function) Question by [Benoit](https://stackoverflow.com/users/10703/benoit), Solutions by toyvo (no longer active) and [Piotr Kukielka](https://stackoverflow.com/users/704905/piotr-kukielka).
- [By example: Continuation-passing style in JavaScript](https://matt.might.net/articles/by-example-continuation-passing-style/) by [Matt Might](https://matt.might.net/)
- [How to compile with continuations](https://matt.might.net/articles/cps-conversion/) by [Matt Might](https://matt.might.net/)
<br>

---
<p align=center>
<img src="https://imgs.xkcd.com/comics/lisp_cycles.png">
</p>
