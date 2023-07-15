# Language Specification

## Special Forms

### Procedures

| Syntax                                                                                 | Description                                                                                                                                                                                                                                                                                                                                       |
| -------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| (**lambda** (_sym<sub>1</sub> ... sym<sub>n</sub>_) _expr+_)                           | Produces a _procedure_ that accepts _n_ arguments. When invoked, bind each argument to a _sym_, evalulate _expr_ in order and return the result of the last _expr_.                                                                                                                                                                               |
| (**lambda** _sym expr+_)                                                               | Produces a _procedure_ that accepts arbitary number of arguments. When invoked, bind parameters to _sym_ as a list, evalulate _expr_ in order and return the result of the last _expr_.                                                                                                                                                           |
| (**lambda** (_sym<sub>1</sub> ... sym<sub>n - 1</sub> **.** sym<sub>n</sub>_) _expr+_) | Produces a _procedure_ that accepts _n - 1_ or more arguments. When invoked, bind each argument to a _sym_, the last variable will be a newly allocated list of the arguments left over after all the other arguments have been matched up against the other formal argument, evalulate _expr_ in order and return the result of the last _expr_. |
| (**procedure** _expr\*_)                                                               | Evaluates each _expr_, invoke _procedure_ with the results as arguments.                                                                                                                                                                                                                                                                          |

### Macros

| Syntax                                                                                        | Description                                                                                                                               |
| --------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------- |
| (**defmacro** _form_ (_sym<sub>1</sub> ... sym<sub>n</sub>_) _expr+_)                         | Register a _procedure_ equivalent to (**lambda** (_sym<sub>1</sub> ... sym<sub>n</sub>_) _expr_) as a _macro_.                            |
| (**defmacro** _form sym expr+_)                                                               | Register a _procedure_ equivalent to (**lambda** _sym expr_) as a _macro_.                                                                |
| (**defmacro** _form (_sym<sub>1</sub> ... sym<sub>n - 1</sub> **.** sym<sub>n</sub>_) expr+_) | Register a _procedure_ equivalent to (**lambda** (_sym<sub>1</sub> ... sym<sub>n - 1</sub> **.** sym<sub>n</sub>_) _expr+_) as a _macro_. |
| (_form_ _expr*_)                                                                              | Invoke the procedure _macro_ is bound to with the unevaluated _expr_ as arugment **during compile time**. Compile the output.             |
| (**quote** _expr_)                                                                            | Returns _expr_ without evaluating it.                                                                                                     |
| **'**_expr_                                                                                   | Equivalent to (**quote** _expr_)                                                                                                          |

### Symbols

| Syntax                                                                                              | Description                                                                                                                    |
| --------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------ |
| **sym**                                                                                             | Returns the value that _sym_ is bound to in the closest lexical scope.                                                         |
| (**define** _sym expr_)                                                                             | Evaluates _expr_, bind the result to _sym_ in current lexical scope.                                                           |
| (**set!** _sym expr_)                                                                               | Evaluates _expr_, find the closest lexical scope where _sym_ is bound, re-bind result to _sym_.                                |
| (**let** ((_sym<sub>1</sub> expr<sub>1</sub>_) ... (_sym<sub>n</sub> expr<sub>n</sub>_)) _expr+_)   | Equivalent to ((**lambda** (_sym<sub>1</sub> ... sym<sub>n</sub>_) _expr+_) _expr<sub>1</sub> ... expr<sub>n</sub>_)           |
| (**let\*** ((_sym<sub>1</sub> expr<sub>1</sub>_) ... (_sym<sub>n</sub> expr<sub>n</sub>_)) _expr+_) | Equivalent to (**let** ((_sym<sub>1</sub> expr<sub>1</sub>_)) ... (**let** ((_sym<sub>n</sub> expr<sub>n</sub>_)) _expr+_)...) |

### Logic

| Syntax                                   | Description                                                                                                                                                                                    |
| ---------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| (**if** _test conseq alt_)               | Evaluates _test_, if the result is _truthy_, evalulate _conseq_, evalulate _alt_ otherwise.                                                                                                    |
| (**cond** _(test expr+)* (else expr+)?_) | Evaluates each pair of _(test expr)_ in sequence, if the result of _test_ is _truthy_, evalulate its _expr_ in order and return the result of the last _expr_ (_else_ always evaluates to #t). |
| (**and** _expr*_)                        | Evaluate _expr_ from left to right. When one of them evaluates to `#f`, return `#f`. If all of them are _truthy_, return the result of the last _expr_.                                        |
| (**or** _expr*_)                         | Evaluate _expr_ from left to right. When one of them evaluates to a _truthy_ value, return the result. If all of them are `#f`, return `#f`.                                                   |

### Sequencing

| Syntax              | Description                                                                                             |
| ------------------- | ------------------------------------------------------------------------------------------------------- |
| (**begin** _expr*_) | Evaluate _expr_ in sequence, return the result of the last _expr_, if no expr is provided, return `'()` |

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
