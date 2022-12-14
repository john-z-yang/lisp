(display "This is a \"string\" atom, use the double quote (\") character to create string atom.")
(display "Use the handy back-slash (\\) character to prevent the lexer from capturing it.")
(display "For instance, \\\" escapes the \" character and \\\\ escapes the \\ character.")
(display "You also do not need to worry about characters like \"(\", \")\" or \";\",")
(display "the lexer will not capture them.")
(display "You can use the str? predicate to test if an s-expression is a string or not.")
(display "For example: (str? \"\") will yield:")
(display (str? ""))
(display "Similarly: (str? 1) will yield:")
(display (str? 1))
(display "")
(display "Happy hacking")
(display (str-sub "We think in generalities, but we live in details." 3 5))
(display (str-con "abc" "def"))
(display (str-con "abc" ""))
(display (str-con "" ""))
(define s "\"This string contains\"")
(define size (str-len s))
(display (str-con (str-con s (str-con " " (->str size))) " characters."))