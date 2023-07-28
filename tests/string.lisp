(display "This is a \"string\" atom, use the double quote (\") character to create string atom.")
(display "Use the handy back-slash (\\) character to prevent the lexer from capturing it.")
(display "For instance, \\\" escapes the \" character and \\\\ escapes the \\ character.")
(display "You also do not need to worry about characters like \"(\", \")\" or \";\",")
(display "the lexer will not capture them.")
(display "You can use the string? predicate to test if an s-expression is a string or not.")
(display "For example: (string? \"\") will yield:")
(display (string? ""))
(display "Similarly: (string? 1) will yield:")
(display (string? 1))
(display "")
(display "Happy hacking")
(display (substring "We think in generalities, but we live in details." 3 5))
(display (string-append "abc" "def"))
(display (string-append "abc" ""))
(display (string-append "" ""))
(define s "\"This string contains\"")
(define size (string-length s))
(display (string-append (string-append s (string-append " " (->str size))) " characters."))
(display (substring "abc" 0 3))
(display (substring "\"\"\"" 0 1))
(display (substring "\"\"\"" 0 3))
