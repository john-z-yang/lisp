(display (quasiquote (0 1 2)))
(display (quasiquote (0 (unquote (+ 1 2)) 4)))
(display `(0 1 2))
(display `(1 ,(+ 1 2) 4))
(display `(1 `,(+ 1 ,(+ 2 3)) 4))

