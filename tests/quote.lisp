(define list (lambda lis lis))
(display (quasiquote (0 1 2)))
(display (quasiquote (0 (unquote (+ 1 2)) 4)))
(display `(0 1 2))
(display `(1 ,(+ 1 2) 4))
(display `(1 `,(+ 1 ,(+ 2 3)) 4))
(display (quasiquote ((unquote-splicing (list 2 3)))))
(display (quasiquote ((unquote-splicing (list 2)))))
(display (quasiquote ((unquote-splicing (list)))))
(display (quasiquote ((unquote-splicing (list 2 3)) 4)))
(display (quasiquote ((unquote-splicing (list 2)) 3)))
(display (quasiquote ((unquote-splicing (list)) 2)))
(display (quasiquote (1 (unquote-splicing (list 2 3)))))
(display (quasiquote (1 (unquote-splicing (list 2)))))
(display (quasiquote (1 (unquote-splicing (list)))))
(display (quasiquote (1 (unquote-splicing (list 2 3)) 4)))
(display (quasiquote (1 (unquote-splicing (list 2)) 3)))
(display (quasiquote (1 (unquote-splicing (list)) 2)))
(display `(1  ```,(unquote-splicing(unquote (unquote-splicing (list (+ 1 2))))) 4))