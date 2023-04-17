(define x 0)
(display x)
((lambda () (set! x 1)))
(display x)
