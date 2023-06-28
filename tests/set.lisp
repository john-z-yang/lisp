(define y 0)
(display y)
((lambda () (set! y 1) y))
(display y)
(define x -8)
(display (list
  x
  ((lambda (x) (list x 0 1 x 3 (begin (set! x 4) x) 5 x)) -1)
  ((lambda (x) (list x 6 7 x 9 (begin (set! x 10) x) 11 x)) -2)
  ((lambda (x) (list x 0 1 x 3 (begin (set! x 4) x) 5 x)) -3)
  x
  (begin (set! x 0) x)
  x))
(display x)
(display (list
  x
  ((lambda (x) (list x 0 1 x 3 (begin (set! x 4) x) 5 x)) -1)
  ((lambda (x) (list x 6 7 x 9 (begin (set! x 10) x) 11 x)) -2)
  ((lambda (x) (list x 0 1 x 3 (begin (set! x 4) x) 5 x)) -3)
  x
  (begin (set! x -8) x)
  x))
(display x)
(display (list
  x
  ((lambda (x) (list x 0 1 x 3 (begin (set! x 4) x) 5 x)) -1)
  ((lambda (x) (list x 6 7 x 9 (begin (set! x 10) x) 11 x)) -2)
  ((lambda (x) (list x 0 1 x 3 (begin (set! x 4) x) 5 x)) -3)
  x
  (begin (set! x 0) x)
  x))
(display x)
(display (list
  x
  ((lambda () (list x 0 1 x 3 (begin (set! x 4) x) 5 x)))
  ((lambda () (list x 6 7 x 9 (begin (set! x 10) x) 11 x)))
  ((lambda () (list x 0 1 x 3 (begin (set! x 4) x) 5 x)))
  x
  (begin (set! x 0) x)
  x))
(display x)
(display (list
  x
  ((lambda lis (list x 0 1 x 3 (begin (set! x 4) x) 5 x)) x x x x)
  ((lambda lis (list x 6 7 x 9 (begin (set! x 10) x) 11 x)) x x x x)
  ((lambda lis (list x 0 1 x 3 (begin (set! x 4) x) 5 x)) x x x x)
  x
  (begin (set! x 0) x)
  x))
