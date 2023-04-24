(define y 0)
(display y)
((lambda () (set! y 1)))
(display y)
(define x -8)
(display (list
  x
  ((lambda (x) (list x 0 1 x 3 (set! x 4) 5 x)) -1)
  ((lambda (x) (list x 6 7 x 9 (set! x 10) 11 x)) -2)
  ((lambda (x) (list x 0 1 x 3 (set! x 4) 5 x)) -3)
  x
  (set! x 0)
  x))
(display x)
(display (list
  x
  ((lambda (x) (list x 0 1 x 3 (set! x 4) 5 x)) -1)
  ((lambda (x) (list x 6 7 x 9 (set! x 10) 11 x)) -2)
  ((lambda (x) (list x 0 1 x 3 (set! x 4) 5 x)) -3)
  x
  (set! x -8)
  x))
(display x)
(display (list
  x
  ((lambda (x) (list x 0 1 x 3 (set! x 4) 5 x)) -1)
  ((lambda (x) (list x 6 7 x 9 (set! x 10) 11 x)) -2)
  ((lambda (x) (list x 0 1 x 3 (set! x 4) 5 x)) -3)
  x
  (set! x 0)
  x))
(display x)
(display (list
  x
  ((lambda () (list x 0 1 x 3 (set! x 4) 5 x)))
  ((lambda () (list x 6 7 x 9 (set! x 10) 11 x)))
  ((lambda () (list x 0 1 x 3 (set! x 4) 5 x)))
  x
  (set! x 0)
  x))
(display x)
(display (list
  x
  ((lambda lis (list x 0 1 x 3 (set! x 4) 5 x)) x x x x)
  ((lambda lis (list x 6 7 x 9 (set! x 10) 11 x)) x x x x)
  ((lambda lis (list x 0 1 x 3 (set! x 4) 5 x)) x x x x)
  x
  (set! x 0)
  x))
