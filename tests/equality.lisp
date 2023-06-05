(define x (list 0 1))
(define y (list 0 1))
(display (eq? x y))
(display (eqv? x y))
(set! y x)
(display (eq? x y))
(display (eqv? x y))
(display (eqv? 0 0))
(display (eqv? 0 1))
(display (eqv? 1 0))
(display (eqv? 1 1))
(define make-counter
  (lambda (i)
    (lambda ()
      (set! i (+ 1 i)))))
(define my-counter-1 (make-counter 1))
(define my-counter-2 (make-counter 1))
(display (eqv? my-counter-1 my-counter-2))
(display (my-counter-1))
(display (eqv? my-counter-1 my-counter-2))
(display (my-counter-2))
(display (eqv? my-counter-1 my-counter-2))
(display (my-counter-1))
(display (eqv? my-counter-1 my-counter-2))
(display (my-counter-2))
(display (eqv? my-counter-1 my-counter-2))
(display (eq? my-counter-1 my-counter-2))