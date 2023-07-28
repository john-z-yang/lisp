(define x (list 0 1))
(define y (list 0 1))
(display (eq? x y))
(display (equal? x y))
(set! y x)
(display (eq? x y))
(display (equal? x y))
(display (equal? 0 0))
(display (equal? 0 1))
(display (equal? 1 0))
(display (equal? 1 1))
(define make-counter
  (lambda (i)
    (lambda ()
      (set! i (+ 1 i))
      i)))
(define my-counter-1 (make-counter 1))
(define my-counter-2 (make-counter 1))
(display (equal? my-counter-1 my-counter-2))
(display (my-counter-1))
(display (equal? my-counter-1 my-counter-2))
(display (my-counter-2))
(display (equal? my-counter-1 my-counter-2))
(display (my-counter-1))
(display (equal? my-counter-1 my-counter-2))
(display (my-counter-2))
(display (equal? my-counter-1 my-counter-2))
(display (eq? my-counter-1 my-counter-2))
(display (eqv? 'a 'a))
(display (eqv? 'a 'b))
(display (eqv? 2 2))
(display (eqv? '() '()))
(display (eqv? 100000000 100000000))
(display (eqv? (cons 1 2) (cons 1 2)))
(display (eqv? (lambda () 1) (lambda () 2)))
(display (eqv? #f 'nil))
(display (let ((p (lambda (x) x))) (eqv? p p)))
(define gen-counter
  (lambda ()
    (let ((n 0))
      (lambda () (set! n (+ n 1)) n))))
(display (let ((g (gen-counter)))
           (eqv? g g)))
(display (eqv? (gen-counter) (gen-counter)))
(define gen-loser
  (lambda ()
    (let ((n 0))
      (lambda () (set! n (+ n 1)) 27))))
(display (let ((g (gen-loser)))
           (eqv? g g)))
(letrec ((f (lambda () (if (eqv? f g) 'f 'both)))
         (g (lambda () (if (eqv? f g) 'g 'both))))
  (display (eqv? f g)))
(let ((x '(a))) (display (eqv? x x)))
(display (eq? 'a 'a))
(display (eq? (list 'a) (list 'a)))
(display (eq? '() '()))
(display (eq? car car))
(display (let ((x '(a)))
  (eq? x x)))
(display (let ((p (lambda (x) x)))
  (eq? p p)))
