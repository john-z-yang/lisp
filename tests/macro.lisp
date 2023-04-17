(define-macro when
  (lambda args
    `(if ,(car args) (sequence ,@(cdr args)) ())))

(when (= 1 1)
  (display (quote (gods in his heaven alls right with the world)))
  (display (quote (anywhere can be a paradise as long as you have the will to live)))
  (define eva (+ 0 (+ 1  2)))
  (display (quote (acts of men are better than acts of god)))
  (display eva))

(define-macro my-or
  (lambda (x y)
    `(if ,x ,x ,y)))

(display (my-or 1 2))
(display (my-or #f 2))

(my-or
  (sequence
  (display (quote doing_first_argument))
  #t)
2)

(define-macro swap
  (lambda (x y)
    (sequence (define tempVar (gensym))
      `(sequence (define ,tempVar ,x)
              (set! ,x ,y)
              (set! ,y ,tempVar)))))

(define a 1)
(define b 2)
(define x 1)
(define y 2)
(swap a b)
(swap a b)
(swap a b)
(swap x y)
(swap x y)
(swap x y)
(display a)
(display b)
(display x)
(display y)
