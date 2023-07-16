(define Z-combinator
  (lambda (f)
    ((lambda (g)
       (f (lambda args (apply (g g) args))))
     (lambda (g)
       (f (lambda args (apply (g g) args)))))))

(display
 ((Z-combinator (lambda (ackermann)
       (lambda (m n)
         (cond
           ((= m 0) (+ n 1))
           ((= n 0) (ackermann (- m 1) 1))
           (else (ackermann (- m 1) (ackermann m (- n 1))))))))
  2
  4))

(display
 ((Z-combinator (lambda (fib)
       (lambda (n)
         (cond
           ((or (= n 1) (= n 2))
            1)
           (else
            (+ (fib (- n 1))
               (fib(- n 2))))))))
  16))
