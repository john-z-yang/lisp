(define make-counter
  (lambda (i)
    (cons (lambda (s)
             (if (= s 0) i
                 (last (list (set! i (+ 1 i)) i))))
          (lambda (s)
             (if (= s 0) i
                 (last (list (set! i (+ 1 i)) i)))))))

(define my-counters (make-counter 0))
(define my-counter-1 (car my-counters))
(define my-counter-2 (cdr my-counters))

(display (my-counter-1 1))
(display (my-counter-1 1))
(display (my-counter-1 1))
(display (my-counter-1 1))
(display (my-counter-1 1))
(display (my-counter-1 0))
(display (my-counter-2 0))

(define f
  (lambda (n)
    (if (null? n) #f
      (lambda () n))))

(f '())
(f 1)
(newline)
