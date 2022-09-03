(display (list 1 2 3))
(display (list 1))
(display (list))
(display (car (list 1 2 3)))
(display (car (list 1)))
(display (cdr (list 1 2 3)))
(display (cdr (list 1)))
(display (first (list 1 2 3)))
(display (first (list 1)))
(display (first (list)))
(display (last (list 1 2 3)))
(display (last (list 1)))
(display (last (list)))
(define fib (lambda (n) (if (= n 1) 1 (if (= n 2) 1 (+ (fib (- n 1)) (fib (- n 2)))))))
(display (map fib (list 1 2 3 4 5 6 7)))
(display
  (foldl (lambda (elem v)
           (+ v (* elem elem)))
         0
         (list 1 2 3))
)
(display
  (foldl (lambda (elem v)
           (+ v (* elem elem)))
         0
         (list 1))
)
(display
  (foldl (lambda (elem v)
           (+ v (* elem elem)))
         0
         (list))
)