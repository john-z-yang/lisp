(define count-to (lambda (n) (if (= n 0) 0 (count-to (- n 1)))))
(count-to 2048)
(quit)
