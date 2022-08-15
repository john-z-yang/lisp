(define list (lambda lis lis))
(define combine (lambda (f) (lambda (x y) (if (null? x) (quote ()) (f (list (car x) (car y)) ((combine f) (cdr x) (cdr y)))))))
(define zip (combine cons))
(display (zip (list 1 2 3 4) (list 5 6 7 8)))
