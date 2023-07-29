(define curry (lambda (n f)
                (cond ((= n 1)
                       f)
                      (else
                       (lambda (x)
                         (curry (- n 1)
                                (lambda args
                                  (apply f (cons x args)))))))))

(display (((curry 2 +) 0) 1))
(newline)
(display (((((curry 4 +) 1) 2) 3) 4))
(newline)
(display ((curry 1 list) 0))
(newline)
(display (((((curry 4 list) 0) 1) 2) 3))
(newline)

(display
 (apply apply
        (list apply
              (list apply
                    (list apply
                          (list apply
                                (list apply
                                      (list apply
                                            (list apply
                                                  (list apply
                                                        (list apply
                                                              (list apply
                                                                    (list apply
                                                                          (list apply
                                                                                (list apply
                                                                                      (list apply
                                                                                            (list apply
                                                                                                  (list apply
                                                                                                        (list apply
                                                                                                              (list apply
                                                                                                                    (list apply
                                                                                                                          (list apply
                                                                                                                                (list apply
                                                                                                                                      (list apply
                                                                                                                                            (list apply
                                                                                                                                                  (list apply
                                                                                                                                                        (list apply
                                                                                                                                                              (list apply
                                                                                                                                                                    (list apply
                                                                                                                                                                          (list apply
                                                                                                                                                                                (list apply
                                                                                                                                                                                      (list apply
                                                                                                                                                                                            (list apply
                                                                                                                                                                                                  (list apply
                                                                                                                                                                                                        (list apply
                                                                                                                                                                                                              (list apply
                                                                                                                                                                                                                    (list apply
                                                                                                                                                                                                                          (list list '(0 1 2))))))))))))))))))))))))))))))))))))))
 )
(newline)
(display (apply list '()))
(newline)
(display (apply list '(1 2 3)))
(newline)
(display (apply list 0 '()))
(newline)
(display (apply list 0 '(1)))
(newline)
(display (apply list 0 1 2 '(3 4 5)))
(newline)
(display (apply + 1 '()))
(newline)
(display (apply + 1 '(2)))
(newline)
(display (apply + '(1 2 3)))
(newline)
(display (apply + 1 2 '(3 4 5)))
(newline)

(apply (lambda (a . b)
         (display a)
         (newline)
         (display b)
         (newline))
       '(1 2))

(apply (lambda (a . b)
         (display a)
         (newline)
         (display b)
         (newline))
       1 2 '())

(apply (lambda (a . b)
         (display a)
         (newline)
         (display b)
         (newline))
       1 '(2))
