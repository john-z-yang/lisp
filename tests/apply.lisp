(define curry (lambda (n f)
  (cond ((= n 1)
         f)
        (else
         (lambda (x)
           (curry (- n 1)
                  (lambda args
                    (apply f (cons x args)))))))))

(display (((curry 2 +) 0) 1))
(display (((((curry 4 +) 1) 2) 3) 4))
(display ((curry 1 list) 0))
(display (((((curry 4 list) 0) 1) 2) 3))

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

(display (apply list '()))
(display (apply list '(1 2 3)))
(display (apply list 0 '()))
(display (apply list 0 '(1)))
(display (apply list 0 1 2 '(3 4 5)))
(display (apply + 1 '()))
(display (apply + 1 '(2)))
(display (apply + '(1 2 3)))
(display (apply + 1 2 '(3 4 5)))

(apply (lambda (a . b)
         (display a)
         (display b))
       '(1 2))

(apply (lambda (a . b)
         (display a)
         (display b))
       1 2 '())

(apply (lambda (a . b)
         (display a)
         (display b))
       1 '(2))
