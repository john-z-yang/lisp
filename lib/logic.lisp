(define not (lambda (v) (if v #f #t)))

(defmacro and args
  (if (null? args) #t
    (if (null? (cdr args)) (car args)
      (list 'if
            (car args)
            (cons 'and
                  (cdr args))
            #f))))

(defmacro or args
  (if (null? args) #f
    (if (null? (cdr args)) (car args)
      (list 'if
            (car args)
            (car args)
            (cons 'or (cdr args))))))
