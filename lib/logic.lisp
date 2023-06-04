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

(defmacro cond sexprs
  (if (null? sexprs) '()
    (list 'if
          (car (car sexprs))
          (if (null? (cdr (car sexprs)))
            (error "Missing expression after test")
            (cdr (car sexprs)))
          (cons 'cond (cdr sexprs)))))
