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
    ((lambda (cur-clause next-clause)
      (if (not (cons? cur-clause))
        (error (str-con "Invalid syntax for cond. "
                        "Expected (test conseq), but got "
                        (->str cur-clause)
                        "."))
        (list 'if
              (if (eqv? (car cur-clause) 'else)
                (if (null? next-clause)
                  #t
                  (error (str-con "Invalid syntax for cond "
                                  "(else clause must be last).")))
                (car cur-clause))
              (if (null? (cdr cur-clause))
                (error (str-con "Invalid syntax for cond "
                                "(missing expression after test)."))
                (car (cdr cur-clause)))
              (cons 'cond next-clause))))
    (car sexprs)
    (cdr sexprs))))
