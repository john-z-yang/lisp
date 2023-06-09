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
          ((lambda (tmp)
             (list (list 'lambda
                         (list tmp)
                         (list 'if
                               tmp
                               tmp
                               (cons 'or (cdr args))))
                   (car args)))
           (gensym)))))

(defmacro cond sexprs
  (if (null? sexprs) '()
      ((lambda (cur-clause next-clause err-msg)
         (if (not (cons? cur-clause))
             (error err-msg)
             (list 'if
                   (if (eqv? (car cur-clause) 'else)
                       (if (null? next-clause)
                           #t
                           (error (str-con err-msg
                                           " (else clause must be last).")))
                       (car cur-clause))
                   (if (null? (cdr cur-clause))
                       (error (str-con err-msg
                                       "(missing expression after test)."))
                       (car (cdr cur-clause)))
                   (cons 'cond next-clause))))
       (car sexprs)
       (cdr sexprs)
       (str-con "Invalid syntax for cond. "
                "Expected (cond (test expr)* (else expr)), but got "
                (->str (cons 'cond sexprs))))))
