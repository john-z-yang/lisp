(defmacro and args
  (if (null? args) #t
      (if (null? (cdr args)) (car args)
          (list 'if
                (car args)
                (cons 'and
                      (cdr args))
                #f))))

(defmacro or args
  (define tmp (gensym))
  (if (null? args) #f
      (if (null? (cdr args)) (car args)
          (list (list 'lambda
                      (list tmp)
                      (list 'if
                            tmp
                            tmp
                            (cons 'or (cdr args))))
                (car args)))))

(defmacro cond sexprs
  (if (null? sexprs) ''()
      ((lambda ()
         (define cur-clause (car sexprs))
         (define next-clause (cdr sexprs))
         (define err-msg
           (string-append "Invalid syntax for cond. "
                    "Expected (cond (test expr)* (else expr)), but got "
                    (->str (cons 'cond sexprs))))
         (if (not (pair? cur-clause))
             (error err-msg)
             (list 'if
                   (if (eqv? (car cur-clause) 'else)
                       (if (null? next-clause)
                           #t
                           (error (string-append err-msg
                                           " (else clause must be last).")))
                       (car cur-clause))
                   (if (null? (cdr cur-clause))
                       (error (string-append err-msg
                                       "(missing expression after test)."))
                       (cons 'begin (cdr cur-clause)))
                   (cons 'cond next-clause)))))))
