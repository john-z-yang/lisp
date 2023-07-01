(defmacro let sexprs
  (define err-msg
    (str-con "Invalid syntax for let. "
             "Expected (let ((sym expr)*) expr), but got "
             (->str (cons 'let sexprs)) ". "))
  (if (< (length sexprs) 2) (error (str-con err-msg))
      ((lambda ()
         (define binding-pairs (validate-binding-pairs-- (car sexprs) err-msg))
         (define body (cdr sexprs))
         (define syms
           (map (lambda (binding-pair) (car binding-pair))
                binding-pairs))
         (define init-exprs
           (map (lambda (binding-pair) (car (cdr binding-pair)))
                binding-pairs))
         (cons (cons 'lambda (cons syms body)) init-exprs)))))

(defmacro let* sexprs
  (let ((err-msg (str-con "Invalid syntax for let*. "
                          "Expected (let* ((sym expr)*) expr), but got "
                          (->str (cons 'let* sexprs)) ". ")))
    (cond ((< (length sexprs) 2)
           (error err-msg))
          ((null? (car sexprs))
           (cons 'begin (cdr sexprs)))
          (else
           ((lambda ()
              (define binding-pairs (validate-binding-pairs-- (car sexprs) err-msg))
              (define cur-binding-pair (car binding-pairs))
              (define rem-binding-pair (cdr (car sexprs)))
              (list 'let
                    (list cur-binding-pair)
                    (cons 'let*
                          (cons rem-binding-pair
                                (cdr sexprs))))))))))
