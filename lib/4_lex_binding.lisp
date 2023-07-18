(defmacro let (binding-pairs . body)
  (define err-msg
    (str-con "Invalid syntax for let. "
             "Expected (let ((sym expr)*) expr), but got "
             (->str (cons 'let (cons binding-pairs body))) ". "))
  (define binding-pairs (validate-binding-pairs-- binding-pairs err-msg))
  (define syms
    (map (lambda (binding-pair) (car binding-pair))
         binding-pairs))
  (define init-exprs
    (map (lambda (binding-pair) (car (cdr binding-pair)))
         binding-pairs))
  (cons (cons 'lambda (cons syms body)) init-exprs))

(defmacro let* (binding-pairs . body)
  (let ((err-msg (str-con "Invalid syntax for let*. "
                          "Expected (let* ((sym expr)*) expr), but got "
                          (->str (cons 'let* (cons binding-pairs body))) ". ")))
    (cond ((null? binding-pairs)
           (cons 'begin body))
          (else
            (let ((binding-pairs (validate-binding-pairs-- binding-pairs err-msg))
                 (cur-binding-pair (car binding-pairs))
                 (rem-binding-pair (cdr binding-pairs)))
                 (list 'let
                    (list cur-binding-pair)
                    (cons 'let*
                          (cons rem-binding-pair
                                body))))))))

(defmacro letrec (binding-pairs . body)
  (let ((err-msg (str-con "Invalid syntax for letrec. "
                          "Expected (letrec ((sym expr)*) expr), but got "
                          (->str (cons 'letrec (cons binding-pairs body))) ". ")))
    (cond ((null? binding-pairs)
           (cons 'begin body))
          (else
           (let ((binding-pairs (validate-binding-pairs-- binding-pairs err-msg)))
             (cons 'let
                   (cons (map (lambda (binding-pair)
                                (list (car binding-pair) ''#<undefined>))
                              binding-pairs)
                         (append (map (lambda (binding-pair)
                                      (list 'set!
                                            (car binding-pair)
                                            (car (cdr binding-pair))))
                                     binding-pairs)
                                  body))))))))
