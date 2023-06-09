(define validate-binding-pairs--
  (lambda (binding-pairs err-msg)
    (if (and (not (cons? binding-pairs))
             (not (null? binding-pairs)))
        (error (str-con err-msg "Binding pairs must be in a list."))
        ((lambda (validate-binding-pair)
           (map (lambda (e) (validate-binding-pair e))
                binding-pairs))
         (lambda (binding-pair)
           (cond ((not (cons? binding-pair))
                  (error (str-con err-msg
                                  "Each binding pair must be a list.")))
                 ((not (sym? (car binding-pair)))
                  (error (str-con err-msg
                                  "First element of a binding pair "
                                  "must be a symbol).")))
                 ((not (= (length binding-pair) 2))
                  (error (str-con err-msg
                                  "Binding pairs must be of list length 2.")))
                 (else binding-pair)))))))

(defmacro let sexprs
  ((lambda (err-msg)
     (if (not (= (length sexprs) 2)) (error (str-con err-msg))
         ((lambda (binding-pairs body)
            ((lambda (syms init-exprs)
               (cons (list 'lambda
                           syms
                           body)
                     init-exprs))
             (map (lambda (binding-pair) (car binding-pair))
                  binding-pairs)
             (map (lambda (binding-pair) (car (cdr binding-pair)))
                  binding-pairs)))
          (validate-binding-pairs-- (car sexprs) err-msg)
          (car (cdr sexprs)))))
   (str-con "Invalid syntax for let. "
            "Expected (let ((sym expr)*) expr), but got "
            (->str (cons 'let sexprs)) ". ")))

(defmacro let* sexprs
  (let ((err-msg (str-con "Invalid syntax for let*. "
                          "Expected (let* ((sym expr)*) expr), but got "
                          (->str (cons 'let* sexprs)) ". ")))
    (cond ((not (= (length sexprs) 2))
           (error err-msg))
          ((null? (car sexprs))
           (car (cdr sexprs)))
          (else
           (let ((binding-pairs (validate-binding-pairs-- (car sexprs) err-msg)))
             (let ((binding-pair (car binding-pairs))
                   (expr (car (cdr sexprs))))
                  (list 'let
                        (list binding-pair)
                              (cons 'let*
                                    (cons (cdr (car sexprs))
                                          (cdr sexprs))))))))))
