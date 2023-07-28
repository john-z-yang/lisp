(define validate-binding-pairs--
  (lambda (binding-pairs err-msg)
    (if (and (not (cons? binding-pairs))
             (not (null? binding-pairs)))
        (error (string-append err-msg "Binding pairs must be in a list."))
        ((lambda ()
           (define validate-binding-pair
             (lambda (binding-pair)
               (cond ((not (cons? binding-pair))
                      (error (string-append err-msg
                                      "Each binding pair must be a list.")))
                     ((not (sym? (car binding-pair)))
                      (error (string-append err-msg
                                      "First element of a binding pair "
                                      "must be a symbol).")))
                     ((not (= (length binding-pair) 2))
                      (error (string-append err-msg
                                      "Binding pairs must be of list length 2.")))
                     (else binding-pair))))
           (map (lambda (e) (validate-binding-pair e))
                binding-pairs))))))
