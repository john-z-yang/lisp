(define split
  (lambda (l)
    (cond ((null? l)
           (list '() '()))
          ((null? (cdr l))
           (list l '()))
          (else
           (let ((rest (split (cdr (cdr l)))))
              (list (cons (car l) (car rest))
                    (cons (car (cdr l)) (car (cdr rest)))))))))


(define merge
  (lambda (l1 l2)
    (cond ((null? l1)
           l2)
          ((null? l2)
           l1)
          ((< (car l1) (car l2))
           (cons (car l1) (merge (cdr l1) l2)))
          (else
           (cons (car l2) (merge (cdr l2) l1))))))


(define merge-sort
  (lambda (l)
    (cond ((null? l)
           '())
          ((null? (cdr l))
           l)
          (else
           (let* ((s (split l))
                  (l1 (car s))
                  (l2 (car (cdr s))))
             (merge (merge-sort l1) (merge-sort l2)))))))

(define dec-range (lambda (a) (if (= a 0) (quote ()) (cons a (dec-range (- a 1))))))

(display (merge-sort (dec-range 512)))
(newline)
