(define list
  (lambda lis lis))

(define first
  (lambda (list)
    (if (null? list)
        list
        (car list))))

(define last
  (lambda (list)
    (if (null? list)
        list
        (if (null? (cdr list))
            (car list)
            (last (cdr list))))))

(define foldl
  (lambda (fn cur list)
    (if (null? list)
        cur
        (foldl fn
               (fn (car list) cur)
               (cdr list)))))

(define reverse
  (lambda (list)
    (foldl (lambda (e v)
             (cons e v))
           (quote ())
           list)))

(define map
  (lambda (fn list)
    (reverse
     (foldl (lambda (e v)
              (cons (fn e) v))
            (quote ())
            list))))

(define append
  (lambda (a b)
    (foldl cons b (reverse a))))

(define length
  (lambda (lis)
    (foldl (lambda (_ cur) (+ 1 cur))
           0
           lis)))

(define memq
  (lambda (obj lis)
    (if (eq? lis '())
        #f
    (if (eq? obj (car lis))
        lis
    (memq obj (cdr lis))))))

(define memv
  (lambda (obj lis)
    (if (eq? lis '())
        #f
    (if (eqv? obj (car lis))
        lis
    (memq obj (cdr lis))))))

(define member
  (lambda (obj lis)
    (if (eq? lis '())
        #f
    (if (equal? obj (car lis))
        lis
    (member obj (cdr lis))))))
