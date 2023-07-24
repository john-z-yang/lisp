(define caar        (lambda (x) (car (car x))))
(define cadr        (lambda (x) (car (cdr x))))
(define cdar        (lambda (x) (cdr (car x))))
(define cddr        (lambda (x) (cdr (cdr x))))
(define caaar       (lambda (x) (car (car (car x)))))
(define caadr       (lambda (x) (car (car (cdr x)))))
(define cadar       (lambda (x) (car (cdr (car x)))))
(define caddr       (lambda (x) (car (cdr (cdr x)))))
(define cdaar       (lambda (x) (cdr (car (car x)))))
(define cdadr       (lambda (x) (cdr (car (cdr x)))))
(define cddar       (lambda (x) (cdr (cdr (car x)))))
(define cdddr       (lambda (x) (cdr (cdr (cdr x)))))
(define caaaar      (lambda (x) (car (car (car (car x))))))
(define caaadr      (lambda (x) (car (car (car (cdr x))))))
(define caadar      (lambda (x) (car (car (cdr (car x))))))
(define caaddr      (lambda (x) (car (car (cdr (cdr x))))))
(define cadaar      (lambda (x) (car (cdr (car (car x))))))
(define cadadr      (lambda (x) (car (cdr (car (cdr x))))))
(define caddar      (lambda (x) (car (cdr (cdr (car x))))))
(define cadddr      (lambda (x) (car (cdr (cdr (cdr x))))))
(define cdaaar      (lambda (x) (cdr (car (car (car x))))))
(define cdaadr      (lambda (x) (cdr (car (car (cdr x))))))
(define cdadar      (lambda (x) (cdr (car (cdr (car x))))))
(define cdaddr      (lambda (x) (cdr (car (cdr (cdr x))))))
(define cddaar      (lambda (x) (cdr (cdr (car (car x))))))
(define cddadr      (lambda (x) (cdr (cdr (car (cdr x))))))
(define cdddar      (lambda (x) (cdr (cdr (cdr (car x))))))
(define cddddr      (lambda (x) (cdr (cdr (cdr (cdr x))))))

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
