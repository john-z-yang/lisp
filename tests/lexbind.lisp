(define x #f)

(display (let () (+ 1 2)))
(display (let ((x 0)) x))
(display (let ((x 1) (y 2) (z 3)) (display "hello") (display "world") (list x y z)))
(display (let ((x 1) (y 2) (z 3)) (list z y x)))
(display (let ((x 1) (y 2) (z 3)) (list y z x)))
(display (let ((a 1) (b 2) (c 3) (d 4)) (display "hello") (display "world") (list a b c d)))
(display (let ((a 1) (b 2) (c 3) (d 4)) (list d b c a)))
(display (let ((a 1) (b 2) (c 3) (d 4)) (list c a d b)))

(display (let* () (+ 1 2)))
(display (let* ((x 0)) x))
(display (let* ((x 1) (y 2) (z 3)) (display "hello") (display "world") (list x y z)))
(display (let* ((x 1) (y 2) (z 3)) (list z y x)))
(display (let* ((x 1) (y 2) (z 3)) (list y x z)))
(display (let* ((a 1) (b 2) (c 3) (d 4)) (display "hello") (display "world") (list a b c d)))
(display (let* ((a 1) (b 2) (c 3) (d 4)) (list d b c a)))
(display (let* ((a 1) (b 2) (c 3) (d 4)) (list c a d b)))
(display (let* ((x 0) (y (+ 1 x)) (z (+ 1 y))) (display "hello") (display "world") (list x y z)))
(display (let* ((x 0) (y (+ 1 x)) (z (+ 1 y))) (list z y x)))
(display (let* ((x 0) (y (+ 1 x)) (z (+ 1 y))) (list y x z)))

(display (let* ((x 0)) (display "hello") (let* ((y (+ 1 x))) (display "world") (let* ((z (+ 1 y))) (display "hello") (list x y z)))))
(display (let* ((x 0)) (let* ((y (+ 1 x))) (let* ((z (+ 1 y))) (list z y x)))))
(display (let* ((x 0)) (let* ((y (+ 1 x))) (let* ((z (+ 1 y))) (list y x z)))))

(display (let* ((x 0)) (let* ((x (+ 1 x))) (let* ((x (+ 1 x))) x))))

(letrec ((factorial
       (lambda (n)
         (if (<= n 1)
        1
        (* n (factorial (- n 1)))))))
  (display (factorial 8)))

(letrec ((even?
          (lambda (n)
            (if (= 0 n)
                #t
                (odd? (- n 1)))))
         (odd?
          (lambda (n)
            (if (= 0 n)
                #f
                (even? (- n 1))))))
  (display (map even? (list 0 1 2 3 4 5 6 7 8)))
  (display (map odd? (list 0 1 2 3 4 5 6 7 8))))

(letrec () (display "?"))
