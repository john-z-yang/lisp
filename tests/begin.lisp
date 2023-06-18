(begin (display 1)
  (define x 0)
  (display 2)
  (define y 1)
  (display x)
  (display y)
  (define l (list x y))
  (list x y)
  (list x y))

(display x)
(display y)
(display l)

(begin (begin
         (display 1)
         (set! x 1)
         (display 2)
         (set! y 2)
         (display x)
         (display y)
         (set! l (list x y))
         (list x y)
         (list x y)))

(display x)
(display y)
(display l)

(display (list
          (begin (+ 1 (+ 2 (+ 3 (+ 4 (+ 5 6)))))
                 y
                 x)
          (begin (begin (+ 1 (+ 2 (+ 3 (+ 4 (+ 5 6)))))
                        x
                        y))
            (begin (begin (begin (+ 1 (+ 2 (+ 3 (+ 4 (+ 5 6)))))
                        x
                        0)))))

(display (begin))
(display (begin (begin) (begin) (begin)))
(display (begin (begin (begin)) (begin (begin)) (begin (begin))))

(begin (begin
         (begin (begin (begin)) (begin (begin)) (begin (begin)))
         (begin (display (begin (begin (begin)) (begin (begin)) (begin (begin 1)))))
         (begin (begin (begin)) (begin (begin)) (begin (begin)))
         (begin (set! x (begin (begin (begin)) (begin (begin)) (begin (begin 1)))))
         (begin (begin (begin)) (begin (begin)) (begin (begin)))
         (begin (display (begin (begin (begin)) (begin (begin)) (begin (begin 2)))))
         (begin (begin (begin)) (begin (begin)) (begin (begin)))
         (begin (set! y (begin (begin (begin)) (begin (begin)) (begin (begin 2)))))
         (begin (begin (begin)) (begin (begin)) (begin (begin)))
         (begin (display (begin (begin (begin)) (begin (begin)) (begin (begin x)))))
         (begin (begin (begin)) (begin (begin)) (begin (begin)))
         (begin (display (begin (begin (begin)) (begin (begin)) (begin (begin y)))))
         (begin (begin (begin)) (begin (begin)) (begin (begin)))
         (begin (set! l (begin (begin (begin)) (begin (begin)) (begin (begin (list x y))))))
         (begin (begin (begin)) (begin (begin)) (begin (begin)))
         (begin (begin (begin)) (begin (begin)) (begin (begin (list x y))))
         (begin (begin (begin)) (begin (begin)) (begin (begin)))
         (begin (begin (begin)) (begin (begin)) (begin (begin (list x y))))
         (begin (begin (begin)) (begin (begin)) (begin (begin)))))

(display x)
(display y)
(display l)
