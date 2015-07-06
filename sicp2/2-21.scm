(define (square-list l)
  (map (lambda (x) (* x x)) l))

(display (square-list '(1 2 3 4 5 6 7 8 9)))
(newline)

(define (square x) (* x x))

(define (square-list l)
  (if (null? l)
      null
      (cons (square (car l)) (square-list (cdr l)))))

(display (square-list '(1 2 3 4 5 6 7 8 9)))
