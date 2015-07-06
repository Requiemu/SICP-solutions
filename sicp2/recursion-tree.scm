(define (expand-tree l)
  (cond ((number? l) (list l))
        ((null? l) '())
        (else (append (expand-tree (car l)) (expand-tree (cdr l))))))

(display (expand-tree '((1 2 3) (1 (2 3)) ((1 2) 3))))
(newline)

(define (count-leaves l)
  (cond ((number? l) 1)
        ((null? l) 0)
        (else (+ (count-leaves (car l)) (count-leaves (cdr l))))))

(display (count-leaves '((1 2 3) (1 (2 3)) ((1 2) 3))))
(newline)

