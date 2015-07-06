;this will work

(define (square x) (* x x))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (append answer
              (list (square (car things)))))))
  (iter items null))

(display (square-list '(1 2 3 4 5 6 7 8 9)))
(newline)
