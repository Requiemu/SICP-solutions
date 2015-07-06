(define (fringe tree)
  (cond ((number? tree) (list tree))
      ((null? tree) '())
      (else (append (fringe (car tree)) (fringe (cdr tree))))))

(define x '((1 2) (3 4)))

(display (fringe x))
(newline)

(display (fringe (list x x)))
(newline)
