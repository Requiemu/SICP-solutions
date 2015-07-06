(define (reverse x)
  (define (iter a b)
    (if (null? a)
        b
        (iter (cdr a) (cons (car a) b))))
  (iter x '()))

(display (reverse '(1 2 3 4)))
