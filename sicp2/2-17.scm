#lang racket
(define (last-pair x)
        (if (equal? (cdr x) '())
            x
            (last-pair (cdr x))))

(display (last-pair '(1 2 3 4)))
