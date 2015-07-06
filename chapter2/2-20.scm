(define (same-parity . w)
  (define (iter-odd a b)
    (cond ((null? a) b)
          ((odd? (car a)) (iter-odd (cdr a) (append b (list (car a)))))
          (else (iter-odd (cdr a) b))))
  (define (iter-even a b)
    (cond ((null? a) b)
          ((even? (car a)) (iter-even (cdr a) (append b (list (car a)))))
          (else (iter-even (cdr a) b))))
  (cond ((null? w) '())
        ((even? (car w)) (iter-even w '()))
        (else (iter-odd w '()))))

(display (same-parity 1 2 3 4 5 6 7 8 9 0))
(newline)

(display (same-parity 2 2 3 4 5 6 7 8 9 0))

