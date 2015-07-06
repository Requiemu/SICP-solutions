(define (subsets s)
  (if (null? s)
      '(())
      (let ((rest (subsets (cdr s))))
         (append rest 
                 (map (lambda (element) (append (list (car s)) element))
                      rest)))))

(define t '(1 2 3))

(display (subsets t))
(newline)

(define t0 '((1 2) (2 3) (3 4)))

(display (subsets t0))
(newline)

