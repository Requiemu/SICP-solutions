#lang planet neil/sicp

;;address transmit

(define (f x) (set-car! x 'iii))
(define b (cons (cons 1 2) (cons 3 4)))
(f (car b))
(display b)

(let ((a b))
  (f (cdr a)))

(display b)