#lang planet neil/sicp

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (begin (set-cdr! x y)
                 (display x)
                 (display "<---")
                 (display y)
                 (newline)
                 (display temp)
                 (newline)
                 (loop temp x)))))
  (loop x '()))

(define v '(a b c d))

(define w (mystery v))

(display v)
(newline)

(display w)
(newline)

(define (change x)
  (if (null? (cdr x))
      x
      (begin 
        (set-cdr! x (cddr x))
        (change x))))

(define t '(a b c d))

(display (change t))

(display t)
            