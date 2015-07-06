#lang planet neil/sicp

(define (make-monitored f)
  (let ((num 0))
    (lambda (x)
      (cond ((eq? x 'how-many-calls?) num)
            ((eq? x 'reset-count) 
             (begin (set! num 0) num))
            (else (begin (set! num (+ 1 num)) (f x) ))))))

(define mm (make-monitored sqrt))

(mm 'how-many-calls?)

(mm 4)

(mm 'how-many-calls?)

(mm 'reset-count)

(mm 'how-many-calls?)
             
