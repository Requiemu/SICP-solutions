#lang racket
(define (make-from-mag-ang mag ang)
  (lambda (op)
    (cond ((equal? op 'magnititude) mag)
          ((equal? op 'angle) ang)
          ((equal? op 'real-part) (* mag (cos ang)))
          ((equal? op 'imag-part) (* mag (sin ang)))
          (else error "procedure not implemented" op))))

(define (apply-generic op arg) (arg op))

(define ex-complex-num (make-from-mag-ang 1 3.14))

(apply-generic 'real-part ex-complex-num)

(define (real-part num) (apply-generic 'real-part num))
(define (imag-part num) (apply-generic 'imag-part num))
(define (angle num) (apply-generic 'angle num))
(define (magnititude num) (apply-generic 'magnititude num))

(real-part ex-complex-num)
(imag-part ex-complex-num)
(angle ex-complex-num)
(magnititude ex-complex-num)