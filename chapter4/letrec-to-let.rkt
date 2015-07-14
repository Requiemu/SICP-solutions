#lang racket
(define (make-produce-symbol n)
  (define make 
    (lambda () 
      (set! n (+ n 1))
      (string->symbol (number->string n))))
  make)

(define sy (make-produce-symbol 1))

(define (letrec->let exp)
  (let ((let-out (map (lambda (x) (list (car x) "*unassigned*")) (cadr exp)))
        (let-in  (map (lambda (x) (list (sy) (cadr x))) (cadr exp))))
    (list 'let 
          let-out
           (cons 'let
                 (cons let-in 
                       (append 
                        (map (lambda (x y) (list 'set! (car x) (car y)))
                             let-out let-in)
                        (cddr exp)))))))

(display (letrec->let '(letrec ((x 1) (y 2)) (+ x y))))

(let ((x *unassigned*) (y *unassigned*)) (let ((2 1) (3 2)) (set! x 2) (set! y 3) (+ x y)))