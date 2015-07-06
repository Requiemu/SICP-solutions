#lang planet neil/sicp

(define (in-list? x alist)
  (cond ((null? alist) #f)
        ((eq? (car alist) x) #t)
        (else (in-list? x (cdr alist)))))

(define (make-count-pairs auxiliary)
  (define (count-pairs x)
    (cond ((not (pair? x)) 0)
          ((in-list? x auxiliary) 0)
          (else (begin (set! auxiliary (cons x auxiliary))
                       (display auxiliary)
                       (newline)
                       (+ (count-pairs (car x))
                          (count-pairs (cdr x))
                          1)))))
  count-pairs)

(define count-pairs (make-count-pairs '()))

(define x (cons 'x 'w))

(define a (cons x x))

(count-pairs a)
                       