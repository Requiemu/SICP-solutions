#lang planet neil/sicp

(define (in-list? x alist)
  (cond ((null? alist) #f)
        ((eq? (car alist) x) #t)
        (else (in-list? x (cdr alist)))))

(define (last-pair x)
  (if (null? (cdr x)) x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define x '(a b c))

(define y '(a b c))

(display (make-cycle x))

(define (examine-cycle x)
  (define (make-examine-cycle auxiliary)
    (define (examine-cycle-inner x)
      (define (iter i)
        (cond ((null? (cdr i)) #f)
              ((in-list? i auxiliary) #t)
              (else (begin (set! auxiliary (cons i auxiliary))
                           (iter (cdr i))))))
      (iter x))
    examine-cycle-inner)
  ((make-examine-cycle '()) x))

(examine-cycle x)

(examine-cycle y)
