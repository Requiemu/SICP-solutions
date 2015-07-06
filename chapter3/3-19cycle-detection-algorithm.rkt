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
(newline)

(define (safe-cdr x)
  (if (pair? x) (cdr x)
      '()))

(define (examine-cycle x)
  (define (iter x1 x2)
    (cond ((null? x1) #f)
          ((null? x2) #f)
          ((eq? x1 x2) #t)
          ((eq? x1 (safe-cdr x2)) #t)
          (else (iter (safe-cdr x1) (safe-cdr (safe-cdr x2))))))
  (iter x (safe-cdr x)))

(examine-cycle x)

(examine-cycle y)