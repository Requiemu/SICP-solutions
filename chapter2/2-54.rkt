#lang racket
(display (eq? '(this is a list) '(this is a list)))
(newline)
(display (equal? '(this is a list) '(this is a list)))
(newline)

(define (new-equal? list1 list2)
  (if (not (and (pair? list1) (pair? list2))) (eq? list1 list2)
      (if (eq? (car list1) (car list2)) (new-equal? (cdr list1) (cdr list2))
          #f)))

(display (new-equal? '(this is a list) '(this is a list)))
(newline)

(display (new-equal? '(this is a list) '(this (is a) list)))