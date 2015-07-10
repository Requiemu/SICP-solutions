#lang racket

(define (and-clauses exp) (cdr exp))
(define (or-clauses exp) (cdr exp))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (and->if exp)
  (expand-and-clauses (and-clauses exp)))

;(define (last-clause? clauses)
;  (null? (cdr clauses))

(define (expand-and-clauses clauses)
  (if (null? clauses)
      'true
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (null? rest)
            (make-if first 'true 'false)
            (make-if first 
                     (expand-and-clauses rest)
                     'false)))))

(define (or->if exp)
  (expand-or-clauses (or-clauses exp)))

(define (expand-or-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (null? rest)
            (make-if first 'true 'false)
            (make-if first
                     'true
                     (expand-or-clauses rest))))))

(define test-and (and->if (list 'and 1 1 1)))

(define test-or (or->if (list 'or 1 0 1)))

(display test-and)
(newline)

(display test-or)
(newline)
