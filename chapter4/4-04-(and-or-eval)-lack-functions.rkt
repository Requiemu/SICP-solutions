#lang racket

;;(define (true? exp) (...))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (first-clause clauses) (car clauses))
(define (rest-clauses clauses) (cdr clauses))

(define (and? exp) (tagged-list? exp 'and))
(define (and-clauses exp) (cdr exp))

(define (eval-and exp env)
  (define (iter clauses)
    (if (null? clauses) #t
        (if (true? (eval (first-clause clauses)))
            (iter (rest-clauses clauses))
            #f)))
  (if (null? exp) 
      (error "no argument in EVAL-AND" exp) 
      (iter (and-clauses exp))))

(define (false? exp) (not (true? exp)))

(define (or? exp) (tagged-list? exp 'or))
(define (or-clauses exp) (cdr exp))

(define (eval-or exp env)
  (define (iter clauses)
    (if (null? clauses) #f
        (if (false? (eval (first-clause clauses)))
            (iter (rest-clauses clauses))
            #t)))
  (if (null? exp)
      (error "no argument in EVAL-OR" exp)
      (iter (or-clauses exp))))

;;================================
;;expand

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


