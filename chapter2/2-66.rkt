#lang racket

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (key record) record)

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) #f)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        ((< given-key (key (car set-of-records)))
         (lookup given-key (left-branch set-of-records)))
        ((> given-key (key (car set-of-records)))
         (lookup given-key (right-branch set-of-records)))))
         

(define t1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))

(define t2 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))

(define t3 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))));this is not a standard binary tree defined in the book

(define t4 '(5 (1 () (11 () ())) (7 (3 () ()) (9 () ()))));this is not a standard binary tree defined in the book

(define t5 '(6 (2 () (4 () ())) (10 (8 () ()) (12 () ()))))

(define t6 '(6 (2 (1 () ()) (4 () (5 () ()))) (10 (8 () ()) (12 () ()))))

(lookup 4 t1)
(newline)

(lookup 7 t2)
(newline)
