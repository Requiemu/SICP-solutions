#lang planet neil/sicp

(define a (cons 1 '()))

(define b (cons 2 a))

(set-cdr! a b)