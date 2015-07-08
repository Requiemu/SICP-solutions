#lang racket
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

;segments->painter

(define v1 (make-vect 1 0))

(define v2 (make-vect 0 1))

(define o (make-vect 0 0))

(define outline
  (segments->painter 
    (list (make-segment o v1) (make-segment o v2))))

(define f (make-frame (make-vect 1 1) (make-vect 2 0) (make-vect 0 1))) 

(paint outline f)