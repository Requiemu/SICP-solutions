#lang racket
(define (person name daughter yacht)
  (list name daughter yacht))

(define Moore (person #f Lorna))
(define Colonel (person #f 'Melissa))
(define Hall (person #f 'Rosalind))
(define Barnacle (person 'Melissa 'Gabrielle))
(define Parker (person #f #f))

(define (boat p) (caddr p))

(define (daughter p) (cadr p))

(define fathers (list Moore Colonel Hall Barnacle Parker))

;;(require (eq? (yacht (father Gabrielle))
;;              (daughter Parker))

(define (solutions
  