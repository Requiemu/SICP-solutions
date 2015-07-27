#lang racket
(define (person name daughter yacht)
  (list name daughter yacht))

(define Moore (person #f Lorna))
(define Colonel (person #f 'Melissa))
(define Hall (person #f 'Rosalind))
(define Barnacle (person 'Melissa 'Gabrielle))
(define Parker (person #f #f))

;;(require (eq? (yacht (father Gabrielle))
;;              (daughter Parker))
  