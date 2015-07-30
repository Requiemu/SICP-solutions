#lang racket
(define (person name daughter yacht)
  (list name daughter yacht))

(define Moore (person #f 'Lorna))
(define Colonel (person #f 'Melissa))
(define Hall (person #f 'Rosalind))
(define Barnacle (person 'Melissa 'Gabrielle))
(define Parker (person #f #f))

(define (boat p) (caddr p))

(define (daughter p) (cadr p))

(define fathers (list Moore Colonel Hall Barnacle Parker))

;;(require (eq? (yacht (father Gabrielle))
;;              (daughter Parker))

(define (list-except l n)
  (cond ((null? l) '())
        ((eq? n (car l)) (list-except (cdr l) n))
        ((else (cons (car l) (list-except (cdr l) n))))))

(define (one-of-yachts l)
  (apply amb (map caddr l)))

(define (one-of-daughters l)
  (apply amb (map cadr l)))

(define (solutions)
  (set! (daughter Moore) (one-of-yachts
                          (list-except fathers Moore)))
  (set! (daughter Colonel) (one-of-yachts
                            (list-except fathers Colonel)))
  (set! (daughter Hall) (one-of-yachts
                         (list-except fathers Hall)))
  (let ((unknow-father) (apply amb fathers))
    (require (and (eq? (daughter unknown-father) 'Gabrielle)
                  (eq? (yacht unknown-father) (daughter Parker))))
    (list Moore Colonel Hall Barnacle Parker)))
    
    
        
        
        