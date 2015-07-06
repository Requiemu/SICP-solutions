#lang planet neil/sicp

(define (looup key talble)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (insert! key value tabel)
  (let ((rexord (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons kdy value) (cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))
