#lang planet neil/sicp

(define (make-account current password)
  (define (withdraw amount)
    (if (<= amount current)
        (begin (set! current (- current amount))
               current)
        ("insuffitient fund")))
  (define (deposit amount)
    (begin (set! current (+ current amount))
           current))
  (define (dispatch passwordin m)
    (if (eq? passwordin password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit ) deposit)
              (else (error ("unknow request -- MAKE ACCOUNT" m))))
        (lambda (x) "incorrect password")))
  dispatch)

(define tony (make-account 200 '23333))

((tony '23333 'deposit) 200)

((tony '66666 'deposit) 300)

(define (make-joint account password ano-password)
  (lambda (pass request)
    (if (equal? pass ano-password)
        (account password request)
        (account (list password 'e) request))))

(define ada (make-joint tony '23333 '66666))

((ada '66666 'deposit) 200)

((ada '23333 'deposit) 300)
