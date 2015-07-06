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
  (define dispatch
    (let ((error-times 0))
      (lambda (passwordin m)
        (if (eq? passwordin password)
            (cond ((eq? m 'withdraw) withdraw)
                  ((eq? m 'deposit ) deposit)
                  (else (error ("unknow request -- MAKE ACCOUNT" m))))
            (begin (set! error-times (+ error-times 1))
                   (if (>= error-times 7) 
                       (call-the-cops))
                   (lambda (x) "incorrect password"))))))
  dispatch)

(define (call-the-cops) (display "already call the cops"))

(define tony (make-account 200 '23333))

((tony '23333 'deposit) 200)

((tony '66666 'deposit) 300)
((tony '66666 'deposit) 300)
((tony '66666 'deposit) 300)
((tony '66666 'deposit) 300)
((tony '66666 'deposit) 300)
((tony '66666 'deposit) 300)
((tony '66666 'deposit) 300)
((tony '66666 'deposit) 300)

