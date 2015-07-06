#lang planet neil/sicp
(define new-withdraw
  (let ((current 100))
    (lambda (amount)
      (if (<= amount current)
          (begin (set! current (- current amount))
                 current)
          "insufficient fund"))))

(new-withdraw 25)

(new-withdraw 25)

(new-withdraw 75)

(define (withdraw-test amount)
  (define current 100)
  (if (<= amount current)
      (begin (set! current (- current amount))
             current)
      "insufficient fund"))

(withdraw-test 25)

(withdraw-test 25)

(withdraw-test 75)

(define (new-withdraw1 amount)
  (let ((current 100))
    (if (<= amount current)
        (begin (set! current (- current amount))
               current)
        "insufficient fund")))

(new-withdraw1 25)

(new-withdraw1 25)

(new-withdraw1 75)

(define (make-account balance)
  (define (withdraw amount)
    (if (<= amount balance) 
        (begin (set! balance (- balance amount))
               balance)
        ("insuffitient fund")))
  (define (deposit amount)
    (begin (set! balance (+ balance amount))
           balance))
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit ) deposit)
          (else (error "unknow request --MAKE-ACCOUNT" m))))
  dispatch)

(define tony (make-account 300))

((tony 'withdraw) 200)

((tony 'withdraw) 99)
        
                     