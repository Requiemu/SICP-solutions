;#lang racket
(load "parallel-execute")

(define account-number (list 0))

(define (get-number x)
  (set-car! x (+ (car x) 1))
  (car x))


(define (make-account-and-serializer balance)
  (let ((num (get-number account-number)))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (let ((balance-serializer (make-serializer)))
      (define (dispatch m)
        (cond ((eq? m 'withdraw) (balance-serializer withdraw))
              ((eq? m 'deposit) (balance-serializer deposit))
              ((eq? m 'balance) balance)
              ((eq? m 'serializer) balance-serializer)
              ((eq? m 'get-account-number) num)
              (else (error "Unknown request -- MAKE-ACCOUNT"
                           m))))
      dispatch)))

(define (get-account-number a) (a 'get-account-number))

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
  (if (>= (get-account-number account1) (get-account-number account2))
          ((serializer2 (serializer1 exchange))
          account1 
          account2)
          (serializer1 (serializer2 exchange))
          account1
          account2)))

