#lang racket
(define (list-set! l n m)
  (define (iter origin result k)
    (if (null? origin) 
        result
        (if (= k n) 
            (iter (cdr origin) (append result (list m)) (+ k 1))
            (iter (cdr origin) (append result (list (car origin))) (+ k 1)))))
  (iter l '() 0))

(display (list-set! '(1 2 3 4 5 6) 3 3))
(newline)

(define (accumulate op initial sequence)
  (if (null? sequence) 
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

;(define (rest-of-queens---------------------------------------------

(define (adjoin-position new-row k rest-of-queens)
  (append rest-of-queens (list new-row)))



(define empty-board '())

(define (safe? k positions) ;positions is one solution having k columns
  (define (iter i result)   ;(iter 1 #t)
    (let ((s (list-ref positions (- i 1))) (e (list-ref positions (- k 1))))
      (if (= i k) 
          result
          (iter (+ i 1) (and result (not (= s e)) (not (= (abs (- s e)) (abs (- i k)))))))))
  (iter 1 #t))

;this method just

(display (safe? 3 '(1 2 4))) 
(newline)

(define (display-list l)
  (define (iter li)
    (if (null? (cdr li))
        (begin (display (car li))
               (newline))
        (begin (display (car li))
               (newline)
               (iter (cdr li)))))
  (iter l))

(display-list (queens 8))







