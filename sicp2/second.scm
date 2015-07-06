;some useful procedure in SICP chapter 2.

(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))

(display (enumerate-interval 2 7))
(newline)

(define (accumulate op initial sequence)
  (if (null? sequence) 
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (filter f s)
  (define (iter origin result)
    (if (null? origin) result
      (if (f (car origin))
          (iter (cdr origin) (append result (list (car origin))))
          (iter (cdr origin) result))))
  (iter s '()))

(display (filter even? '(1 2 3 4 5 6 7 8)))
(newline)

(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

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


