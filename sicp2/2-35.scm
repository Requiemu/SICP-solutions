(define (fringe tree)
  (cond ((number? tree) (list tree))
      ((null? tree) '())
      (else (append (fringe (car tree)) (fringe (cdr tree))))))

(define (accumulate op initial sequence)
  (if (null? sequence) 
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (count-leaves t)
  (accumulate (lambda (x y) (+ y 1)) 0 (fringe t)))

(display (count-leaves '((1 2 3) (2 3) 4 ((1 2) 3)))) 
(newline)

;Here is the answer obey descriptionS of the question, but it's not so straightforward as the one above.

(define (count-leaves t)
  (accumulate (lambda (x y) (+ (length x) y)) 0 (map fringe t)))

(display (count-leaves '((1 2 3) (2 3) 4 ((1 2) 3)))) 
(newline)  
