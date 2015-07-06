(define (reverse l)
  (define (iter origin result)
     (if (null? origin) 
         result
         (iter (cdr origin) (cons (car origin) result))))
  (iter l '()))

(display (reverse '(1 2 3 4))) 


(define (deep-reverse l)
  (cond ((number? l) l)
        ((null? l) '())
        (else (append (deep-reverse (cdr l)) (list (deep-reverse (car l)))))))

(display (deep-reverse '((1 2) (3 4))))
(newline)

(display (deep-reverse '((1 2) (3 4) (5 (6 7)))))
(newline)

;here are two identity procedure:

(define l '((1 2) (3 4) (5 (6 7))))

(display (append (list (car l)) (cdr l)));the car needs a "list" cast to append
(newline)

(display (cons (car l) (cdr l))) 
(newline)
