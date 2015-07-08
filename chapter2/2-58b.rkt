#lang racket
;实现了对正常符号顺序，包含连加，连乘格式表达式的求导运算
(define (deriv exp var)
  (let ((exp (transform exp)))
  (cond 
        ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp) 
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum 
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp) 
         (let ((b (base exp)) (e (exponent exp)))
           (make-product (make-product e (make-exponentiation b (- e 1)))
                         (deriv b var))))
        (else
         (error "unknown expresion type -- DERIV" exp)))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        ((and (number? b) (number? e) (expt b e)))
        (else (list b '** e))))

(define (base e) (car e))

(define (exponent e) (caddr e))

(define (exponentiation? e) (eq? (cadr e) '**))

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (addend s) (car s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))

(define (multiplicand p) (caddr p))

;define a transform procedure to transform the situation to 2-58a: the two arguments situation.

(define (transform s)
  (cond ((not (pair? s)) s)
        ((all-+? s) (all-+-trans s))
        ((all-*? s) (all-*-trans s))
        ((+-*-mix? s) (+-*-trans s))
        (else (error "unknown type"))))
             
(define (*-count s)
  (define (iter l n)
    (if (eq? l '()) n
        (if (eq? (car l) '*) (iter (cdr l) (+ n 1))
            (iter (cdr l) n))))
  (if (list? s) (iter s 0)
      (begin (display s)
      (error "not a list"))))

(define (+-count s)
  (define (iter l n)
    (if (eq? l '()) n
        (if (eq? (car l) '+) (iter (cdr l) (+ n 1))
            (iter (cdr l) n))))
  (iter s 0))

(define (all-*? s)
  (and (= (+-count s) 0) (not (= (*-count s) 0))))

(define (+-*-mix? s)
  (and (not (= (+-count s) 0)) (not (= (*-count s) 0))))

(define (all-+? s)
  (and (= (*-count s) 0) (not (= (+-count s) 0))))

(define (all-+-trans s)
  (cond ((and (= (length s) 3) 
              (eq? (cadr s) '+)) 
         (list (transform (car s)) '+ (transform (caddr s))))
        ((and (> (length s) 3) 
              (eq? (cadr s) '+))
         (list (transform (car s)) '+ (transform (cddr s))))
        (else (error "+ form error"))))

(define (all-*-trans s)
  (cond ((and (= (length s) 3) 
              (eq? (cadr s) '*)) 
         (list (transform (car s)) '* (transform (caddr s))))
        ((and (> (length s) 3)
              (eq? (cadr s) '*))
         (list (transform (car s)) '* (transform (cddr s))))
        (else (error "* form error"))))


(define (+-*-trans s)
  (define (add-bracket front back n ref)
    (if (= n (- ref 1)) (append front 
                                (list (list
                                       (list-ref s (- ref 1))
                                       (list-ref s ref)
                                       (list-ref s (+ ref 1))));or draw back's first 3 element,it should be same
                                (cdddr back))
        (add-bracket (append front (list (car back))) (cdr back) (+ n 1) ref)))                           
  (define (iter s ref)
    (cond ((> (+ ref 1) (length s)) s)
          ((eq? (list-ref s ref) '*) (+-*-trans (add-bracket '() s 0 ref)))
          ((not (eq? (list-ref s ref) '*)) (iter s (+ ref 1)))
          (else (error "+-*-trans error"))))
  (cond ((all-*? s) (all-*-trans s))
        ((all-+? s) (all-+-trans s))
        ((+-*-mix? s) (iter s 0))
        (else (error "+-*-trans error"))))
          


;test

(display (deriv '(x + (3 * (x + (y + 2)))) 'x))
(newline)

(transform '(x + x + x + x * x))
(newline)

(deriv '(x + x + x + x * x) 'x)
(newline)

(deriv '(x * x * x * x) 'x)
(newline)