#lang planet neil/sicp
;刚开始试图自己先创建一个table用，发现并不好创建，于是直接找了3-24的答案，其中包括table一套方法,但是还有的地
;方需要修改--主要是修改了eps-equal？的内容。
;从这道题开始采用#lang planet neil/sicp，这是一个专门为racket适配了sicp的包。
;(define (make-from-real-img (real-part z) (imag-part z))

;(define (make-from-mag-ang (magnitude z) (angle z))

;(define (add-complex z1 z2);  (make-from-real-imag (

;the answer:

;a.Because the number? and variable don't have a tag.

;(define (list-set! l position element)
;  (define (iter front back n e)
;    (if (= n 0) (append front (cons e (cdr back)))
;        (iter (append front (list (car back))) (cdr back) (- n 1) e)))
;  (iter '() l position element))
;
;(list-set! '(1 2 3 4 5) 1 6)
;
;(define (list-set!-2 l position-row position-col element)
;  (list-set! l position-col (list-set! (list-ref l position-col) position-row element))) 
;
;(list-set!-2 '((1 2) (3 4)) 0 1 'e)
;
;(list-position '(1 2 3) 2)
;
;(define row '(deriv));global
;
;(define table (list (list 'sum #f) (list 'product #f)));global
;
;(define (put row-element col-element content)
;  (if (not (list-position (map (lambda (x) (car x)) table) col-element))
;      
;      (list-set!-2 table 
;                   (+ (list-position row row-element) 1)
;                   (list-position (map (lambda (x) (car x)) table) col-element)
;                   content))
;  
;

;(define (eps-equal? a b)
;  (< (abs (- a b)) 0.1))

(define (eps-equal? a b)
  (equal? a b))

(define (my-assoc key records equal-func)
  (cond ((null? records) #f)
        ((equal-func key (caar records)) (car records))
        (else (my-assoc key (cdr records) equal-func))))

(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    
    (define (lookup key-1 key-2)
      (let ((subtable (my-assoc key-1 (cdr local-table) equal?)))
        (if subtable
            (let ((record (my-assoc key-2 (cdr subtable) same-key?)))
              (if record
                  (cdr record)
                  #f))
            #f)))
    
    (define (insert! key-1 key-2 value)
      (let ((subtable (my-assoc key-1 (cdr local-table) equal?)))
        (if subtable
            (let ((record (my-assoc key-2 (cdr subtable) same-key?)))
              (if record
                  (set-cdr! record value) 
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc) insert!)
            ((eq? m 'print) local-table)
            (else (error "Undefined operation -- TABLE" m))))
    dispatch))

(define table (make-table eps-equal?))
(define get (table 'lookup-proc))
(define put (table 'insert-proc))

;(put 'math '5.43 43)
;(put 'math '6.43 45)
;(put 'math '7.43 42)
;
;(put 'letters '8.43 97)
;(put 'letters '9.43 98)
;
;(table 'print)
;
;(put 'math '7.42 142)
;
;(table 'print)
;The answer for the question starts here:

;a. Because the number? variable? type have no tag.

;b.

;(define (type-tag x) (car x))
;
;(define (contents x) (cdr x))
;
;(define (apply-generic op . args)
;  (let ((type-tags (map type-tag args)))
;    (let　((proc (get op type-tags)))
;      (if proc
;          (apply proc (map contents args))
;          (error
;           "No method for these types --APPLY-GENERIC"
;           (list op type-tags))))))
;here, the method seems can be use to multiple args, but indeed it can't...

(define (variable? x) (symbol? x))

(define (same-variable? a b)
  (and (variable? a) (variable? b) (eq? a b)))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp)
                                           var))))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (install-sum-package)
  ;;internal procedure
  (define (addend expr) (car expr))
  (define (augend expr) (cadr expr))
  (define (deriv-sum expr var)
    (make-sum 
     (deriv (addend expr) var)
     (deriv (augend expr) var)))
  (define (make-sum x y) (list '+ x y)) 
  ;;the interface for the rest of the procedure
  (put 'make-sum '+ make-sum)
  (put 'deriv '+ deriv-sum)
  'done)

(install-sum-package)

(define make-sum (get 'make-sum '+))

(deriv '(+ x x) 'x)

(define (install-product-package)
  ;;internal procedure
  (define (multiplier expr) (car expr))
  (define (multiplicand expr) (cadr expr))
  (define (make-product x y)
    (list '* x y))
  (define (deriv-product expr var)
    (let ((er (multiplier expr))
          (d (multiplicand expr)))
      (make-sum 
       (make-product (deriv er var) d)
       (make-product (deriv d var) er))))
  ;;the interface for the rest of the procedure
  (put 'make-product '* make-product)
  (put 'deriv '* deriv-product)
  'done)

(install-product-package)

(define make-product (get 'make-product '*))



(deriv '(* x x) 'x)

;c.
(define (install-expotential-package)
  ;;internal procedure
  (define (base expr) (car expr))
  (define (exponent expr) (cadr expr))
  (define (make-expotential base exponent)
    (list '** base exponent))
  (define (deriv-expotential expr var)
    (let ((b (base expr)) (e (exponent expr)))
      (make-product e
                    (make-expotential b (- e 1)))))
  ;;the interface for the rest of the procedure
  (put 'make-expotential '** make-expotential)
  (put 'deriv '** deriv-expotential)
  'done)

(install-expotential-package)

(define make-expotential (get 'make-expotential '**))

(deriv '(** x 3) 'x)

;d

;Seems just need to change the row and colum...

       
  
  
  
  
  
  
  
  