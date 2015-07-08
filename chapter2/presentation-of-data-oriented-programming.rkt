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

(define (square x) (* x x))

(define (attach-tag tag x) (cons tag x))

(define (install-rectangular-package)
  ;;internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;;interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;;internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;;interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (type-tag x) (car x))

(define (contents x) (cdr x))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let　((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types --APPLY-GENERIC"
           (list op type-tags))))))
;here, the method seems can be use to multiple args, but indeed it can't...

(define (real-part z) (apply-generic 'real-part z))

(define (imag-part z) (apply-generic 'imag-part z))

(define (magnitude z) (apply-generic 'magnitude z))

(define (angle z) (apply-generic 'angle z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))

(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

(install-rectangular-package)

(install-polar-package)

(define r-i (make-from-real-imag 3 4))

(magnitude r-i)




