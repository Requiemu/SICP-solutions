#lang planet neil/sicp
;;这道题完成得不好,只实现了sparse数据类型的div除法,且进行除法的多项式的系数和指数都只能是整数类型(导致了整数除法相除所得商一定是整数带一个余数从使得结果不精确,但要实现非整数情况也成立就必须对几种数据结构全部定义大于小于和等于运算,比较麻烦,所以本题暂时先实现到这里,有机会的话,需要修改一下两点:
;;1.增加所有数据类型的大于小于等于运算,从而使多项式运算中的系数和指数能够是各种数据类型。
;;2.将各种与数据相关联的运算封装到相关数据包中,而不是一种运算单独成立一个包,这样这些运算可以调用相关数据的内部函数。本系统中有许多地方无法调用数据内部函数,直接根据数据结构用car cdr代替,这是不合理的。或者反过来说,凡是需要调用数据结构内部函数的相关函数,在定义的时候一定要放在数据结构的package中。
;;2015-6-12补充:以上第一个问题已通过加入一个real的package部分解决,即使得多项式指数和系数 可以为integer和real。

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

;;above is the implementation of methods related to the "table".
;;apply-generic procedure' definition:

(define (make-list n x)
  (define (iter m result)
    (if (= m 0) result
        (iter (- m 1) (cons x result))))
  (iter n '()))

(define (square x) (* x x))

(define (type-tag x) 
  (if (pair? x)
      (car x)
      (cond ((and (integer? x) (exact? x)) 'integer)
            ((inexact? x) 'real)
            (else 
             (error "Bad tagged datum -- TYPE-TAG" x)))))

(define (attach-tag tag x)
  (if (and (not (pair? x)) (number? x))
      (cond ((and (integer? x) (exact? x)) x)
            ((inexact? x) x)
            (else (cons tag x)))
      (cons tag x)))

(define (content arg) 
  (if (pair? arg)
      (cdr arg)
      (cond ((and (exact? arg) (integer? arg)) arg)
            ((inexact? arg) arg)
            (else (error "Bad tagged datum -- CONTENTS" arg)))))


(define (all-same? l)
  (define (iter t)
    (cond ((null? t) #t)
          ((not (equal? (car t) (car l))) #f)
          (else (iter (cdr t)))))
  (iter (cdr l)))


(define (true-map proc sequence) 
  (define (true-map-iter proc sequence result) 
    (if (null? sequence) 
        (reverse result) 
        (let ((item (proc (car sequence)))) 
          (if item 
              (true-map-iter proc (cdr sequence) (cons item result)) 
              #f)))) 
  (true-map-iter proc sequence '())) 

;(define (apply-generic op . args) 
;  (define (iter type-tags args) 
;    (if (null? type-tags) 
;        (error "No method for these types-ITER") 
;        (let ((type1 (car type-tags))) 
;          (let ((filtered-args (true-map (lambda (x) 
;                                           (let ((type2 (type-tag x))) 
;                                             (if (eq? type1 type2) 
;                                                 x 
;                                                 (let ((t2->t1 (get type2 type1))) 
;                                                   (if t2->t1 (t2->t1 x) #f))))) 
;                                         args))) 
;            (or filtered-args 
;                (iter (cdr type-tags) args)))))) 
;  (let ((type-tags (map type-tag args))) 
;    (if (and 
;         (> (length type-tags) 2) 
;         (all-same? type-tags))
;        (let ((proc (get op (list (car type-tags)
;                                  (cadr type-tags))))) 
;          (if proc 
;              (apply proc (map content args)) 
;              (apply apply-generic (cons op (iter type-tags args)))))
;        (let ((proc (get op type-tags))) 
;          (if proc 
;              (apply proc (map content args))
;              (if (= (length type-tags) 1) #f;;here???
;                  (apply apply-generic (cons op (iter type-tags args)))))))))

(define (apply-generic op . args)  
  (let ((type-tags (map type-tag args))) 
    (if (and 
         (>= (length type-tags) 2) 
         (all-same? type-tags))
        (let ((proc (get op (list (car type-tags)
                                  (cadr type-tags))))) 
          (if proc 
              (drop (apply proc (map content args))) 
              (error "no procedure for multiple" (list op (car type-tags)))))
        (let ((proc (get op type-tags))) 
          (if proc 
              (apply proc (map content args))
              (if (= (length type-tags) 1) #f;;here???
                  (drop (apply apply-generic (cons op (list-to-max args))))))))))

(define (apply-generic1 op . args)  
  (let ((type-tags (map type-tag args))) 
    (if (and 
         (>= (length type-tags) 2) 
         (all-same? type-tags))
        (let ((proc (get op (list (car type-tags)
                                  (cadr type-tags))))) 
          (if proc 
              (apply proc (map content args)) 
              (error "no procedure for multiple" (car type-tags))))
        (let ((proc (get op type-tags))) 
          (if proc 
              (apply proc (map content args))
              (if (= (length type-tags) 1) #f;;here???
                  (apply apply-generic1 (cons op (list-to-max args)))))))))

(define (add . args) (apply apply-generic (cons 'add args)))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

;;integer, rational, complex package 


(define (install-integer-package)
  (define (tag x)
    (attach-tag 'integer x))
  (put 'make 'integer
       (lambda (x) (tag x)))
  (put 'add '(integer integer)
       +)
  (put 'sub '(integer integer)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer)
       (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer)
       (lambda (x y) (tag (/ (- x (remainder x y)) y))))
  'done)

(install-integer-package)

(define make-integer (get 'make 'integer))

(define (install-real-package)
  (define (tag x)
    (attach-tag 'real x))
  (put 'make 'real
       (lambda (x) (tag x)))
  (put 'add '(real real)
       +)
  (put 'sub '(real real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
       (lambda (x y) (/ x y)))
  'done)

(install-real-package)

(define make-real (get 'make 'real))

(define (install-rational-package)
  ;;internal procidures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (cons n d))
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (add (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))
  (define (project-rational-to-integer xra)
    (if (and (number? (numer xra)) (number? (denom xra)))
        (inexact->exact (round (div (numer xra)
                                    (denom xra))))
        #f))
  ;;interface to test of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make '(rational)
       (lambda (n d) (tag (make-rat n d))))
  (put 'numer '(rational) numer)
  (put 'denom '(rational) denom)
  (put 'project '(rational) 
       (lambda (x) (attach-tag 'integer (project-rational-to-integer x))))
  'done)

(install-rational-package)

(define make-rational (get 'make '(rational)))

(define (numer x) (apply-generic 'numer x))

(define (denom x) (apply-generic 'denom x))

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

(install-polar-package)

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
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(install-rectangular-package)

(define (real-part z)
  (apply-generic 'real-part z))

(define (imag-part z)
  (apply-generic 'imag-part z))

(define (magnitude z)
  (apply-generic 'magnitude z))

(define (angle z)
  (apply-generic 'angle z))

(define (install-complex-package)
  ;;imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;;internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  (define (integer->complex n)
    (make-complex-from-real-imag (content n) 0))
  ;;interface to the rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex) 
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'integer 'complex integer->complex)
  'done)

(install-complex-package)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(display "--------2.83--------")
(newline)

;;raise-package's definition:

(define (install-raise-package)
  ;;the internal procedure
  (define (raise-integer-to-rational i)
    (make-rational i 1))
  (define (raise-rational-to-real rat)
    (attach-tag 'real (exact->inexact (/ (car rat) (cdr rat)))))
  (define (raise-real-to-complex re)
    (make-complex-from-real-imag re 0))
  ;the main modification for 2.90, make the operation between dense and sparse possible.
  (define (raise-dense-to-sparse d)
    (define (iter dd result)
      (if (null? dd) (attach-tag 'sparse (cons (car d) (reverse result)))
          (iter (cdr dd) (cons (list (- (length dd) 1) (car dd)) result))))
    (iter (cdr d) '()))
  
  ;;the interface to the rest of the system
  (put 'raise '(integer) raise-integer-to-rational)
  (put 'raise '(rational) raise-rational-to-real)
  (put 'raise '(real) raise-real-to-complex)
  (put 'raise '(dense) raise-dense-to-sparse)
  'done)

(install-raise-package)

(define (raise n)
  (apply-generic1 'raise n))



(raise (make-integer 2))

(raise (raise (make-integer 2)))

(raise (raise (raise (make-integer 2))))

(raise (raise (raise (raise (make-integer 2)))))

(display "--------2.84--------")
(newline)

(define (level t1 t2)
  (define (level-equal? t1 t2) 
    (equal? (type-tag t1) (type-tag t2)))
  (define (iter-low t1 t2)
    (cond ((not t1) #f)
          ((level-equal? t1 t2) #t)
          (else (iter-low (raise t1) t2))))
  (define (iter-high t1 t2)
    (cond ((not t2) #f)
          ((level-equal? t1 t2) #t)
          (else (iter-high t1 (raise t2)))))
  (cond ((level-equal? t1 t2) 'equal)
        ((iter-high t1 t2) 'high)
        ((iter-low t1 t2) 'low)
        (else (error (list t1 t2) "can't be compared by type"))))

(define xn (make-integer 2))

(define yn (make-integer 3))

(define xr (make-rational 2 3))
;
(define yr (make-rational 3 4))

(define xc (make-complex-from-real-imag 3 4))
;
(define yc (make-complex-from-mag-ang 1 (/ 3.1415926 2)))

;(level xn xc)
;(level xr xc)
;(level xn xr)
;(level yc yc)
;(level yc xn)

(define (raise-to max t)
  (if (equal? (level max t) 'equal) t
      (raise-to max (raise t))))

(define (level-max . args)
  (define (iter max l)
    (cond ((null? l) max) 
          ((equal? 'low (level max (car l)))
           (iter (car l) (cdr l)))
          (else (iter max (cdr l)))))
  (iter (car args) (cdr args)))

;(level-max xc yc xn yn xr yr)

(define (list-to-max args)
  (let ((max (apply level-max args)))
    (define (iter l result)
      (if (null? l) (reverse result)
          (iter (cdr l) (cons (raise-to max (car l)) result))))
    (iter args '())))

;(add xc xn);excellent!

(display "--------2.85--------")
(newline)

(define (install-equ?-package)
  ;;internal procedure;
  ;  (define (equ? x y)
  ;    (if (equal? x y) #t #f))
  (define (equ? x y)
    (if (not (and (pair? x) (pair? y)))
        (if (and (number? x) (number? y))
            (= x y)
            (equal? x y))
        (and (if (and (number? (car x)) 
                      (number? (car y))) 
                 (= (car x) (car y))
                 (equal? (car x) (car y)))
             (equ? (cdr x) (cdr y)))))
  (define (complex-equ? c1 c2) (and (< (abs (- (real-part c1) (real-part c2))) 0.00001)
                                    (< (abs (- (imag-part c1) (imag-part c2))) 0.00001)))
  ;;the interface to the rest of the system
  (put 'equ? '(integer integer) equ?)
  (put 'equ? '(integer rational) (lambda (x y) (#f)))
  (put 'equ? '(integer complex) (lambda (x y) (#f)))
  (put 'equ? '(integer real) (lambda (x y) (#f)))
  (put 'equ? '(rational real) (lambda (x y) (#f)))
  (put 'equ? '(complex real) (lambda (x y) (#f)))
  (put 'equ? '(rational rational) equ?)
  (put 'equ? '(rational complex) (lambda (x y) (#f)))
  (put 'equ? '(rational integer) (lambda (x y) (#f)))
  (put 'equ? '(complex complex) complex-equ?)
  (put 'equ? '(complex rational) (lambda (x y) (#f)))
  (put 'equ? '(complex integer) (lambda (x y) (#f)))
  (put 'equ? '(real real) equ?)
  (put 'equ? '(real complex) (lambda (x y) (#f)))
  (put 'equ? '(real rational) (lambda (x y) (#f)))
  (put 'equ? '(real integer) (lambda (x y) (#f)))
  'done)

(install-equ?-package)

(define (equ? x y)
  (apply-generic1 'equ?  x y))

(define (install-project-package)
  ;;internal procedure
  (define (project-complex-to-real xc)
    (if (exact? (real-part xc))
        (exact->inexact (real-part xc))
        (real-part xc)))
  (define (project-real-to-rational xre)
    (let ((xra (inexact->exact xre)))
      (make-rational
       (numerator xra)
       (denominator xra))))
  ;  (define (project-rational-to-integer xra)
  ;    (inexact->exact (round (/ (car xra)
  ;                              (cdr xra)))));;not a good idea...
  ;;the interface to the rest of the system
  (put 'project '(complex) 
       (lambda (x) (attach-tag 'real (project-complex-to-real x))))
  (put 'project '(real) 
       project-real-to-rational)
  ;  (put 'project '(rational) 
  ;       (lambda (x) (attach-tag 'integer (project-rational-to-integer x))))
  'done)

(install-project-package)

(define (project x)
  (apply-generic1 'project x))

(define zc (make-complex-from-real-imag 2.14 3.14))

;;the project procedure's test:

;(display xc)
;(newline)
;
;(project xc)
;
;(project (project xc))
;
;(project (project (project xc)))
;
;(display zc)
;(newline)
;
;(project zc)
;
;(project (project zc))
;
;(define tt (project (project (project zc))))
;
;(raise tt)
;
;(raise (raise tt))
;
;(raise (raise (raise tt)))





;(define (drop x)
;  (if (not (project x)) 
;      x
;      (if (not (equ? x (raise (project x))))
;          x
;          (begin (display 'wrong) (drop (project x))))))

(define (drop x)
  (cond ((not (project x)) x)
        ((not (equ? x (raise (project x)))) x)
        (else (drop (project x)))))


(define kc (make-complex-from-real-imag 2.1 0))
(define schemec (make-complex-from-real-imag 2.0 0))

;;the drop procedure's test:

;(drop xc)
;(drop zc)
;(drop kc)
;(drop schemec)


;;2-86 just need to change the make method and selectors in the complex module:change the + - * / to add sub mul div. And also as the question said, the sine and cosine should be implemented for the rational number.

(define (install-=zero?-package)
  ;;internal procedure
  (define (=zero?-integer x)
    (if (= x 0) #t #f))
  (define (=zero?-rational x)
    (if (equal? x (content (make-rational 0 1))) #t #f))
  (define (=zero?-complex x)
    (if (= (magnitude x) 0)
        #t
        #f))
  ;  (define (=zero?-dense x)
  ;    (empty-termlist? (cadr x)));;not reasonable...
  ;;interfaces for the rest of the system
  (put '=zero? '(integer) =zero?-integer)
  (put '=zero? '(rational) =zero?-rational)
  (put '=zero? '(complex) =zero?-complex)
  ;  (put '=zero? '(dense) =zero?-dense)
  'done)

(install-=zero?-package)

(define (=zero? x) (apply-generic '=zero? x))

(define (nega x) (sub (make-complex-from-real-imag 0 0) x));<----------2-88 the negation operation

(define (install-dense-package);<----------------------------the dense package
  ;;internal procedures
  ;;representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (same-variable? s1 s2)
    (and (symbol? s1) (eq? s1 s2)))
  (define (variable? s)
    (symbol? s))
  ;;representation of terms and term lists
  (define (the-empty-termlist) '())
  (define (adjoin-term term term-list)
    (if (= term 0) 
        term-list
        (cons term term-list)));;the term added must be bigger than these already in the term-list.
  (define (empty-termlist? term-list) (null? term-list))
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  ;(define (make-term order coeff) (list order coeff))
  ;(define (order term) (car term))
  ;(define (coeff term) (cadr term))
  ;(define (coeff))
  ;;continued on next page
  
  (define (add-terms L1 L2)
    (define (iter l1 l2 result)
      (cond ((and (null? l1) (null? l2)) (reverse result)) 
            ((= (length l1) (length l2)) 
             (iter (cdr l1) (cdr l2) (cons (add (car l1) (car l2)) result)))
            ((< (length l1) (length l2))
             (iter l1 (cdr l2) (cons (car l2) result)))
            ((> (length l1) (length l2))
             (iter (cdr l1) l2 (cons (car l1) result)))
            (else (error "error in add-terms" (list L1 L2)))))
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else 
           (iter L1 L2 '()))))
  
  (define (sub-terms L1 L2)
    (define (nega-list l)
      (define (iter li re)
        (if (null? li) (reverse re)
            (iter (cdr li) (cons (nega (car li))
                                 re))))
      (iter l '()))
    (define (iter0 l1 l2 result)
      (cond ((and (null? l1) (null? l2)) (reverse result)) 
            ((= (length l1) (length l2)) 
             (iter0 (cdr l1) (cdr l2) (cons (sub (car l1) (car l2)) result)))
            ((< (length l1) (length l2))
             (iter0 l1 (cdr l2) (cons (nega (car l2)) result)))
            ((> (length l1) (length l2))
             (iter0 (cdr l1) l2 (cons (car l1) result)))
            (else (error "error in sub-terms" (list L1 L2)))))
    (cond ((empty-termlist? L1) (nega-list L2))
          ((empty-termlist? L2) L1)
          (else 
           (iter0 L1 L2 '()))))
  
  
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (append (mul-term-by-all-terms (first-term L1) L2) (make-list (- (length L1) 1) 0))
                   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (mul t1 t2)
           (mul-term-by-all-terms t1 (rest-terms L))))))
  
  
  (define (add-poly p1 p2) 
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var --ADD-POLY"
               (list p1 p2))))
  (define (sub-poly p1 p2) 
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (sub-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var --SUB-POLY"
               (list p1 p2))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var --MUL-POLY"
               (list p1 p2))))
  (define (=zero?-dense x)
    (empty-termlist? (term-list x)))
  ;;interfaces to rest of the system
  (define (tag p) (attach-tag 'dense p))
  (put 'add '(dense dense)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(dense dense) 
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul '(dense dense)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make '(dense)
       (lambda (var terms) (tag (make-poly var terms))))
  (put '=zero? '(dense) =zero?-dense)
  (put 'variable '(dense) variable)
  (put 'term-list '(dense) term-list)
  'done)

(install-dense-package)

(define make-dense
  (get 'make '(dense)))

(define p1 (cons 'dense '(x 3 2 1)))
(define p2 (cons 'dense '(x 3 0 2 0 1)))
(display (add p1 p2))
(newline)
(display (sub p1 p2))
(newline)
(display (mul p1 p2))
(newline)




(define (install-sparse-package);<----------------the sparse package
  ;;internal procedures
  ;;representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (same-variable? s1 s2)
    (and (symbol? s1) (eq? s1 s2)))
  (define (variable? s)
    (symbol? s))
  ;;representation of terms and term lists
  (define (the-empty-termlist) '())
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (empty-termlist? term-list) (null? term-list))
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  ;(define (coeff))
  ;;continued on next page
  (define (gcd-terms a b)
    (if (empty-termlist? b)
        a 
        (gcd-terms b (remainder-terms a b))))
  
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else 
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else 
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))
  (define (sub-terms L1 L2)
    (define (nega-list l)
      (define (iter li re)
        (if (null? li) (reverse re)
            (iter (cdr li) (cons (make-term (order (car li))
                                            (nega (coeff (car li))))
                                 re))))
      (iter l '()))
    (cond ((empty-termlist? L1) (nega-list L2))
          ((empty-termlist? L2) L1)
          (else 
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (sub-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     (make-term (order t2) (nega (coeff t2))) 
                     (sub-terms L1 (rest-terms L2))))
                   (else 
                    (adjoin-term
                     (make-term (order t1)
                                (sub (coeff t1) (coeff t2)))
                     (sub-terms (rest-terms L1)
                                (rest-terms L2)))))))))
  
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (add (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))
  (define (div-terms t1 t2);<-----------------------------------2.91
    (define (iter it1 it2 result)
      (if (or (empty-termlist? it1)
              (< (order (first-term it1))
                 (order (first-term it2))))
          (reverse result)
          (let ((r (make-term (sub (order (first-term it1))
                                   (order (first-term it2)))
                              (div (coeff (first-term it1))
                                   (coeff (first-term it2))))))
            (if (null? (mul-term-by-all-terms r it2))
                (iter (rest-terms it1) it2 (adjoin-term r result))
                (iter (sub-terms (rest-terms it1)
                                 (rest-terms (mul-term-by-all-terms r it2)))
                      it2
                      (adjoin-term r result))))))
    (iter t1 t2 '()))
  
  (define (remainder-terms t1 t2)
    (define (iter it1 it2 result)
      (if (or (empty-termlist? it1)
              (< (order (first-term it1))
                 (order (first-term it2))))
          it1
          (let ((r (make-term (sub (order (first-term it1))
                                   (order (first-term it2)))
                              (div (coeff (first-term it1))
                                   (coeff (first-term it2))))))
            (if (null? (mul-term-by-all-terms r it2))
                (iter (rest-terms it1) it2 (adjoin-term r result))
                (iter (sub-terms (rest-terms it1)
                                 (rest-terms (mul-term-by-all-terms r it2)))
                      it2
                      (adjoin-term r result))))))
    (iter t1 t2 '()))
          
  
  
  
  (define (add-poly sp1 sp2) 
    (if (same-variable? (variable sp1) (variable sp2))
        (make-poly (variable sp1)
                   (add-terms (term-list sp1)
                              (term-list sp2)))
        (error "Polys not in same var --ADD-POLY"
               (list sp1 sp2))))
  (define (sub-poly sp1 sp2) 
    (if (same-variable? (variable sp1) (variable sp2))
        (make-poly (variable sp1)
                   (sub-terms (term-list sp1)
                              (term-list sp2)))
        (error "Polys not in same var --SUB-POLY"
               (list sp1 sp2))))
  (define (mul-poly sp1 sp2)
    (if (same-variable? (variable sp1) (variable sp2))
        (make-poly (variable sp1)
                   (mul-terms (term-list sp1)
                              (term-list sp2)))
        (error "Polys not in same var --MUL-POLY"
               (list sp1 sp2))))
  (define (div-poly sp1 sp2)
    (if (same-variable? (variable sp1) (variable sp2))
        (make-poly (variable sp1)
                   (div-terms (term-list sp1)
                              (term-list sp2)))
        (error "Polys not in same var --DIV-POLY"
               (list sp1 sp2))))
  (define (remainder-poly sp1 sp2)
    (if (same-variable? (variable sp1) (variable sp2))
        (make-poly (variable sp1)
                   (remainder-terms (term-list sp1)
                              (term-list sp2)))
        (error "Polys not in same var --DIV-POLY"
               (list sp1 sp2))))
  
  (define (=zero?-sparse x)
    (empty-termlist? (term-list x)))
  ;;interfaces to rest of the system
  (define (tag p) (attach-tag 'sparse p))
  (put 'add '(sparse sparse)
       (lambda (sp1 sp2) (tag (add-poly sp1 sp2))))
  (put 'sub '(sparse sparse) 
       (lambda (sp1 sp2) (tag (sub-poly sp1 sp2))))
  (put 'mul '(sparse sparse)
       (lambda (sp1 sp2) (tag (mul-poly sp1 sp2))))
  (put 'div '(sparse sparse)
       (lambda (sp1 sp2) (tag (div-poly sp1 sp2))))
  (put 'remainder-my '(sparse sparse)
       (lambda (sp1 sp2) (tag (remainder-poly sp1 sp2))))
  (put 'make '(sparse)
       (lambda (var terms) (tag (make-poly var terms))))
  (put '=zero? '(sparse) =zero?-sparse)
  (put 'variable '(sparse) variable)
  (put 'term-list '(sparse) term-list)
  'done)

(install-sparse-package)

(define (remainder-my x y)
  (apply-generic 'remainder-my x y))

(define (variable p)
  (apply-generic 'variable p))

(define (term-list p)
  (apply-generic 'term-list p))

(define make-sparse
  (get 'make '(sparse)))

(define sp1 (cons 'sparse '(x (3 3.0) (2 2.0) (1 1.0))))
(define sp2 (cons 'sparse '(x (4 3) (2 2) (0 1))))
(newline)
(display (add sp1 sp2))
(newline)
(display (sub sp1 sp2))
(newline)
(display (mul sp1 sp2))
(newline)
;(define sp3 (cons 'sparse '(x (3 3) (2 2) (0 1))))
(display "----------2.91-----------")
(newline)
(display (div sp2 sp1))
(newline)
(display (div sp1 sp2))
(newline)
(display "---------remainder------------")
(newline)         
(display (remainder-my sp1 sp2))
(newline)
(display (remainder-my sp2 sp1))
(newline)
(display "--------------------------")
(newline)
(display "--------2.90--------")
(newline)
(newline)

;;implement the raise method for dense->sparse, and then the operation for ;;these two different types become possible.
(display (add sp1 p1))
(newline)

(define (install-polynomial-package)
  ;;internal procedure
  (define (tag x) (attach-tag 'polynomial x))
  (define (make-poly-from-sparse sp)
    (tag (attach-tag 'sparse sp)))
  (define (make-poly-from-dense d)
    (tag (attach-tag 'dense d)))
  ;;the interface to the rest of the system
  (put 'add '(polynomial polynomial)
       (lambda (x y) (tag (add x y))))
  (put 'sub '(polynomial polynomial)
       (lambda (x y) (tag (sub x y))))
  (put 'mul '(polynomial polynomial)
       (lambda (x y) (tag (mul x y))))
  (put 'make-polynomial '(sparse) make-poly-from-sparse)
  (put 'make-polynomial '(dense) make-poly-from-dense)
  'polynomial_package_is_installed)

(install-polynomial-package)

(define (make-polynomial x)
  (apply-generic 'make-polynomial x))

(define ply1 (make-polynomial sp1))

(define ply2 (make-polynomial p1))

(display (add ply1 ply2))
(newline)

(display (sub ply1 ply2))
(newline)

(display (mul ply1 ply2))
(newline)

;(define pp1 (make-polynomial 'x '((2 1) (0 1))))

;(define pp2 (make-polynomial 'x '((3 1) (0 1))))

(define rf (make-rational sp2 sp1))

(add rf rf)


