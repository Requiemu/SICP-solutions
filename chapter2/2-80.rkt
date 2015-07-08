#lang planet neil/sicp
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

(define (square x) (* x x))

(define (type-tag x) 
  (if (number? x) 'scheme-number;<------------------------2-78.answer:modify this
      (if (pair? x)
          (car x)
          (error "Bad tagged datum -- TYPE-TAG" x))))

(define (content arg) 
  (if (number? arg) arg;<---------------------------------2-78.answer:modify this
      (if (pair? arg)
          (cdr arg)
          (error "Bad tagged datum -- CONTENTS" arg))))

(define (apply-generic op . arg)
  (let ((proc (get op (map type-tag arg))))
    (if proc
        (apply proc (map content arg))
        (error
         "No method for these types -- APPLY-GENERIC"
         (list op (map type-tag arg))))))


(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (attach-tag tag x)
  (if (number? x) x;<-------------------------------------2-78.answer:modify this
      (cons tag x)))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  'done)

(install-scheme-number-package)

(define make-scheme-number (get 'make 'scheme-number))

(define (install-rational-package)
  ;;internal procidures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
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
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(install-rational-package)

(define make-rational (get 'make 'rational))

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
  'done)

(install-complex-package)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(display "--------scheme-number--------")

(define xn (make-scheme-number 2))

(define yn (make-scheme-number 3))

(add xn yn)
(sub xn yn)
(mul xn yn)
(div xn yn)

(display "--------complex-number--------")

(define xc (make-complex-from-real-imag 3 4))

(define yc (make-complex-from-mag-ang 1 (/ 3.1415926 2)))

(add xc yc)
(sub xc yc)
(mul xc yc)
(div xc yc)
(real-part xc)
(imag-part xc)
(angle xc)
(magnitude xc)

(display "--------rational-number--------")

(define xr (make-rational 2 3))

(define yr (make-rational 3 4))

(add xr yr)
(sub xr yr)
(mul xr yr)
(div xr yr)

;;2-77 answer
;;2 times. First to lookup 'magnitude 'complex in the table, and then look up 'magnitude 'rectangular in the table.
;;两次, 第一次根据tag找表中’complex 那一列的方法, 第二次‘complex的tag被分离出来了,直接去找’rectangular的tag


;;2-79 answer
;;include a package, a defination useing apply-generic, and tests


(define (install-equ?-package)
  ;;internal procedure;
  (define (equ? x y)
    (if (equal? x y) #t #f))
  ;;the interface to the rest of the system
  (put 'equ? '(scheme-number scheme-number) equ?)
  (put 'equ? '(scheme-number rational) equ?)
  (put 'equ? '(scheme-number complex) equ?)
  (put 'equ? '(rational rational) equ?)
  (put 'equ? '(rational complex) equ?)
  (put 'equ? '(rational scheme-number) equ?)
  (put 'equ? '(complex complex) equ?)
  (put 'equ? '(complex rational) equ?)
  (put 'equ? '(complex scheme-number) equ?)
  'done)

(install-equ?-package)

(define (equ? x y)
  (and (equal? (type-tag x) (type-tag y))
       (apply-generic 'equ?  x y)))

(equ? xn xn)

(equ? xn yn)

(equ? xn xc)

(equ? xc xc)

(equ? xr xr)

;;2-80


(define (install-=zero?-package)
  ;;internal procedure
  (define (=zero?-scheme-number x)
    (if (= x 0) #t #f))
  (define (=zero?-rational x)
    (if (equal? x (content (make-rational 0 1))) #t #f))
  (define (=zero?-complex x)
    (if (= (magnitude x) 0)
        #t
        #f))
  ;;interfaces for the rest of the system
  (put '=zero? '(scheme-number) =zero?-scheme-number)
  (put '=zero? '(rational) =zero?-rational)
  (put '=zero? '(complex) =zero?-complex)
  'done)

(install-=zero?-package)

(define (=zero? x) (apply-generic '=zero? x))

(display "-----------2-80-----------")
(newline)

(define 0n 0)

(define 0r (make-rational 0 9))

(define 0cri (make-complex-from-real-imag 0 0))

(define 0cma (make-complex-from-mag-ang 0 1))

(=zero? 0n)

(=zero? 0r)

(=zero? 0cri)

(=zero? 0cma)

(=zero? xn)

(=zero? xr)

(=zero? xc)

(=zero? yc)