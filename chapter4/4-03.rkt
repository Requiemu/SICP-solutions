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
  (if (not (pair? x))
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

(define (raise n)
  (apply-generic1 'raise n))

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

(define (list-to-max args)
  (let ((max (apply level-max args)))
    (define (iter l result)
      (if (null? l) (reverse result)
          (iter (cdr l) (cons (raise-to max (car l)) result))))
    (iter args '())))

;;===============================================================

(define (eval exp env)
  (apply apply-generic1 (cons 'eval exp)))

;(define (install-eval-package)
;  write a package to supprot the eval...

