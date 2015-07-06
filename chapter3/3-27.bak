#lang planet neil/sicp

;tree  ((key value) left-branch right-branch))

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (adjoin-tree x tree)
  (cond ((null? tree) 
         (make-tree x '() '()))
        ((= (car x) (car (entry tree)))
         (make-tree x 
                    (left-branch tree)
                    (right-branch tree)))
        ((> (car x) (car (entry tree)))
         (make-tree (entry tree)
                    (left-branch tree)
                    (adjoin-tree x (right-branch tree))))
        ((< (car x) (car (entry tree)))
         (make-tree (entry tree)
                    (adjoin-tree x (left-branch tree))
                    (right-branch tree)))))



(define (make-table)
  (let ((local-table '()))
    
    (define (lookup key records)
             (cond ((null? records) #f)
                   ((= key (car (entry records)))
                    (entry records))
                   ((> key (car (entry records)))
                    (lookup key (right-branch records)))
                   ((< key (car (entry records)))
                    (lookup key (left-branch records)))))
    
    (define (insert! key value)
      (let ((record (lookup key local-table)))
        (if record
            (set-cdr! record value)
            (set! local-table (adjoin-tree 
                               (cons key value) local-table)))))
    
    (define (get key)
      (let ((pair (lookup key local-table)))
        (if pair
            (cdr pair)
            pair)))
    
    (define (dispatch m)
      (cond ((eq? m 'get-proc) get)
            ((eq? m 'insert-proc) insert!)
            ((eq? m 'print) local-table)
            (else (error "Undefined operation" m))))
    dispatch))

(define (lookup key table) ((table 'get-proc) key))

(define (insert! key value table) ((table 'insert-proc) key value))

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (begin (insert! x result table)
                     result)))))))



(define memo-fib
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))))))))

(memo-fib 50)




