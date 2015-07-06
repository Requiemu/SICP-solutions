#lang planet neil/sicp

;tree  ((key value) left-branch right-branch))

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (adjoin-tree x tree)
  (if (null? tree) (make-tree x '() '())))

(define (make-table)
  (let ((local-table '()))
    
    (define (look-up key records)
      (cond ((null? records) #f)
            ((= key (car (entry records)))
             (entry records))
            ((> key (car (entry records)))
             (look-up key (right-branch records)))
            ((< key (car (entry records)))
             (look-up key (left-branch records)))))
    
    (define (insert! key value)
      (let ((record (look-up key local-table)))
        (if record
            (set-cdr! record value)
            (set! local-table (adjoin-tree 
                               (cons key value) local-table)))))
    
    (define (get key)
      (look-up key local-table))
    
    (define (dispatch m)
      (cond ((eq? m 'get-proc) get)
            ((eq? m 'insert-proc) insert!)
            ((eq? m 'print) local-table)
            (else (error "Undefined operation" m))))
    dispatch))

(define t (make-table))

(define get (t 'get-proc))

(define put (t 'insert-proc))

(define (print) (t 'print)) 
               
(put 2 'a)

(get 2)

(print)

             
            
            
            
             
    
