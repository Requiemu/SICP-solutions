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

;;an example division:
(define (load-ex-division)
  ;;internal procedure
  (define ex-division 
    '(ex-division
      (ex-division ex-name1 ex-address1 ex-salary1)
      (ex-division ex-name2 ex-address2 ex-salary2)))
  (define (get-record name)
    (define (iter name n)
      (cond ((eq? n (length ex-division)) 
             (error "no person found" name))
            ((eq? name (cadr (list-ref ex-division n))) 
             (list-ref ex-division n))
            (else (iter name (+ n 1)))))
    (iter name 1))
  (define (get-salary record)
    (caddr record))
  ;;the interface for the rest of the procedure
  (put 'get-record 'ex-division get-record)
  (put 'get-salary 'ex-division get-salary)
  'done)

(load-ex-division)

(define (division-tag name) (car name))

(define (content name) (cdr name))

;;a.

(define (get-record name)
  ((get 'get-record (division-tag name)) (content name)))

(get-record (cons 'ex-division 'ex-name1))

;;b.

(define (get-salary record)
  ((get 'get-salary (division-tag record)) (content record)))

(get-salary (get-record (cons 'ex-division 'ex-name1)))

;;c

(define (make-name division name)
  (cons division name))

(define (find-employee-record name division-list)
  (define (iter name division-list result)
    (if (null? division-list) result
        (iter name 
              (cdr division-list)
              (append result
                      (list (get-record (make-name (car division-list) name)))))))
  (iter name division-list '()))

(find-employee-record 'ex-name1 (list 'ex-division))

;;d

;;Will should to make a package to be load. The package should give out the basic interface.
  
  