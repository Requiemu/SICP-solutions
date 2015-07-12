#lang planet neil/sicp

;;unbound variable in the first frame.

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())
;
;(define (make-frame variables values)
;  (cons variables values))
;(define (frame-variables frame) (car frame))
;(define (frame-values frame) (cdr frame))
;(define (add-binding-to-frame! var val frame)
;  (set-car! frame (cons var (car frame)))
;  (set-cdr! frame (cons val (cdr frame))))
;
;(define (extend-environment vars vals base-env)
;  (if (= (length vars) (length vals))
;      (cons (make-frame vars vals) base-env)
;      (if (< (length vars) (length vals))
;          (error "Too many arguments supplied" vars vals)
;          (error "Too few arguments supplied" vars vals))))

;(define (lookup-variable-value var env)
;  (define (env-loop env)
;    (define (scan vars vals)
;      (cond ((null? vars)
;             (env-loop (enclosing-environment env)))
;            ((eq? var (car vars))
;             (car vals))
;            (else (scan (cdr vars) (cdr vals)))))
;    (if (eq? env the-empty-environment)
;        (error "Unbound variable" var)
;        (let ((frame (first-frame env)))
;          (scan (frame-variables frame)
;                (frame-values frame)))))
;  (env-loop env))
;
;(define (set-variable-value! var val env)
;  (define (env-loop env)
;    (define (scan vars vals)
;      (cond ((null? vars)
;             (env-loop (enclosing-environment env)))
;            ((eq? var (car vars))
;             (set-car! vals val))
;            (else (scan (cdr vars) (cdr vals)))))
;    (if (eq? env the-empty-environment)
;        (error "Unbound variable -- SET!" var)
;        (let ((frame (first-frame env)))
;          (scan(frame-variables frame)
;               (frame-values frame)))))
;  (env-loop env))
;
;(define (define-variable! var val env)
;  (let ((frame (first-frame env)))
;    (define (scan vars vals)
;      (cond ((null? vars)
;             (add-binding-to-frame! var val frame))
;            ((eq? var (car vars))
;             (set-car! values value))
;            (else (scan (cdr vars) (cdr vals)))))
;    (scan (frame-variables frame)
;          (frame-values frame))))

;(lookup-variable-value )
;(extend-environment )
;(define-variable! )
;(set-variable-value!)

;;environment-frame: ((cons a b) (c d) ...)
;;environment: (cons frame1 (cons frame2 frame3))

(define (make-frame variables values)
  (define (iter sub-var sub-val)
    (if (null? sub-var) 
        '()
        (cons (cons (car sub-var) (car sub-val))
              (iter (cdr sub-var)
                    (cdr sub-val)))))
  (iter variables values))
(define (frame-variables frame)
  (map car frame))
(define (frame-values frame)
  (map cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-cdr! frame (cons (cons var val) (cdr frame))))

(define the-empty-frame '())

(define (first-pair frame) (car frame))

(define (rest-pairs frame) (cdr frame))

(define (pair-var pair0) (car pair0))

(define (pair-val pair0) (cdr pair0))

(define (set-pair-value! pair0 val) (set-cdr! pair0 val))

(define (unbound! var env)
  (let ((frame (first-frame env)))
    (define (scan frame)
      (cond ((eq? frame the-empty-frame)
             (error "no pairs this name" var))
            ((eq? var (pair-var (first-pair frame)))
             (begin (set-car! frame (first-pair (rest-pairs frame)))
                    (set-cdr! frame (rest-pairs (rest-pairs frame)))))
            (else (scan (rest-pairs frame)))))
    (scan frame)))

(define test-env (cons (make-frame '(1 2 3 4)
                                   '(5 6 7 8))
                       (make-frame '(q w e r)
                                   '(a s d f))))

(unbound! 1 test-env)

(display test-env)


             
