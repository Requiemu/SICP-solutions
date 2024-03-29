#lang racket

;(define (eval exp env)
;  (cond ((self-evaluating? exp) exp)
;        ((variable? exp) (lookup-variable-value exp env))
;        ((quoted? exp) (text-of-quotation exp))
;        ((assignment? exp) (eval-assignment exp env))
;        ((definition? exp) (eval-definition exp env))
;        ((if? exp) (eval-if exp env))
;        ((lambda? exp)
;         (make-procedure (lambda-parameters exp)
;                         (lambda-body exp)
;                         env))
;        ((begin? exp)
;         (eval-sequence (begin-actions exp) env))
;        ((cond? exp) (eval (cond->if exp) env))
;        ((application? exp)                           ;;primitive operator is actually here
;         (apply (eval (operator exp) env)
;                (list-of-values (operands exp) env)))
;        (else
;         (error "Unknown expression type -
;- EVAL" exp))))

;(define (apply procedure arguments)
;  (cond ((primitive-procedure? procedure)
;         (apply-primitive-procedure procedure arguments))
;        ((compound-procedure? procedure)
;         (eval-sequence
;          (procedure-body procedure)
;          (extend-environment
;           (procedure-parameters procedure)
;           arguments
;           (procedure-environment procedure))))
;        (else
;         (error
;          "Unknown procedure type -- APPLY" procedure))))

;(define (list-of-values exps env)
;  (if (no-operands? exps)
;      '()
;      (cons (eval (first-operand exps) env)
;            (list-of-values (rest-operands exps) env))))

;(define (eval-if exp env)
;  (if (true? (eval (if-predicate exp) env))
;      (eval (if-consequent exp) env)
;      (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

;(define (eval-assignment exp env)
;  (set-variable-value! (assignment-variable exp)
;                       (eval (assignment-value exp) env)
;                       env)
;  'ok)

;(define (eval-definition exp env)
;  (define-variable! 
;    (definition-variable exp)
;    (eval (definition-value exp) env)
;    env)
;  'ok)

;(define (list-of-values-right-to-left exps env)
;  (define (iter exp l)
;    (if (no-poerands? exps)
;        l
;        (iter (rest-operands exps) 
;              (append l 
;                      (eval (first-operand exps) env)))))
;  (iter exps '()))

;(define (list-of-values-left-to-right exps env)
;  (reverse (list-of-values-right-to-left (reverse exps env))))

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (caddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (list parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operand? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (if (eq? (car (cond-actions first)) '=>)
                         (list (cadr (cond-actions first)) (cond-predicate first))
                         (sequence->exp (cond-actions first)))
                     (expand-clauses rest))))))

(define test (cond->if (list 'cond 
                             (list 'true? 1)
                             (list 2 '=> (lambda (x) (+ x 2)))
                             (list '??? 22)
                             (list 'else 33))))
;;(display test)
;
;;(cond ((assoc 'b '((a 1) (b 2))) => cadr)
;;      (else false))
;;
;;(cond (2 => (lambda (x) (+ x 2))))
;
;;(let ((<var1> <exp1>) ... (<varn> <expn>))
;;<body>)
;;
;;((lambda (<var1> ... <varn>)
;;<body>)
;;<exp1>
;;<expn>)
;
;(define (let-type exp)
;  (if (eq? (car exp) 'let)
;      (cond ((= (length exp) 3) 'ordinary)
;            ((= (length exp) 4) 'named)
;            (else (error "unknown let type -- LET-TYPE")))
;      (error "not a let sentence -- LET-TYPE" exp)))
;(define (let-func-name exp)
;  (cond ((eq? (let-type exp) 'ordinary) 
;         (error "the let sentence is ordinary-- LET-FUNC-NAME" exp))
;        ((eq? (let-type exp) 'named)
;         (cadr exp))
;        (else (error "unknow let type -- LET-FUNC-NAME" exp))))
;(define (let-pairs exp) 
;  (cond ((eq? (let-type exp) 'ordinary) (cadr exp))
;        ((eq? (let-type exp) 'named) (caddr exp))
;        (else (error "unknow let type -- LET-FUNC-NAME" exp))))                             
;(define (pairs-var pair0) (car pair0))
;(define (pairs-val exp0) (cadr exp0))
;(define (let-body exp) 
;  (cond ((eq? (let-type exp) 'ordinary) (caddr exp))
;        ((eq? (let-type exp) 'named) (cadddr exp))
;        (else (error "unknow let type -- LET-FUNC-NAME" exp))))
;
;(define (let->lambda exp)
;  (cons (make-lambda 
;         (map pairs-var (let-pairs exp))
;         (let-body exp))
;        (map pairs-val (let-pairs exp))))
;
;(define test-let (list 'let '((x 1) (y 2)) '(+ x y)))
;
;(let->lambda test-let)
;
;;(define (fib n)
;;  (let fib-iter ((a 1)
;;                 (b 0)
;;                 (count n))
;;    (if (= count 0)
;;        b
;;        (fib-iter (+ a b) a (- count 1)))))
;
;
;
(define (make-define var value)
  (list 'define var value))
;
;(define (let->combination exp)
;  (cond ((eq? (let-type exp) 'ordinary)
;         (let->lambda exp))
;        ((eq? (let-type exp) 'named)
;         (make-begin (list (make-define (let-func-name exp)
;                                        (make-lambda 
;                                         (map pairs-var (let-pairs exp))
;                                         (let-body exp)))
;                           (list (let-func-name exp) 
;                                 (map pairs-val (let-pairs exp))))))))
;
;(define test-named (list 'let 
;                   'fib-iter 
;                   '((a 1) (b 0) (count n))
;                   (list 'if '(= count 0) 'b '(fib-iter (+ a b) a (- count 1)))))
;
;(let->combination test-named)
;                   
;                     
;                     

;;;;do for while until
;;('while (< i 100) body) => (f i) => (begin (define f (lambda () (if (< i 100) body f #f)))

(define (while-predicate exp) (cadr exp))
(define (while-body exp) (caddr exp))

(define (while->combination exp)
  (make-begin (list (make-define 'f 
                                 (make-lambda '() 
                                              (make-if (while-predicate exp) 
                                                       (make-begin (list (while-body exp)
                                                                   '(f)))
                                                       #f)))
              '(f))))

(define test-while '(while (< i 100) body))

(while->combination test-while)