#lang planet neil/sicp

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((set!? exp) (eval-set! exp env))
        ((quoted? exp) (text-of-quotation exp))
        ;((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((let? exp) (eval (let->lambda exp) env))
        ((letrec? exp) (eval (letrec->let exp) env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply-new (actual-value (operator exp) env)
                    (operands exp) env))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (apply-new procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure 
          procedure 
          (list-of-arg-values arguments env)))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           (list-of-delayed-args arguments env);changed
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY-NEW" procedure))))

;;====================================================
;;lazy evaluation added function

(define (actual-value exp env)
  (force-it (eval exp env)))

;(define (force-it obj)
;  (cond ((thunk? obj)
;         (let ((result (actual-value
;                        (thunk-exp obj)
;                        (thunk-env obj))))
;           (set-car! obj 'evaluated-thunk)
;           (set-car! (cdr obj) result)
;           (set-cdr! (cdr obj) '())
;           result))
;        ((evaluated-thunk? obj)
;         (thunk-value obj))
;        (else obj)))

(define (force-it obj)
  (if (thunk? obj)
      (actual-value (thunk-exp obj) (thunk-env obj))
      obj))

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
            (list-of-arg-values (rest-operands exps)
                                env))))

(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it (first-operand exps) env)
            (list-of-delayed-args (rest-operands exps)
                                  env))))

;(define (force-it exp obj)
;  (if (thunk? obj)
;      (actual-value (thunk-exp obj) (thunk-env obj))
;      obj))

(define (delay-it exp env)
  (list 'thunk exp env))

(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (thunk-exp thunk) (cadr thunk))

(define (thunk-env thunk) (caddr thunk))

(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))










;;====================================================

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (set!? exp) (tagged-list? exp 'set!))
(define (set!-var exp) (cadr exp))
(define (set!-val exp) (caddr exp))
(define (eval-set! exp env)
  (set-variable-value! (set!-var exp)
                       (eval (set!-val exp) env)
                       env))

;(define (eval-if exp env)
;  (if (true? (eval (if-predicate exp) env))
;      (eval (if-consequent exp) env)
;      (eval (if-alternative exp) env)))

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! 
    (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

(define (list-of-values-right-to-left exps env)
  (define (iter exp l)
    (if (no-operands? exps)
        l
        (iter (rest-operands exps) 
              (append l 
                      (eval (first-operand exps) env)))))
  (iter exps '()))

;(display "--------------")

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

(define (make-define var value)
  (list 'define var value))

(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-type exp)
  (if (pair? (cadr exp)) 'function 'normal))

(define (definition-variable exp)
  (cond ((eq? (definition-type exp) 'function) (caadr exp))
        ((eq? (definition-type exp) 'normal) (cadr exp))
        (else (error "unknow definition type -- DEFINITION-VARIABLE" exp))))

(define (function-parameters exp) (cdadr exp))

(define (definition-value exp)
  (cond ((eq? (definition-type exp) 'normal) (caddr exp))
        ((eq? (definition-type exp) 'function)
         (make-lambda (function-parameters exp) (cddr exp)))
        (else (error "unknow definition type -- DEFINITION-VALUE" exp))))

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

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
(define (no-operands? ops) (null? ops))
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
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

(define (true? x)
  (not (eq? x false)))
(define (false? x)
  (eq? x false))

(define (let-pairs exp) (cadr exp))
(define (pairs-var pair0) (car pair0))
(define (pairs-exp exp0) (cadr exp0))
(define (let-body exp) (cddr exp))
(define (let? exp)
  (eq? (car exp) 'let))

(define (let->lambda exp)
  (cons (make-lambda 
         (map pairs-var (let-pairs exp))
         (let-body exp))
        (map pairs-exp (let-pairs exp))))

;(define test-let (list 'let '((x 1) (y 2)) '(+ x y)))

;(let->lambda test-let)

;;(apply-primitive-procedure <proc> <args>)
;;(primitive-procedure? <proc>)

(define (make-procedure parameters body env)
  (list 'procedure parameters (body-transform body) env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

;(lookup-variable-value )
;(extend-environment )
;(define-variable! )
;(set-variable-value!)

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (if (eq? (car vals) "*unassigned*")
                 (error 
                  "the variable is unassigned -- LOOKUP-VARIABLE-VALUE"
                  var)
                 (car vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan(frame-variables frame)
               (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'map map)
        (list 'reverse reverse)
        (list 'display display)
        (list '+ +)
        (list '* *)
        (list '- -)
        (list '/ /)
        (list '= =)
        ;(list 'set! set!)
        ;;=========more primitives=========
        ))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply
   (primitive-implementation proc) args))

;(define (apply-primitive-procedure proc args)
;  (apply (primitive-implementation proc) args))

(define (setup-environment)
  (let ((initial-env 
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))
(define the-global-environment (setup-environment))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))





;
;(define input-prompt ";;; M-Eval input:")
;(define output-prompt ";;; M-Eval value:")

(define input-prompt ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value:")
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output
           (actual-value input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

;(define (driver-loop)
;  (prompt-for-input input-prompt)
;  (let ((input (read)))
;    (let ((output (eval input the-global-environment)))
;      (announce-output output-prompt)
;      (user-print output)))
;  (driver-loop))



(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))



(define (scan-out-defines exp)
  (define (iter origin del-defs defs)
    (if (null? origin) 
        (cons (reverse del-defs) (reverse defs))
        (let ((x (car origin))) 
          (cond ((or (number? x)
                     (symbol? x)
                     (string? x))
                 (iter (cdr origin) 
                       (cons x del-defs)
                       defs))
                ((pair? x) 
                 (if (eq? (car x) 'define)
                     (iter (cdr origin) 
                           del-defs
                           (cons x defs))
                     (iter (cdr origin) 
                           (cons (if (lambda? x) 
                                     (scan-out-defines x)
                                     x)
                                 del-defs)
                           defs)))
                (else "unknow type -- SCAN-OUT-DEFINES" exp)))))
  ;;nondef-def-pairs: (cons ('lambda <parameters> body0 body1 ...)
  ;;                        defs)
  (let ((nondefs (car (iter exp '() '())))
        (defs (cdr (iter exp '() '()))))
    (if (null? defs) exp
        (make-lambda (cadr nondefs)
                     (list (cons 'let 
                                 (cons (map (lambda (x) (list (definition-variable x) "*unassigned*"))
                                            defs)
                                       (append 
                                        (map (lambda (x) (list 'set! (definition-variable x) (definition-value x)))
                                             defs)
                                        (cddr nondefs)))))))))

;(define test '(lambda (x) (define u 1) (define v 2) (+ 1 2)))

;(display (scan-out-defines test))

(define (body-transform body)
  (cddr (scan-out-defines (cons 'lambda (cons 'vars body)))))




;;;;;;
;;(let ((x y) (z i) (j u)

(define (letrec? exp)
  (if (pair? exp)
      (if (eq? (car exp) 'letrec)
          #t
          #f)
      #f))

(define (make-produce-symbol n)
  (define make 
    (lambda () 
      (set! n (+ n 1))
      (string->symbol (number->string n))))
  make)

(define sy (make-produce-symbol 1))

(define (letrec->let exp)
  (let ((let-out (map (lambda (x) (list (car x) "*unassigned*")) (cadr exp)))
        (let-in  (map (lambda (x) (list (sy) (cadr x))) (cadr exp))))
    (list 'let 
          let-out
          (cons 'let
                (cons let-in 
                      (append 
                       (map (lambda (x y) (list 'set! (car x) (car y)))
                            let-out let-in)
                       (cddr exp)))))))

(driver-loop)

;;the test:

(define (square x) (* x x))
(define c 0)
(define (idc x) (set! c (+ c 1)) x)
(square (idc 2))
c

;;; L-Eval input:
(square (id 10))
;;; L-Eval value:
10
;;; L-Eval input:
count
;;; L-Eval value(with memorization):
1

;;; L-Eval value(without memorization):
2
















