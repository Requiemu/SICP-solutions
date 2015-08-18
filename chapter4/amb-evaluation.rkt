#lang planet neil/sicp

(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ;((amb? exp) (analyze-amb exp));;analyze-amb
        ((quoted? exp) (analyze-variable exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp));;...
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((application? exp) (analyze-application exp))
        (else 
         (error "Unknown expression type -- ANALYZE" exp))))

;;====================================================
;;analyze procedure

;(define (analyze-self-evaluating exp)
;  (lambda (env) exp))

(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))

;(define (analyze-quoted exp)
;  (let ((qval (text-of-quotation exp)))
;    (lambda (env) qval)))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed qval fail))))

;(define (analyze-variable exp)
;  (lambda (env) (lookup-variable-value exp env)))

(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env) fail)))

;(define (analyze-assignment exp)
;  (let ((var (assignment-variable exp))
;        (vproc (analyze (assignment-value exp))))
;    (lambda (env)
;      (set-variable-value! var (vproc env) env)
;      'ok)))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)  ;*1*
               (let ((old-value
                      (lookup-variable-value var env)))
                 (set-variable-value! var val env)
                 (succeed 'ok
                          (lamda ()  ;*2*
                                 (set-variable-value!
                                  var old-value env)
                                 (fail2)))))
             fail))))

;(define (analyze-definition exp)
;  (let ((var (definition-variable exp))
;        (vproc (analyze (definition-value exp))))
;    (lambda (env)
;      (define-variable! var (vproc env) env)
;      'ok)))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (define-variable! var val env)
               (succeed 'ok fail2))
             fail))))

;(define (analyze-if exp)
;  (let ((pproc (analyze (if-predicate exp)))
;        (cproc (analyze (if-consequent exp)))
;        (aproc (analyze (if-alternative exp))))
;    (lambda (env)
;      (if (true? (pproc env))
;          (cproc env)
;          (aproc env)))))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
             ;;success continuation for evaluating the predicate
             ;;to obtain pred-value
             (lambda (pred-value fail2)
               (if (true? pred-value)
                   (cproc env succeed fail2)
                   (aproc env succeed fial2)))
             ;;failure continuation for evaluating the predicate
             fail))))


;(define (analyze-lambda exp)
;  (let ((vars (lambda-parameters exp))
;        (bproc (analyze-sequence (lambda-body exp))))
;    (lambda (env) (make-procedure vars bproc env))))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env) fail))))

;(define (analyze-sequence exps)
;  (define (sequentially proc1 proc2)
;    (lambda (env) (proc1 env) (proc2 env)))
;  (define (loop first-proc rest-procs)
;    (if (null? rest-procs)
;        first-proc
;        (loop (sequentially first-proc (car rest-procs))
;              (cdr rest-procs))))
;  (let ((procs (map analyze exps)))
;    (if (null? procs)
;        (error "Empty sequence -- ANALYZE"))
;    (loop (car procs) (cdr procs))))

(define (analyze-sequence exps)
  (define (sequentially a b)
    (lambda (env succeed fail)
      (a env
         ;;success continuation for calling a
         (lambda (a-value fail2)
           (b env succeed fail2))
         ;;failure continuation for calling a
         fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc
                            (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence: ANALYZE"))
    (loop (car procs) (cdr procs)))) 

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env)
      (execute-application (fproc env)
                           (map (lambda (aproc) (aproc env))
                                aprocs)))))


(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))))
        (else
         (error 
          "Unknown procedure type -- EXECUTE-APPLICATION"
          proc))))

;;==================================
;;surpport grammar data structure

(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))

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

;;====================================================
;;support environment

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
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




(define (eval exp env)
  ((analyze exp) env))

(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))






(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

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

(driver-loop)












