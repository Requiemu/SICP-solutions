#lang racket




;(define (analyze-lambda exp)
;  (let ((vars (lambda-parameters exp))
;        (bproc (analyze-sequence (lambda-body exp))))
;    (lambda (env) (make-procedure vars bproc env))))
;;text's




;(define (analyze-sequence exps)
;  (define (execute-sequence procs env)
;    (cond ((null? (cdr procs)) ((car procs) env))
;          (else ((car procs) env)
;                (execute-sequence (cdr procs) env))))
;  (let ((procs (map analyze exps)))
;    (if (null? procs)
;        (error "Empty sequence -- ANALYZE"))
;    (lambda (env) (execute-sequence procs env))))
;;Alyssa's


;;While text's will call the procedure we need directly, 
;;Alyssa's will first call the extra execute-sequence first, which is not efficient as the text's/