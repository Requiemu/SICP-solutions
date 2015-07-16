;(define count 0)
;(define (id x)
;(set! count (+ count 1))
;x)
;Give the missing values in the following sequence of interactions, and explain your answers.38
;(define w (id (id 10)))
;;;; L-Eval input:
;count
;;;; L-Eval value:
;1 ;;;;;;;;;;;;;;;;;;when define, the outer id is evaluated, so count +1, but inner id is delayed
;;;; L-Eval input:
;w
;;;; L-Eval value:
;<response>
;;;; L-Eval input:
;count 
;;;; L-Eval value:
;2 ;;;;;;;;;;;;;;;;;;when called w, the real value of w is calculated, so the inner id is called(then list-of-arg-values), then count+1.