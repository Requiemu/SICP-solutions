#lang racket
;;actually the define-included procedure is evaluated as follows:
;;define-included-lambda->let-included-lambda->lambda-included-lambda.
;;since it creat a new lambda, it is not strange that a new scop is created.


;;the "simultanneous" way is to let define to be implemented by directly modify the environment(creat a new variable-value and add it to current environment).
