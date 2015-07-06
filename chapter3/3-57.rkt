#lang racket
;;n-2 "+" will be run to caculate the nth number.
;;if we didn't use the memo-proc,every time all the elements needed by the procedure will be caculated, the step is approximately n*(n-1)