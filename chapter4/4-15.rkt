#lang racket

;; the (try try) is self-contradiction.
;;if (try try) halt, (halts? try try) will (run-forever)
;;if (try try) don't halt, (halts? try try) will 'halted.