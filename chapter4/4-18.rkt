#lang racket


;;it won't work, because when (let ((a <e1>) (b <e2>)) ..), the e1 and e2 depend on the value of u and v, but until now the u and v is just *unassigned.So the method inn the excersise will not work while that in the text will work.