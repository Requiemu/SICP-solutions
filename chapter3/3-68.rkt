#lang racket

;;because this is applictive evaluation, the pairs didn't have delay, so the procedure will just call the pairs iteratively, causing infinite loop.