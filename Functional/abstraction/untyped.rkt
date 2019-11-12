#lang racket
(require "deriv_typed.rkt")
(define (fun y)
  (if (> y 0) (- (expt y 3) y 1) void))

(if #t (deriv 0.01 fun 3) (deriv 0.01 fun -3))

  
