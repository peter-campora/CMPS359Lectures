#lang typed/racket
(: deriv (-> Number (-> Number Number) Number Number))
(provide deriv)
(define (deriv d f x)
  (/ (- (f (+ x d)) (f (- x d))) (* 2 d)))
