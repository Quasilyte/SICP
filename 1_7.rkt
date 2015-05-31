#lang racket

(define (sqrt x)
  ;; `x' is visible inside all nested functions: it is called lexical scoping.
  (define (good-enough? approx next-approx)
    (< (abs (- approx next-approx)) 0.0001))
  (define (improve approx)
    (/ (+ approx (/ x approx)) 2.0))
  (define (sqrt-iter approx)
    (let ([next-approx (improve approx)])
      (if (good-enough? approx next-approx)
        approx
        (sqrt-iter next-approx))))
  (sqrt-iter 1.0))

(sqrt 0.001)