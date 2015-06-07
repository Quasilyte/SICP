#lang racket

(define (n-times fn n)
  (define (iter n result)
    (if (= 0 n) result
      (iter (sub1 n) (fn result))))
  (λ (x) (iter (sub1 n) (fn x))))

(define (twice fn)
  (n-times fn 2))

(define quad (twice (λ (x) (* x x))))
(define add4 (twice (twice add1)))

; (2^2)^2 times add1 to 5
(((twice (twice twice)) add1) 5)

(quad 2)
(add4 2)
