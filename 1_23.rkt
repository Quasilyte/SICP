#lang racket

(define (divides? a b)
    (= (remainder b a) 0))

(define (next i)
  (if (= i 2) 3
    (+ i 2)))

(define (find-divisor x test-divisor)
  (cond ([> (sqr test-divisor) x] x)
    ([divides? test-divisor x] test-divisor)
    (else (find-divisor x (next test-divisor)))))

(define (smallest-divisor x)
  (find-divisor x 2))

(time (for ([i 100])
  (smallest-divisor 8724879577)))
