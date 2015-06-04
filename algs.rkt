#lang racket

; Euclid Greatest Common Divisors reduction.
; GCD(40, 6) = GCD(6, 4) = GCD(4, 2) = GCD(2, 0) b == 0 -> a
(define (egcd a b)
  (if (= b 0) a
    (egcd b (remainder a b))))

; Prime numbers: O(sqrt[n]).
(define (prime? maybe-prime)
  (define (divides? a b)
    (= (remainder b a) 0))
  (define (find-divisor maybe-prime test-divisor)
    (cond ([> (sqr test-divisor) maybe-prime] maybe-prime)
      ([divides? test-divisor maybe-prime] test-divisor)
      (else (find-divisor maybe-prime (add1 test-divisor)))))
  (define (smallest-divisor maybe-prime)
    (find-divisor maybe-prime 2))
  (= maybe-prime (smallest-divisor maybe-prime)))

; Fermat test. Prime numbers: O(log[n]) ~~ probabilistic.
(define (expmod base pow denom)
    (remainder (expt base pow) denom))
(define (fermat-test maybe-prime prior)
  (= (expmod prior maybe-prime maybe-prime) prior))
(define (approx-prime? maybe-prime shots (tested (make-hash)))
  (define (next-shot)
    (let ([shot (+ 1 (random (sub1 maybe-prime)))])
      (if (hash-ref tested shot #f)
        (next-shot)
        (begin
          (hash-set! tested shot #t)
          shot))))
  (cond ([= shots 0] #t)
    ([fermat-test maybe-prime (next-shot)]
      (approx-prime? maybe-prime (sub1 shots) tested))
    (else #f)))

