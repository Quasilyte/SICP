#lang racket

(define (iterative-improve improve-guess good-enough?)
  (define (iterator x guess)
    (let ([next-guess (improve-guess x guess)])
      (if (good-enough? guess next-guess) guess
        (iterator x next-guess))))
  (λ (x [guess 1.0]) (iterator x guess)))

(define approx-sqrt (iterative-improve
  (λ (x approx) (/ (+ approx (/ x approx)) 2.0))
  (λ (prev-guess next-guess) (< (abs (- next-guess prev-guess)) 0.0001))))

(approx-sqrt 9)
(approx-sqrt 9 3)
