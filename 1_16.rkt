#lang racket

(letrec ([fast-exp (λ (base pow)
  (cond ([= 0 pow] 1)
    ([even? pow] (sqr (fast-exp base (/ pow 2))))
    (else (* base (fast-exp base (- pow 1))))))])
  (displayln (fast-exp 6 6)))

(define (fast-exp base pow)
  (define (reduction-possible? pow)
    (and (even? pow) (> pow 2)))
  (define (fast-exp-iter base pow acc)
    (define-syntax-rule (base^2)
      (* base base))
    (cond ([= 0 pow] acc)
      ([reduction-possible? pow] (fast-exp-iter (base^2) (/ pow 2) 1))
      (else (fast-exp-iter base (- pow 1) (* acc base)))))
  (fast-exp-iter base pow 1))

(displayln (fast-exp 4 32)) ; => 18446744073709551616, 7 steps
