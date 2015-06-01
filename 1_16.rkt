#lang racket

(provide fast-exp)

; (letrec ([fast-exp (λ (base pow)
;   (cond ([= 0 pow] 1)
;     ([even? pow] (sqr (fast-exp base (/ pow 2))))
;     (else (* base (fast-exp base (- pow 1))))))])
;   (displayln (fast-exp 6 6)))

(define (fast-exp base pow)
  (define (reduction-possible? pow)
    (and (even? pow) (> pow 2)))
  (define (fast-exp-iter base pow acc)
    (cond ([= 0 pow] acc)
      ([reduction-possible? pow] (fast-exp-iter (sqr base) (/ pow 2) 1))
      (else (fast-exp-iter base (- pow 1) (* acc base)))))
  (if (reduction-possible? (sub1 pow))
    (* base (fast-exp-iter (sqr base) (/ (sub1 pow) 2) 1))
    (fast-exp-iter base pow 1)))

; (displayln (fast-exp 4 32)) ; => 18446744073709551616, 7 steps
; (displayln (fast-exp 4 5)) ; => 1024, 3 steps

