#lang racket

; Recursive sum.
; (define (sum term a next b)
;   (if (> a b) 0
;     (+ (term a) (sum term (next a) next b))))

; Iterative sum.
(define (sum term a next b)
  (define (sum-iter a result)
    (if (> a b) result
      (sum-iter (next a) (+ result (term a)))))
  (sum-iter a 0))

(define (cast op initial term a next b)
  (define (cast-iter a result)
    (if (> a b) result
      (cast-iter (next a) (op result (term a)))))
  (cast-iter a initial))

(define (cast-sum term a next b)
  (cast + 0 term a next b))

(define (cast-prod term a next b)
  (cast * 1 term a next b))

(define (self x) x)

(define (factorial x)
  (cast-prod self 1 add1 x))

(define (cast-filter pred op initial term a next b)
  (define (cast-filter-iter a result)
    (if (> a b) result
      (cast-filter-iter (next a) (let ([t (term a)])
        (if (pred t) (op result t) result)))))
  (cast-filter-iter a initial))

(define (cast-filter-sum pred term a next b)
  (cast-filter pred + 0 term a next b))

(define (cast-filter-prod pred term a next b)
  (cast-filter pred * 1 term a next b))

(cast-sum self 1 add1 10)
(= (cast-prod self 1 add1 10) (factorial 10))
(cast-filter-sum even? self 1 add1 10)
