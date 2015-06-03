#lang racket

;; 1_18 is also here.

(define (mul-guard fn a b)
  (cond ([= 0 b] 0)
    ([> b 0] (fn a b))
    (else (- (fn a (- b))))))

(define (mul a b)
  (define (mul-iter acc b)
    (if (= 1 b) acc
      (mul-iter (+ acc a) (sub1 b))))
  (mul-guard mul-iter a b))

(define (fast-mul a b)
  (define (reduction-possible? b)
    (and (even? b) (> b 2)))
  (define (reduce a b)
    (list (* a 2) (/ b 2) (* a 2)))
  (define (fast-mul-iter a b acc)
    (cond ([= 1 b] acc)
      ([reduction-possible? b] (apply fast-mul-iter (reduce a b)))
      (else (fast-mul-iter a (sub1 b) (+ acc a)))))
  (define (entry-point a b)
    (if (reduction-possible? (- b 1))
      (+ a (apply fast-mul-iter (reduce a (- b 1))))
      (fast-mul-iter a b a)))
  (mul-guard entry-point a b))

(for ([i 6])
  (let ([result (mul 5 i)] [fast-result (fast-mul 5 i)])
    (display (list 5 "*" i "=" fast-result))
    (displayln (= (* 5 i) result fast-result))))
