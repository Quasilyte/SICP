#lang racket

;; f(n) = n if n < 3
;; f(n) = f(n-1) + 2f(n-2) + 3f(n - 3) if n >= 3
; f(0) = 0
; f(1) = 1
; f(2) = 2
; f(3) = 4
; f(4) = 11
; f(5) = 25

;; [tree] Recursive solution:
; (define (f n)
;   (if (< n 3) n
;     (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

;; Iterative solution:
(define (f n)
  (define (f-calc-member prev3)
    (+ (first prev3) (* 2 (second prev3)) (* 3 (third prev3))))
  (define (f-iter n prev3)
    (let ([next-first (f-calc-member prev3)])
      (if (> n 3)
        (f-iter (- n 1) (cons next-first (take prev3 2)))
        next-first)))
  (if (< n 3) n
    (f-iter n (list 2 1 0))))

(for ([i 6])
  (printf "f(~a) = ~a\n" i (f i)))
