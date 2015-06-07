#lang racket

(define (compose f g)
  (λ (x) (f (g x))))

(define (do-times f n)
  (define (iter compound n)
    (if (< n 2) compound
      (iter (compose f compound) (sub1 n))))
  (iter f n))

(define (rep x)
  (displayln (list "x= " x))
  (add1 x))

; ((compose sqr add1) 6) ; => 49
; ((do-times sqr 2) 5) ; => 625

(define (smoothed f)
  (λ (x)
    (let* ([dx 0.0001] [lft (f (- x dx))] [mid (f x)] [rgt (f (+ x dx))])
      (/ (+ lft mid rgt) 3))))

(define (n-fold-smooth f n)
  ((do-times smoothed n) f))

(define smoothies (hasheq
  5 ((smoothed (smoothed (smoothed (smoothed (smoothed sin))))) 5)
  4 ((smoothed (smoothed (smoothed (smoothed sin)))) 5)
  3 ((smoothed (smoothed (smoothed sin))) 5)))

(for ([(smooth-level sin-value) (in-hash smoothies)])
  (displayln (= sin-value ((n-fold-smooth sin smooth-level) 5))))
