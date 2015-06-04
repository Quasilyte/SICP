#lang racket

(define (fib n)
  (define (fib-iter a b p q count)
    (define-syntax-rule (px)
      (+ (sqr p) (sqr q)))
    (define-syntax-rule (qx)
      (- (sqr (+ p q)) (sqr p)))
    (cond ([= 0 count] b)
      ([and (even? count) (> count 2)] (fib-iter a b (px) (qx) (/ count 2)))
      (else (let ([bq (* b q)] [aq (* a q)] [ap (* a p)] [bp (* b p)])
        (fib-iter (+ bq aq ap) (+ bp aq) p q (- count 1))))))
  (fib-iter 1 0 0 1 n))

(define fib-seq (list
  0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597))

(for ([i (length fib-seq)])
  (let ([expected (list-ref fib-seq i)] [got (fib i)])
    (if (= expected got)
      (printf "fib[~a] = ~a\n" i got)
      (printf "< failed! expected ~a, got ~a\n" expected got))))
