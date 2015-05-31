#lang racket

; 0|           1
; 1|         1   1
; 2|       1   2   1
; 3|     1   3   3   1
; 4|   1   4   6   4   1
; 5| 1   5   A   A   5   1

(define (pascal-triangle n)
  (define (lr-push-1 li)
    (cons 1 (reverse (cons 1 li))))
  (define (pascal-triangle-iter li)
    (if (> (length li) n) li
      (pascal-triangle-iter (lr-push-1
        (for/list ([i (rest li)] [j (drop-right li 1)]) (+ i j))))))
  (cond ([= n 0] '(1)) ([= n 1] '(1 1))
    (else (pascal-triangle-iter '(1 1)))))

(define height 5)
(for ([i height])
  (printf "~a~a\n" (make-string (- height i) #\space) (pascal-triangle i)))
