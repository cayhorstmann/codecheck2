;;SOLUTION
#lang racket
(provide positive-nums-only)
;;CALL '(1 2 -4 90 -4)
;;CALL '(-4 -3 0)
(define (positive-nums-only lst)
;;HIDE
  (cond [(null? lst) lst]
        [(> (car lst) 0) (cons (car lst) (positive-nums-only (cdr lst)))]
        [else (positive-nums-only (cdr lst))])
;;SHOW ...
)
