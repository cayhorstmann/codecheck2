#lang racket
(require test-engine/racket-tests)
(require "Rev.rkt")
(check-expect (reverse-list '(1 2 3)) '(3 2 1))
(check-expect (reverse-list '(1)) '(1))
(check-expect (reverse-list '()) '())
(test)
