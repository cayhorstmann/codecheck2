;;SOLUTION
#lang racket
(provide reverse-list)
(define (reverse-list lst)
  ;;HIDE
  (foldl
  cons '() lst)
  ;;SHOW ...
  )      
