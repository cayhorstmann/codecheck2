;;SOLUTION
;;IN (1 2 3 4 5)
;;IN ()
#lang racket

(define (reverse-list lst)
  (foldl
  ;;HIDE
  cons '() lst)
  ;;SHOW ...
  )

(let ((arg (read (current-input-port))))
     (writeln (reverse-list arg)))
