;;SOLUTION
;;IN (1 2 3 4 5)
;;IN ()
#lang racket
(provide main)

(define (reverse-list lst)
  (foldl
  ;;HIDE
  cons '() lst)
  ;;SHOW ...
  )

(define (main . args)
        (let ((arg (read (current-input-port))))
                (writeln (reverse-list arg))))     
