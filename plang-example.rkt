#lang plang

;; in the interactive, call
;; (spawn-my-place f)
;;
;; this example should work but does not
;; dynamic-require: name is not provided

(define (spawn-my-place f)
  (define p2 (spawn f))
  (! p2 "hello world")
  (place-wait p2))

(define (f self)
  (recv self (m _b) (displayln m)))


