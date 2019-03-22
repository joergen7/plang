#lang plang

(define (f1 _a) (displayln "hello world"))
(define p1 (spawn f1))


(define (f2 self) (recv self (m _b) (displayln m)))
(define p2 (spawn f2))
(! p2 "hello world")

