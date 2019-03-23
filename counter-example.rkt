#lang racket/base
(require racket/place
         racket/serialize
         web-server/lang/serial-lambda)

(define mys
  (serial-lambda (f)
                 (define p (let ([p (place c (define f (deserialize (place-channel-get c))) (f c))])
                             (place-channel-put p (serialize f)) p))
                 (let-values ([(a b) (place-channel)]) (place-channel-put p (cons (serialize "pmatos") b)) a)
                 (place-wait p)))

(define f
  (serial-lambda (self)
                 (let-values ([(m q)
                               (let ([x (place-channel-get self)])
                                 (values (deserialize (car x)) (cdr x)))])
                   (displayln (format "hello world ~a" m)))))


