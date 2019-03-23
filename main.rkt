;;-----------------------------------------------------------------------------
;;
;; Racket #lang for places and parallel computing
;;
;; Copyright 2019 Jörgen Brandt <joergen@cuneiform-lang.org>
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;
;;-----------------------------------------------------------------------------

#lang racket/base


;;=============================================================================
;; Provides
;;=============================================================================

(provide #%app
         #%module-begin
         #%top
         #%top-interaction
         (rename-out [_#%datum #%datum])

         define-values
         let-values
         make-pipe
         spawn
         !
         ?
         recv
         place-wait
         module+
         format
         (rename-out [_car       car]
                     [_cdr       cdr]
                     [_define    define]
                     [_displayln displayln]
                     [_lambda    lambda]
                     [_lambda    λ]
                     [_read      read]
                     [_values    values]
                     [_write     write]))


;;=============================================================================
;; Requires
;;=============================================================================

(require (for-syntax (only-in racket/base
                              #%app
                              #%datum
                              quote
                              raise-syntax-error
                              string
                              syntax)
                     
                     (only-in syntax/parse
                              boolean
                              id
                              number
                              syntax-parse))

         (only-in racket/place
                  place
                  place-wait
                  place-channel
                  place-channel-get
                  place-channel-put)

         (only-in web-server/lang/serial-lambda
                  serial-lambda)

         (only-in racket/serialize
                  deserialize
                  serialize))


;;=============================================================================
;; Reader
;;=============================================================================

(module reader syntax/module-reader
  plang)


;;=============================================================================
;; Literals
;;=============================================================================

(define-syntax (_#%datum stx)
  (syntax-parse stx
    [(_ . v:number)  #'(#%datum . v)]
    [(_ . v:string)  #'(#%datum . v)]
    [(_ . v:boolean) #'(#%datum . v)]
    [(_ . v)        (raise-syntax-error '#%datum "bad datum literal" #'v)]))



;;=============================================================================
;; Serializable Functions
;;=============================================================================

(define-syntax (_define stx)
  (syntax-parse stx
    [(_ x:id e)                       #'(define x e)]
    [(_ (f:id x:id ...) e ...)        #'(define f (serial-lambda (x ...) e ...))]
    [(_ (f:id x:id ... . y:id) e ...) #'(define f (serial-lambda (x ... . y) e ...))]
    [(_ e ...)                        (raise-syntax-error 'define "bad definition" stx)]))

(define-syntax (_lambda stx)                                          ; our lambdas support neither default
  (syntax-parse stx                                                   ; arguments nor keyword arguments
    [(_ (x:id ...) e ...)        #'(serial-lambda (x ...) e ...)]
    [(_ (x:id ... . y:id) e ...) #'(serial-lambda (x ... . y) e ...)] ; (λ x e) is a special case of this clause
    [(_ e ...)                   (raise-syntax-error 'lambda "bad function definition" stx)]))



;;=============================================================================
;; Pipes
;;=============================================================================

(define (_read in) (deserialize (read in)))
(define (_write x out) (write (serialize x) out))

;;=============================================================================
;; Places
;;=============================================================================

(define-syntax (spawn stx)
  (syntax-parse stx
    [(_ f) #'(let ([p (place c (define f (deserialize (place-channel-get c))) (f c))])
               (place-channel-put p (serialize f)) p)]))
; [(_ m f a) #'()]))

(define-syntax (! stx)
  (syntax-parse stx
    [(_ p v) #'(let-values ([(a b) (place-channel)]) (place-channel-put p (cons (serialize v) b)) a)]))

(define-syntax (? stx)
  (syntax-parse stx
    [(_ p) #'(let ([x (place-channel-get p)]) (values (deserialize (car x)) (cdr x)))]))

(define-syntax (recv stx)
  (syntax-parse stx
    [(_ p (m:id q:id) e ...) #'(let-values ([(m q) (? p)]) e ...)]))

;;=============================================================================
;; Racket
;;=============================================================================

(define _car (serial-lambda (l) (car l)))
(define _cdr (serial-lambda (l) (cdr l)))
(define _values (serial-lambda l (apply values l)))
(define _displayln (serial-lambda (s) (displayln s)))

