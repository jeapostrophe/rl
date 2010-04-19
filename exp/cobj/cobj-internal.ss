#lang scheme/base
(require (for-syntax scheme/base))

(define-struct interface-info (supers methods))

(define-struct class-info (instantiate))

(define-struct object (methods))

(define (instantiate class% . args)
  (apply (class-info-instantiate class%) args))

(define-syntax (send stx)
  (syntax-case stx ()
    [(_ obj method arg ...)
     (syntax/loc stx
       (((object-methods obj) 'method) arg ...))]))

(provide (all-defined-out))
