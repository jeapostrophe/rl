#lang scheme
(require scheme/package
         tests/eli-tester)

(define XXX #f)

(define-struct obj (values messages parents)
  #:property prop:procedure
  (lambda (self slot . args)
    (hash-ref (obj-values self) slot
              (lambda ()
                (let/ec esc
                  (apply
                   (hash-ref (obj-messages self) slot
                             (lambda ()
                               (esc (self 'message-not-understood slot args))))
                   self XXX args))))))

(define (add-value self slot val)
  (struct-copy obj self
               [values (hash-set (obj-values self) slot val)]))

(define (add-method self slot imp)
  (struct-copy obj self
               [messages (hash-set (obj-messages self) slot imp)]))

(define-syntax add
  (syntax-rules (method value)
    [(_ self)
     self]
    [(_ self [method . args] . more)
     (add (add-method self . args) . more)]
    [(_ self [value . args] . more)
     (add (add-value self . args) . more)]))

(define self/c obj?)
(define resend/c false/c)
(define slot-id/c symbol?)
(define message-not-understood/c
  (self/c resend/c slot-id/c (listof any/c) . -> . any))

(define object% 
  (add (make-obj (make-immutable-hasheq empty)
                 (make-immutable-hasheq empty)
                 (make-immutable-hasheq empty))
       [method 'clone
               (contract (self/c resend/c . -> . self/c)
                         (lambda (self resend)
                           self)
                         (#%variable-reference) 'caller)]
       [method 'message-not-understood
               (contract message-not-understood/c
                         (lambda (self resend slot args)
                           (error 'fproto "Message not understood: ~e with arguments ~e" slot args))
                         (#%variable-reference) 'caller)]
       [method 'add-method-slot
               (contract (self/c resend/c slot-id/c contract? procedure? . -> . self/c)
                         (lambda (self resend method-slot slot/c method-imp)
                           (add self
                                [method method-slot 
                                        (contract slot/c method-imp
                                                  (#%variable-reference) 'caller)]))
                         (#%variable-reference) 'caller)]
       [method 'add-value-slot
               (contract (self/c resend/c slot-id/c slot-id/c contract? any/c . -> . self/c)
                         (lambda (self resend value-slot update-value-slot slot/c initial-value)
                           (add self
                                [method update-value-slot
                                        (contract (self/c resend/c slot/c . -> . self/c)
                                                  (lambda (self resend new-value)
                                                    (add-value self value-slot new-value))
                                                  (#%variable-reference) 'caller)]
                                [value value-slot
                                       (contract slot/c initial-value
                                                 (#%variable-reference) 'caller)]))
                         (#%variable-reference) 'caller)]))

(define-syntax send*
  (syntax-rules ()
    [(_ obj) obj]
    [(_ obj call . rest)
     (send* (obj . call) . rest)]))

; Examples
(package-begin
 (define fnord%
   (send* (object% 'clone)
     ['add-value-slot 'fnord 'update-fnord number?
                      23]))
 (test
  (fnord% 'fnord) => 23
  (fnord% 'unknown) =error> "understood"
  (fnord% 'update-fnord 'foo) =error> "contract"
  ((fnord% 'update-fnord 24) 'fnord) => 24))

(test
 (object% 'add-value-slot 'fnord 'update-fnord number?
          'foo) =error> "contract")

(package-begin
 (define account% 
   (send* object%
     ['add-value-slot 
      'balance 'set-balance number?
      0]
     ['add-method-slot
      'payment (self/c resend/c number? . -> . self/c)
      (lambda (self resend amount)
        (self 'set-balance
              (+ (self 'balance)
                 amount)))]))
 (define a1 (account% 'payment 100))
 (define a2 (account% 'payment 200))
 (test
  (a1 'balance) => 100
  (a2 'balance) => 200
  (a1 'payment #f) =error> "contract"
  ((a1 'payment -20) 'balance) => 80))

(package-begin
 (define create-on-use% 
   (send* object%
     ['add-method-slot
      'message-not-understood message-not-understood/c 
      (lambda (self resend slot args)
        42)]))
 (test
  (create-on-use% 'foo) => 42
  (create-on-use% 'bar) => 42))