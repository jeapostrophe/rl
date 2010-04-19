#lang scheme
(require tests/eli-tester
         scheme/package
         (for-syntax scheme))

(define-struct object (class))
(define-struct class-info (iface this? new methods))
(define-struct interface-info (pos-blame mk-method->c))

(define object<%> (make-interface-info (#%variable-reference) (make-immutable-hasheq empty)))

; XXX Add super classes
; XXX Add multiple interfaces (or rather, interface operators)
; XXX Test struct copy with different classes
; XXX Record interface info in class, so is-a? will work, etc
; XXX Does something implement an interface if it changes the contract?

; hash-merge : hash hash -> hash
; merges all of h2 into h1, preferring h1 
(define (hash-merge h1 h2)
  (for/fold ([h h1])
    ([(k v) (in-hash h2)])
    (hash-update h k (lambda (ok) ok) (lambda () v))))

(package-begin
 (define h1 (make-immutable-hash (list (cons 'x 1) (cons 'y 2))))
 (define h2 (make-immutable-hash (list (cons 'x 2) (cons 'z 2))))
 (define h3 (hash-merge h1 h2))
 (test
  (hash-ref h3 'x) => 1
  (hash-ref h3 'y) => 2
  (hash-ref h3 'z) => 2))

; interface-merge : interface interface -> interface
; create a new interface with all of i1's methods and i2's methods where i1 does not override them
(define (interface-merge i1 i2)
  (make-interface-info
   (interface-info-pos-blame i1)
   (hash-merge (interface-info-mk-method->c i1)
               (interface-info-mk-method->c i2))))

(define-syntax (contract-prepend stx)
  (syntax-case stx (-> ->* ->d)
    [(_ this? (-> . rest))
     (syntax/loc stx
       (-> this? . rest))]
    [(_ this? (->* man . rest))
     (syntax/loc stx
       (->* (this? . man) . rest))]
    [(_ this? (->d man . rest))
     (syntax/loc stx
       (->* ([this this?] . man) . rest))]))

; XXX Check for duplicates in methods
(define-syntax (interface stx)
  (syntax-case stx ()
    [(_ (super<%>) [method pre-method-contract] ...)
     (with-syntax ([this? (datum->syntax stx 'this? stx)])
       (with-syntax ([(method-contract ...)
                      (map (lambda (m pmc)
                             (syntax-case m (new)
                               [new pmc]
                               [_ (quasisyntax/loc pmc (contract-prepend this? #,pmc))]))
                           (syntax->list #'(method ...))
                           (syntax->list #'(pre-method-contract ...)))])
         (syntax/loc stx
           (interface-merge
            (make-interface-info
             (#%variable-reference)
             (make-immutable-hasheq
              (list (cons 'method (lambda (this?) method-contract))
                    ...)))
            super<%>))))]))

(define-syntax (class* stx)
  (syntax-case stx (fields new define)
    [(_ super% interface<%>
        (fields f ...)
        (define (new . new-fmls) new-body ...)
        (define (method . fmls) method-body ...) ...)
     (with-syntax
         ([this (datum->syntax stx 'this stx)]
          [update-this (datum->syntax stx 'update-this stx)]
          [make-this (datum->syntax stx 'make-this stx)]
          [this? (datum->syntax stx 'this? stx)]
          [obj (datum->syntax stx 'obj stx)]
          [(this-f ...) 
           (map (lambda (f) (datum->syntax f (string->symbol (format "this-~a" (syntax->datum f))) f))
                (syntax->list #'(f ...)))])
       (syntax/loc stx
         (local [(define-struct (this object) (f ...))
                 (define the-class
                   (make-class-info interface<%> this? 
                                    (local [(define (new . new-fmls)
                                              (let ([make-this
                                                     (lambda (f ...)
                                                       (make-this the-class f ...))])
                                                new-body ...))]
                                      new)
                                    (make-immutable-hash
                                     (list (cons 'method
                                                 (local [(define (method obj . fmls)
                                                           (local [(define-syntax-rule (update-this [field new-value] (... ...))
                                                                     (struct-copy this obj
                                                                                  [field new-value]
                                                                                  (... ...)))
                                                                   (define f (this-f obj))
                                                                   ...]
                                                             method-body
                                                             ...))]
                                                   method))
                                           ...))))]
           the-class)))]))



(define-syntax (new stx)
  (syntax-case stx ()
    [(_ class% arg ...)
     (quasisyntax/loc stx
       (match class%
         [(struct class-info (iface this? new methods))
          (define method->c (interface-info-mk-method->c iface))
          ((contract ((hash-ref method->c 'new) this?) new
                     (interface-info-pos-blame iface) (#%variable-reference)
                     #'#,(syntax/loc stx new))
           arg ...)]))]))

(define-syntax (object-method stx)
  (syntax-case stx ()
    [(_ obj method-id)
     (quasisyntax/loc stx
       (match (object-class obj)
         [(struct class-info (iface this? new methods))
          (define method->c (interface-info-mk-method->c iface))
          (contract ((hash-ref method->c 'method-id) this?) (hash-ref methods 'method-id)
                    (interface-info-pos-blame iface) (#%variable-reference)
                    #'#,(quasisyntax/loc stx method-id))]))]))

(define-syntax (send stx)
  (syntax-case stx ()
    [(_ obj method-id arg ...)
     (quasisyntax/loc stx
       (let ([obj* obj])
         ((object-method obj* #,(datum->syntax stx (syntax->datum #'method-id) stx))
          obj* arg ...)))]))

(define-syntax (send/apply stx)
  (syntax-case stx ()
    [(_ obj method-id arg ... arg-list-expr)
     (quasisyntax/loc stx
       (let ([obj* obj])
         (apply (object-method obj* #,(datum->syntax stx (syntax->datum #'method-id) stx))
                obj* arg ... arg-list-expr)))]))

(provide object<%>
         interface 
         class*
         new
         send
         send/apply)