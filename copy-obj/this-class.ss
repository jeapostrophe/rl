#lang scheme/base
(require (for-syntax scheme/base)
         scheme/class
         scheme/stxparam)

(provide class% class%* init-slot slot copy this%)

;; define-stxparam-override
;; Hack, since splicing-syntax-parameterize doesn't work inside of classes.
(require (for-syntax scheme/private/stxparamkey))
(define-syntax (define-stxparam-locally stx)
  (syntax-case stx ()
    [(define-stxparam-locally sp-id rhs)
     (let* ([rt (syntax-local-value #'sp-id)]
            [sp (if (set!-transformer? rt)
                    (set!-transformer-procedure rt)
                    rt)]
            [id (syntax-local-get-shadower
                 (syntax-local-introduce
                  (syntax-parameter-target sp)))])
       (with-syntax ([id id])
         #'(define-syntaxes (id) rhs)))]))

;; Support for 'this%'

;; The has-this%<%> protocol:
;;  - class implements interface 'has-this<%>'
;;  - method 'get-this%' returns the most specific class of
;;    its target, if that class was declared with 'class%'
;;  - class accepts 'prototype-init' and if the super
;;    class is also a 'has-this%<%>' class, also passes it
;;    to the superclass initializer

;; "secret" method
(define-local-member-name get-this%)

;; interface tagging classes with 'this%' support
(define has-this%<%> (interface () get-this%))

;; this% SYNTAX
;; (Depends on lexical context of occurrence)
(define-syntax-parameter this%
  (lambda (stx)
    (raise-syntax-error 'this% "not inside class% body" stx)))


;; Support for 'copy'

;; "secret" init name for prototype object
(define-local-member-name prototype-init)

;; prototype-var
;; Within a class body, the value of prototype-var is
;; the fresh internal name for the the prototype object
(define-syntax-parameter prototype-var #f)

;; get-prototype SYNTAX
;; within a class% body, the value of the prototype object
;; May only be used in same position 'init' variables may be used
;; (not within methods).
(define-syntax (get-prototype stx)
  (syntax-case stx ()
    [(get-prototype)
     (let ([base (syntax-parameter-value #'prototype-var)])
       (unless base
         (raise-syntax-error 'get-prototype "used out of context" stx))
       (syntax-local-get-shadower base))]))

;; copy SYNTAX
(define-syntax (copy stx)
  (syntax-case stx ()
    [(copy [init-name init-value] ...)
     (with-syntax ([this% (datum->syntax #'here 'this% stx)])
       (syntax/loc stx
         (new this%
              [prototype-init this]
              [init-name init-value] ...)))]))


;; Syntax for self-aware, copiable classes

;; class%
(define-syntax (class% stx)
  (syntax-case stx ()
    [(class% s% form ...)
     #'(class%* s% () form ...)]))

;; make-identifier-transformer : syntax -> syntax -> syntax
(define-for-syntax (make-identifier-transformer out-stx)
  (lambda (stx)
    (syntax-case stx ()
      [(me . args)
       (datum->syntax stx
                      `(,#'(#%expression me) . ,#'args)
                      stx)]
      [me
       (identifier? #'me)
       out-stx])))

(define (ensure-has-this%<%> %)
  (if (implementation? % has-this%<%>)
      %
      (has-this%-mixin %)))

;; class%*
(define-syntax (class%* stx)
  (syntax-case stx ()
    [(class%* s% (interface ...) rst ...)
     (with-syntax ([c% (or (syntax-local-name) #'anonymous%)])
       #`(let ([super% (ensure-has-this%<%> s%)])
           (letrec ([base%
                     (class* super% (has-this%<%> interface ...)
                       ;; information for 'this%'
                       (define/override (get-this%) c%)
                       ;; information for initialization via 'copy'
                       (init [(internal-prototype-var prototype-init)])
                       (define-stxparam-locally this% (make-identifier-transformer #'(get-this%)))
                       (define-stxparam-locally prototype-var #'internal-prototype-var)
                       rst ...)]
                    [c%
                     (class base%
                       ;; Just duplicates the special init arg:
                       ;; one copy for base%, one copy for super%
                       (init [prototype-init #f])
                       (super-new [prototype-init prototype-init]
                                  [prototype-init prototype-init]))])
             c%)))]))

;; has-this%-mixin
;; Makes an ordinary class follow the 'has-this%<%>' protocol
(define has-this%-mixin
  (mixin () (has-this%<%>)
    (init prototype-init)
    (define/public (get-this%) (error 'never-called))
    (super-new)))


;; Syntax for defining fields that can be initialized
;; from a prototype's fields.

;; init-slot SYNTAX
;; The initial value of a field defined via 'init-slot' is obtained
;; from the following sources, in order of precedence:
;; - specified explicitly at 'new' or 'copy' site
;; - implicitly from object calling 'copy'
;; - the default value expression
(define-syntax (init-slot stx)
  (syntax-case stx ()
    [(init-slot slot ...)
     (with-syntax
         ([(field ...)
           (map (lambda (s)
                  (syntax-case s ()
                    [[name default-value]
                     (syntax/loc s
                       [name
                    (or (and (get-prototype) (get-field name (get-prototype)))
                        default-value)])]
                    [name
                     (syntax/loc s
                       [name (and (get-prototype) (get-field name (get-prototype)))])]))
                (syntax->list #'(slot ...)))])                     
     (syntax/loc stx
       (init-field field ...)))]))

;; slot SYNTAX
;; The initial value of a field defined via 'slot' is obtained from
;; the following sources, in order of precedence:
;;  - implicitly from object calling 'copy'
;;  - the default value expression
(define-syntax (slot stx)
  (syntax-case stx ()
    [(slot [name default-value] ...)
     #'(field [name
               (or (and (get-prototype) (get-field name (get-prototype)))
                   default-value)]
              ...)]))

