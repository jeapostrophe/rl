#lang scheme

(define obj<%>
  (interface ()
    short-desc
    render))

(define obj%
  (class* object% (obj<%>)
    (define/public (short-desc)
      (error 'short-desc "No short description"))
    
    (define/public (render)
      (error 'render "No rendering"))
    
    (super-new)))

(define paren%
  (class* obj% (obj<%>)
    (init-field color orient)
    
    (define/override (short-desc)
      (format "~a ~a paren" color orient))
    
    (define/override (render)
      (string->symbol (format "~a-~a-paren" color orient)))
    
    (super-new)))

(provide/contract
 [obj% (implementation?/c obj<%>)]
 [paren% (implementation?/c obj<%>)])