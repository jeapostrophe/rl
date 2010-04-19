#lang scheme

(define (retry-monad* tries default go)
  (if (zero? tries)
      default
      (go
       (lambda ()
         (retry-monad* (sub1 tries) default go)))))
(define-syntax retry-monad
  (syntax-rules ()
    [(_ tries default [id expr] ...)
     (retry-monad* tries default
                   (lambda (retry)
                     (retry-inner retry [id expr] ...)))]))
(define-syntax retry-inner
  (syntax-rules ()
    [(_ retry [fid fexpr] . rst)
     (retry-inner2 retry fid [fid fexpr] . rst)]))
(define-syntax retry-inner2
  (syntax-rules ()
    [(_ retry last)
     last]
    [(_ retry last [id expr] . rst)
     (let ([id expr])
       (if id (retry-inner2 retry id . rst)
           (retry)))]))

(provide retry-monad)