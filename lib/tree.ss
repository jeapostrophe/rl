#lang scheme

#|
This simple tree data-structure is efficient, because it doesn't require allocation for unary trees, and doesn't overload lists, so they can be
elements.
|#

(define-struct tree ())
(define-struct (a-tree-node tree) (e))
(define-struct (an-empty-tree tree) ())
(define-struct (a-tree-union tree) (left right))
(define-struct (a-tree-list tree) (lst))

(define tree-empty (make-an-empty-tree))
(define (tree-for-each f t)
  (match t
    [(struct an-empty-tree ())
     (void)]
    [(struct a-tree-union (l r))
     (tree-for-each f l)
     (tree-for-each f r)]
    [(struct a-tree-list (l))
     (for-each f l)]
    [(struct a-tree-node (e))
     (f e)]))
(define (tree-union l r) 
  (make-a-tree-union l r))
(define (tree-list l)
  (make-a-tree-list l))
(define (tree-node v)
  (make-a-tree-node v))

(define tree/c tree?)

(provide/contract
 [tree? (any/c . -> . boolean?)]
 [tree/c contract?]
 [tree-empty tree?]
 [tree-node (any/c . -> . tree/c)]
 [tree-union (tree/c tree/c . -> . tree?)]
 [tree-list ((listof any/c) . -> . tree?)]
 [tree-for-each ((any/c . -> . void) tree/c . -> . void)])