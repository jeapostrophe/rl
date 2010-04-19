#lang scheme
(require (planet jaymccarthy/fib-heap))

; http://en.wikipedia.org/wiki/A*_search_algorithm

#|
 function A*(start,goal)
     closedset := the empty set                 % The set of nodes already evaluated.
     openset := set containing the initial node % The set of tentative nodes to be evaluated.
     g_score[start] := 0                        % Distance from start along optimal path.
     h_score[start] := heuristic_estimate_of_distance(start, goal)
     f_score[start] := h_score[start]           % Estimated total distance from start to goal through y.
     while openset is not empty
         x := the node in openset having the lowest f_score[] value
         if x = goal
             return reconstruct_path(came_from,goal)
         remove x from openset
         add x to closedset
         foreach y in neighbor_nodes(x)
             if y in closedset
                 continue
             tentative_g_score := g_score[x] + dist_between(x,y)
             tentative_is_better := false
             if y not in openset
                 add y to openset
                 h_score[y] := heuristic_estimate_of_distance(y, goal)
                 tentative_is_better := true
             elseif tentative_g_score < g_score[y]
                 tentative_is_better := true
             if tentative_is_better = true
                 came_from[y] := x
                 g_score[y] := tentative_g_score
                 f_score[y] := g_score[y] + h_score[y]
     return failure
 
 function reconstruct_path(came_from,current_node)
     if came_from[current_node] is set
         p = reconstruct_path(came_from,came_from[current_node])
         return (p + current_node)
     else
         return the empty path
|#

; XXX I think this has a bug because the f-score updates aren't consider in the fib-heap
(define (a* distance heuristic neighbors start goal)
  (define closedset (make-hash))
  (define in-openset (make-hash))
  (define openset 
    (make-fib-heap
     (lambda (n1 n2) 
       (< (hash-ref f-score n1 +inf.0) 
          (hash-ref f-score n2 +inf.0)))))
  (define g-score (make-hash))
  (define f-score (make-hash))
  (define came-from (make-hash))
  (hash-set! g-score start 0)
  (hash-set! f-score start (heuristic start goal))
  (fib-heap-insert! openset start)
  
  (printf "New search~n")
  (let loop ()
    (match (fib-heap-extract-min! openset)
      [#f
       (error 'a* "No shortest path exists")]
      [x
       (if (equal? x goal)
           (list* start (reverse (reconstruct-path came-from goal)))
           (begin (printf "Visiting ~S~n" x)
                  (hash-set! closedset x #t)
                  (for ([y (in-list (neighbors x))])
                    (unless (hash-has-key? closedset y)
                      (printf "\tExpanding ~S~n" y)
                      (local [(define tentative-g-score 
                                (+ (hash-ref g-score x +inf.0) (distance x y)))
                              (define (update-scores!)
                                (hash-set! came-from y x)
                                (hash-set! g-score y tentative-g-score)
                                (hash-set! f-score y 
                                           (+ (hash-ref g-score y)
                                              (heuristic start goal))))]                              
                        (cond
                          [(not (hash-has-key? in-openset y))
                           (printf "\tAdding to open set (goal? ~S)~n" (equal? y goal))
                           (hash-set! in-openset y #t)
                           (update-scores!)
                           (fib-heap-insert! openset y)]
                          [(tentative-g-score . < . (hash-ref g-score y))
                           (update-scores!)]))))
                  (loop)))])))

(define (reconstruct-path came-from current)
  (cond
    [(hash-ref came-from current #f)
     => (lambda (from)
          (list* current (reconstruct-path came-from from)))]
    [else
     empty]))

(provide/contract
 [a* ((any/c any/c . -> . number?)
      (any/c any/c . -> . number?)
      (any/c . -> . (listof any/c))
      any/c
      any/c
      . -> .
      (listof any/c))])

#;(a* (match-lambda*
      [(list 1 6) 14]
      [(list 1 3) 9]
      [(list 1 2) 7]
      [(list 2 1) 7]
      [(list 2 3) 10]
      [(list 2 4) 14]
      [(list 3 6) 2]
      [(list 3 4) 11]
      [(list 3 2) 10]
      [(list 3 1) 9]
      [(list 4 2) 15]
      [(list 4 3) 11]
      [(list 4 5) 6]
      [(list 5 6) 9]
      [(list 5 4) 6]
      [(list 6 1) 14]
      [(list 6 5) 9]
      [else +inf.0])
    (lambda (x y) 
      (abs (- x y)))
    (match-lambda
      [1 (list 2 3 6)]
      [2 (list 1 3 4)]
      [3 (list 1 2 4 6)]
      [4 (list 2 3 5)]
      [5 (list 4 6)]
      [6 (list 1 3 5)])
    1
    5)
    