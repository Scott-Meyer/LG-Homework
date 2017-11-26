#lang racket
(require "main.rkt")
(require plot)

;first -> head
(define (head ls) (first ls))

;rest -> tail
(define (tail ls) (rest ls))

;Union together two lists of reachability
(define (union ls1 ls2)
  ;Used to fold, takes a position, 2nd reacability list, accumulator
  (define (addsecond p ls acc)
    (let [(from-ls (memf (λ (arg) (equal? (head p) (head arg))) ls))]
      (if from-ls
          (cons (list (head p) (+ (second p) (second (head from-ls)))) acc)
          acc)))
  (filter (λ (item) (> (second item) 0))
          (foldl (λ (x acc) (addsecond x ls2 acc)) empty ls1)))

;Filter for posisionts viable for trajectories
(define (valid-spots ls)
  (let [(m (apply min (map second ls)))]
    (filter (λ (x) (equal? m (second x))) ls)))

;Get shorest possible path for a trajectory
;(shortest-len (distance/union)) -> 4
(define (shortest-len ls)
  (apply min (map second ls)))

;;>(move-positions (distance ---) 2)
;;((2 1) (1 2))
(define (move-positions spots move)
  (map first
       (filter
        (λ (x) (equal? move (second x)))
        spots)))

;Get the shortest trajectory
(define shortest
         (λ (reachability init final dem [block (list empty)])
  (let* [(start (distance reachability init dem block))
        (end (distance reachability final dem block))
        (valids (map first(valid-spots (union start end))))
        (dist (λ (cur-p) (distance reachability cur-p dem block)))
        (next-moves (λ (cur-p cur-m)
                      (set-intersect
                       valids
                       (move-positions start cur-m)
                       (move-positions (dist cur-p) 1))))]
    (shortest2 next-moves (list init) 1))))

;>(shortest2 (f => (x y) i -> [[x y][x y]]) ([[x y]->[x y]]) i)
;'(((init) (x y) (x y) (final))
;  ((init) (x y) (x y) (final))
; etc)
(define (shortest2 next-moves path cur-m)
  (let [(next (next-moves (head path) cur-m))
        (shortest-map (λ (p) (shortest2 next-moves (cons p path) (+ cur-m 1))))]
    (if (empty? next)
        (list (reverse path))
        (apply append (map shortest-map next)))))


;merge path to stop and path from stop
;;>(merge '(([1 1] [8 1])) '(([8 1] [1 8])))
;;'(([1 1] [8 1] [1 8]))
(define (merge to-stop from-stop)
  (append*
   (for/list ([t to-stop]
              [f from-stop])
     (append*
      (for/list ([t-traj t])
        (for/list ([f-traj f])
          (append t-traj (tail f-traj))))))))

;Get the SUM table/matrix
(define (sum reachability init final dem block)
  (union (distance reachability init dem block)
         (distance reachability final dem block)))

;length->degree
;;>(length->degree 2 (sum Queen '(1 1) '(1 8) '(8 8) '(empty)))
;;2
(define (length->degree len SUM)
  (let ([lengths (sort (remove-duplicates (map second SUM)) < )])
    (if (member len lengths)
        (first
         (flatten
          (for/list ([i (range 1 (+ (length lengths) 1))]
                     [l lengths])
            (if (equal? l len) i empty))))
        #f)))

;admissible
;;>(admissible 2 Queen '(1 1) '(1 8) '(8 8) empty)
;;'(((1 1) (8 8) (1 8))
;;  etc)
(define admissible
  (λ (n reachability init final dem [block (list empty)])
    (let* ([degree (- n 1)]
           [start (distance reachability init dem block)]
           [end (distance reachability final dem block)]
           [SUM (union start end)]
           [short (λ (i f) (shortest reachability i f dem block))])
      (cond
        [(>= 1 n) (short init final)]
        [(= 2 n) (let* ([dist (+ (shortest-len SUM) degree)]
                        [stops (map first(filter (λ (x) (equal? dist (second x))) SUM))]
                        [to-stop (map (λ (f) (short init f)) stops)]
                        [from-stop (map (λ (i) (short i final)) stops)])
                   (merge to-stop from-stop))]
        [else (let* ([shortest (shortest-len SUM)]
                     [dist (+ shortest degree)]
                     [stops (map first (filter (λ (x) (member (second x) (range 2 (+ 1 dist)))) SUM))])
                (remove-duplicates
                 (append*
                  (for/list ([stop stops])
                    (append*
                     (for/list ([i (range shortest dist)]
                                [j (reverse (range shortest dist))])
                       (let* ([sum-to-stop (sum reachability init stop dem block)]
                              [ad-to-stop (length->degree i sum-to-stop)]
                              [sum-from-stop (sum reachability stop final dem block)]
                              [ad-from-stop (length->degree j sum-from-stop)])
                         (if (and ad-from-stop ad-to-stop)
                             (let ([to-stop (admissible ad-to-stop reachability init stop dem block)]
                                   [from-stop (admissible ad-from-stop reachability stop final dem block)])  
                               (map (λ (xs) (append (first xs) (tail (second xs))))
                                    (cartesian-product to-stop from-stop)))
                             empty))))))))]))))
    

;Paths with horizon
;;>(horizon 5 Queen '(1 1) '(1 8) '(8 8) empty)
;;'((;1 (([1 1] [1 8])))
;;  (;2 (([1 1] [8 8] [1 8])
;;      ([1 1] [8 1] [1 8])
;; etc
(define horizon
  (λ (h reachability init final dem [block (list empty)])
    (let* ([start (distance reachability init dem block)]
           [end (distance reachability final dem block)]
           [degrees (sort (remove-duplicates (map second (union start end))) < )]
           [h-deg (filter (λ (x) (<= x h)) degrees)])
      (append
       (for/list ([i (range 1 (head degrees))])
         (list (list empty)))
       (for/list ([i (range 1 (+ (length h-deg) 1))])
         (admissible i reachability init final dem block))))))


;example data
;(distance piece [x y] [h w] [(xy) (xy)])
;(define start (distance King (list 1 1) (list 8 8) empty))
;(define end (distance King (list 2 5) (list 8 8) empty))
;(define startq (distance Queen (list 1 1) (list 8 8) empty))
;(define endq (distance Queen (list 1 8) (list 8 8) empty))
;(define example (union start end))

;(map first (valid-spots example))

;(let [(one-move (map first(filter (λ (x) (equal? 1 (second x))) start)))]
;  (filter (λ (x) (member (first x) one-move)) (valid-spots example)))

;(shortest King (list 1 1) (list 2 5) (list 8 8) empty)


;Makes the lines to draw a board onto a graph
(define (make-board dem)
  (let [(x-min 0.5)
        (x-max (+ 0.5 (second dem)))
        (y-min 0.5)
        (y-max (+ 0.5 (first dem)))]
    (map lines
         (append
          (for/list [(x (range x-min (+ 1 x-max)))]
            (list (list y-min x)(list y-max x)))
          (for/list [(y (range y-min (+ 1 y-max)))]
            (list (list y x-min)(list y x-max)))))))

;Graph all shortest paths from point to point for piece
(define graph
  (λ (h reachability init final dems [blocked (list empty)])
  (let [(admissibilities (horizon h reachability init final dems blocked))]
    (for/list [(paths admissibilities)]
      (if (empty? (flatten paths))
          empty
          (plot (append
                 (list (points (list init final) #:size 10 #:line-width 10))
                 (if (empty? (flatten blocked))
                     empty
                     (list (points blocked #:size 12 #:line-width 12 #:color "red")))
                 (make-board dems)
                 (map (λ (x) (points x #:size 15 #:line-width 1)) paths)
                 ;    (map (λ (x) (lines x #:width 5 #:alpha 0.2)) paths)))))
                 (for/list [(path paths)
                            (i (range 1 (+ (length paths) 1)))]
                   (lines path #:color i #:width 5 #:alpha 0.2)))))))))


;Main function for exe distribution
(define (main)
  (graph Queen '(4 2) '(6 5) '(8 8) empty))

  