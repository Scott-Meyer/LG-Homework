#lang racket
(require "dyoo-while-loop.rkt")
;(require dyoo-while-loop)
;(println "Hello")
;(define a (read-string (current-input-port) 'any))
;(println a)
(provide distance Queen Rook Knight King Pawn Bishop)

(define dems '(0 0))
(define blocked empty)

(define (inbounds? x y p)
  (let ([x1 (first p)]
        [y1 (second p)])
        (if (and
         (<= 1 x1)
         (<= x1 x)
         (<= 1 y1)
         (<= y1 y)) #t #f)))
(define (movement x)
  (cond
    [(equal? x 0) 0]
    [(< x 0) (+ x 1)]
    [(< 0 x) (- x 1)]))
(define (King xc yc p)(and (<= (abs xc) 1) (<= (abs yc) 1)))
(define (Knight xc yc p) (or (and (equal? 2 (abs xc)) (equal? 1 (abs yc))) (and (equal? 1 (abs xc)) (equal? 2 (abs yc)))))
(define (Bishop xc yc p)
  (if (and (equal? xc 0) (equal? yc 0))
      #t
      (and
       (equal? (abs xc) (abs yc))
       (not (member (list (+ (first p) xc) (+ (second p) yc)) blocked))
       (Bishop (movement xc) (movement yc) p))))
(define (Rook xc yc p)
  (if (and (equal? xc 0) (equal? yc 0))
      #t
      (and 
       (or (equal? yc 0) (equal? xc 0))
       (not (member (list (+ (first p) xc) (+ (second p) yc)) blocked))
       (Rook (movement xc) (movement yc) p))))
(define (Pawn xc yc p) (and (equal? (abs yc) 1) (zero? xc)))
(define (Queen xc yc p) (or (Rook xc yc p) (Bishop xc yc p)))

(define (reachable? reachability p1 p2)
  (let* ([x1 (first p1)]
         [y1 (second p1)]
         [x2 (first p2)]
         [y2 (second p2)]
         [xc (- x2 x1)]
         [yc (- y2 y1)])
    (if (and
         (inbounds? (first dems) (second dems) p2)
        ;Do Rk
         (reachability xc yc p1))
        #t
        #f)))


;Given a reachable? function, an initial position, and possibly a list of unreachable locations
;   return a list of all the valid next positions.
(define valid-pos
  (λ (reachable? reachability initial [blocked (list initial)])
    ;A function that returns a list of all position pairs
    (define (positions h w) (for*/list ([i h]
                                        [j w])
                              (list (+ 1 j) (+ 1 i))))
    ;A function that, given a position, says if its possible to get there with reacability
    (define (valid? p) (if (and (reachable? reachability initial p)
                                (not (member p blocked))) #t #f))
    ;Filter the list of all positions, to only valid ones
    (filter valid? (positions (second dems) (first dems)))))

;given an initial position, get pairs of all rechable positions and their distances
;   (x, y) -> [((x, y) n)]
(define (distances reachability pos blocked)
  (let ([start (cons (list pos 0) (map (λ (x) (list x -1)) blocked))])
    (define (distances* visited)
      (let* ([cur-dist (apply max (map second visited))]
             [cur-posl (map first (filter (λ (x) (equal? cur-dist (second x))) visited))]
             [next-pos (drop
                        (foldl (λ (p found) (append found (valid-pos reachable? reachability p found))) (map first visited) cur-posl)
                        (length visited))])
        (if (empty? next-pos)
            visited
            (distances* (append (map (λ (x) (list x (+ 1 cur-dist))) next-pos) visited)))))
    (distances* start)))

;reachability (function) init (x y) dem (Width Height) block '((3 2) (1 1))
(define distance
         (λ (reachability init dem [block (list empty)])
           (begin
             (set! blocked block)
             (set! dems dem)
             (distances reachability init blocked))))

;;; This is for project one, and printing/running
;;; Not actually needed to solve the problem.

;Print the board based on reacabilites.
(define (print-board positions)
  (for/list ([i (range 1 (+ 1 (second dems)))])
    (for/list ([j (range 1 (+ 1(first dems)))])
      (let* ([cur-p (list j i)]
             [indx (index-of (map first positions) cur-p)])
        (cond
          [(member cur-p blocked) "X"]
          [indx (number->string (second (list-ref positions indx)))]
          [else " "])))))

;Adding auto run ability for .exe file, comment out for testing
(define (main)
  (let* ([p1 (display "Piece? ")]
         [piece (read-line)]
         [p2 (display "Initial x position? ")]
         [x (string->number (read-line))]
         [p3 (display "Initial y position? ")]
         [y (string->number (read-line))]
         [p4 (display "Height of board? ")]
         [height (string->number (read-line))]
         [p5 (display "Width of board? ")]
         [width (string->number (read-line))]
         [p6 (display "Blocked positions? (specific format, x1,y1;x2,y2;x3,y3) \n        ")]
         [blocked (map (λ (p) (list (string->number (first (string-split p ",")))
                                    (string->number (second (string-split p ","))))) (string-split (read-line) ";"))])
    (cond
      [(equal? piece "knight") (print-board (distance Knight (list x y) (list height width) blocked))]
      [(equal? piece "king") (print-board (distance King (list x y) (list height width) blocked))]
      [(equal? piece "queen") (print-board (distance Queen (list x y) (list height width) blocked))]
      [(equal? piece "bishop") (print-board (distance Bishop (list x y) (list height width) blocked))]
      [(equal? piece "pawn") (print-board (distance Pawn (list x y) (list height width) blocked))]
      [(equal? piece "rook") (print-board (distance Rook (list x y) (list height width) blocked))])))