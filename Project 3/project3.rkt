#lang racket
(require "main.rkt")
(require "project2.rkt")
(require plot)

;Pieces have a reachability function, a player, and a position
(struct P (reach player pos))

;u has an initial piece, final piece, horizon
(struct U (Pi Pf hori))
(struct G (Ps dem block))
;TODO: use this
(struct T (Pi tra len))
(struct Fix (reach func))

(define player-specific (list (Fix Pawn (λ (p) (Pawnp p)))))
(define (get-plsp p)
  (if (member (P-reach p) (map Fix-reach player-specific))
      (first (filter (λ (ps) (equal? (P-reach p) (Fix-reach ps))) player-specific))
      #f))
(define (fixG g)
  (G (foldl (λ (p acc)
              (if (get-plsp p)
                  (cons
                   (P ((Fix-func (get-plsp p)) (P-player p)) (P-player p) (P-pos p))
                   acc)
                  (cons p acc))) empty (G-Ps g))
     (G-dem g)
     (G-block g)))

(define (notempty? x)
  (not (empty? x)))

;TODO: Create input
;'((piece player position)(piece player position)
;  (piece player position)(piece player position))
;Reti endgame
(define reti (G (list (P Pawn W '(3 6))(P King W '(8 8))
                      (P Pawn B '(8 5))(P King B '(1 6)))
                '(8 8)
                (list empty)))

;midterm
(define midterm (list (P Pawn 1 '(8 5))(P Pawn 1 '(7 5))(P King 1 '(7 8))(P Bishop 1 '(6 2))
                      (P Pawn 2 '(5 6))(P King 2 '(6 5))(P Knight 2 '(5 1))))

;pdf8
(define pdf8 (G (list (P Pawn B '(8 5))(P King B '(2 4))
                      (P King W '(6 6))(P Bishop W '(4 7)))
                '(8 8)
                '((4 5)(5 6)(6 7)(7 4)(7 3))))


;Lets do a simple problem first
; Given a main piece and a goal for it, lets make its zone
;;>(simple (list Pawn (list 3 6)) '(3 8) reti)
(define (simple main goal game)
  (let* ([main-tra (first (admissible 1 (first main) (second main) goal '(8 8)))]
         [u '(0 0 0)]
         [v '((map (λ (p) (list p 1)) main-tra))]
         [w '((empty))]
         [time (for/list ([i (range 2 (+ 2 (length main-tra)))]
                          [p main-tra])
                 (list p i))]
         [next-time '((empty))])
    time))

;TODO: update graph, make it take a game state (because zones)

(define (tailG g)
  (G (tail (G-Ps g)) (G-dem g) (G-block g)))
(define (fpG g)
  (first (G-Ps g)))


;;;;;;;BEGIN ZONE BASED ON CLASS ZONE, REQUIRES MAIN START/GOAL;;;;;;;;;;
;;>(Z1 reti (U (P Pawn 1 '(3 6)) (P Pawn 1 '(3 8)) 2) empty empty))
;;>(Z1 pdf8 (U (P King 1 '(8 5)) (P King 1 '(8 1)) 4) empty empty))
;Zone1
(define Z1
  (λ (g u v w time next-time [f-main empty])
  (define gf (fixG g))
  (define (Q1 horizon)
    (not (empty? (filter (λ (h) (not (empty? (flatten h)))) horizon))))
  (let ([main (horizon (U-hori u) (P-reach (U-Pi u)) (P-pos (U-Pi u)) (P-pos (U-Pf u)) '(8 8))])
    (if (Q1 main) ;Q1
        (Z2 gf u v w time next-time f-main) ;two
        empty))))

;Zone2
(define (Z2 g u v w time next-time f-main)
  (define Q2 #t)
  (if Q2
      (let* ([tras (admissible 1 (P-reach (U-Pi u)) (P-pos (U-Pi u)) (P-pos (U-Pf u)) (G-dem g) (G-block g))]
             ;first shortest tra from p to p in u, unless we are forcing a main
             [tra (if (empty? f-main)
                      (first tras)
                      f-main)]
             [v (map (λ (p) (list p 1)) (tail tra))]
             [time (for/list ([i (range 2 (+ 1 (length tra)))]
                              [p (tail tra)])
                     (list p i))])
        (cons
         (list (U-Pi u) tra (- (length tra) 1))
         (Z3 g u v w time next-time))) ;three
      empty))

;Zone3
(define Z3
  (λ (g u v w time next-time [not-connected empty])
    (define Q3
      (not (empty? (G-Ps g))))
    (if Q3
        (if (member (P-pos (first (G-Ps g))) (map first time))
            (Z3 (tailG g) u v w time next-time)
            (Z4 g u v w time next-time not-connected))
        (Z5 g u v w time next-time not-connected)))) ;five

;Zone4
(define Z4
  (λ (g u v w time next-time [not-connected empty])
    (define Q4 #t)
    (define (is-mp? p)
      (equal? (P-player p) (P-player (U-Pi u))))
    (if Q4
        (let* ([next-time next-time]
               [w w]
               [p (first (G-Ps g))]
               ;Get trajectories that link from p to points on time
               [tras (foldl (λ (f acc)
                              (let* (;cur-time is based on the 2nd value of each time (f) for opposite player
                                     [cur-time (if (is-mp? p) 1 (second f))]
                                     ;Get trajectories from each piece to each spot on time (f)
                                     [tra (max-horizon cur-time    ;max horizon based on time
                                                       (P-reach p)   ;reachability of piece
                                                       (P-pos p)     ;position of piece
                                                       (first f)     ;goal based on time
                                                       (G-dem g)
                                                       (G-block g))])
                                (if tra
                                    ;Remove paths that go through main trajectory
                                    (let ([tra2 (filter (λ (t) (not (check-duplicates
                                                                     (append (map first time)
                                                                             (tail (reverse t)))))) tra)])
                                      (if (empty? tra2)
                                          acc
                                          ;We have decided to add a trajectory from a piece to something on V/Time
                                          ;We have to add that trajectory to W/Nexttime, and add it to the acc list
                                          (let* ([tra2 (first tra2)] ;only do the first trajectory
                                                 [tra2-len (- (length tra2) 1)]
                                                 [n-t-val (+ 1 (- cur-time tra2-len))]
                                                 [w-pts (if (empty? w) w (map first w))]
                                                 [nt-pts (if (empty? next-time) next-time (map first next-time))]
                                                 [ispos? (λ (pt pos) (equal? (first pt) pos))])
                                            (begin
                                              (set! w (foldl (λ (p acc) (if (member p w-pts)
                                                                            acc
                                                                            (cons (list p 1) acc))) w (tail tra2)))
                                              (set! next-time (foldl (λ (p acc) (if (member p nt-pts)
                                                                                    (if (> n-t-val (second (first (filter (λ (po) (ispos? po p)) acc))))
                                                                                        (cons (list p n-t-val) (filter (λ (w-p) (not (ispos? w-p p))) acc))
                                                                                        acc)
                                                                                    (cons (list p n-t-val) acc))) next-time (tail tra2)))
                                              (cons tra2 acc)))))
                                    acc)))
                            empty time)])  
          (if (not (empty? tras))
              (append
               (foldl (λ (t acc) (cons
                                  (list p t (- (length t) 1))
                                  acc)) empty tras)
               (Z3 (tailG g) u v w time next-time not-connected))
              ;(let* ([tra (first tras)]
              ;       [w (map (λ (p) (list p 1)) tra)]
              ;       [next-time (for/list ([i (range 2 (+ 2 (length tra)))]
              ;                             [p tra])
              ;                    (list p i))])
              ;  (cons
              ;   (list p tra (- (length tra) 1))
              ;   (Z3 (tailG g) v w time next-time)))
              (Z3 (tailG g) u v w time next-time (cons (fpG g) not-connected))));three
        empty))) ;three
  
;Zone5
(define (Z5 g u v w time next-time not-connected)
  (define Q5
    (not (empty? w)))
  (if Q5
      (let ([new-g (G not-connected (G-dem g) (G-block g))])
        (Z3  new-g u w empty next-time empty))
      (Z6 g u v w time next-time not-connected))) ;six

;Zone6
(define (Z6 g u v w time next-time not-connected) empty)
;;;;;;;END ZONE BASED ON CLASS ZONE, REQUIRES MAIN START/GOAL;;;;;;;;;;



;;;;;;;BEGIN GRAPHING/DISPLAYING;;;;;;;
;(Z1 pdf8 (U (P King 1 '(8 5)) (P King 1 '(8 1)) 4) '(empty) '(empty))
;;>(graph-zone pdf8 (U (P (Pawnp B) B '(8 5)) (P (Pawnp B) B '(8 1)) 4))
;;>(graph-zone reti (U (P (Pawnp W) 1 '(3 6)) (P (Pawnp W) 1 '(3 8)) 2))
;;>(graph-zone reti (U (P King B '(1 6)) (P King B '(3 8)) 2))
#|>Assignment 5-11
(graph-zone (G (list (P King B '(2 7))(P Knight B '(7 7))(P Pawn B '(5 5))
                     (P King W '(2 1))(P Bishop W '(6 2))(P Pawn W '(3 3))(P Pawn W '(4 5))) '(8 8) empty)
            (U (P Bishop W '(6 2))(P Bishop W '(5 5)) 2)
            '((6 2)(4 4)(5 5)))
|##|Assignment 5-12
(zone (G (list (P King W '(8 8))(P Pawn W '(3 6))
               (P King B '(1 6))(P Pawn B '(8 5)))
       '(8 8)
       empty))
|#
;TODO: Print pieces
(define graph-zone
  (λ (g u [f-main empty])
  (let* ([p1-color "blue"]
         [p2-color "black"]
         [blocked (G-block g)]
         [nonup (filter (λ(p)(not(or(equal? (P-pos p) (P-pos (U-Pi u)))
                                    (equal? (P-pos p) (P-pos (U-Pf u)))))) (G-Ps g))]
         [newblock (if (empty? nonup)
                       (G-block g)
                       (append (G-block g) (map P-pos nonup)))]
         [g2 (G (G-Ps g) (G-dem g) newblock)]
         [z (Z1 g2 u empty empty empty empty f-main)]
         [pieces (λ (p color) (list (points (map P-pos p) #:size 10 #:line-width 10 #:color color)))]
         [filter-for-player (λ (player p) (equal? (P-player p) player))]
         [P1p (filter (λ (p) (filter-for-player 1 p)) (G-Ps g))]
         [P2p (filter (λ (p) (filter-for-player 2 p)) (G-Ps g))])

    ;(for/list ([t z])
    ;  (write-string (string-join (list "t(" (~a (P-reach (first t))) "," (~a (second t)) "," (~a (third t)) ")\n"))))
    (plot (append*
           (make-board (G-dem g))
           ;Graph blocked spaces
           (if (empty? (flatten blocked))
               empty
               (list (points blocked #:size 12 #:line-width 12 #:color "red")))
           ;Graph player 1 pieces
           (pieces P1p p1-color)
           ;Graph player 2 pieces
           (pieces P2p p2-color)
           (if (> (length z) 0)
               (list
                ;make main traj look different
                (let* ([t (first z)]
                       [color (if (equal? (P-player (first t)) 1) p1-color p2-color)]
                       [tra (second t)])
                  (cons
                   (points tra #:size 1 #:line-width 11 #:alpha 0.8 #:color color)
                   (list (lines tra #:width 3 #:alpha 0.5 #:color color))))
                ;Map the zone
                (for/list ([t (tail z)])
                  (let ([color (if (equal? (P-player (first t)) 1) p1-color p2-color)]
                        [tra (second t)])
                    (cons
                     (points tra #:size 1 #:line-width 10 #:alpha 0.5 #:color color)
                     (list (lines tra #:width 3 #:alpha 0.25 #:color color))))))
               empty))
          #:width 250
          #:height 250
          #:title (string-append "Zone: "
                                 (~a (P-reach (first (first z))))
                                 (~a (first (second (first z))))
                                 (~a (last (second (first z))))
                                 "\n")
          #:x-label #f
          #:y-label #f))))
;;;;;;;END GRAPHING/DISPLAYING;;;;;;;


;;;;;;;Modify zone, to run for all attack                      ;;;;;;;;;;
;;>(zone reti)
(define (zone g)
  (let* ([g (fixG g)]
         [pieces (G-Ps g)]
         ;pieces other than given piece
         [other-p (λ(p) (filter (λ(p2)
                                  (not (equal?(P-pos p)
                                              (P-pos p2))))
                                pieces))]
         ;attackable pieces
         [attack-p (λ(p) (filter (λ(p2)
                                   (not (equal?(P-player p)
                                               (P-player p2))))
                                 (other-p p)))])
    (filter notempty?
            (flatten
             (for/list ([p pieces])
               (let* ([pdist (distance (P-reach p) (P-pos p) (G-dem g) (G-block g))]
                      ;;Special case, pawns attack end of board
                      [x (first (G-dem g))]
                      [y (second (G-dem g))]
                      [apieces (if (member (P-reach p) (list Pawn PawnB PawnW))
                                   (foldl (λ(x acc)(cons (P empty empty (list x 1))
                                                         (cons (P empty empty (list x y))
                                                               acc)))
                                          (attack-p p) (range 1 (+ 1 x)))
                                   (attack-p p))])
                 (for/list ([ap apieces])
                   (let* ([appos (P-pos ap)]
                          [pdistap (filter (λ(p) (equal? appos (first p))) pdist)])
                     (if (empty? pdistap)
                         empty
                         (let* ([minhor (second (first pdistap))]
                                [ap (P (P-reach p) (P-player p) (P-pos ap))]
                                [u (U p ap minhor)])
                           (begin
                             ;(println (list (P-reach p) (P-pos p) (P-pos ap) minhor))
                             (graph-zone g u)
                             )))))))))))
  
;;;;;;;End modify                                              ;;;;;;;;;;