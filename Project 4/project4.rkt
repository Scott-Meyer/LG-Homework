#lang racket
(require "main.rkt")
(require "project2.rkt")
(require "project3.rkt")
(require plot)
(provide (all-defined-out))
(require math/array)	

;Reti endgame
(define reti (fixG (G (list (P Pawn W '(3 6))(P King W '(8 8))
                            (P Pawn B '(8 5))(P King B '(1 6)))
                      '(8 8)
                      (list empty))))

;move
(struct move (player p1 p2))
(struct node (move children))

(define pdf18p10
  (let* ([a (node (move B "b7" "c8") empty)]
         [b (node (move B "b7" "c7") empty)]
         [c (node (move W "c7" "c8") (list a))]
         [d (node (move W "h8" "g7") (list b))]
         [e (node (move B "a6" "b7") (list c d))]
         [f (node (move W "c6" "c7") (list e))]

         [g (node (move W "e5" "d6") empty)]
         [h (node (move B "h4" "h3") (list g))]
         [i (node (move B "b6" "c6") empty)]
         [j (node (move B "b6" "c7") empty)]
         [k (node (move W "f6" "e5") (list i h))]
         [l (node (move W "c6" "c7") (list j))]
         [m (node (move W "c6" "b7") empty)]
         [n (node (move B "b6" "c7") empty)]
         [o (node (move B "b6" "c7") empty)]
         [p (node (move B "h5" "h4") (list l k))]
         [q (node (move B "b6" "b7") (list m))]
         [r (node (move W "c6" "c7") (list n))]
         [s (node (move W "g7" "f6") (list o p q))]
         [t (node (move B "a6" "b7") (list r s))]

         [u (node (move W "e5" "d6") empty)]
         [v (node (move W "c6" "b7") empty)]
         [1a (node (move B "b7" "c8") empty)]
         [2a (node (move B "b7" "c7") empty)]
         [w (node (move B "b6" "c7") empty)]
         [x (node (move B "b6" "c6") empty)]
         [y (node (move B "h4" "h3") (list u))]
         [z (node (move B "b6" "b7") (list v))]
         [3a (node (move W "c7" "c8") (list 1a))]
         [4a (node (move W "g7" "f6") (list 2a))]
         [a2 (node (move W "c6" "b7") empty)]
         [b2 (node (move W "c6" "c7") (list w))]
         [c2 (node (move W "f6" "e5") (list x y z))]
         [d2 (node (move W "f6" "e7") empty)]
         [e2 (node (move W "c7" "c8") empty)]
         [5a (node (move B "a6" "b7") (list 3a 4a))]
         [f2 (node (move B "a6" "b7") (list a2))]
         [g2 (node (move B "a6" "b6") (list b2 c2))]
         [h2 (node (move B "h4" "h3") (list d2))]
         [i2 (node (move B "h5" "h4") (list e2))]
         [j2 (node (move W "c6" "c7") (list 5a))]
         [k2 (node (move W "g7" "f6") (list f2 g2 h2))]
         [l2 (node (move W "c6" "c7") (list i2))]
         [m2 (node (move B "h5" "h4") (list j2 k2))]
         [n2 (node (move B "a6" "b5") (list l2))]

         [o2 (node (move W "h8" "g7") (list t m2 n2))])
    (node empty (list f o2))))

(define (print-tree node)
  (cond
    [(empty? (node-children node)) (println (list (move-p1 (node-move node)) (move-p2 (node-move node))))]
    [else (map print-tree (node-children node))]))


;Move a piece along a trajectory
(define (move-t p t)
  (if (< (length t) 2)
      (error (string-append "Trajectory given to move-t doesn't have moves"
                            (~a t)))
      (if (not (equal? (P-pos p) (first t)))
          (error "move-t given a t for a different p?")
          (P (P-reach p) (P-player p) (second t)))))

;Given a zt return the new piece
(define (movezt zt)
  (let* ([p (first zt)]
         [tra (second zt)]
         [h (third zt)])
    (move-t p tra)))

;Apply a t object from a zone
(define (applyzt g zt)
  (let* ([p (first zt)]
         [g-ps (remove p (G-Ps g))]
         [p2 (movezt zt)])
    (G (cons p2 g-ps) (G-dem g) (G-block g))))

;Given a list of zones, give back only those that include a player
(define (zones-with player zones)
  (define (is-player? zt)
    (equal? player (P-player (first zt))))
  (define (has-player? z)
    (ormap is-player? z))
  (filter has-player? zones))

;given a zone, return only the t objects with a player
(define (zones-player player zones)
  (define (is-player? zt)
    (equal? player (P-player (first zt))))
  (define (has-player? z)
    (filter is-player? z))
  (filter notempty? (map has-player? zones)))

;All possible games for one player's possible moves
(define (player-move-games g p)
  ;stuff
  empty)

;Play game
(define (play-game g fp)
  (let* ([zones (zone g #:graph #f)]
         [withmoves (zones-player fp zones)]
         [sp (other-player fp)]
         [mains (first (zones-player sp (zones-with sp zones)))])
    (if (> (length withmoves) 0)
        (map graph-game
             (map (λ(z) (applyzt g z)) (map first withmoves)))
        empty)))

(define (first-move g)
  (let* ([zones (zone reti #:graph #f)]
         [mainzone (first zones)]
         [mainz (first mainzone)]
         [g2 (applyzt g mainz)])
    (list (graph-game g)
          (graph-game g2))))
    ;(zone g2)))

;Graph a game
(define (graph-game g)
  (let* ([p1-color "blue"]
         [p2-color "black"]
         [blocked (G-block g)]
         [pieces (λ (p color) (list (points (map P-pos p) #:size 10 #:line-width 10 #:color color)))]
         [filter-for-player (λ (player p) (equal? (P-player p) player))]
         [P1p (filter (λ (p) (filter-for-player 1 p)) (G-Ps g))]
         [P2p (filter (λ (p) (filter-for-player 2 p)) (G-Ps g))]
         [d1? (λ (d) (equal? 1 (second d)))]
         [r1 (λ (p) (filter d1? (distance (P-reach p) (P-pos p) (G-dem g) (G-block g))))]
         [m1 (λ (p) (for/list ([r (r1 p)]) (list (P-pos p) (first r))))]
         [P1pr (map m1 P1p)]
         [P2pr (map m1 P2p)])
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
           (for/list ([p P1pr])
             (for/list ([tra p])
                   (lines tra #:width 3 #:alpha 0.5 #:color p1-color)))
           (for/list ([p P2pr])
             (for/list ([tra p])
                   (lines tra #:width 3 #:alpha 0.5 #:color p2-color))))
          #:width g-board-size
          #:height g-board-size
          ;#:title "Game"
          #:x-label #f
          #:y-label #f)))


;;;;----useful state functions
(struct state (i end d m v from to child sibling parent who sign board))
;(struct-copy state cstate [i 10])


;;;;-------Grammar functions------
(define BIG_NUMBER 999999)
(define (m g)
  (let* ([pieces (G-Ps g)]
         [filter-for (λ (player)
                       (λ (p)
                         (equal? (P-player p) player)))]
         [w-pieces (filter (filter-for W) pieces)]
         [b-pieces (filter (filter-for B) pieces)])
    (- (length w-pieces)
       (length b-pieces))))

;;;;-------GRS--------
(define (Grs1 i)
  (define Q1
    #t)
  (let ([start-state (state 0
                             1
                             0
                             (list (m reti))
                             (list BIG_NUMBER)
                             (list from?)
                             (list to?)
                             (list 0)
                             (list 0)
                             (list 0)
                             (list who?)
                             1
                             reti)])
    start-state))
    

(define (Grs2 i fvar)
  (define Q2
    #t)
  "stuff")

(define (Grs3 i fvar)
  (define Q3
    #t)
  "stuff")