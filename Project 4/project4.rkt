#lang racket
(require "main.rkt")
(require "project2.rkt")
(require "project3.rkt")
(require plot)
(provide (all-defined-out))

;Reti endgame
(define reti (fixG (G (list (P Pawn W '(3 6))(P King W '(8 8))
                            (P Pawn B '(8 5))(P King B '(1 6)))
                      '(8 8)
                      (list empty))))



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