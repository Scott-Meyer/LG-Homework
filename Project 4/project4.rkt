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

(define (first-move g)
  (let* ([zones (zone reti #:graph #f)]
         [maint (first (first zones))]
         [mainp (first maint)]
         [maintra (second maint)]
         [g-ps (remove mainp (G-Ps g))]
         [maint-fm (second maintra)]
         [mainpm (P (P-reach mainp) (P-player mainp) maint-fm)]
         [g2 (G (cons mainpm g-ps) (G-dem g) (G-block g))])
    (zone g2)))

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
    (println P1pr)
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
           (if (> (length P1pr) 0)
             (list (lines P1pr #:width 3 #:alpha 0.5 #:color p1-color))
             empty)
           (if (> (length P2pr) 0)
             (list (lines P2pr #:width 3 #:alpha 0.5 #:color p2-color))
             empty)
          #:width g-board-size
          #:height g-board-size
          ;#:title "Game"
          #:x-label #f
          #:y-label #f))))