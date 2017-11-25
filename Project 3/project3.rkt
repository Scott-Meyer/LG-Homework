#lang racket
(require "main.rkt")
(require "project2.rkt")

;Pieces have a reachability function, a player, and a position
(struct P (reach player pos))

;u has an initial piece, final piece, horizon
(struct U (Pi Pf hori))
(struct G (Ps dem block))
;TODO: use this
(struct T (Pi tra len))

;TODO: Create input
;'((piece player position)(piece player position)
;  (piece player position)(piece player position))
;Reti endgame
(define reti (G (list (P Pawn 1 '(3 6))(P King 1 '(8 8))
                      (P Pawn 2 '(8 5))(P King 2 '(1 6)))
                '(8 8)
                '(empty)))

;midterm
(define midterm (list (P Pawn 1 '(8 5))(P Pawn 1 '(7 5))(P King 1 '(7 8))(P Bishop 1 '(6 2))
                      (P Pawn 2 '(5 6))(P King 2 '(6 5))(P Knight 2 '(5 1))))

;pdf8
(define pdf8 (G (list (P King 1 '(8 5))
                      (P King 2 '(6 6))(P Bishop 2 '(4 7)))
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

;steps
;;>(Z1 reti (U (P Pawn 1 '(3 6)) (P Pawn 1 '(3 8)) 2) '(empty) '(empty))
;;>(Z1 pdf8 (U (P King 1 '(8 5)) (P King 1 '(8 1)) 4) '(empty) '(empty))
;Zone1
(define (Z1 g u v w)
  (define (Q1 horizon)
    (not (empty? (filter (λ (h) (not (empty? (flatten h)))) horizon))))
  (let ([main (horizon (U-hori u) (P-reach (U-Pi u)) (P-pos (U-Pi u)) (P-pos (U-Pf u)) '(8 8))])
    (if (Q1 main) ;Q1
        (Z2 g u v w '(empty) '(empty)) ;two
        empty)))

;Zone2
(define (Z2 g u v w time next-time)
  (if #t ;Q2
      (let* ([tras (admissible 1 (P-reach (U-Pi u)) (P-pos (U-Pi u)) (P-pos (U-Pf u)) (G-dem g) (G-block g))]
             [tra (first tras)]
             [v (map (λ (p) (list p 1)) (tail tra))]
             [time (for/list ([i (range 2 (+ 1 (length tra)))]
                              [p (tail tra)])
                     (list p i))])
        (println (G-block g))
        (cons
         (list (U-Pi u) tra (- (length tra) 1))
         (Z3 g v w time next-time))) ;three
      empty))

;Zone3
(define (Z3 g v w time next-time)
  (define Q3
    (not (empty? (G-Ps g))))
  (if (not (empty? (G-Ps g))) ;Q3
      (if (member (P-pos (first (G-Ps g))) (map first time))
          (Z3 (tailG g) v w time next-time)
          (Z4 g v w time next-time))
      empty)) ;five

;Zone4
(define (Z4 g v w time next-time) 
  (define Q4 #t)
  (if #t ;Q4
      (let* ([p (first (G-Ps g))]
             ;Get trajectories that link from p to points on time
             [tras (foldl (λ (f acc)
                            (let ([tra (max-horizon (second f)    ;max horizon based on time
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
                                        (cons (first tra2) acc)))
                                  acc)))
                          empty time)])  
        (if (not (empty? tras))
            (append
             (foldl (λ (t acc) (cons
                                (list p t (- (length t) 1))
                                acc)) empty tras)
             (Z3 (tailG g) v w time next-time))
            ;(let* ([tra (first tras)]
            ;       [w (map (λ (p) (list p 1)) tra)]
            ;       [next-time (for/list ([i (range 2 (+ 2 (length tra)))]
            ;                             [p tra])
            ;                    (list p i))])
            ;  (cons
            ;   (list p tra (- (length tra) 1))
            ;   (Z3 (tailG g) v w time next-time)))
            (Z3 (tailG g) v w time next-time)));three
      empty)) ;three
  
;Zone5
(define (five u v w time next-time)
  (if (#t) ;Q5
      empty ;three
      empty)) ;six

;Zone6
(define (six u v w time next-time) empty)