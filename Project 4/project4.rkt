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


;;;Depth first traversal of tree, to get ordered list of moves.
(define (moves2 n)
  (if (empty? (node-children n))
      (node-move n)
      (cons
       (node-move n)
       (flatten (map moves2 (node-children n))))))
(define moves
  (let* ([head pdf18p10]
         [hc (node-children head)])
    (append* (map moves2 hc))))
(define moves-global moves)
(define dmoves-global empty)

(define (print-move m)
  (print (list (move-player m) (move-p1 m) (move-p2 m))))


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

;know other player
(define (other-player p)
  (if (zero? p) 1 0))

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

;given a list, change value at position k to val
(define (list-set! ls k val)
  (define (list-set2! ls k val)
    (if (zero? k)
        (cons val (cdr ls))
        (cons (car ls) (list-set! (cdr ls) (- k 1) val))))
        
  ;;;Specific to our problem. If position k doens't exist, initialize it to 0
  (if (> (+ k 1) (length ls))
      (list-set! (append ls (list 0)) k val)
      (list-set2! ls k val)))

;;in the "moves" list, positions on the board are given in the form, a2. We need the form (list 1 2)
(define (convert-move m)
  (let* ([x (string-ref m 0)]
         [y (string-ref m 1)])
    (list (- (char->integer x) 96)
          (- (char->integer y) 48))))
  

;;;;----useful state functions
;Ever node has a parent, child sibling.
(struct snode (p c s))

(struct state (end d m v from to parent child sibling who sign board))
;(struct-copy state cstate [i 10])
; to change one thing in a struct, (struct-copy `struct name` `struct instance` [`thing` `new value`])

(define (print-state fvar)
  (println (list
            "end" (state-end fvar)
            "d" (state-d fvar)
            "m" (state-m fvar)
            "v" (state-v fvar)
            "from" (state-from fvar)
            "to" (state-to fvar)
            "parent" (state-parent fvar)
            "child" (state-child fvar)
            "sibling" (state-sibling fvar)
            "who" (state-who fvar)
            "sign" (state-sign fvar))))

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
(define (LEAF a b)
  (if (equal? a  BIG_NUMBER)
      b
      a))
(define (parent i fvar)
  (list-ref (state-parent fvar) i))
(define (TRANSITION-1 fvar)
  (let* ([ps (G-Ps (state-board fvar))]
         [cmove (last dmoves-global)]
         [p# (- (length ps)
                (length (member (convert-move (move-p2 cmove))
                                (map P-pos ps))))]
         [cp (list-ref ps p#)]
         [nps (list-set! ps p# (P (P-reach cp) (P-player cp) (convert-move (move-p1 cmove))))])
    (begin
     (set! dmoves-global (remove (last dmoves-global) dmoves-global equal?))
     ;(println (graph-game (struct-copy G (state-board fvar) [Ps nps])))
     (struct-copy G (state-board fvar) [Ps nps]))))
(define (TRANSITION fvar)
  (let* ([ps (G-Ps (state-board fvar))]
         [cmove (first moves-global)]
         [p# (- (length ps)
                (length (member (convert-move (move-p1 cmove))
                                (map P-pos ps))))]
         [cp (list-ref ps p#)]
         [nps (list-set! ps p# (P (P-reach cp) (P-player cp) (convert-move (move-p2 cmove))))])
    (begin
      ;FIX: make the single move a list of one.
     (set! dmoves-global (append dmoves-global (list (head moves-global))))
     (set! moves-global (tail moves-global))
     (struct-copy G (state-board fvar) [Ps nps]))))
(define (MINIMAX SN v1 v2)
  (if (equal? 1 SN)
      (max v1 v2)
      (min v1 v2)))
(define (CUT fvar)
  (let* ([ps (G-Ps (state-board fvar))]
         [p-pos (map P-pos ps)])
  (and
   (member (convert-move (move-p1 (head moves-global))) (map P-pos (G-Ps (state-board fvar))))
   (if (check-duplicates p-pos)
       #f
       #t))))
;;;; Global State
;;;;-------GRS--------
(define (Grs1 i)
  (define Q1
    #t)
  (let ([start-state (state 
                      1 ;end
                      0 ;d
                      (list (m reti)) ;m
                      (list BIG_NUMBER) ;v
                      (list 0) ;from
                      (list 0) ;to
                      (list 0) ;parent
                      (list 0) ;child
                      (list 0) ;sibling
                      (list 0) ;who
                      1 ;sign
                      reti)]) ;board ;game
    (Grs2 0 start-state)))
    

(define (Grs2 i fvar)
  (define Q2
    (and
     (or
      (equal? (state-sign fvar) (move-player (first moves-global)))
      (and
       (equal? (state-sign fvar) -1)
       (equal? (move-player (first moves-global)) 2)))
     (< (state-d fvar) 20)
     (CUT fvar)))
  (if Q2
      ;if q2, do Grs2
      (let* ([np (list-set! (state-parent fvar) (state-end fvar) i)]
             ;FIX: problems here due to non initialized, check if child(i) exists.
             [nzc (if (>= (length (state-child fvar)) (+ i 1))
                      (not (zero? (list-ref (state-child fvar) i)))
                      #f)]
             [ns (if nzc
                     (list-set! (state-sibling fvar)
                                (list-ref (state-child fvar) i)
                                (state-end fvar))
                     (list-set! (state-sibling fvar) i 0))]
             [nc (if nzc
                     ;FIX: need to add a case here it initialize
                     ;(list-set! (state-child fvar) i 0)
                     (list-set! (state-child fvar) i (state-end fvar)))]
             [nboard (TRANSITION fvar)]
             [nm (list-set! (state-m fvar) (state-end fvar) 0)]
             [nv (list-set! (state-v fvar) (state-end fvar) (* BIG_NUMBER (state-sign fvar)))]
             [nwho (list-set! (state-who fvar) (state-end fvar) 0)]
             [nfrom (list-set! (state-from fvar) (state-end fvar) 0)]
             [nto (list-set! (state-to fvar) (state-end fvar) 0)]
             [nstate (struct-copy state fvar
                                  [parent np]
                                  [child nc]
                                  [sibling ns]
                                  [end (+ (state-end fvar) 1)]
                                  [d (+ (state-d fvar) 1)]
                                  [sign (- (state-sign fvar))]
                                  [board nboard]
                                  [m nm]
                                  [v nv]
                                  [who nwho]
                                  [from nfrom]
                                  [to nto])])
        (begin
         (println (string-append "Grammar 2 with: i=" (number->string i)))
         (print-state fvar)
         (println (graph-game (state-board fvar)))
         (let*-values ([(pis cstate) (Grs2 (state-end fvar) nstate)]
                       [(pis2 cstate2) (Grs2 i cstate)])
           (values (append pis (list (state-end fvar)) pis2) cstate2))))
      ;else do Grs3
      (Grs3 i fvar)))
               

(define (Grs3 i fvar)
  (define Q3
    #t)
  (begin
    (println (string-append "Grammar 3 with: i=" (number->string i)))
    (print-state fvar)
    (println (graph-game (state-board fvar)))
    (let* ([dnzero? (not (zero? (state-d fvar)))]
           [nd (if dnzero?
                   (- (state-d fvar) 1)
                   (state-d fvar))]
           [nsign (if dnzero?
                      (- (state-sign fvar))
                      (state-sign fvar))]
           [pi (parent i fvar)]
           [vs (state-v fvar)]
           [ms (state-m fvar)]
           [leafvmi (LEAF (list-ref vs i)
                          (list-ref ms i))]
           [pnv (list-set! vs ;list of v
                           pi ;specfici v we are chaning
                           ;new value
                           (MINIMAX (state-sign fvar)
                                    (list-ref vs pi)
                                    leafvmi))]
           [nv (list-set! pnv i leafvmi)]
           [nboard (TRANSITION-1 fvar)])
      ;return that there is no pi for this, and updated state
      (values empty
              (struct-copy state fvar
                           [d nd]
                           [sign nsign]
                           [v nv]
                           [board nboard])))))
 