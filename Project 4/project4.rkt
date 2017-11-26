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