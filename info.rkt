#lang info

;; ------------------------------------------------------------------------

;; For the Package

(define collection "turn-based-game")

(define deps
  '("base"
    "agile"
    "collections-lib"
    "htdp-lib"
    ))

(define build-deps
  '("rackunit-lib"
    "scribble-lib"
    "racket-doc"
    "htdp-doc"
    "collections-doc"
    ))

;; ------------------------------------------------------------------------

;; For the Collection

(define scribblings
  '(["scribblings/turn-based-game.scrbl" () ("Game development")]))

;; ------------------------------------------------------------------------

