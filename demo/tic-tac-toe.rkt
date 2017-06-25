#lang agile

(require "../controller/computer-player-gui-controller.rkt"
         "../examples/tic-tac-toe-gui.rkt"
         "../computer-player/n-ahead.rkt")

(module+ main

  ;; A game of Tic Tac Toe can only last 5 turns for X and 4 turns for O
  (start/computer TIC-TAC-TOE (hash O (computer/n-ahead 5)))

  ;; Computer starting on offense
  (start/computer TIC-TAC-TOE (hash X (computer/n-ahead 5)))

  )

