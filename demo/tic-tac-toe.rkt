#lang agile

(require "../computer-player-gui-controller.rkt"
         "../examples/tic-tac-toe-gui.rkt"
         "../computer-player/n-ahead.rkt")

(start/computer TIC-TAC-TOE (hash O (computer/n-ahead 9)))

