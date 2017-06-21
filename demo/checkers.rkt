#lang agile

(require "../controller/computer-player-gui-controller.rkt"
         "../examples/checkers-gui.rkt"
         "../computer-player/n-ahead.rkt")

(start/computer CHECKERS (hash O COMPUTER/3))

