#lang agile

(require "../computer-player-gui-controller.rkt"
         "../examples/connect-four-gui.rkt"
         "../computer-player/n-ahead.rkt")

(start/computer CONNECT-FOUR (hash O (computer/n-ahead 3)))

