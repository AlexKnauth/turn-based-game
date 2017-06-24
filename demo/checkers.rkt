#lang agile

(require "../controller/computer-player-gui-controller.rkt"
         "../examples/checkers-gui.rkt"
         "../computer-player/score-explore-random.rkt")

(start/computer CHECKERS (hash O (computer/score-explore-random 2 3 40)))

