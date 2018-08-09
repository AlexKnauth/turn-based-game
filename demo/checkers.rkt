#lang agile

(require "../controller/computer-player-gui-controller.rkt"
         "../examples/checkers-gui.rkt"
         "../computer-player/score-explore-random.rkt")

(module+ main

  ;Human vs. Human
  ;(start/computer CHECKERS (hash))

  ;Human vs. Computer
  (start/computer CHECKERS (hash O (computer/score-explore-random 2 3 40)))

  )

