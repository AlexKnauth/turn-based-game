#lang agile

(require "../controller/computer-player-gui-controller.rkt"
         "../examples/connect-four-gui.rkt"
         "../computer-player/n-ahead.rkt"
         "../computer-player/score-explore-random.rkt")

;(start/computer CONNECT-FOUR (hash O (computer/n-ahead 3)))

(start/computer CONNECT-FOUR (hash O (computer/score-explore-random 2 20 20)))

;; So far Black is winning a lot
;; Red won once so far.
;(start/computer CONNECT-FOUR (hash X (computer/n-ahead 3)
;                                   O (computer/score-explore-random 2 20 20)))

;; Playing itself
;(start/computer CONNECT-FOUR (hash X (computer/score-explore-random 2 20 20)
;                                   O (computer/score-explore-random 2 20 20)))

