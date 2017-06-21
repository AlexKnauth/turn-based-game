#lang agile

(provide gen:turn-based-game/gui
         turn-based-game/gui?
         display-game-state
         display-end-state
         handle-mouse
         handle-key
         start-turn
         continue-turn
         finish-turn
         )

(require 2htdp/universe
         lang/posn
         racket/generic
         "turn-based-game.rkt")

;; ------------------------------------------------------------------------

;; A TBGG is an instance of both `gen:turn-based-game` and
;; `gen:turn-based-game/gui`.

;; A TBGGI is an instance of `gen:turn-based-game`,
;; `gen:turn-based-game/gui`, and
;; `gen:turn-based-game/standard-initial-state`.

;; These are meant to be dictionaries of game-related methods.
;; They are *NOT* meant to hold the state of the game.

;; A HandlerResult is one of:
;;  - (continue-turn TurnState)
;;  - (finish-turn MoveChoice)
(struct continue-turn [state] #:transparent)
(struct finish-turn [move] #:transparent)

(define-generics turn-based-game/gui

  ;; ----------------------------------------

  ;; Each instance has these types defined as part of the
  ;; gen:turn-based-game interface:
  ;;   Side
  ;;   GameState
  ;;   MoveChoice

  ;; In addition each instance defines:

  ;; type TurnState

  ;; The TurnState type keeps track of any state that a user could build up
  ;; before making a final MoveChoice for their turn.

  ;; ----------------------------------------

  ;; Each instance defines these methods:

  ;; display-game-state : TBGG GameState Side TurnState -> Image
  (display-game-state turn-based-game/gui state side turn-state)

  ;; display-end-state : TBGG GameState Side [Maybe Side] -> Image
  (display-end-state turn-based-game/gui state side winner)

  ;; handle-mouse :
  ;; TBGG GameState Side TurnState Posn MouseEvent -> HandlerResult
  (handle-mouse turn-based-game/gui state side turn-state posn mouse)

  ;; handle-key :
  ;; TBGG GameState Side TurnState KeyEvent -> HandlerResult
  (handle-key turn-based-game/gui state side turn-state key)

  ;; start-turn : TBGG -> TurnState
  (start-turn turn-based-game/gui)

  ;; ----------------------------------------
  )

;; ------------------------------------------------------------------------

