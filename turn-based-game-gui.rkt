#lang agile

(provide gen:turn-based-game/gui
         turn-based-game/gui?
         ;; main functions
         start
         turn-based-game-start
         ;; other functions
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

;; Running a world program using a TBGG instance

;; A TbggState is a TbgState that contains a TBGG instead of a TBG

;; A TbggGuiState is one of:
;;  - TbggInteractState
;;  - TbggEndState

;; A TbggInteractState is a (interact-state TbggState TurnState)
;; A TbggEndState is a (end-state TbggState [Maybe Side])
(struct interact-state [ts turn-state] #:transparent)
(struct end-state [ts winner] #:transparent)

;; start : TBGGI -> Void
;; Starts the turn-based game with its standard initial state
(define (start tbggi)
  (turn-based-game-start tbggi
                         (standard-initial-state tbggi)
                         (standard-initial-side tbggi)))

;; turn-based-game-start : TBGG GameState Side -> Void
;; Starts the turn-based-game with the givin initial state
(define (turn-based-game-start tbgg state side)
  (void
   (big-bang (tbgg-state-start-turn (tbg-state tbgg state side))
     [to-draw tbgg-display]
     [on-mouse tbgg-handle-mouse]
     [on-key tbgg-handle-key])))

;; ------------------------------------------------------------------------

;; Private Implementation Details of the World Program

;; tbgg-display : TbggGuiState -> Image
(define (tbgg-display gs)
  (match gs
    [(interact-state (tbg-state tbgg state side) turn-state)
     (display-game-state tbgg state side turn-state)]
    [(end-state (tbg-state tbgg state side) winner)
     (display-end-state tbgg state side winner)]))

;; tbgg-handle-mouse : TbggGuiState Int Int MouseEvent -> TbggGuiState
(define (tbgg-handle-mouse gs x y mouse)
  (match gs
    [(interact-state ts turn-state)
     (match-define (tbg-state tbgg state side) ts)
     (tbgg-handle-event-result
      ts
      (handle-mouse tbgg state side turn-state (make-posn x y) mouse))]
    [(end-state _ _)
     gs]))

;; tbgg-handle-key : TbggGuiState KeyEvent -> TbggGuiState
(define (tbgg-handle-key gs key)
  (match gs
    [(interact-state ts turn-state)
     (match-define (tbg-state tbgg state side) ts)
     (tbgg-handle-event-result
      ts
      (handle-key tbgg state side turn-state key))]
    [(end-state _ _)
     gs]))

;; tbgg-handle-event-result : TbggState HandlerResult -> TbggGuiState
(define (tbgg-handle-event-result ts hr)
  (match hr
    [(continue-turn turn-state)
     (interact-state ts turn-state)]
    [(finish-turn move)
     (match-define (tbg-state _ _ side) ts)
     (tbgg-state-check-winner
      (tbg-state-handle-move-choice ts move)
      side)]))

;; tbgg-state-check-winner : TbggState Side -> TbggGuiState
(define (tbgg-state-check-winner ts side)
  (cond [(tbg-state-win? ts side) (end-state ts side)]
        [else (tbgg-state-start-turn ts)]))

;; tbgg-state-start-turn : TbggState -> TbggGuiState
(define (tbgg-state-start-turn ts)
  (match-define (tbg-state tbgg _ _) ts)
  (interact-state ts (start-turn tbgg)))

;; ------------------------------------------------------------------------

