#lang agile

(provide start
         turn-based-game-start)

(require 2htdp/universe
         lang/posn
         "../turn-based-game.rkt"
         "../turn-based-game-gui.rkt")

;; ------------------------------------------------------------------------

;; Running a world program using a TBGG instance

;; A TbggState is a TbgState that contains a TBGG instead of a TBG

;; A GuiState is one of:
;;  - InteractState
;;  - EndState

;; A InteractState is a (interact-state TbggState TurnState)
;; A EndState is a (end-state TbggState [Maybe Side])
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

;; tbgg-display : GuiState -> Image
(define (tbgg-display gs)
  (match gs
    [(interact-state (tbg-state tbgg state side) turn-state)
     (display-game-state tbgg state side turn-state)]
    [(end-state (tbg-state tbgg state side) winner)
     (display-end-state tbgg state side winner)]))

;; tbgg-handle-mouse : GuiState Int Int MouseEvent -> GuiState
(define (tbgg-handle-mouse gs x y mouse)
  (match gs
    [(interact-state ts turn-state)
     (match-define (tbg-state tbgg state side) ts)
     (tbgg-handle-event-result
      ts
      (handle-mouse tbgg state side turn-state (make-posn x y) mouse))]
    [(end-state _ _)
     gs]))

;; tbgg-handle-key : GuiState KeyEvent -> GuiState
(define (tbgg-handle-key gs key)
  (match gs
    [(interact-state ts turn-state)
     (match-define (tbg-state tbgg state side) ts)
     (tbgg-handle-event-result
      ts
      (handle-key tbgg state side turn-state key))]
    [(end-state _ _)
     gs]))

;; tbgg-handle-event-result : TbggState HandlerResult -> GuiState
(define (tbgg-handle-event-result ts hr)
  (match hr
    [(continue-turn turn-state)
     (interact-state ts turn-state)]
    [(finish-turn move)
     (match-define (tbg-state _ _ side) ts)
     (tbgg-state-check-winner
      (tbg-state-handle-move-choice ts move)
      side)]))

;; tbgg-state-check-winner : TbggState Side -> GuiState
(define (tbgg-state-check-winner ts side)
  (cond [(tbg-state-win? ts side) (end-state ts side)]
        [else (tbgg-state-start-turn ts)]))

;; tbgg-state-start-turn : TbggState -> GuiState
(define (tbgg-state-start-turn ts)
  (match-define (tbg-state tbgg _ _) ts)
  (interact-state ts (start-turn tbgg)))

;; ------------------------------------------------------------------------

