#lang agile

(provide start/computer
         turn-based-game-start/computer)

(require 2htdp/universe
         lang/posn
         "../computer-player.rkt"
         "../turn-based-game.rkt"
         "../turn-based-game-gui.rkt")

;; ------------------------------------------------------------------------

;; Running a world program using a TBGG and Comp instances

;; A GuiState is one of:
;;  - InteractState
;;  - EndState

;; A InteractState is a
;;   (interact-state TbggState TurnState [Hashof Side Comp])
;; A EndState is a
;;   (end-state TbggState [Maybe Side])
(struct interact-state [ts turn-state comps] #:transparent)
(struct end-state [ts winner] #:transparent)

;; start/computer : TBGGI [Hashof Side Comp] -> Void
;; Starts the turn-based game with its standard initial state, with some of
;; the sides played by the computer
(define (start/computer tbggi comps)
  (turn-based-game-start/computer tbggi
                                  (standard-initial-state tbggi)
                                  (standard-initial-side tbggi)
                                  comps))

;; turn-based-game-start/computer :
;; TBGG GameState Side [Hashof Side Comp] -> Void
;; Starts the turn-based-game with the givin initial state, with some of
;; the sides played by the computer
(define (turn-based-game-start/computer tbgg state side comps)
  (void
   (big-bang (start-gui-state (tbg-state tbgg state side)
                              (for/hash ([(k v) (in-hash comps)])
                                (values k (start-comp-state v))))
     [to-draw gui-display]
     [on-tick gui-handle-tick]
     [on-mouse gui-handle-mouse]
     [on-key gui-handle-key])))

;; ------------------------------------------------------------------------

;; Private Implementation Details of the World Program

;; A Comps is a [Hashof Side CompState]

;; start-gui-state : TbggState Comps -> GuiState
(define (start-gui-state ts comps)
  (match-define (tbg-state tbgg _ _) ts)
  (interact-state ts (start-turn tbgg) comps))

;; gui-display : GuiState -> Image
(define (gui-display gs)
  (match gs
    [(interact-state (tbg-state tbgg state side) turn-state comps)
     (display-game-state tbgg state side turn-state)]
    [(end-state (tbg-state tbgg state side) winner)
     (display-end-state tbgg state side winner)]))

;; gui-handle-tick : GuiState -> GuiState
(define (gui-handle-tick gs)
  (match gs
    [(interact-state ts turn-state comps)
     (match-define (tbg-state tbgg game-state side) ts)
     (cond
       [(hash-has-key? comps side)
        ;; comp : CompState
        (define comp
          (comp-state-next-state (hash-ref comps side) tbgg game-state side))
        (define moves
          (comp-state-moves comp tbgg game-state side))
        (gui-handle-move-choice ts
                                (random-element moves)
                                (hash-set comps side comp))]
       [else
        gs])]
    [(end-state _ _)
     gs]))

;; gui-handle-mouse : GuiState Int Int MouseEvent -> GuiState
(define (gui-handle-mouse gs x y mouse)
  (match gs
    [(interact-state ts turn-state comps)
     (match-define (tbg-state tbgg state side) ts)
     (cond
       [(hash-has-key? comps side) gs]
       [else
        (gui-handle-event-result
         ts
         (handle-mouse tbgg state side turn-state (make-posn x y) mouse)
         comps)])]
    [(end-state _ _)
     gs]))

;; gui-handle-key : GuiState KeyEvent -> GuiState
(define (gui-handle-key gs key)
  (match gs
    [(interact-state ts turn-state comps)
     (match-define (tbg-state tbgg state side) ts)
     (cond
       [(hash-has-key? comps side) gs]
       [else
        (gui-handle-event-result
         ts
         (handle-key tbgg state side turn-state key)
         comps)])]
    [(end-state _ _)
     gs]))

;; gui-handle-event-result : TbggState HandlerResult Comps -> GuiState
(define (gui-handle-event-result ts hr comps)
  (match hr
    [(continue-turn turn-state)
     (interact-state ts turn-state comps)]
    [(finish-turn move)
     (gui-handle-move-choice ts move comps)]))

;; gui-handle-move-choice : TbggState MoveChoice Comps -> GuiState
(define (gui-handle-move-choice ts move comps)
  (match-define (tbg-state tbg _ side) ts)
  (gui-state-check-winner
   (tbg-state-handle-move-choice ts move)
   side
   (comps-handle-move-choice comps tbg side move)))

;; comps-handle-move-choice : Comps TBG Side MoveChoice -> Comps
(define (comps-handle-move-choice comps tbg side move)
  (for/hash ([(k v) (in-hash comps)])
    (values k (comp-state-add-move v tbg side move))))

;; gui-state-check-winner : TbggState Side Comps -> GuiState
(define (gui-state-check-winner ts side comps)
  (cond [(tbg-state-win? ts side) (end-state ts side)]
        [else (start-gui-state ts comps)]))

;; ------------------------------------------------------------------------

(define (random-element lst)
  (list-ref lst (random (length lst))))

;; ------------------------------------------------------------------------

