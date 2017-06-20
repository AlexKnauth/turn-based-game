#lang agile

(provide gen:computer-player
         computer-player?
         computer-player-start-state
         computer-player-next-state
         computer-player-state-moves
         computer-player-state-add-move
         ;; Bundling the Comp with its state
         comp-state
         start-comp-state
         comp-state-next-state
         comp-state-moves
         comp-state-add-move
         )

(require racket/generic)

;; ------------------------------------------------------------------------

;; A Comp is an instance of gen:computer-player

;; This is meant to be a dictionary of methods.
;; It is *NOT* meant to be the state of the computer.

(define-generics computer-player

  ;; ----------------------------------------

  ;; Each instance defines these types:

  ;; type State

  ;; And uses these types from gen:turn-based-game:

  ;; type Side
  ;; type GameState
  ;; type MoveChoice

  ;; ----------------------------------------

  ;; Each instance defines these methods:

  ;; computer-player-start-state :
  ;; Comp -> State
  (computer-player-start-state computer-player)

  ;; computer-player-next-state :
  ;; Comp TBG State GameState Side -> State
  (computer-player-next-state computer-player tbg state game side)

  ;; computer-player-state-moves :
  ;; Comp TBG State GameState Side -> [Listof MoveChoice]
  (computer-player-state-moves computer-player tbg state game side)

  ;; computer-player-state-add-move :
  ;; Comp TBG State Side MoveChoice -> State
  (computer-player-state-add-move computer-player tbg state side move)

  ;; ----------------------------------------
  )

;; ------------------------------------------------------------------------

;; Bundling the Comp with its state

;; A CompState is a (comp-state Comp State)
(struct comp-state [comp state] #:transparent)

;; start-comp-state : Comp -> CompState
(define (start-comp-state comp)
  (comp-state comp
              (computer-player-start-state comp)))

;; comp-state-next-state : CompState TBG GameState Side -> CompState
(define (comp-state-next-state c tbg game side)
  (match-define (comp-state comp state) c)
  (comp-state comp
              (computer-player-next-state comp tbg state game side)))

;; comp-state-moves : CompState TBG GameState Side -> [Listof MoveChoice]
(define (comp-state-moves c tbg game side)
  (match-define (comp-state comp state) c)
  (computer-player-state-moves comp tbg state game side))

;; comp-state-add-move : CompState TBG Side MoveChoice -> CompState
(define (comp-state-add-move c tbg side move)
  (match-define (comp-state comp state) c)
  (comp-state comp
              (computer-player-state-add-move comp tbg state side move)))

;; ------------------------------------------------------------------------

