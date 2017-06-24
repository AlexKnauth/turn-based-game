#lang agile

(provide gen:turn-based-game
         turn-based-game?
         sides
         next-side
         (rename-out [play-at/check play-at])
         valid-move-choice?
         winning-state?
         valid-move-choices
         ;; Standard initial states
         gen:turn-based-game/standard-initial-state
         turn-based-game/standard-initial-state?
         standard-initial-state
         standard-initial-side
         ;; Bundling the TBG with the game state
         tbg-state
         tbg-state-win?
         tbg-state-handle-move-choice
         )

(require racket/generic)

;; A [Sequenceof X] is an instance of `gen:sequence` and `gen:countable`
;; from data/collection in the "collections-lib" package.
;; It implements these methods:
;;   empty?         : [Sequenceof X] -> Bool
;;   first          : [Sequenceof X] -> X
;;   rest           : [Sequenceof X] -> [Sequenceof X]
;;   nth            : [Sequenceof X] Nat -> X
;;   set-nth        : [Sequenceof X] Nat X -> [Sequenceof X]
;;   update-nth     : [Sequenceof X] Nat [X -> X] -> [Sequenceof X]
;;   reverse        : [Sequenceof X] -> [Sequenceof X]
;;   random-access? : [Sequenceof X] -> Bool
;;   sequence->list : [Sequenceof X] -> [Listof X]
;;   length         : [Sequenceof X] -> Nat
;;   known-finite?  : [Sequenceof X] -> Bool

;; ------------------------------------------------------------------------

;; A TBG is an instance of gen:turn-based-game

;; This is meant to be a dictionary of game-related methods.
;; It is *NOT* meant to be the state of the game.

(define-generics turn-based-game

  ;; ----------------------------------------

  ;; Each instance defines these types:

  ;; type Side
  ;; type GameState
  ;; type MoveChoice

  ;; None of these types should not contain #false, and the GameState
  ;; type does not usually need to keep track of turns.

  ;; ----------------------------------------

  ;; Each instance defines these methods:

  ;; sides : TBG GameState -> [Listof Side]
  ;; Order in the result does not matter
  (sides turn-based-game state)

  ;; next-side : TBG GameState Side -> Side
  ;; Should not mutate the game state
  (next-side turn-based-game state side)

  ;; valid-move-choice? : TBG GameState Side MoveChoice -> Boolean
  (valid-move-choice? turn-based-game state side move)

  ;; valid-move-choices : TBG GameState Side -> [Sequenceof MoveChoice]
  (valid-move-choices turn-based-game state side)

  ;; play-at : TBG GameState Side MoveChoice -> GameState
  ;; Should not mutate the original game state, should return a new one
  ;; ASSUME that the given move is valid
  (play-at turn-based-game state side move)

  ;; winning-state? : TBG GameState Side -> Boolean
  ;; Should not mutate the game state
  (winning-state? turn-based-game state side)

  ;; ----------------------------------------
  )

;; play-at/check : TBG GameState Side MoveChoice -> GameState
(define (play-at/check tbg state side move)
  (unless (valid-move-choice? tbg state side move)
    (error 'plat-at
           (string-append "invalid move choice:\n"
                          "  game:  ~v\n"
                          "  state: ~v\n"
                          "  side:  ~v\n"
                          "  move:  ~v")
           tbg state side move))
  (play-at tbg state side move))

;; ------------------------------------------------------------------------

;; A TBGI is an instance of both `gen:turn-based-game` and
;; `gen:turn-based-game/standard-initial-state`.

;; Again, this is meant to be a dictionary of game-related methods,
;; and *NOT* the state of the game.

(define-generics turn-based-game/standard-initial-state

  ;; ----------------------------------------

  ;; Each instance has these types defined as part of the
  ;; gen:turn-based-game interface:
  ;;   Side
  ;;   GameState
  ;;   MoveChoice

  ;; ----------------------------------------

  ;; Each instance defines these methods:

  ;; standard-initial-state : TBGI -> GameState
  (standard-initial-state turn-based-game/standard-initial-state)

  ;; standard-initial-side : TBGI -> Side
  (standard-initial-side turn-based-game/standard-initial-state)

  ;; ----------------------------------------
  )

;; ------------------------------------------------------------------------

;; Bundling the TBG with the game state

;; A TbgState is a (tbg-state TBG GameState Side)
(struct tbg-state [tbg state turn] #:transparent)

;; tbg-state-win? : TbgState Side -> TbgState
(define (tbg-state-win? ts side)
  (match-define (tbg-state tbg state _) ts)
  (winning-state? tbg state side))

;; tbg-state-handle-move-choice : TbgState MoveChoice -> TbgState
(define (tbg-state-handle-move-choice ts move)
  (match-define (tbg-state tbg state side) ts)
  (cond
    [(valid-move-choice? tbg state side move)
     (tbg-state
      tbg
      ;; don't need play-at/check because valid-move-choice? already
      ;; returned true
      (play-at tbg state side move)
      (next-side tbg state side))]
    [else ts]))

;; ------------------------------------------------------------------------

