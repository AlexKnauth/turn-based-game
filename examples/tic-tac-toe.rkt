#lang agile

(provide tic-tac-toe
         X O X? O?
         pos pos? pos-x pos-y
         board-space
         board-valid-move-choice?
         )

(require racket/bool
         turn-based-game/turn-based-game)

;; ------------------------------------------

;; A Side is one of:
;;  - 0
;;  - 1
(define X 0)
(define O 1)

;; X? and O? : Side -> Bool
(define (X? s) (eq? s X))
(define (O? s) (eq? s O))

;; ------------------------------------------

;; A GameState is a Board

;; ------------------------------------------

;; A MoveChoice is a Pos

;; A Pos is a (make-pos XPos YPos)
(struct pos [x y] #:transparent)

;; A XPos is an Integer[0,2]
;; A YPos is an Integer[0,2]

;; ------------------------------------------

(struct tic-tac-toe []

  ;; Not marked final, so other structs can inherit from this one.
  ;; For example tic-tac-toe/gui can be defined as a sub-struct to
  ;; implement interfaces like gen:turn-based-game/gui.

  #:methods gen:turn-based-game
  [;; next-side : TBG GameState Side -> Side
   (define (next-side self state side)
     (cond [(X? side) O]
           [(O? side) X]))

   ;; play-at : TBG GameState Side MoveChoice -> GameState
   ;; ASSUME no one has won yet
   (define (play-at self state side move)
     (board-play-at state side move))
   
   ;; valid-move-choice? : TBG GameState Side MoveChoice -> Boolean
   ;; ASSUME no one has won yet
   (define (valid-move-choice? self state side move)
     (board-valid-move-choice? state side move))
   
   ;; winning-state? : TBG GameState Side -> Boolean
   (define (winning-state? self state side)
     (winning-board? state side))
   
   ;; valid-move-choices : TBG GameState Side -> [Sequenceof MoveChoice]
   (define (valid-move-choices self state side)
     (board-valid-move-choices state side))]

  #:methods gen:turn-based-game/standard-initial-state
  [;; standard-initial-state : TBGI -> GameState
   (define (standard-initial-state self) EMPTY-BOARD)
   ;; standard-initial-side : TBGI -> Side
   (define (standard-initial-side self) X)]
  )

;; ------------------------------------------

;; Other Data Definitions

;; A Board is a [Hashof Pos [Maybe Side]]
(define EMPTY-BOARD (hash))

;; board-play-at : Board Side MoveChoice -> Board
(define (board-play-at board side move)
  (hash-set board move side))

;; board-valid-move-choice? : Board Side MoveChoice -> Board
(define (board-valid-move-choice? board side move)
  (false? (hash-ref board move #false)))

;; board-valid-move-choices : Board Side -> [Listof MoveChoice]
(define (board-valid-move-choices board side)
  (for*/list ([x (in-range 0 3)]
              [y (in-range 0 3)]
              #:when (board-valid-move-choice? board side (pos x y)))
    (pos x y)))

;; board-space : Board Pos -> [Maybe Side]
(define (board-space board pos)
  (hash-ref board pos #false))

;; board-space=? : Board Pos Side -> Bool
(define (board-space=? board pos side)
  (eq? side (board-space board pos)))

;; board-spaces=? : Board [Listof Pos] Side -> Bool
(define (board-spaces=? board poss side)
  (for/and ([pos (in-list poss)])
    (board-space=? board pos side)))

;; ------------------------------------------

;; Other Functions

;; A WinPath is a [Listof Pos] such that filling all of the places in the
;; list with your pieces gives you a win.

(define VERTICALS
  (for/list ([x (in-range 0 3)]) ; [Listof WinPath]
    (for/list ([y (in-range 0 3)])
      (pos x y))))

(define HORIZONTALS
  (for/list ([y (in-range 0 3)]) ; [Listof WinPath]
    (for/list ([x (in-range 0 3)])
      (pos x y))))

(define DIAGONALS
  (list
   (for/list ([x=y (in-range 0 3)]) (pos x=y x=y)) ; WinPath
   (for/list ([x=2-y (in-range 0 3)]) (pos x=2-y (- 2 x=2-y))))) ; WinPath

;; WIN-PATHS : [Listof WinPath]
(define WIN-PATHS
  (append VERTICALS HORIZONTALS DIAGONALS))

;; winning-board? : Board Side -> Bool
(define (winning-board? board side)
  (for/or ([path (in-list WIN-PATHS)])
    (board-spaces=? board path side)))

;; ------------------------------------------

