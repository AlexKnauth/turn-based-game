#lang agile

(provide checkers
         W H
         X O X? O?
         move-choice? move jump
         pos valid-pos? pos-x pos-y
         pawn king pawn? king? piece-side
         board-space
         diagonal-1?
         diagonal-2?
         diagonal-forward-1?
         diagonal-forward-2?
         )

(require racket/bool
         (prefix-in collection/ data/collection)
         turn-based-game/turn-based-game)

;; ------------------------------------------

(define W 8)
(define H 8)
(define N 3)

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

;; A MoveChoice is one of:
;;  - (move Pos Pos)
;;  - (jump Pos [Listof Pos])
(struct move [from to] #:transparent)
(struct jump [from path] #:transparent)

(define (move-choice? v)
  (or (move? v) (jump? v)))

;; A Pos is a (make-pos XPos YPos)
;; such that (+ x y) is even
(struct pos [x y] #:transparent)

;; A XPos is an Integer[0,W)
;; A YPos is an Integer[0,H)

;; valid-pos? : Any -> Bool
(define (valid-pos? v)
  (match v
    [(pos x y)
     (and (exact-integer? x)
          (exact-integer? y)
          (<= 0 x) (< x W)
          (<= 0 y) (< y H)
          (even? (+ x y)))]
    [_ #false]))

;; ------------------------------------------

(struct checkers []

  ;; Not marked final, so other structs can inherit from this one.
  ;; For example checkers/gui can be defined as a sub-struct to
  ;; implement interfaces like gen:turn-based-game/gui.

  #:methods gen:turn-based-game
  [;; sides : TBG GameState -> [Listof Sides]
   ;; Order in the result does not matter
   (define (sides self state)
     (list X O))

   ;; next-side : TBG GameState Side -> Side
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
   (define (standard-initial-state self) INIT-BOARD)
   ;; standard-initial-side : TBGI -> Side
   (define (standard-initial-side self) X)])

;; ------------------------------------------

;; Other Data Definitions

;; A Board is a [Hashof Pos Piece]

;; A Piece is one of:
;;  - (pawn Side)
;;  - (king Side)
(struct pawn [side] #:transparent)
(struct king [side] #:transparent)

;; piece-side : Piece -> Side
(define (piece-side piece)
  (match piece
    [(pawn side) side]
    [(king side) side]))

(define EMPTY-BOARD (hash))
(define INIT-BOARD
  (for*/hash ([side (in-list (list X O))]
              [y (in-list (cond [(X? side) (range 0 N)]
                                [(O? side) (range (- H N) H)]))]
              [x (in-range (modulo y 2) W 2)])
    (values (pos x y) (pawn side))))

;; board-play-at : Board Side MoveChoice -> Board
;; ASSUME this is a valid move
(define (board-play-at board side mc)
  (match mc
    [(move from to)
     (define piece (board-space board from))
     (board-set-piece (board-remove board from) to (maybe-crown piece to))]
    [(jump from path)
     (define piece (board-space board from))
     (board-play-at/jump (board-remove board from) piece from path)]))

;; board-play-at/jump : Board Piece Pos [Listof Pos] -> Board
(define (board-play-at/jump board piece pos path)
  (match path
    ['() (board-set-piece board pos (maybe-crown piece pos))]
    [(cons first rest)
     (board-play-at/jump
      (board-remove board (pos-in-between pos first))
      piece
      first
      rest)]))

;; pos-in-between : Pos Pos -> Pos
(define (pos-in-between a b)
  (define p
    (pos (* 1/2 (+ (pos-x a) (pos-x b)))
         (* 1/2 (+ (pos-y a) (pos-y b)))))
  (unless (valid-pos? p)
    (error 'pos-in-between "bad!"))
  p)

;; board-valid-move-choice? : Board Side MoveChoice -> Board
;; In the version of checkers I know, jumping is not mandatory, pieces
;; can't jump "through edges," pawns can only move and jump forwards, and
;; kings can move and jump forwards or backwards, but they can't "fly."
(define (board-valid-move-choice? board side mc)
  (match mc
    [(move from to)
     (match (board-space board from)
       [(pawn (== side))
        (and (false? (board-space board to))
             (diagonal-forward-1? side from to))]
       [(king (== side))
        (and (false? (board-space board to))
             (diagonal-1? from to))]
       [_ #false])]
    [(jump from path)
     (match (board-space board from)
       [(pawn (== side))
        (let loop ([pos from] [path path])
          (match path
            ['() #true]
            [(cons first rest)
             (and
              (false? (board-space board first))
              (diagonal-forward-2? side pos first)
              (jumpable-pos? board side (pos-in-between pos first))
              (loop first rest))]))]
       [(king (== side))
        (let loop ([pos from] [path path])
          (match path
            ['() #true]
            [(cons first rest)
             (and
              (false? (board-space board first))
              (diagonal-2? pos first)
              (jumpable-pos? board side (pos-in-between pos first))
              (loop first rest))]))]
       [_ #false])]))

;; board-valid-move-choices : Board Side -> [Sequenceof MoveChoice]
(define (board-valid-move-choices board side)
  (for*/list ([(pos piece) (in-hash board)]
              #:when (equal? side (piece-side piece))
              [mc (in-list (piece-valid-move-choices board side pos piece))])
    mc))

;; piece-valid-move-choices : Board Side Pos Piece -> [Sequenceof MoveChoice]
(define (piece-valid-move-choices board side pos piece)
  (append
   (for/list ([to (in-list (cond
                             [(pawn? piece) (diagonal-forward-1-poss side pos)]
                             [(king? piece) (diagonal-1-poss pos)]))]
              #:when (false? (board-space board to)))
     (move pos to))
   (piece-valid-jumps board side pos pos piece '())))

;; piece-valid-jumps :
;; Board Side Pos Pos Piece [Listof Pos] -> [Sequenceof MoveChoice]
(define (piece-valid-jumps board side from pos piece acc)
  (define nexts
    (for/list
        ([next (in-list (cond
                          [(pawn? piece) (diagonal-forward-2-poss side pos)]
                          [(king? piece) (diagonal-2-poss pos)]))]
         #:when (and (not (member next acc))
                     (false? (board-space board next))
                     (jumpable-pos? board side
                                    (pos-in-between pos next))))
      next))
  (append*
   (for/list ([final (in-list nexts)])
     (jump from (reverse (cons final acc))))
   (for/list ([next (in-list nexts)])
     (piece-valid-jumps board side from next piece (cons next acc)))))

;; board-space : Board Pos -> [Maybe Piece]
(define (board-space board pos)
  (hash-ref board pos #false))

;; board-remove : Board Pos -> Board
(define (board-remove board pos)
  (hash-remove board pos))

;; board-set-piece : Board Pos Piece -> Board
(define (board-set-piece board pos piece)
  (hash-set board pos piece))

;; winning-board? : Board Side -> Bool
(define (winning-board? board side)
  (for/and ([(k v) (in-hash board)])
    (equal? side (piece-side v))))

(define (jumpable-pos? board side pos)
  (define space (board-space board pos))
  (and (not (false? space))
       (not (equal? side (piece-side space)))))

;; ------------------------------------------

;; Diagonals

;; diagonal-n? : Nat [Pos Pos -> Bool]
(define ((diagonal-n? n) from to)
  (define ∆x (- (pos-x to) (pos-x from)))
  (define ∆y (- (pos-y to) (pos-y from)))
  (and (or (= n ∆x) (= (- n) ∆x))
       (or (= n ∆y) (= (- n) ∆y))))

;; diagonal-forward-n? : Nat [Side Pos Pos -> Bool]
(define ((diagonal-forward-n? n) side from to)
  (define ∆x (- (pos-x to) (pos-x from)))
  (define ∆y (- (pos-y to) (pos-y from)))
  (and (or (= n ∆x) (= (- n) ∆x))
       (cond [(X? side) (= n ∆y)]
             [(O? side) (= (- n) ∆y)])))

(define diagonal-1? (diagonal-n? 1))
(define diagonal-2? (diagonal-n? 2))
(define diagonal-forward-1? (diagonal-forward-n? 1))
(define diagonal-forward-2? (diagonal-forward-n? 2))

;; diagonal-n-poss : Nat -> [Pos -> [Listof Pos]]
(define ((diagonal-n-poss n) from)
  (filter
   valid-pos?
   (list (pos (+ (pos-x from) n) (+ (pos-y from) n))
         (pos (- (pos-x from) n) (+ (pos-y from) n))
         (pos (+ (pos-x from) n) (- (pos-y from) n))
         (pos (- (pos-x from) n) (- (pos-y from) n)))))

;; diagonal-forward-n-poss : Nat -> [Side Pos -> [Listof Pos]]
(define ((diagonal-forward-n-poss n) side from)
  (filter
   valid-pos?
   (cond [(X? side)
          (list (pos (+ (pos-x from) n) (+ (pos-y from) n))
                (pos (- (pos-x from) n) (+ (pos-y from) n)))]
         [(O? side)
          (list (pos (+ (pos-x from) n) (- (pos-y from) n))
                (pos (- (pos-x from) n) (- (pos-y from) n)))])))

(define diagonal-1-poss (diagonal-n-poss 1))
(define diagonal-2-poss (diagonal-n-poss 2))
(define diagonal-forward-1-poss (diagonal-forward-n-poss 1))
(define diagonal-forward-2-poss (diagonal-forward-n-poss 2))

;; ------------------------------------------

;; Crowning

;; maybe-crown : Piece Pos -> Piece
(define (maybe-crown piece pos)
  (match piece
    [(pawn side)
     (cond [(crowning-pos? side pos) (king side)]
           [else piece])]
    [(king _) piece]))

;; crowning-pos? : Side Pos -> Bool
(define (crowning-pos? side pos)
  (cond [(X? side) (back-row? pos)]
        [(O? side) (front-row? pos)]))

;; back-row? : Pos -> Bool
;; front-row? : Pos -> Bool
(define (back-row? pos) (= (sub1 H) (pos-y pos)))
(define (front-row? pos) (= 0 (pos-y pos)))

