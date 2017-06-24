#lang agile

(provide connect-four
         W H N
         X O X? O?
         pos pos? pos-column pos-row
         board-ref
         board-columns
         board-valid-move?
         )

(require racket/bool
         racket/function
         turn-based-game/turn-based-game)
(module+ test
  (require rackunit))

;; ----------------------------------------------------------------------------

(define W 7)
(define H 6)
(define N 4)

;; ----------------------------------------------------------------------------

;; Data Definitions

;; A GameState is a Board
;; A MoveChoice is a XPos

;; A Board is a [Listof BoardColumn]
;; A BoardColumn is a [Listof Space]

;; A Space is a [Maybe Side]

;; A Side is one of:
;;  - X
;;  - O
(define X 'X)
(define O 'O)
(define (X? s) (eq? s X))
(define (O? s) (eq? s O))

(struct pos [column row] #:transparent)
;; A Pos is a (make-pos XPos YPos)
;; A XPos is a Natural[0,W)
;; A YPos is a Natural[0,H)

;; 5 | | | | | | | |
;; 4 | | | | | | | |
;; 3 | | | | | | | |
;; 2 | | | | | | | |
;; 1 | | | | | | | |
;; 0 | | | | | | | |
;;   ---------------
;;    0 1 2 3 4 5 6

;; valid-column+row? : Integer Integer -> Boolean
(define (valid-column+row? column row)
  (and (<= 0 column) (< column W)
       (<= 0 row)    (< row H)))

;; ----------------------------------------------------------------------------

(struct connect-four []

  ;; Not marked final, so other structs can inherit from this one.
  ;; For example connect-four/gui can be defined as a sub-struct to
  ;; implement interfaces like gen:turn-based-game/gui.

  #:methods gen:turn-based-game
  [;; sides : TBG GameState -> [Listof Sides]
   ;; Order in the result does not matter
   (define (sides self state)
     (list X O))

   ;; next-side : TBG GameState Side -> Side
   (define (next-side self state s)
     (cond [(X? s) O]
           [(O? s) X]))

   ;; play-at : TBG GameState Side MoveChoice -> GameState
   (define (play-at self state side move)
     (board-play-at state side move))

   ;; valid-move-choice? : TBG GameState Side MoveChoice -> Bool
   ;; ASSUME no one has won yet
   (define (valid-move-choice? self state side move)
     (board-valid-move? state move))

   ;; valid-move-choices : TBG GameState Side -> [Sequenceof MoveChoice]
   (define (valid-move-choices self state side)
     (board-valid-moves state))
   
   ;; winning-state? : TBG GameState Side -> Boolean
   (define (winning-state? self state side)
     (winning-board? side state))]

  #:methods gen:turn-based-game/standard-initial-state
  [;; standard-initial-state : TBGI -> GameState
   (define (standard-initial-state self) EMPTY-BOARD)
   ;; standard-initial-side : TBGI -> Side
   (define (standard-initial-side self) X)])

;; ----------------------------------------------------------------------------

;; Board and BoardColumn functions

;; board-ref : Board Pos -> Space
(define (board-ref b p)
  (list-ref (list-ref b (pos-column p))
            (pos-row p)))

;; board-columns : Board -> [List-of BoardColumn]
(define board-columns identity)

;; board-rows : Board -> [List-of [List-of Space]]
(define (board-rows b)
  (apply map list b))

;; board-set-column : Board Natural BoardColumn -> Board
(define board-set-column list-set)

;; column-set-space : BoardColumn Natural Space -> BoardColumn
(define column-set-space list-set)

;; board-set : Board Pos Space -> Board
(define (board-set b p s)
  (board-set-column
   b
   (pos-column p)
   (column-set-space (list-ref b (pos-column p))
                     (pos-row p)
                     s)))

;; build-board : [Pos -> Space] -> Board
(define (build-board f)
  (build-list
   W
   (λ (x)
     (build-list H (λ (y) (f (pos x y)))))))

(define EMPTY-BOARD
  (build-board (λ (p) #false)))

;; ----------------------------------------------------------------------------

;; Valid Moves

;; board-valid-move? : Board Natural -> Boolean
(define (board-valid-move? b c)
  (valid-move-in-column? (list-ref b c)))

;; valid-move-in-column? : BoardColumn -> Boolean
(define (valid-move-in-column? column)
  (and (member #false column) #t))

;; board-valid-moves : Board -> [List-of Natural]
(define (board-valid-moves b)
  ;; valid-column? : Natural -> Boolean
  (define (valid-column? c)
    (board-valid-move? b c))
  (filter valid-column? (build-list W identity)))

;; ----------------------------------------------------------------------------

;; Playing at a certain place

;; board-play-at : Board Side Natural -> Board
(define (board-play-at b side column)
  (board-set-column
   b
   column
   (board-column-play-at (list-ref b column) side)))

;; board-column-play-at : BoardColumn Side -> BoardColumn
(module+ test
  (check-equal? (board-column-play-at
                 (list #false #false #false #false #false #false)
                 X)
                (list X #false #false #false #false #false))
  (check-equal? (board-column-play-at
                 (list X #false #false #false #false #false)
                 X)
                (list X X #false #false #false #false))
  (check-equal? (board-column-play-at
                 (list X X #false #false #false #false)
                 O)
                (list X X O #false #false #false)))

(define (board-column-play-at c s)
  (cond [(empty? c) (error "invalid move: out of space")]
        [else
         (if (false? (first c))
             (cons s (rest c))
             (cons (first c) (board-column-play-at (rest c) s)))]))

;; ----------------------------------------------------------------------------

;; Determining whether a board is winning

;; winning-board? : Side Board -> Boolean
(define (winning-board? s b)
  ;; winning-list? : BoardColumn -> Boolean
  (define (winning-list? c)
    (winning-list-of-spaces? s c 0))
  (or (ormap winning-list? (board-columns b))
      (ormap winning-list? (board-rows b))
      (ormap winning-list? (board-left-up-diagonals b))
      (ormap winning-list? (board-right-up-diagonals b))))

;; winning-list-of-spaces? : Side [List-of Space] Natural -> Boolean
;; accumulator i represents the number in a row seen so far
(define (winning-list-of-spaces? s c i)
  (cond [(empty? c) #false]
        [else
         (if (equal? s (first c))
             (or (<= N (add1 i))
                 (winning-list-of-spaces? s (rest c) (add1 i)))
             (winning-list-of-spaces? s (rest c) 0))]))

;; ----------------------------------------------------------------------------

;; Diagonals

;; board-right-up-diagonals : Board -> [List-of [List-of Space]]
(define (board-right-up-diagonals b)
  (for/list ([x (range (- H) W 1)])
    (for/list ([y (in-range 0 H 1)])
      (if (valid-column+row? (+ x y) y)
          (board-ref b (pos (+ x y) y))
          #false))))

(module+ test
  (check-equal? (board-right-up-diagonals
                 (list (list X X #false #false #false #false)
                       (list O O O #false #false #false)
                       (list X X X #false #false #false)
                       (list O O O #false #false #false)
                       (list X #false #false #false #false #false)
                       (list O O O #false #false #false)
                       (list X X X #false #false #false)))
                (list (list #false #false #false #false #false #false)
                      (list #false #false #false #false #false #false)
                      (list #false #false #false #false #false #false)
                      (list #false #false #false #false #false #false)
                      (list #false #false #false #false #false #false)
                      (list #false X O #false #false #false)
                      (list X O X #false #false #false)
                      (list O X O #false #false #false)
                      (list X O #false #false #false #false)
                      (list O #false O #false #false #false)
                      (list X O X #false #false #false)
                      (list O X #false #false #false #false)
                      (list X #false #false #false #false #false))))

;; board-left-up-diagonals : Board -> [List-of [List-of Space]]
(define (board-left-up-diagonals b)
  (for/list ([x (in-range 0 (+ W H) 1)])
    (for/list ([y (in-range 0 H 1)])
      (if (valid-column+row? (- x y) y)
          (board-ref b (pos (- x y) y))
          #false))))

(module+ test
  (check-equal? (board-left-up-diagonals
                 (list (list X X #false #false #false #false)
                       (list O O O #false #false #false)
                       (list X X X #false #false #false)
                       (list O O O #false #false #false)
                       (list X #false #false #false #false #false)
                       (list O O O #false #false #false)
                       (list X X X #false #false #false)))
                (list (list X #false #false #false #false #false)
                      (list O X #false #false #false #false)
                      (list X O #false #false #false #false)
                      (list O X O #false #false #false)
                      (list X O X #false #false #false)
                      (list O #false O #false #false #false)
                      (list X O #false #false #false #false)
                      (list #false X O #false #false #false)
                      (list #false #false X #false #false #false)
                      (list #false #false #false #false #false #false)
                      (list #false #false #false #false #false #false)
                      (list #false #false #false #false #false #false)
                      (list #false #false #false #false #false #false))))

;; ------------------------------------------------------------------------

;; side->string : Side -> String
(define (side->string s)
  (cond [(equal? s X) "Red"]
        [(equal? s O) "Black"]))

