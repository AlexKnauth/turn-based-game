#lang agile

(provide CHECKERS X O)

(require 2htdp/image
         2htdp/universe
         lang/posn
         racket/bool
         racket/math
         turn-based-game/turn-based-game-gui
         "checkers.rkt")

;; ------------------------------------------

;; Constants

(define BLACK "black")
(define RED "red")
(define YELLOW "yellow")
(define TAN "tan")
(define BROWN "brown")
(define DARK-BROWN "dark brown")
(define DARK-RED "dark red")
(define GREY "grey")
(define TRANSPARENT "transparent")

(define SQUARE-SIZE 90)
(define CIRCLE-RADIUS 34)
(define SELECTION-CIRCLE-RADIUS 40)

(define DARK-SQUARE
  (overlay (square SQUARE-SIZE "outline" DARK-BROWN)
           (square SQUARE-SIZE "solid" BROWN)))
(define LIGHT-SQUARE
  (overlay (square SQUARE-SIZE "outline" DARK-BROWN)
           (square SQUARE-SIZE "solid" TAN)))

(define SELECTION-SQUARE
  (overlay (circle SELECTION-CIRCLE-RADIUS 25 YELLOW)
           (square SQUARE-SIZE "solid" TRANSPARENT)))

(define X-SQUARE (overlay (circle CIRCLE-RADIUS "solid" RED) DARK-SQUARE))
(define O-SQUARE (overlay (circle CIRCLE-RADIUS "solid" BLACK) DARK-SQUARE))
(define X-KING-SQUARE
  (overlay (circle CIRCLE-RADIUS "solid" RED)
           (star-polygon CIRCLE-RADIUS 7 3 "solid" DARK-RED)
           DARK-SQUARE))
(define O-KING-SQUARE
  (overlay (circle CIRCLE-RADIUS "solid" BLACK)
           (star-polygon CIRCLE-RADIUS 7 3 "solid" GREY)
           DARK-SQUARE))

(define BOARD-WIDTH (* W SQUARE-SIZE))
(define BOARD-HEIGHT (* H SQUARE-SIZE))

(define TURN-MSG-HEIGHT 50)

(define WIDTH BOARD-WIDTH)
(define HEIGHT (+ TURN-MSG-HEIGHT BOARD-HEIGHT))
(define BACKGROUND-IMG (empty-scene WIDTH HEIGHT))

(define TURN-MSG-POSN (make-posn 0 0))
(define TURN-MSG-FONT-SIZE 24)
(define TURN-MSG-FONT-COLOR "black")
(define TURN-MSG-BASE-IMG (rectangle WIDTH TURN-MSG-HEIGHT "outline" "black"))

(define BOARD-IMG-POSN (make-posn 0 TURN-MSG-HEIGHT))
(define BOARD-BASE-IMG (rectangle BOARD-WIDTH BOARD-HEIGHT "outline" "black"))

;; ------------------------------------------

;; A TurnState is one of:
;;  - #false
;;  - (select-piece Pos)
;;  - MoveChoice
(struct select-piece [pos] #:transparent)

;; ------------------------------------------

(struct checkers/gui checkers []
  #:methods gen:turn-based-game/gui
  [;; start-turn : TBGG -> TurnState
   (define (start-turn self) #false)

   ;; display-game-state : TBGG GameState Side TurnState -> Image
   (define (display-game-state self state side turn-state)
     (place-images/align
      (list (display-turn side)
            (display-board/turn-state state side turn-state))
      (list TURN-MSG-POSN
            BOARD-IMG-POSN)
      "left" "top"
      BACKGROUND-IMG))

   (define (display-end-state self state side winner)
     (display-winner-alert
      self
      winner
      (display-game-state self state side (start-turn self))))

   ;; handle-mouse :
   ;; TBGG GameState Side TurnState Posn MouseEvent -> HandlerResult
   (define (handle-mouse self state side turn-state posn mouse)
     (define x_b (- (posn-x posn) (posn-x BOARD-IMG-POSN)))
     (define y_b (- (posn-y posn) (posn-y BOARD-IMG-POSN)))
     (cond
       [(and (<= 0 x_b) (< x_b BOARD-WIDTH)
             (<= 0 y_b) (< y_b BOARD-HEIGHT))
        (cond
          [(mouse=? mouse "button-down")
           (define p
             (pos (quotient x_b SQUARE-SIZE)
                  (- H 1 (quotient y_b SQUARE-SIZE))))
           (cond [(valid-pos? p)
                  (handle-click-pos state side turn-state p)]
                 [else
                  (continue-turn turn-state)])]
          [else
           (continue-turn turn-state)])]
       [else (continue-turn turn-state)]))

   ;; handle-key :
   ;; TBGG GameState Side TurnState KeyEvent -> HandlerResult
   (define (handle-key self state side turn-state key)
     (cond
       [(key=? key "\r")
        (cond [(move-choice? turn-state) (finish-turn turn-state)]
              [else (continue-turn turn-state)])]
       [else (continue-turn turn-state)]))])

;; CHECKERS : TBGGI
(define CHECKERS (checkers/gui))

;; ------------------------------------------

;; handle-click-pos : Board Side TurnState Pos -> HandlerResult
(define (handle-click-pos board side turn-state pos)
  (define space (board-space board pos))
  (match turn-state
    [#false
     (cond [(and space (equal? side (piece-side space)))
            (continue-turn (select-piece pos))]
           [else
            (continue-turn turn-state)])]
    [(select-piece sel-pos)
     (define sel-piece (board-space board sel-pos))
     (cond
       [(and space (equal? side (piece-side space)))
        (continue-turn (select-piece pos))]
       [(false? space)
        (cond
          [(and (pawn? sel-piece) (diagonal-forward-1? side sel-pos pos))
           (continue-turn (move sel-pos pos))]
          [(and (pawn? sel-piece) (diagonal-forward-2? side sel-pos pos))
           (continue-turn (jump sel-pos (list pos)))]
          [(and (king? sel-piece) (diagonal-1? sel-pos pos))
           (continue-turn (move sel-pos pos))]
          [(and (king? sel-piece) (diagonal-2? sel-pos pos))
           (continue-turn (jump sel-pos (list pos)))]
          [else
           (continue-turn turn-state)])]
       [else
        (continue-turn turn-state)])]
    [(move from to)
     (define from-piece (board-space board from))
     (cond
       [(and space (equal? side (piece-side space)))
        (continue-turn (select-piece pos))]
       [(false? space)
        (cond
          [(and (pawn? from-piece) (diagonal-forward-1? side from pos))
           (continue-turn (move from pos))]
          [(and (pawn? from-piece) (diagonal-forward-2? side from pos))
           (continue-turn (jump from (list pos)))]
          [(and (king? from-piece) (diagonal-1? from pos))
           (continue-turn (move from pos))]
          [(and (king? from-piece) (diagonal-2? from pos))
           (continue-turn (jump from (list pos)))]
          [else
           (continue-turn turn-state)])]
       [else
        (continue-turn turn-state)])]
    [(jump from (list path ... last))
     (define from-piece (board-space board from))
     (cond
       [(and space (equal? side (piece-side space)))
        (continue-turn (select-piece pos))]
       [(false? space)
        (cond
          [(and (pawn? from-piece) (diagonal-forward-1? side from pos))
           (continue-turn (move from pos))]
          [(and (pawn? from-piece) (diagonal-forward-2? side from pos))
           (continue-turn (jump from (list pos)))]
          [(and (pawn? from-piece) (diagonal-forward-2? side last pos))
           (continue-turn (jump from (append path (list last pos))))]
          [(and (king? from-piece) (diagonal-1? from pos))
           (continue-turn (move from pos))]
          [(and (king? from-piece) (diagonal-2? from pos))
           (continue-turn (jump from (list pos)))]
          [(and (king? from-piece) (diagonal-2? last pos))
           (continue-turn (jump from (append path (list last pos))))]
          [else
           (continue-turn turn-state)])]
       [else
        (continue-turn turn-state)])]))

;; ------------------------------------------

;; display-turn : Side -> Image
(define (display-turn side)
  (overlay
   (text (format "It is ~a's turn" (side->string side))
         TURN-MSG-FONT-SIZE
         TURN-MSG-FONT-COLOR)
   TURN-MSG-BASE-IMG))

;; display-board/turn-state : Board Side TurnState -> Image
(define (display-board/turn-state board side turn-state)
  (match turn-state
    [#false (display-board board)]
    [(select-piece pos)
     (display-selection-squares
      (list pos)
      (display-board board))]
    [(move a b)
     (display-selection-squares
      (list a b)
      (display-board board))]
    [(jump a bs)
     (display-selection-squares
      (cons a bs)
      (display-board board))]))

;; display-selection-squares : [Listof Pos] Image -> Image
(define (display-selection-squares ps scene)
  (for/fold ([scene scene])
            ([p (in-list ps)])
    (place-image/align
     SELECTION-SQUARE
     (* (pos-x p) SQUARE-SIZE)
     (* (- H 1 (pos-y p)) SQUARE-SIZE)
     "left" "top"
     scene)))

;; display-board : Board -> Image
(define (display-board board)
  (for*/fold ([img BOARD-BASE-IMG])
             ([x (in-range 0 W)]
              [y (in-range 0 H)])
    (place-image/align
     (cond [(valid-pos? (pos x y))
            (display-board-space (board-space board (pos x y)))]
           [else
            LIGHT-SQUARE])
     (* x SQUARE-SIZE)
     (* (- H 1 y) SQUARE-SIZE)
     "left" "top"
     img)))

;; display-board-space : [Maybe Piece] -> Image
(define (display-board-space space)
  (match space
    [#false DARK-SQUARE]
    [(pawn side)
     (cond [(X? side) X-SQUARE]
           [(O? side) O-SQUARE])]
    [(king side)
     (cond [(X? side) X-KING-SQUARE]
           [(O? side) O-KING-SQUARE])]))

;; display-winner-alert : TBGG Side Image -> Image
(define (display-winner-alert tbgg winner scene)
  (define W_alert (* 3/4 (image-width scene)))
  (define H_alert (* 1/2 (image-width scene)))
  (define FONT-SIZE (exact-round (* 1/6 W_alert)))
  (overlay
   (overlay (text (format "Winner: ~a" (side->string winner))
                  FONT-SIZE
                  (color 0 0 0 200))
            (rectangle W_alert H_alert "outline" (color 0 0 0 255))
            (rectangle W_alert H_alert "solid" (color 127 127 127 200)))
   scene))

;; side->string : Side -> String
(define (side->string side)
  (cond [(X? side) "Red"]
        [(O? side) "Black"]))

;; ------------------------------------------

