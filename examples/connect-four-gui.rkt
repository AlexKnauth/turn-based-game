#lang agile

(provide CONNECT-FOUR X O)

(require 2htdp/image
         2htdp/universe
         lang/posn
         racket/bool
         racket/math
         turn-based-game/turn-based-game-gui
         "connect-four.rkt"
         )

;; ------------------------------------------

;; Constants

(define BLACK "black")
(define WHITE "white")
(define RED "red")
(define YELLOW "yellow")
(define BROWN "brown")

(define SQUARE-SIZE 100)
(define CIRCLE-RADIUS 40)

(define FRAME-COLOR YELLOW)
(define SQUARE-OUTLINE-COLOR BROWN)
(define SQUARE (overlay (circle CIRCLE-RADIUS "solid" WHITE)
                        (square SQUARE-SIZE "outline" SQUARE-OUTLINE-COLOR)
                        (square SQUARE-SIZE "solid" FRAME-COLOR)))
(define X-SQUARE (overlay (circle CIRCLE-RADIUS "solid" RED) SQUARE))
(define O-SQUARE (overlay (circle CIRCLE-RADIUS "solid" BLACK) SQUARE))
(define X-GHOST-SQUARE (overlay (circle CIRCLE-RADIUS 100 RED) SQUARE))
(define O-GHOST-SQUARE (overlay (circle CIRCLE-RADIUS 100 BLACK) SQUARE))

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

;; type TurnState = [Maybe XPos]

;; ------------------------------------------

(struct connect-four/gui connect-four []
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
     (cond [(and (<= 0 x_b) (< x_b BOARD-WIDTH)
                 (<= 0 y_b) (< y_b BOARD-HEIGHT))
            (cond
              [(mouse=? mouse "button-down")
               (finish-turn (quotient x_b SQUARE-SIZE))]
              [else
               (continue-turn (quotient x_b SQUARE-SIZE))])]
           [else (continue-turn turn-state)]))

   ;; handle-key :
   ;; TBGG GameState Side TurnState KeyEvent -> HandlerResult
   (define (handle-key self state side turn-state key)
     (cond
       [(key=? key "\r")
        (cond [turn-state (finish-turn turn-state)]
              [else (continue-turn turn-state)])]
       [else (continue-turn turn-state)]))])

;; CONNECT-FOUR : TBGGI
(define CONNECT-FOUR (connect-four/gui))

;; main : -> ???
(define (main)
  (start CONNECT-FOUR))

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
  (cond [(and turn-state
              (board-valid-move? board turn-state))
         ;; TODO: show a ghost piece at the position given by turn-state
         (define row
           (index-of (list-ref (board-columns board) turn-state) #false))
         (place-image/align
          (display-ghost-board-space side)
          (* turn-state SQUARE-SIZE)
          (* (- H 1 row) SQUARE-SIZE)
          "left" "top"
          (display-board board))]
        [else
         (display-board board)]))

;; display-board : Board -> Image
(define (display-board board)
  (for*/fold ([img BOARD-BASE-IMG])
             ([col (in-range 0 W)]
              [row (in-range 0 H)])
    (place-image/align
     (display-board-space (board-ref board (pos col row)))
     (* col SQUARE-SIZE)
     (* (- H 1 row) SQUARE-SIZE)
     "left" "top"
     img)))

;; display-board-space : [Maybe Side] -> Image
(define (display-board-space space)
  (cond [(false? space) SQUARE]
        [(X? space) X-SQUARE]
        [(O? space) O-SQUARE]))

;; display-ghost-board-space : Side -> Image
(define (display-ghost-board-space space)
  (cond [(X? space) X-GHOST-SQUARE]
        [(O? space) O-GHOST-SQUARE]))

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

