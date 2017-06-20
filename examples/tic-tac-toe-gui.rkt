#lang agile

(provide TIC-TAC-TOE X O)

(require 2htdp/image
         2htdp/universe
         lang/posn
         racket/bool
         racket/math
         turn-based-game/turn-based-game-gui
         "tic-tac-toe.rkt"
         )

;; ------------------------------------------

;; Constants

(define BOARD-SPACE-WIDTH 100)
(define BOARD-SPACE-HEIGHT 100)

(define BOARD-WIDTH (* 3 BOARD-SPACE-WIDTH))
(define BOARD-HEIGHT (* 3 BOARD-SPACE-HEIGHT))

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

(define BOARD-SPACE-FONT-SIZE 72)
(define BOARD-SPACE-FONT-COLOR "black")
(define BOARD-GHOST-SPACE-FONT-COLOR "lightgray")
(define BOARD-SPACE-BASE-IMG
  (rectangle BOARD-SPACE-WIDTH BOARD-SPACE-HEIGHT "outline" "black"))

;; ------------------------------------------

;; type TurnState = [Maybe Pos]

;; ------------------------------------------

(struct tic-tac-toe/gui tic-tac-toe []
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
               (finish-turn
                (pos (quotient x_b BOARD-SPACE-WIDTH)
                     (quotient y_b BOARD-SPACE-HEIGHT)))]
              [else
               (continue-turn
                (pos (quotient x_b BOARD-SPACE-WIDTH)
                     (quotient y_b BOARD-SPACE-HEIGHT)))])]
           [else (continue-turn turn-state)]))

   ;; handle-key :
   ;; TBGG GameState Side TurnState KeyEvent -> HandlerResult
   (define (handle-key self state side turn-state key)
     (cond
       [(key=? key "\r")
        (cond [(pos? turn-state) (finish-turn turn-state)]
              [else (continue-turn turn-state)])]
       [else (continue-turn turn-state)]))])

;; TIC-TAC-TOE : TBGGI
(define TIC-TAC-TOE (tic-tac-toe/gui))

;; main : -> ???
(define (main)
  (start TIC-TAC-TOE))

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
  (cond [(and (pos? turn-state)
              (board-valid-move-choice? board side turn-state))
         (place-image/align
          (display-ghost-board-space side)
          (* (pos-x turn-state) BOARD-SPACE-WIDTH)
          (* (pos-y turn-state) BOARD-SPACE-HEIGHT)
          "left" "top"
          (display-board board))]
        [else
         (display-board board)]))

;; display-board : Board -> Image
(define (display-board board)
  (for*/fold ([img BOARD-BASE-IMG])
             ([x (in-range 0 3)]
              [y (in-range 0 3)])
    (place-image/align
     (display-board-space (board-space board (pos x y)))
     (* x BOARD-SPACE-WIDTH)
     (* y BOARD-SPACE-HEIGHT)
     "left" "top"
     img)))

;; display-board-space : [Maybe Side] -> Image
(define (display-board-space space)
  (cond [(false? space) BOARD-SPACE-BASE-IMG]
        [else
         (overlay
          (text (side->string space)
                BOARD-SPACE-FONT-SIZE
                BOARD-SPACE-FONT-COLOR)
          BOARD-SPACE-BASE-IMG)]))

;; display-ghost-board-space : Side -> Image
(define (display-ghost-board-space space)
  (overlay
   (text (side->string space)
         BOARD-SPACE-FONT-SIZE
         BOARD-GHOST-SPACE-FONT-COLOR)
   BOARD-SPACE-BASE-IMG))

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
  (cond [(X? side) "X"]
        [(O? side) "O"]))

;; ------------------------------------------

