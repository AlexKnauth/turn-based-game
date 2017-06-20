#lang agile

(provide computer/n-ahead
         COMPUTER/1
         COMPUTER/2
         COMPUTER/3)

(require racket/bool
         racket/local
         "../turn-based-game.rkt"
         "../computer-player.rkt")
(module+ test-util
  (provide (except-out (all-defined-out) W))
  (define W 7))
(module+ test
  (require rackunit
           (submod ".." test-util)
           "../examples/connect-four.rkt")
  (define CONNECT-FOUR (connect-four)))

;; ----------------------------------------------------------------------------

;; An automated connect-four player that only looks one move ahead

(define MOVES-AHEAD 1)

;; A State is one of:
;;  - #false
;;  - Result

;; A Result is a (result [Maybe Side] [List-of ChoiceResult])
(struct result [winner nexts] #:transparent)

;; A ChoiceResult is a (choice-result MoveChoice State)
(struct choice-result [move state] #:transparent)

;; choice-result-winner=? : ChoiceResult Side -> Bool
(define (choice-result-winner=? c side)
  (match (choice-result-state c)
    [#false #false]
    [(result winner _) (equal? winner side)]))

;; INIT-STATE : State
(define INIT-STATE #false)

;; ----------------------------------------------------------------------------

;; The computer-player instance

(struct computer/n-ahead [n]

  #:methods gen:computer-player
  [;; computer-player-start-state : Comp -> State
   (define (computer-player-start-state self)
     INIT-STATE)

   ;; computer-player-next-state : Comp TBG State GameState Side -> State
   (define (computer-player-next-state self tbg state game side)
     (match-define (computer/n-ahead n) self)
     (next-state/depth tbg state game side (* 2 n)))

   ;; computer-player-state-moves :
   ;; Comp TBG State GameState Side -> [Listof MoveChoice]
   (define (computer-player-state-moves self tbg state game side)
     (cond [(false? state) (valid-move-choices tbg game side)]
           [else (map choice-result-move (result-nexts state))]))

   ;; computer-player-state-add-move : Comp TBG State Side MoveChoice -> State
   (define (computer-player-state-add-move self tbg state side move)
     (cond
       [(false? state) #false]
       [(result? state)
        (lookup-move-result (result-nexts state) side move)]))])

;; Goes 2 levels deep: one turn for s, and one turn for the other side
(define COMPUTER/1 (computer/n-ahead 1))

;; Goes 4 levels deep
(define COMPUTER/2 (computer/n-ahead 2))

;; Goes 6 levels deep
(define COMPUTER/3 (computer/n-ahead 3))

;; Goes (2 * MOVES-AHEAD) levels deep
(define COMPUTER (computer/n-ahead MOVES-AHEAD))

;; ----------------------------------------------------------------------------

;; next-state/depth : TBG State GameState Side Natural -> State
;; Goes d levels deep.
(define (next-state/depth tbg state game side d)
  (cond
    [(or (false? state) (zero? d))
     (best-outcomes tbg game side d (valid-move-choices tbg game side))]
    [(result? state)
     ;; update-choice-result : ChoiceResult -> ChoiceResult
     (define (update-choice-result c)
       (define move (choice-result-move c))
       (choice-result
        move
        (next-state/depth tbg (choice-result-state c)
                          (play-at tbg game side move)
                          (next-side tbg game side)
                          (sub1 d))))
     (define old-next-states
       (map choice-result-state (result-nexts state)))
     (define choices
       (map update-choice-result (result-nexts state)))
     ;; optimization: if the updated choices result in the same winner
     ;;               possibilities, just use them all without filtering
     (cond
       [(and (andmap result? old-next-states)
             (equal? (map result-winner old-next-states)
                     (map result-winner (map choice-result-state choices))))
        (result (result-winner state)
                choices)]
       [else
        (best-choices tbg game side choices)])]))

;; ----------------------------------------------------------------------------

;; Tests for COMPUTER/1

(module+ test
  (check-equal? (computer-player-next-state
                 COMPUTER/1
                 CONNECT-FOUR
                 INIT-STATE
                 (list (list X X #false #false #false #false)
                       (list O O O #false #false #false)
                       (list X X X #false #false #false)
                       (list O O O #false #false #false)
                       (list X #false #false #false #false #false)
                       (list O O O #false #false #false)
                       (list X X X #false #false #false))
                 X)
                (only-paths
                 X
                 (list (choice-result 2 (imm-win X))
                       (choice-result 6 (imm-win X)))))
  (check-equal? (computer-player-next-state
                 COMPUTER/1
                 CONNECT-FOUR
                 INIT-STATE
                 (list (list X X #false #false #false #false)
                       (list O O O #false #false #false)
                       (list X X #false #false #false #false)
                       (list O O #false #false #false #false)
                       (list X #false #false #false #false #false)
                       (list O O #false #false #false #false)
                       (list O X X #false #false #false))
                 X)
                (only-paths
                 #false
                 (list (choice-result 1 NO-IMM-WIN-1))))
  (check-equal? (computer-player-next-state
                 COMPUTER/1
                 CONNECT-FOUR
                 INIT-STATE
                 (list (list X X #false #false #false #false)
                       (list O O #false #false #false #false)
                       (list X O X #false #false #false)
                       (list O O #false #false #false #false)
                       (list X #false #false #false #false #false)
                       (list O O #false #false #false #false)
                       (list O X X #false #false #false))
                 X)
                (only-paths
                 #false
                 (list (choice-result 4 NO-IMM-WIN-1))))
  (check-equal? (computer-player-next-state
                 COMPUTER/1
                 CONNECT-FOUR
                 INIT-STATE
                 (list (list O X X #false #false #false)
                       (list O X O #false #false #false)
                       (list X O X #false #false #false)
                       (list O O #false #false #false #false)
                       (list X X #false #false #false #false)
                       (list O O #false #false #false #false)
                       (list O X X #false #false #false))
                 X)
                (only-paths
                 #false
                 (list (choice-result 0 NO-IMM-WIN-1)))))

;; ----------------------------------------------------------------------------

;; Tests for COMPUTER/2

(module+ test
  (check-equal? (computer-player-next-state
                 COMPUTER/2
                 CONNECT-FOUR
                 INIT-STATE
                 (list (list #false #false #false #false #false #false)
                       (list #false #false #false #false #false #false)
                       (list O X #false #false #false #false)
                       (list O X #false #false #false #false)
                       (list #false #false #false #false #false #false)
                       (list #false #false #false #false #false #false)
                       (list X #false #false #false #false #false))
                 X)
                (only-paths
                 #false
                 (list (choice-result
                        1
                        (paths
                         #false
                         (list (choice-result
                                4
                                (only-paths
                                 #false
                                 (list (choice-result 5 NO-IMM-WIN-1))))
                               (choice-result
                                5
                                (only-paths
                                 #false
                                 (list (choice-result 4 NO-IMM-WIN-1)))))
                         NO-IMM-WIN-2))
                       (choice-result
                        4
                        (paths
                         #false
                         (list (choice-result
                                0
                                (only-paths
                                 #false
                                 (list (choice-result 1 NO-IMM-WIN-1))))
                               (choice-result
                                1
                                (only-paths
                                 #false
                                 (list (choice-result 0 NO-IMM-WIN-1)))))
                         NO-IMM-WIN-2)))))

  (check-equal? (computer-player-next-state
                 COMPUTER/2
                 CONNECT-FOUR
                 INIT-STATE
                 (list (list #false #false #false #false #false #false)
                       (list #false #false #false #false #false #false)
                       (list O X #false #false #false #false)
                       (list O X #false #false #false #false)
                       (list #false #false #false #false #false #false)
                       (list X #false #false #false #false #false)
                       (list #false #false #false #false #false #false))
                 X)
                (only-paths
                 #false
                 (list
                  (choice-result
                   0
                   (paths
                    #false
                    (list
                     (choice-result
                      1
                      (only-paths
                       #false
                       (list (choice-result 4 NO-IMM-WIN-1))))
                     (choice-result
                      4
                      (only-paths
                       #false
                       (list (choice-result 1 NO-IMM-WIN-1)))))
                    NO-IMM-WIN-2))
                  (choice-result 1 NO-IMM-WIN-3)
                  (choice-result
                   4
                   (paths
                    #false
                    (list
                     (choice-result
                      0
                      (only-paths
                       #false
                       (list (choice-result 1 NO-IMM-WIN-1))))
                     (choice-result
                      1
                      (only-paths
                       #false
                       (list (choice-result 0 NO-IMM-WIN-1)))))
                    NO-IMM-WIN-2)))))

  (check-equal? (computer-player-next-state
                 COMPUTER/2
                 CONNECT-FOUR
                 INIT-STATE
                 (list (list #false #false #false #false #false #false)
                       (list #false #false #false #false #false #false)
                       (list X #false #false #false #false #false)
                       (list X #false #false #false #false #false)
                       (list #false #false #false #false #false #false)
                       (list #false #false #false #false #false #false)
                       (list O O #false #false #false #false))
                 X)
                (only-paths
                 X
                 (list
                  (choice-result
                   1
                   (paths
                    X
                    (list
                     (choice-result
                      0
                      (only-paths
                       X
                       (list (choice-result 4 (imm-win X)))))
                     (choice-result
                      4
                      (only-paths
                       X
                       (list (choice-result 0 (imm-win X))))))
                    (only-paths
                     X
                     (list
                      (choice-result 0 (imm-win X))
                      (choice-result 4 (imm-win X))))))
                  (choice-result
                   4
                   (paths
                    X
                    (list
                     (choice-result
                      1
                      (only-paths
                       X
                       (list (choice-result 5 (imm-win X)))))
                     (choice-result
                      5
                      (only-paths
                       X
                       (list (choice-result 1 (imm-win X))))))
                    (only-paths
                     X
                     (list
                      (choice-result 1 (imm-win X))
                      (choice-result 5 (imm-win X)))))))))
  (check-equal? (computer-player-next-state
                 COMPUTER/2
                 CONNECT-FOUR
                 INIT-STATE
                 (list (list #false #false #false #false #false #false)
                       (list #false #false #false #false #false #false)
                       (list X #false #false #false #false #false)
                       (list X #false #false #false #false #false)
                       (list #false #false #false #false #false #false)
                       (list O O #false #false #false #false)
                       (list #false #false #false #false #false #false))
                 X)
                (only-paths
                 X
                 (list
                  (choice-result
                   1
                   (paths
                    X
                    (list
                     (choice-result
                      0
                      (only-paths
                       X
                       (list (choice-result 4 (imm-win X)))))
                     (choice-result
                      4
                      (only-paths
                       X
                       (list (choice-result 0 (imm-win X))))))
                    (only-paths
                     X
                     (list
                      (choice-result 0 (imm-win X))
                      (choice-result 4 (imm-win X)))))))))

  (check-equal? (computer-player-next-state
                 COMPUTER/2
                 CONNECT-FOUR
                 INIT-STATE
                 (list (list #false #false #false #false #false #false)
                       (list #false #false #false #false #false #false)
                       (list O #false #false #false #false #false)
                       (list O #false #false #false #false #false)
                       (list #false #false #false #false #false #false)
                       (list #false #false #false #false #false #false)
                       (list X X #false #false #false #false))
                 X)
                (only-paths
                 #false
                 (list
                  (choice-result
                   1
                   (paths
                    #false
                    (list
                     (choice-result
                      4
                      (only-paths
                       #false
                       (list (choice-result 5 NO-IMM-WIN-1))))
                     (choice-result
                      5
                      (only-paths
                       #false
                       (list (choice-result 4 NO-IMM-WIN-1)))))
                    NO-IMM-WIN-2))
                  (choice-result
                   4
                   (paths
                    #false
                    (list
                     (choice-result
                      0
                      (only-paths
                       #false
                       (list (choice-result 1 NO-IMM-WIN-1))))
                     (choice-result
                      1
                      (only-paths
                       #false
                       (list (choice-result 0 NO-IMM-WIN-1)))))
                    NO-IMM-WIN-2))
                  (choice-result
                   6
                   (only-paths
                    #false
                    (list
                     (choice-result
                      6
                      (paths
                       #false
                       (list
                        (choice-result
                         6
                         (paths
                          #false
                          (list
                           (choice-result
                            6
                            (only-paths
                             #false
                             (list (choice-result 0 #false)
                                   (choice-result 1 #false)
                                   (choice-result 2 #false)
                                   (choice-result 3 #false)
                                   (choice-result 4 #false)
                                   (choice-result 5 #false)))))
                          NO-IMM-WIN-0)))
                       NO-IMM-WIN-1)))))))))

;; ----------------------------------------------------------------------------

;; best-outcomes : TBG GameState Side Natural [List-of MoveChoice] -> State
(define (best-outcomes tbg game side n mvs)
  (define side* (next-side tbg game side))
  (cond
    [(winning-state? tbg game side) (imm-win side)]
    [(winning-state? tbg game side*) (imm-win side*)]
    [(zero? n) (result #false
                       (map (λ (mv) (choice-result mv #false)) mvs))]
    [(empty? mvs) IMM-TIE]
    [else
     ;; next-outcome : MoveChoice -> ChoiceResult
     (define (next-outcome c)
       (define game* (play-at tbg game side c))
       (choice-result
        c
        (best-outcomes tbg game* side* (sub1 n)
                       (valid-move-choices tbg game* side*))))
     (best-choices tbg game side (map next-outcome mvs))]))

;; best-choices : TBG GameState Side [List-of ChoiceResult] -> Result
(define (best-choices tbg game side choices)
  (define side* (next-side tbg game side))
  ;; winning-choice? : ChoiceResult -> Boolean
  (define (winning-choice? entry) (choice-result-winner=? entry side))
  ;; losing-choice? : ChoiceResult -> Boolean
  (define (losing-choice? entry) (choice-result-winner=? entry side*))

  (define winning-choices
    (filter winning-choice? choices))
  (cond
    [(not (empty? winning-choices))
     (result side winning-choices)]
    [else
     (define non-losing-choices
       (filter (compose not losing-choice?) choices))
     (cond
       [(not (empty? non-losing-choices))
        (result #false non-losing-choices)]
       [else
        (result side* choices)])]))

;; ----------------------------------------------------------------------------

;; lookup-move-result : [List-of ChoiceResult] Side MoveChoice -> State
(define (lookup-move-result choices side move)
  (cond
    [(empty? choices) #false]
    [(cons? choices)
     (if (equal? move (choice-result-move (first choices)))
         (choice-result-state (first choices))
         (lookup-move-result (rest choices) side move))]))

;; ----------------------------------------------------------------------------

;; States for immediate wins or ties

;; imm-win : [Maybe Side] -> Result
(define (imm-win winner) (result winner '()))

;; IMM-TIE : Result
(define IMM-TIE (imm-win #false))

;; ----------------------------------------------------------------------------

(module+ test-util
  
  ;; States for no immediate winner

  ;; only-paths : [Maybe Side] [List-of Choice-Result] -> State
  (define (only-paths winner choices)
    (result winner choices))

  ;; paths : [Maybe Side] [List-of Choice-Result] State -> State
  (define (paths winner meaningful-choices default)
    (result winner (choice-paths meaningful-choices default)))

  ;; choice-paths :
  ;; [List-of ChoiceResult] State -> [List-of ChoiceResult]
  (define (choice-paths meaningful-choices default)
    (choice-paths/a meaningful-choices 0 default))

  ;; choice-paths/a :
  ;; [List-of ChoiceResult] [Maybe MoveChoice] State -> [List-of ChoiceResult]
  (define (choice-paths/a meaningful-choices start default)
    (cond [(false? start) '()]
          [(empty? meaningful-choices)
           (cons
            (choice-result start default)
            (choice-paths/a '()
                            (move-choice-next start)
                            default))]
          [(= start (choice-result-move (first meaningful-choices)))
           (cons
            (first meaningful-choices)
            (choice-paths/a (rest meaningful-choices)
                            (move-choice-next start)
                            default))]
          [else
           (cons
            (choice-result start default)
            (choice-paths/a meaningful-choices
                            (move-choice-next start)
                            default))]))

  ;; move-choice-next : MoveChoice -> [Maybe MoveChoice]
  (define (move-choice-next move)
    (cond [(= W (add1 move)) #false]
          [else (add1 move)]))

  (define NO-IMM-WIN-0
    (paths #false '() #false))

  (define NO-IMM-WIN-1
    (paths #false '() NO-IMM-WIN-0))

  (define NO-IMM-WIN-2
    (paths #false '() NO-IMM-WIN-1))

  (define NO-IMM-WIN-3
    (paths #false '() NO-IMM-WIN-2))

  (define NO-IMM-WIN-4
    (paths #false '() NO-IMM-WIN-3)))

;; ----------------------------------------------------------------------------

(module+ test
  (check-equal? NO-IMM-WIN-1
                (result
                 #false
                 (build-list W
                             (λ (i)
                               (choice-result i NO-IMM-WIN-0)))))

  (check-equal? NO-IMM-WIN-2
                (result
                 #false
                 (build-list W
                             (λ (i)
                               (choice-result i NO-IMM-WIN-1)))))

  (check-equal? NO-IMM-WIN-3
                (result
                 #false
                 (build-list W
                             (λ (i)
                               (choice-result i NO-IMM-WIN-2)))))

  (check-equal? NO-IMM-WIN-4
                (result
                 #false
                 (build-list W
                             (λ (i)
                               (choice-result i NO-IMM-WIN-3))))))

;; ----------------------------------------------------------------------------

