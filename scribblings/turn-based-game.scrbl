#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     racket/math
                     turn-based-game/turn-based-game
                     turn-based-game/turn-based-game-gui
                     turn-based-game/computer-player
                     turn-based-game/computer-player/n-ahead
                     turn-based-game/computer-player/score-explore-random
                     turn-based-game/controller/human-player-gui-controller
                     turn-based-game/controller/computer-player-gui-controller
                     (only-in data/collection sequenceof known-finite?)
                     (only-in 2htdp/image image? real-valued-posn?)
                     (only-in 2htdp/universe mouse-event? key-event?)
                     ))

@(define-syntax-rule (defid id)
   (defidentifier #'id))

@(define Boolean @racketlink[boolean?]{Boolean})
@(define Natural @racketlink[natural?]{Natural})
@(define Image @racketlink[image?]{Image})
@(define Posn @racketlink[real-valued-posn?]{Posn})
@(define MouseEvent @racketlink[mouse-event?]{MouseEvent})
@(define KeyEvent @racketlink[key-event?]{KeyEvent})

@title{Turn-Based Games}

@section{Basic turn-based game interfaces}

@(define TBG @tech{TBG})
@(define Side @tech{Side})
@(define GameState @tech{GameState})
@(define MoveChoice @tech{MoveChoice})

@(define TBGI @tech{TBGI})

@defmodule[turn-based-game/turn-based-game]{

@defidform[#:kind "generic interface"
           gen:turn-based-game]{
  A @deftech[TBG] is an instance of @racket[gen:turn-based-game]. This is
  meant to be a dictionary of game-related methods. It is @emph{NOT} meant
  to be the state of the game.

  Each instance defines its own types for @deftech[Side],
  @deftech[GameState], and @deftech[MoveChoice], which are used by the
  methods to keep track of players and turns, what state the game is in,
  and possible choices a player could make on a given turn.

  Each instance needs to define the methods:
  @itemlist[
    @item{@racket[sides] :
          @racket[@#,TBG @#,GameState -> (listof @#,Side)]}
    @item{@racket[next-side] :
          @racket[@#,TBG @#,GameState @#,Side -> @#,Side]}
    @item{@racket[valid-move-choice?] :
          @racket[@#,TBG @#,GameState @#,Side @#,MoveChoice -> @#,Boolean]}
    @item{@racket[valid-move-choices] :
          @racket[@#,TBG @#,GameState @#,Side -> (sequenceof @#,MoveChoice)]}
    @item{@racket[play-move-choice] :
          @racket[@#,TBG @#,GameState @#,Side @#,MoveChoice -> @#,GameState]}
    @item{@racket[winning-state?] :
          @racket[@#,TBG @#,GameState @#,Side -> @#,Boolean]}
  ]

@nested[#:style 'inset]{
    
  @defproc[(sides [tbg @#,TBG] [game-state @#,GameState]) (listof @#,Side)]{
    Returns the set of sides that are currently in the game. Order in the
    result list does not matter.
  }

  @defproc[(next-side [tbg @#,TBG] [game-state @#,GameState] [side @#,Side])
           @#,Side]{
    Returns the side to play once @racket[side]'s turn is over.
  }

  @defproc[(valid-move-choice? [tbg @#,TBG]
                               [game-state @#,GameState]
                               [side @#,Side]
                               [move-choice @#,MoveChoice])
           @#,Boolean]{
    Determines whether the given @racket[move-choice] is a valid legal move
    according to the rules of the game. If it is legal,
    @racket[valid-move-choice?] returns true, otherwise it returns false.
  }

  @defproc[(valid-move-choices [tbg @#,TBG]
                               [game-state @#,GameState]
                               [side @#,Side])
           (sequenceof @#,MoveChoice)]{
    Returns a sequence which will enumerate all possible legal moves within
    the rules of the game.

    NOTE: This sequence may or may not be finite, so if you're relying on
    using it to iterate through all the elements, check with
    @racket[known-finite?] first.
  }

  @defproc[(play-move-choice [tbg @#,TBG]
                             [game-state @#,GameState]
                             [side @#,Side]
                             [move-choice @#,MoveChoice])
           @#,GameState]{
    Returns the game state after @racket[side] plays the given
    @racket[move-choice] for their turn. This should not mutate the
    original game state, but return a new one.
  }

  @defproc[(winning-state? [tbg @#,TBG]
                           [game-state @#,GameState]
                           [side @#,Side])
           @#,Boolean]{
    Determines whether the given @racket[side] has won in the given
    @racket[game-state] according to the rules of the game.
  }
}}

@defidform[#:kind "generic interface"
           gen:turn-based-game/standard-initial-state]{
  A @deftech[TBGI] is an instance of both @racket[gen:turn-based-game] and
  @racket[gen:turn-based-game/standard-initial-state].

  This interface includes two more methods:
  @itemlist[
    @item{@racket[standard-initial-state] :
          @racket[@#,TBGI -> @#,GameState]}
    @item{@racket[standard-initial-side] :
          @racket[@#,TBGI -> @#,Side]}
  ]

@nested[#:style 'inset]{
    
  @defproc[(standard-initial-state [tbg @#,TBGI]) @#,GameState]{
    Produces the standard initial game state for the given turn-based game.
  }

  @defproc[(standard-initial-side [tbg @#,TBGI]) @#,Side]{
    Produces the standard starting side for the given turn-based game, that
    is, the side that starts the game with their turn.
  }
}}}

@section{User interfaces for playing turn-based games}

@subsection[#:style 'hidden]{}

@(define TBGG @tech{TBGG})
@(define TBGGI @tech{TBGGI})
@(define TurnState @tech{TurnState})
@(define HandlerResult @tech{HandlerResult})

@defmodule[turn-based-game/turn-based-game-gui]{

@defidform[#:kind "generic interface"
           gen:turn-based-game/gui]{
  In order to be used for a user interface like the @racket[start]
  function, turn-based games must implement the
  @racket[gen:turn-based-game/gui] interface in addition to
  @racket[gen:turn-based-game]. A @deftech[TBGG] is an instance of both of
  these interfaces. A @deftech[TBGGI] is a @TBGG that also specifies
  initial states with @racket[gen:turn-based-game/standard-initial-state].
  Again, instances of these are meant to be dictionaries
  of game-related methods, not the state of the game.

  In addition to the types @Side, @GameState, and
  @MoveChoice, each instance should define its own type for
  @deftech[TurnState]. The @TurnState type keeps track of any state
  that a user could build up before making a final @MoveChoice for
  their turn.

  Each instance needs to define the methods:
  @itemlist[
    @item{@defid[display-game-state] :
          @racket[@#,TBGG @#,GameState @#,Side @#,TurnState -> @#,Image]}
    @item{@defid[display-end-state] :
          @racket[@#,TBGG @#,GameState @#,Side (or/c #f @#,Side) -> @#,Image]}
    @item{@defid[start-turn] :
          @racket[@#,TBGG -> @#,TurnState]}
    @item{@defid[handle-mouse] :
      @racket[@#,TBGG @#,GameState @#,Side @#,TurnState @#,Posn @#,MouseEvent
              -> @#,HandlerResult]}
    @item{@defid[handle-key] :
          @racket[@#,TBGG @#,GameState @#,Side @#,TurnState @#,KeyEvent
                  -> @#,HandlerResult]}
  ]

  The key and mouse handlers should return @|HandlerResult|s according to
  the data definition below.

  @nested[#:style 'code-inset]{
    A @deftech[HandlerResult] is one of: @linebreak[]
    @hspace[1] -- @racket[(@#,defid[continue-turn] @#,TurnState)] @linebreak[]
    @hspace[1] -- @racket[(@#,defid[finish-turn] @#,MoveChoice)]
  }
}}

@subsection[#:style 'hidden]{}

@defmodule[turn-based-game/controller/human-player-gui-controller]{

@defproc[(start [game-desc @#,TBGGI]) void?]{
  Starts the turn-based game with its standard initial state.
}}

@section{Playing against the computer}

@(define ComputerPlayer @tech{ComputerPlayer})

A @deftech[ComputerPlayer] is an object that describes a strategy the
computer will use to play a game. Some will be specific to a particular
game, but some can be generic across all @TBG instances, although
for some they might be increbibly slow.

@subsection[#:style 'hidden]{}

@defmodule[turn-based-game/computer-player/n-ahead]{

@defproc[(computer/n-ahead [n @#,Natural]) @#,ComputerPlayer]{
  An automated turn-based-game player that only looks @racket[n] moves
  ahead.
}}

@subsection[#:style 'hidden]{}

@defmodule[turn-based-game/computer-player/score-explore-random]{

@defproc[(computer/score-explore-random [n @#,Natural]
                                        [p @#,Natural]
                                        [k @#,Natural])
         @#,ComputerPlayer]{
  An automated turn-based-game player that looks @racket[n] moves ahead,
  and for each game state after that it randomly explores @racket[p]
  different paths of @racket[k] more moves ahead, scoring each game state
  according to the percentage of those paths that produce winning results.
}}

@subsection[#:style 'hidden]{}

@defmodule[turn-based-game/controller/computer-player-gui-controller]{

@defproc[(start/computer [game-desc @#,tech{TBGGI}]
                         [computer-players
                          (hash/c @#,tech{Side} @#,tech{ComputerPlayer})])
         void?]{
  Starts the turn-based game with its standard initial state, with some of
  the sides played by the computer.
}}

