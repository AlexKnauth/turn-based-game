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
                     ))

@(define-syntax-rule (defid id)
   (defidentifier #'id))

@title{Turn-Based Games}

@section{Basic turn-based game interfaces}

@defmodule[turn-based-game/turn-based-game]{

@defidform[#:kind "generic interface"
           gen:turn-based-game]{
  A @deftech{TBG} is an instance of @racket[gen:turn-based-game]. This is
  meant to be a dictionary of game-related methods. It is @emph{NOT} meant
  to be the state of the game.

  Each instance defines its own types for @deftech{Side},
  @deftech{GameState}, and @deftech{MoveChoice}, which are used by the
  methods to keep track of players and turns, what state the game is in,
  and possible choices a player could make on a given turn.

  Each instance needs to define the methods:
  @itemlist[
    @item{@defid[sides]}
    @item{@defid[next-side]}
    @item{@defid[valid-move-choice?]}
    @item{@defid[valid-move-choices]}
    @item{@defid[play-at]}
    @item{@defid[winning-state?]}
  ]
}

@defidform[#:kind "generic interface"
           gen:turn-based-game/standard-initial-state]{
  A @deftech{TBGI} is an instance of both @racket[gen:turn-based-game] and
  @racket[gen:turn-based-game/standard-initial-state].

  This interface includes two more methods:
  @itemlist[
    @item{@defid[standard-initial-state]}
    @item{@defid[standard-initial-side]}
  ]
}}

@section{User interfaces for playing turn-based games}

@subsection[#:style 'hidden]{}

@defmodule[turn-based-game/turn-based-game-gui]{

@defidform[#:kind "generic interface"
           gen:turn-based-game/gui]{
  In order to be used for a user interface like the @racket[start]
  function, turn-based games must implement the
  @racket[gen:turn-based-game/gui] interface in addition to
  @racket[gen:turn-based-game]. A @deftech{TBGG} is an instance of both of
  these interfaces. A @deftech{TBGGI} is a @tech{TBGG} that also specifies
  initial states with @racket[gen:turn-based-game/standard-initial-state].
  Again, instances of these are meant to be dictionaries
  of game-related methods, not the state of the game.

  In addition to the types @tech{Side}, @tech{GameState}, and
  @tech{MoveChoice}, each instance should define its own type for
  @deftech{TurnState}. The @tech{TurnState} type keeps track of any state
  that a user could build up before making a final @tech{MoveChoice} for
  their turn.

  Each instance needs to define the methods:
  @itemlist[
    @item{@defid[display-game-state]}
    @item{@defid[display-end-state]}
    @item{@defid[handle-mouse]}
    @item{@defid[handle-key]}
    @item{@defid[start-turn]}
  ]
}}

@subsection[#:style 'hidden]{}

@defmodule[turn-based-game/controller/human-player-gui-controller]{

@defproc[(start [game-desc @#,tech{TBGGI}]) void?]{
  Starts the turn-based game with its standard initial state.
}}

@section{Playing against the computer}

A @deftech{ComputerPlayer} is an object that describes a strategy the
computer will use to play a game. Some will be specific to a particular
game, but some can be generic across all @tech{TBG} instances, although
for some they might be increbibly slow.

@subsection[#:style 'hidden]{}

@defmodule[turn-based-game/computer-player/n-ahead]{

@defproc[(computer/n-ahead [n natural?]) @#,tech{ComputerPlayer}]{
  An automated turn-based-game player that only looks @racket[n] moves
  ahead.
}}

@subsection[#:style 'hidden]{}

@defmodule[turn-based-game/computer-player/score-explore-random]{

@defproc[(computer/score-explore-random [n natural?] [p natural?] [k natural?])
         @#,tech{ComputerPlayer}]{
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

