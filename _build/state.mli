open Game

(** Representation of dynamic chess state.

    This module represents the state of the game as it is being played,
    including whose turn it currently is, where the pieces are on the,
    and the functions that cause the state to change.
*)
module type State = sig

  type piece = ChessPiece.t
  type player = Game.player
  type board = ChessBoard.t

  (** Abstract type representing game state *)
  type t

  (** [init_state a] is the initial state of the game when playing adventure [a]. 
      In that state the adventurer is currently located in the starting room,
      and they have visited only that room. *)
  val init_state : t

  (** [state_from_board st board] changes the board in [st] to [board] *)
  val state_from_board : t -> board -> t

  (** [state_copy st] returns a copy of [st] *)
  val state_copy : t -> t

  (** [visualize st] returns the string representation of a board state
        formatted for printing to the console *)
  val visualize : t -> string

  (** [captured st player] returns the list of pieces that [player] has captured
        in the game represented by [st] *)
  val captured : t -> player -> piece list

  (** [score st player] returns the total score of all the pieces that [player]
        has captured in the game represented by [st] *)
  val score : t -> player -> int

  (** [renaining st color] is the list of all remaining pieces for the 
      player [color] on the chess board*)
  val remaining : t -> player -> piece list

  (** [curr_turn st] returns the current turn in the game represented by [st] *)
  val curr_turn : t -> player

  (** [curr_board st] returns the current board in the game represented by [st]*)
  val curr_board : t -> board

  (** [change_turn st] changes the turn in the game represented by [st] *)
  val change_turn : t -> t

  (** [check_win s color] returns [true] if [color] has won in [s]. Returns
      [false] otherwise.
      i.e. Will check that the opposing king is under attack and that the 
      opposing player has no valid moves. (logic subject to change) *)
  val check_win : t -> player -> bool

  (** [move st start finish] moves the piece located at [start] to the position
      [finish] and then changes the turn in [st] *)
  val move : t -> position -> position -> t

  (** [move_options state pos] provides the list of valid moves for a piece at 
      position [pos] in the board store din [state] *)
  val move_options : t -> position -> position list

  (** [is_players_piece st pos] returns [true] if the piece located at [pos]
      belongs to the player w*)
  val is_players_piece : t -> position -> bool

  (** [compare st st'] returns [-1] if the board state in [st] is considered 
      weaker than in [st'], [0] if the board states are considered equal, and 
      [1] if the board state in [st] is considered stronger than in [st'] *)
  val compare : t -> t-> int

  (** [minimax st] returns the minimax value of the current board state from the
        perspective of the current player *)
  val minimax : t -> int

  (** To do: functions to retrieve information about state.t such as score *)

end

module GameState : State