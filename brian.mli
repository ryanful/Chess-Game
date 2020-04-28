open State
open Game

(** Representation of dynamic state of the artificial intelligence.

    This module represents the state of the game as Brian (the AI) plays it,
    including the gamestate, what difficulty Brian is set to, what moves Brian
    has available, and what color Brian is playing as
*)

module type Brian = sig

  type piece = ChessPiece.t
  type player = Game.player
  type board = ChessBoard.t

  (** Abstract type representing the Brian state *)
  type t

  (** Type that represents a possible board state and the move that Brian would
      have to make to reach that board state *)
  type possibility = ((Game.position*Game.position)*t)

  (** [init_state_black] is the initial state of Brian as the black player*)
  val init_state_black : t

  (** [init_state_white] is the initial state of Brian as the white player *)
  val init_state_white : t

  (** [get_state br] returns the state inside br *)
  val get_state : t -> State.GameState.t

  (** [moves color st] is the list of valid moves for [color] in [state] *)
  val moves : player -> State.GameState.t -> (Game.position*Game.position) list

  (** [run color difficulty] runs Brian as [color] with difficulty set to [difficulty]*)
  val run : player -> int -> unit
end

module BrianState : Brian