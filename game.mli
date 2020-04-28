(** TODO: Figuring out what's useful information by Raheel*)
(*type piece*)

(** The type [position] represents a position on a chess board, where
    if [p] is a [position], [fst p] is the file and [snd p] is the rank. *)
type position = int * int

(** [position_to_string pos] is the string representation of position [pos]. *)
val position_to_string : position -> string

(** The type [player] represents a player. *)
type player = Black | White

(** The type [name] represents the name of a chess piece. *)
type name = Rook | Knight | Bishop | King | Queen | Pawn

type threats = {black : int; white : int}

(** A module that matches [Piece] can be a piece on the board of a module that
    matches [Board]. *)
module type Piece = sig 

  (** The abstract type of values representing a chess piece.*)
  type t 

  (** Gets “Moveability” list of the certain piece type
      Check if any of the moves are cut off by pieces
      If friendly do not include the square
      If enemy include the square but none past
  *)

  (** [build name position player] is piece [p'] with name [n], position [p],
       and owned by player [p]. *)
  val build : name -> position -> player -> t

  (** [get_name piece] is the name of [piece] *)
  val get_name : t -> name

  (** [get_player piece] is the player that owns [piece]. *)
  val get_player : t -> player

  (** [get_position piece] is the current location of [piece] *)
  val get_position : t -> position

  (** [move p pos] is piece [p'], which is piece [p] with its position changed 
      to position [pos]*)
  val move : t -> position -> t (*should we have this in board or piece*)

  (** [is_player_piece player p] is [true] if the player associated with piece [p]
      matches [player]. Otherwise [false]. *)
  val is_players_piece : player -> t option -> bool

  (** [player_to_string pl] is the string representation of player [pl]. *)
  val player_to_string : player -> string 

  (** [to_string p] is the string representation of piece [p]. *)
  val to_string : t -> string

end

(** *)
module ChessPiece : Piece

(**the type of a square on a chess board.*)
(** add threat status? 
    type square = 
    {pos : position; item : ChessPiece.t option; 
    w_threatened : int; b_threatened : int}*)

module type Square = sig
  (** The abstract type of a square on a Chess Board. *)
  type t

  (** [build pos piece_opt w_thr b_thr] creates a new square with position 
      [pos], item [piece_opt], and is threatened by int [w_thr] white pieces and 
      int [b_thr] black pieces. *)
  val build : position -> ChessPiece.t option -> int -> int  -> t

  (** [build_with_piece pos name player w_thr b_thr] creates a new square with 
      position [pos], piece with the same position, name [name], and player 
      [player], and is threatened by int [w_thr] white pieces and int [b_thr] 
      black pieces.*)
  val build_with_piece : position -> name -> player -> int -> int -> t

  (** the coordinates of the square [t] on the chessboard*)
  val get_position : t -> position

  (**[get_piece] returns the chess piece that is on the square, 
     and None of the square is empty*)
  val get_piece : t -> ChessPiece.t option

  (** [get_wthreat] is the amount of white pieces on the board that can 
      legally move to this square*)
  val get_wthreat : t -> int

  (**[get_bthreat] is the amount of white pieces on the board that can 
     legally move to this square*)
  val get_bthreat : t -> int

  (** *)
  val remove_piece : t -> t

  (** *)
  val change_position : t -> position -> t

end

module ChessSquare : Square

(** To do: specs by nathaniel *)
module type Board = sig

  (** *)
  type t

  (*list of pieces for a player*)
  type pieces

  (** *)
  val empty : unit -> t

  (**[init] is a board with the initial piece configuration of a chess game *)
  val init : t

  (**[copyboard] is a copy of the 2d array representing the chessboard*)
  val copyboard : t -> t

  val init4testing : unit -> t

  (**the square at the specified position on board *)
  val get_square : t -> position -> ChessSquare.t

  (**the piece at the position specified on the board*)
  val get_piece : t -> position -> ChessPiece.t option

  (**the list of all pieces for a given player on the board*)
  val piece_list : t -> player -> ChessPiece.t list

  (**a new board with piece at start position moved to end position. 
     If a piece already exists at end location, the piece is removed*)
  val move : t -> position -> position -> t 
  (** To do: 1. Figure out abstract types
             2. Useful methods specficiations and their types

             3.  Valid move (for implementation) other helpful functions *)

  (**is true if the piece at start position can legally move to end position 
     val valid_move : t -> position -> position -> bool*)

  (** [check board player] returns [true] if [player]'s king is under attack
       in [board], [false] otherwise *)
  val check : t -> player -> bool

  (** [checkmate b player] is [true]if a player's king is in checkmate, 
      [false] otherwise. *)
  val checkmate : t -> player -> bool

  (** *)
  val valid_moves : t -> ChessPiece.t -> position list 

  (** *)
  val nearby_knight_check : t -> position -> player -> bool 

  (** *)
  val enemies_around_king : t -> position -> player -> int * int -> bool 

  (** *)
  val king_is_safe : t -> player -> position -> position -> bool

  (**All validmoves for player. Also thought of as all squares threatened by player*)
  val list_threatened : t -> player -> position list


  val get_rank : t -> int -> ChessSquare.t array

end

(** *)
module ChessBoard : Board