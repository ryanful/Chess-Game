open State
open Game

(**This module represents the tree of the possible game states that result 
   from every possible move for each player. *)

module type BoardTree = sig

  (**the type of a move - piece starting position and ending position*)
  type move = position * position

  (**the type representing the minimax value *)
  type minimax = int

  (**the type representing the contents of a potential board state*)
  type state = GameState.t * move * minimax 

  (**the type of the game tree*)
  type t = Nodes of state * t list * t option


  (**[init_tree st] creates the root of the game tree based 
     on the gamestate [st] *)
  val init_tree : GameState.t -> t

  (**[minimax st] is the minimax value for the board state [st] 
     based on the material value of pieces left on the board for each player*)
  val minimax : GameState.t -> int

  (**[add_depth n] is node [n] with all possible child nodes 
     (potential game states) added*)
  val add_depth : t -> t

  (**[next_move st] is the most profitable move to make based 
     on minimax values of all potential moves*)
  val next_move : GameState.t -> move
end

module Findmove : BoardTree 