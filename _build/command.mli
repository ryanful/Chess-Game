(** This module defines the valid commands a user can make. It handles the 
    parsing of strings to commands. 
*)

(** The type [position] represents the position that can be part of a player 
    command. It represents the first 2 non-whitespace characters after the 
    action as ints, where characters A-H, a-z, and 1-8 are equivalent to ints 
    1-8 and all other characters are equivalent to 0.

    Examples:
    - if the player command is ["select B6"] or ["select B   6"] then the 
      position is [(2,6)].
    - if the player command is ["unselct Z9"] then the position is [(0,0)].*)
type position = Game.position

(** The type [command] represents a player command that is decomposed into an 
    action and possibly position. *)
type command = 
  | Select of position
  | Unselect of position
  | Move of position
  | Score
  | Draw
  | Quit

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is parsed. *)
exception Malformed


(** [parse str] parses a player's input into a [command]. The first word 
    (i.e., consecutive sequence of non-space characters) of [str] becomes 
    the action. The rest of the characters, stripped of whitespace, become the 
    position.

    Examples: 
    - [parse "select A 7"] is [Select (1,7)]
    - [parse "quit"] is Quit

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space 
    characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises: [Empty] if [str] is the empty string or contains only spaces.

    Raise: [Malformed] if the command isn't formed properly. A command isn't 
    formed properly if:
    - the action isn't "select", "unselect", "score", "quit", 
      "draw", or "move". 
    - the action is "score", "draw", or "quit" and the position is not [""]
    - the action is "select", "unselect", or "move" and the position is not
      in the form [c0c1] where c0 is a character A-H or a-h and c1 is a 
      character 1-8. *)
val parse : string -> command