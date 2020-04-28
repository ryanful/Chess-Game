type position = Game.position

type command = 
  | Select of position
  | Unselect of position
  | Move of position
  | Score
  | Draw
  | Quit

exception Empty

exception Malformed

(** [remove_empty lst] is string list [lst] without any empty strings. *)
let remove_empty lst = 
  List.filter (fun x -> x <> "") lst

(** [helper lst acc] is a list of the first 2 chars in list [lst]. *)
let rec helper lst acc =
  match lst with
  | [] -> acc |> List.rev
  | h::t -> 
    if String.length h > 1 && acc = []
    then helper [] [h.[1];h.[0]]
    else if String.length h = 1 
    then helper t (h.[0]::acc)
    else []

(** [get_positon lst] is the position on the chess board that list [lst] 
    represents.
    Raises: Malformed if [lst] is not a valid representation of a position.*)
let get_position lst =
  match helper lst [] with
  | [] -> raise Malformed
  | h::h1::t ->  
    let rank = (int_of_char h1) - 48 in 
    let file = (h  |> Char.lowercase_ascii |> Char.code) - 96 in
    if file <= 8 && file >= 1 && rank <= 8 && rank >= 1 then (file, rank) 
    else raise Malformed
  | _ -> raise Malformed

let parse s =
  let user_in = 
    s |> String.trim |> String.split_on_char ' ' |> remove_empty 
  in
  match user_in with
  | [] -> raise Empty
  | h::t -> 
    if h = "select" && t <> [] then Select (get_position t)
    else if h = "unselect" && t <> [] then Unselect (get_position t) 
    else if h = "move" && t <> [] then Move (get_position t)
    else if h = "score" && t = [] then Score
    else if h = "draw" && t = [] then Draw
    else if h = "quit" && t = [] then Quit
    else raise Malformed
