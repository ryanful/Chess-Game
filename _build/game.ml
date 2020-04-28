type name = Rook | Knight | Bishop | King | Queen | Pawn

(**(File * Rank) representing the coordinates of seuares on a chess board *)
type position = int * int

(** [position_to_string pos] returns a string representation of [pos] *)
let position_to_string pos = 
  let left = fst pos |> string_of_int in
  let right = snd pos |> string_of_int in
  "(" ^ left ^ ", " ^ right ^ ")"


type player = Black | White

type threats = {black : int; white : int}

module type Piece = sig 
  type t 
  val build : name -> position -> player -> t
  val get_name : t -> name
  val get_player : t -> player
  val get_position : t -> position
  val move : t -> position -> t
  val is_players_piece : player -> t option -> bool
  val player_to_string : player -> string 
  val to_string : t -> string
end

module ChessPiece : Piece = struct

  type t = {
    name : name;
    position : position;
    player : player 
  }

  let build name position player = {
    name = name; 
    position = position; 
    player = player
  }

  let get_name piece =
    piece.name

  let get_player piece = 
    piece.player

  let get_position piece = 
    piece.position

  let move piece pos = {
    name = piece.name;
    position = pos;
    player = piece.player;
  }

  let is_players_piece player = function
    | Some piece -> get_player piece = player
    | None -> false

  let player_to_string = function
    | Black -> "Black "
    | White -> "White "

  (** [name_to_string] returns the string representation of
      the inputted name *)
  let name_to_string = function 
    | Rook -> "Rook "
    | Knight -> "Knight "
    | Bishop -> "Bishop "
    | King -> "King "
    | Queen -> "Queen "
    | Pawn -> "Pawn "

  let to_string piece = 
    let player = get_player piece |> player_to_string in 
    let name = get_name piece |> name_to_string in
    let position = get_position piece |> position_to_string in
    player ^ name ^ "is at " ^ position

end


module type Square = sig 
  type t
  val build : position -> ChessPiece.t option -> int -> int  -> t
  val build_with_piece : position -> name -> player -> int -> int -> t
  val get_position : t -> position
  val get_piece : t -> ChessPiece.t option
  val get_wthreat : t -> int
  val get_bthreat : t -> int
  val remove_piece : t -> t
  val change_position : t -> position -> t
end

module ChessSquare = struct

  type t = {
    pos : position; 
    item : ChessPiece.t option;
    w_threatened : int; 
    b_threatened : int
  }

  let build pos piece_opt w_threat b_threat = { 
    pos = pos; 
    item = piece_opt; 
    w_threatened = w_threat; 
    b_threatened = b_threat;
  }

  let build_with_piece pos name player w_threat b_threat = { 
    pos = pos; 
    item = Some (ChessPiece.build name pos player); 
    w_threatened = w_threat; 
    b_threatened = b_threat;
  }

  let get_position sq = sq.pos

  let get_piece sq = sq.item

  let get_wthreat sq = sq.w_threatened

  let get_bthreat sq = sq.b_threatened

  let remove_piece sq = { 
    pos = get_position sq; 
    item = None; 
    w_threatened = get_wthreat sq; 
    b_threatened = get_bthreat sq;
  }

  let change_position sq pos = { 
    pos = pos; 
    item = begin
      match get_piece sq with 
      | None -> None
      | Some piece -> Some (ChessPiece.move piece pos) 
    end; 
    w_threatened = get_wthreat sq; 
    b_threatened = get_bthreat sq;
  }
end

module type Board = sig
  type t
  type pieces
  val empty : unit -> t
  val init : t
  val copyboard : t -> t
  val init4testing : unit -> t
  val get_square : t -> position -> ChessSquare.t
  val get_piece : t -> position -> ChessPiece.t option
  val piece_list : t -> player -> ChessPiece.t list
  val move : t -> position -> position -> t
  val check : t -> player -> bool
  val checkmate : t -> player -> bool
  val valid_moves : t -> ChessPiece.t -> position list
  val nearby_knight_check : t -> position -> player -> bool 
  val enemies_around_king : t -> position -> player -> int * int -> bool 
  val king_is_safe : t -> player -> position -> position -> bool
  val list_threatened : t -> player -> position list 
  val get_rank : t -> int -> ChessSquare.t array
end

module ChessBoard : Board = struct
  type t = (ChessSquare.t array) array
  type pieces

  (** Initialization of Rank 1 *)
  let rank_1 =
    let initial = Array.make 8 (ChessSquare.build (0,0) None 0 0) in
    Array.set initial 0 (ChessSquare.build_with_piece (1,1) Rook White 0 0);
    Array.set initial 1 (ChessSquare.build_with_piece (2,1) Knight White 1 0);
    Array.set initial 2 (ChessSquare.build_with_piece (3,1) Bishop White 1 0);
    Array.set initial 3 (ChessSquare.build_with_piece (4,1) Queen White 1 0);
    Array.set initial 4 (ChessSquare.build_with_piece (5,1) King White 1 0);
    Array.set initial 5 (ChessSquare.build_with_piece (6,1) Bishop White 1 0);
    Array.set initial 6 (ChessSquare.build_with_piece (7,1) Knight White 1 0);
    Array.set initial 7 (ChessSquare.build_with_piece (8,1) Rook White 0 0);
    Array.copy initial

  (** Initialization of Rank 2 *)
  let rank_2 =
    let initial = Array.make 8 (ChessSquare.build (0,0) None 0 0) in
    Array.set initial 0 (ChessSquare.build_with_piece (1,2) Pawn White 1 0);
    Array.set initial 1 (ChessSquare.build_with_piece (2,2) Pawn White 1 0);
    Array.set initial 2 (ChessSquare.build_with_piece (3,2) Pawn White 1 0);
    Array.set initial 3 (ChessSquare.build_with_piece (4,2) Pawn White 4 0);
    Array.set initial 4 (ChessSquare.build_with_piece (5,2) Pawn White 4 0);
    Array.set initial 5 (ChessSquare.build_with_piece (6,2) Pawn White 1 0);
    Array.set initial 6 (ChessSquare.build_with_piece (7,2) Pawn White 1 0);
    Array.set initial 7 (ChessSquare.build_with_piece (8,2) Pawn White 1 0);
    Array.copy initial


  (** Initialization of Rank 3 *)
  let rank_3 =
    let initial = Array.make 8 (ChessSquare.build (0,0) None 0 0) in
    Array.set initial 0 (ChessSquare.build (1,3) None 2 0);
    Array.set initial 1 (ChessSquare.build (2,3) None 2 0);
    Array.set initial 2 (ChessSquare.build (3,3) None 3 0);
    Array.set initial 3 (ChessSquare.build (4,3) None 2 0);
    Array.set initial 4 (ChessSquare.build (5,3) None 2 0);
    Array.set initial 5 (ChessSquare.build (6,3) None 3 0);
    Array.set initial 6 (ChessSquare.build (7,3) None 2 0);
    Array.set initial 7 (ChessSquare.build (8,3) None 2 0);
    Array.copy initial

  (** Initialization of Rank 4 *)
  let rank_4 =
    let initial = Array.make 8 (ChessSquare.build (0,0) None 0 0) in
    Array.set initial 0 (ChessSquare.build (1,4) None 0 0);
    Array.set initial 1 (ChessSquare.build (2,4) None 0 0);
    Array.set initial 2 (ChessSquare.build (3,4) None 0 0);
    Array.set initial 3 (ChessSquare.build (4,4) None 0 0);
    Array.set initial 4 (ChessSquare.build (5,4) None 0 0);
    Array.set initial 5 (ChessSquare.build (6,4) None 0 0);
    Array.set initial 6 (ChessSquare.build (7,4) None 0 0);
    Array.set initial 7 (ChessSquare.build (8,4) None 0 0);
    Array.copy initial 

  (** Initialization of Rank 5 *)
  let rank_5 =
    let initial = Array.make 8 (ChessSquare.build (0,0) None 0 0) in
    Array.set initial 0 (ChessSquare.build (1,5) None 0 0);
    Array.set initial 1 (ChessSquare.build (2,5) None 0 0);
    Array.set initial 2 (ChessSquare.build (3,5) None 0 0);
    Array.set initial 3 (ChessSquare.build (4,5) None 0 0);
    Array.set initial 4 (ChessSquare.build (5,5) None 0 0);
    Array.set initial 5 (ChessSquare.build (6,5) None 0 0);
    Array.set initial 6 (ChessSquare.build (7,5) None 0 0);
    Array.set initial 7 (ChessSquare.build (8,5) None 0 0);
    Array.copy initial

  (** Initialization of Rank 6 *)
  let rank_6 =
    let initial = Array.make 8 (ChessSquare.build (0,0) None 0 0) in
    Array.set initial 0 (ChessSquare.build (1,6) None 0 2);
    Array.set initial 1 (ChessSquare.build (2,6) None 0 2);
    Array.set initial 2 (ChessSquare.build (3,6) None 0 3);
    Array.set initial 3 (ChessSquare.build (4,6) None 0 2);
    Array.set initial 4 (ChessSquare.build (5,6) None 0 2);
    Array.set initial 5 (ChessSquare.build (6,6) None 0 3);
    Array.set initial 6 (ChessSquare.build (7,6) None 0 2);
    Array.set initial 7 (ChessSquare.build (8,6) None 0 2);
    Array.copy initial

  (** Initialization of Rank 7 *)
  let rank_7 =
    let initial = Array.make 8 (ChessSquare.build (0,0) None 0 0) in
    Array.set initial 0 (ChessSquare.build_with_piece (1,7) Pawn Black 0 1);
    Array.set initial 1 (ChessSquare.build_with_piece (2,7) Pawn Black 0 1);
    Array.set initial 2 (ChessSquare.build_with_piece (3,7) Pawn Black 0 1);
    Array.set initial 3 (ChessSquare.build_with_piece (4,7) Pawn Black 0 4);
    Array.set initial 4 (ChessSquare.build_with_piece (5,7) Pawn Black 0 4);
    Array.set initial 5 (ChessSquare.build_with_piece (6,7) Pawn Black 0 1);
    Array.set initial 6 (ChessSquare.build_with_piece (7,7) Pawn Black 0 1);
    Array.set initial 7 (ChessSquare.build_with_piece (8,7) Pawn Black 0 1);
    Array.copy initial

  (** Initialization of Rank 8 *)
  let rank_8 =
    let initial = Array.make 8 (ChessSquare.build (0,0) None 0 0) in
    Array.set initial 0 (ChessSquare.build_with_piece (1,8) Rook Black 0 0);
    Array.set initial 1 (ChessSquare.build_with_piece (2,8) Knight Black 0 1);
    Array.set initial 2 (ChessSquare.build_with_piece (3,8) Bishop Black 0 1);
    Array.set initial 3 (ChessSquare.build_with_piece (4,8) Queen Black 0 1);
    Array.set initial 4 (ChessSquare.build_with_piece (5,8) King Black 0 1);
    Array.set initial 5 (ChessSquare.build_with_piece (6,8) Bishop Black 0 1);
    Array.set initial 6 (ChessSquare.build_with_piece (7,8) Knight Black 0 1);
    Array.set initial 7 (ChessSquare.build_with_piece (8,8) Rook Black 0 0);
    Array.copy initial

  let empty () = Array.make 8 (Array.make 8 (ChessSquare.build (0,0) None 0 0))

  (* Initializes starting board *)
  let init = 
    let initial = Array.make 8 (Array.make 8 (ChessSquare.build (0,0) None 0 0)) 
    in
    Array.set initial 0 rank_1;
    Array.set initial 1 rank_2;
    Array.set initial 2 rank_3;
    Array.set initial 3 rank_4;
    Array.set initial 4 rank_5;
    Array.set initial 5 rank_6;
    Array.set initial 6 rank_7;
    Array.set initial 7 rank_8;
    Array.copy initial

  let rank1t = Array.copy rank_1
  let rank2t = Array.copy rank_2
  let rank3t = Array.copy rank_3
  let rank4t = Array.copy rank_4
  let rank5t = Array.copy rank_5
  let rank6t = Array.copy rank_6
  let rank7t = Array.copy rank_7
  let rank8t = Array.copy rank_8

  let init4testing () = 
    let initial = Array.make 8 (Array.make 8 (ChessSquare.build (0,0) None 0 0)) 
    in
    Array.set initial 0 rank1t;
    Array.set initial 1 rank2t;
    Array.set initial 2 rank3t;
    Array.set initial 3 rank4t;
    Array.set initial 4 rank5t;
    Array.set initial 5 rank6t;
    Array.set initial 6 rank7t;
    Array.set initial 7 rank8t;
    Array.copy initial

  let copyboard b = 
    let copy = Array.make 8 (Array.make 8 (ChessSquare.build (0,0) None 0 0)) in

    let copyranks =
      Array.set copy 0 (Array.copy (Array.get b 0));
      Array.set copy 1 (Array.copy (Array.get b 1));
      Array.set copy 2 (Array.copy (Array.get b 2));
      Array.set copy 3 (Array.copy (Array.get b 3));
      Array.set copy 4 (Array.copy (Array.get b 4));
      Array.set copy 5 (Array.copy (Array.get b 5));
      Array.set copy 6 (Array.copy (Array.get b 6));
      Array.set copy 7 (Array.copy (Array.get b 7)) in 

    copyranks;
    copy

  let get_square (board : t) (pos : position) =
    let rank = (snd pos)-1 in
    let file = (fst pos)-1 in
    Array.get (Array.get board rank) file

  let get_piece board pos =
    (get_square board pos).item


  let contains_piece board pos player =
    match get_piece board pos with
    | None -> false
    | Some p -> ChessPiece.get_player p = player

  let piece_list board c = 
    let rec piece_list_help board c acc x y =
      match (x, y) with 
      | (8, 8) -> acc
      | (_, 8) -> (
          if contains_piece board (x,y) c
          then (
            let acc' = (Option.get (get_piece board (x,y)))::acc in
            piece_list_help board c acc' (x + 1) 1   
          )
          else piece_list_help board c acc (x + 1) 1
        )
      | (_, _) -> 
        if contains_piece board (x,y) c
        then ( 
          let acc' = ((Option.get(get_piece board (x,y)))::acc) in
          piece_list_help board c acc' x (y + 1)
        )
        else piece_list_help board c acc x (y + 1) 
    in piece_list_help board c [] 1 1

  (** [remove_piece_from_square baord pos square] removes the piece located at 
      [pos] on [square] in [board] *)
  let remove_piece_from_square board pos square =
    let rank = (snd pos)-1 in
    let file = (fst pos)-1 in
    let rank_list = Array.get board rank in
    let empty_square = ChessSquare.remove_piece square in
    Array.set rank_list file empty_square

  (** [change_pos_of_square] changes the position of the [square] in [board]
      to [pos] *)
  let change_pos_of_square board pos square = 
    let rank = (snd pos)-1 in
    let file = (fst pos)-1 in
    let rank_list = Array.get board rank in
    let square' = ChessSquare.change_position square pos in
    Array.set rank_list file square'

  let move board start finish = 
    let sq = get_square board start in 
    remove_piece_from_square board start sq;
    change_pos_of_square board finish sq; 
    copyboard board


  let rec find_king (l : ChessPiece.t list) =
    match l with
    | [] -> failwith("error, your king is missing")
    | h::t -> begin
        match ChessPiece.get_name h with 
        | King -> h
        | _ -> find_king t 
      end 

  let nearby_knight_check board (x,y) player =
    let on_the_board (x,y) = not (0 >= x || x > 8 || y <= 0 || y > 8) in

    let k1 = 
      (if on_the_board (x+1,y+2) then (x+1, y+2) else (x,y)) 
      |> get_piece board in
    let k2 = 
      (if on_the_board (x+2,y+1) then (x+2, y+1) else (x,y)) 
      |> get_piece board in
    let k3 = 
      (if on_the_board (x-1,y+2) then (x-1, y+2) else (x,y)) 
      |> get_piece board in
    let k4 = 
      (if on_the_board (x+1,y-2) then (x+1, y-2) else (x,y)) 
      |> get_piece board in
    let k5 = 
      (if on_the_board (x-2,y+1) then (x-2, y+1) else (x,y)) 
      |> get_piece board in
    let k6 = 
      (if on_the_board (x+2,y-1) then (x+2, y-1) else (x,y)) 
      |> get_piece board in
    let k7 = 
      (if on_the_board (x-1,y-2) then (x-1, y-2) else (x,y)) 
      |> get_piece board in
    let k8 = 
      (if on_the_board (x-2,y-1) then (x-2, y-1) else (x,y)) 
      |> get_piece board in

    let contains_enemy_knight piece = 
      match piece with 
      | None -> false
      | Some piece -> ChessPiece.get_player piece <> player && 
                      ChessPiece.get_name piece = Knight 
    in
    contains_enemy_knight k1 || contains_enemy_knight k2 || 
    contains_enemy_knight k3 || contains_enemy_knight k4
    || contains_enemy_knight k5 || contains_enemy_knight k6
    || contains_enemy_knight k7 || contains_enemy_knight k8


  let rec enemies_around_king board (x,y) player (xdir, ydir) =
    if (0 >= x + xdir || x + xdir > 8 || y + ydir <= 0 || y + ydir > 8) 
    then false
    else if xdir = 0 && ydir = 0 
    then
      enemies_around_king board (x, y) player (1,1) || 
      enemies_around_king board (x, y) player (1,-1) || 
      enemies_around_king board (x, y) player (-1,-1) || 
      enemies_around_king board (x, y) player (-1,1) ||
      enemies_around_king board (x, y) player (0,1) ||
      enemies_around_king board (x, y) player (0,1) ||
      enemies_around_king board (x, y) player (1,0) ||
      enemies_around_king board (x, y) player (-1,0)
    else
      match (x+xdir,y+ydir) |> get_piece board with 
      | None -> 
        enemies_around_king board (x + xdir, y + ydir) player (xdir,ydir) 
      | Some piece -> 
        if ChessPiece.get_player piece <> player 
        then 
          let name = ChessPiece.get_name piece in 
          if xdir = ydir || xdir = -ydir
          then name = Bishop || name = Queen  || name = Pawn || name = King
          else if xdir * ydir = 0 && xdir + ydir <> 0
          then name = Rook || name = Queen || name = King
          else false
        else false

  let king_is_safe board player start finish =
    let board' = 
      if start = finish then board 
      else move (copyboard board) start finish in
    let king_pos = find_king (piece_list board' player) 
                   |> ChessPiece.get_position in
    nearby_knight_check board' king_pos player |> not && 
    enemies_around_king board' king_pos player (0, 0) |> not 

  (**true if square at [finish] has no piece or has a piece of the same color as 
     [p] and false otherwise *)
  let valid_square b c start finish = 
    let x = fst finish in 
    let y = snd finish in

    if (0 >= x || x > 8 || y <= 0 || y > 8) 
    then false
    else if get_piece b finish = None then king_is_safe b c start finish 
    else (get_piece b finish |> Option.get |> ChessPiece.get_player) <> c 
         && king_is_safe b c start finish


  (**a list of positions in a direction specified by f 
     until a position is not valid*)
  let rec iterate f b c pos acc start = 
    let canidate = f pos in
    if (valid_square b c start canidate ) && (get_piece b canidate = None) 
    then iterate f b c canidate (canidate::acc) start
    else if valid_square b c start canidate then  (canidate::acc) else acc

  (**list of valid moves for a knight of color c at position (x,y) *)
  let valid_knight board c x y =

    let potential_moves = [(x + 1, y + 2); (x + 1, y - 2);
                           (x - 1, y + 2); (x - 1, y - 2);
                           (x + 2, y + 1); (x + 2, y - 1);
                           (x - 2, y + 1); (x - 2, y - 1)] in 

    (**filters out potential positions that are out of bounds*)
    let rec add_piece l acc = 
      match l with
      | [] -> acc
      | (file,rank)::t -> if valid_square board c (x,y) (file,rank) 
        then add_piece t ((file,rank)::acc) else add_piece t acc 
    in add_piece potential_moves [] 

  let valid_pawn board c x y = 
    (*if c = White && valid_square board c (x,y+1) then [(x,y+1)] 
      else if c = Black && valid_square board c (x,y-1) then [(x,y-1)]
      else []*)
    let lst = [] in

    if c = White then 
      let move_up1 = valid_square board c (x,y) (x,y+1) 
                     && get_piece board (x,y+1) = None in
      let lst' = 
        if move_up1 then ((x,y+1)::lst) else lst in
      let lst'' = 
        if y = 2 && move_up1 && valid_square board c (x,y) (x,y+2) 
           && get_piece board (x,y+2) = None
        then ((x,y+2)::lst') else lst' in
      let lst''' = 
        if valid_square board c (x,y) (x+1,y+1) 
        && get_piece board (x+1,y+1) <> None
        then ((x+1,y+1)::lst'') else lst'' in 
      let lst'''' = 
        if valid_square board c (x,y) (x-1,y+1) 
        && get_piece board (x-1,y+1) <> None 
        then ((x-1,y+1)::lst''') else lst''' in
      lst''''

    else (*if c is black*)
      let move_down1 = valid_square board c (x,y) (x,y-1)
                       && get_piece board (x,y-1) = None in
      let lst' = 
        if move_down1 then ((x,y-1)::lst) else lst in
      let lst'' = 
        if y = 7 && move_down1 && valid_square board c (x,y) (x,y-2)
           && get_piece board (x,y-2) = None
        then ((x,y-2)::lst') else lst' in
      let lst''' = 
        if valid_square board c (x,y) (x+1,y-1) 
        && get_piece board (x+1,y-1) <> None
        then ((x+1,y-1)::lst'') else lst'' in 
      let lst'''' = 
        if valid_square board c (x,y) (x-1,y-1) 
        && get_piece board (x-1,y-1) <> None
        then ((x-1,y-1)::lst''') else lst''' in
      lst''''

  let valid_king board c x y = 
    let lst = [(x+1,y+1); (x+1,y); (x+1,y-1); (x,y+1); (x,y-1); (x-1,y+1);
               (x-1,y); (x-1,y-1)] in 
    let rec king_help l acc = 
      match l with 
      | [] -> acc
      | (file,rank)::t -> if 
        valid_square board c (x,y) (file,rank) 
        then king_help t ((file,rank)::acc) else king_help t acc in 
    king_help lst []

  let valid_bishop board c x y = 
    let nw = iterate (fun (file,rank)-> (file-1,rank+1)) board c (x,y) [] (x,y) 
    in 
    let ne = iterate (fun (file,rank)-> (file+1,rank+1)) board c (x,y) [] (x,y) 
    in
    let sw = iterate (fun (file,rank)-> (file-1,rank-1)) board c (x,y) [] (x,y)
    in
    let se = iterate (fun (file,rank)-> (file+1,rank-1)) board c (x,y) [] (x,y)
    in nw @ ne @ sw @ se

  let valid_rook board c x y =
    let left = iterate (fun (file,rank)-> (file+1,rank)) board c (x,y) [] (x,y)
    in 
    let right = iterate (fun (file,rank)-> (file-1,rank)) board c (x,y) [] (x,y)
    in
    let up = iterate (fun (file,rank)-> (file,rank+1)) board c (x,y) [] (x,y)
    in
    let down = iterate (fun (file,rank)-> (file,rank-1)) board c (x,y) [] (x,y)
    in left @ right @ up @ down

  let valid_moves b (p : ChessPiece.t) =
    let c = ChessPiece.get_player p in
    let piecetype = ChessPiece.get_name p in
    let x = p |> ChessPiece.get_position |> fst in
    let y = p |> ChessPiece.get_position |> snd in

    match piecetype with
    | Pawn -> valid_pawn b c x y
    | Knight -> (valid_knight b c x y)
    | Rook -> valid_rook b c x y
    | Bishop -> valid_bishop b c x y
    | Queen -> (valid_rook b c x y) @ (valid_bishop b c x y)
    | King -> (valid_king b c x y)

  let pos_compare (p1 : position) (p2 : position) = 
    if fst p1 = fst p2 && snd p1 = snd p2 then 0 else -1 

  (**all squares threatened by player*)
  let list_threatened board player = 
    let piecelist = piece_list board player in 
    let rec lt_help plist acc = 
      match plist with 
      | [] -> acc
      | h::t -> lt_help t ((valid_moves board h) @ acc) in 
    List.sort_uniq pos_compare (lt_help piecelist [])

  let king_threathened king board player = 
    let king_pos = ChessPiece.get_position king in
    nearby_knight_check board king_pos player ||
    enemies_around_king board king_pos player (0, 0) 

  let check board player =
    king_is_safe board player (0,0) (0,0) |> not

  let checkmate board player = 
    check board player && (list_threatened board player = [])

  let get_rank board rank =
    Array.get board rank
end
