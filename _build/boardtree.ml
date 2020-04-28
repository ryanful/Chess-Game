open State
open Game

module type BoardTree = sig
  type move = position * position
  type minimax = int
  type state = State.GameState.t * move * minimax
  type t = Nodes of state * (t list) * t option

  val init_tree : GameState.t -> t
  val minimax : State.GameState.t -> int
  val add_depth : t -> t
  val next_move : State.GameState.t -> move
end

module Findmove : BoardTree = struct
  type move = position * position
  type minimax = int
  type state = State.GameState.t * move * minimax
  type t = Nodes of state * (t list) * t option

  (**[tup_string t] is a string representation of a tuple of ints*)
  let tup_string t = 
    "(" ^ string_of_int (fst t) ^ ", " ^ string_of_int (snd t) ^ ")"

  (**[tree_string tr] is a string representation of the game tree datatype*)
  let tree_string tr = 
    match tr with 
    | Nodes((gs,mo,mm), children, parent) -> 
      GameState.visualize gs ^
      "/n" ^ tup_string (fst mo) ^ tup_string (snd mo) ^
      "/n" ^ string_of_int mm ^
      "/n" ^ string_of_int (List.length children)

  let init_tree st = Nodes ((st, ((0, 0), (0, 0)), 0), [], None)

  let minimax st = 
    let plist = (GameState.remaining st White) @ (GameState.remaining st Black) 
    in 

    let rec mm_help lst acc c =
      match lst with
      | [] -> acc
      | h::t -> match ChessPiece.get_name h with 
        | King -> if ChessPiece.get_player h = GameState.curr_turn st 
          then mm_help t (acc - 900) c else mm_help t (acc + 900) c
        | Queen -> if ChessPiece.get_player h = GameState.curr_turn st 
          then mm_help t (acc - 90) c else mm_help t (acc + 90) c
        | Bishop -> if ChessPiece.get_player h = GameState.curr_turn st 
          then mm_help t (acc - 30) c else mm_help t (acc + 30) c
        | Rook -> if ChessPiece.get_player h = GameState.curr_turn st 
          then mm_help t (acc - 50) c else mm_help t (acc + 50) c
        | Pawn -> if ChessPiece.get_player h = GameState.curr_turn st 
          then mm_help t (acc - 10) c else mm_help t (acc + 10) c
        | Knight -> if ChessPiece.get_player h = GameState.curr_turn st 
          then mm_help t (acc - 30) c else mm_help t (acc + 30) c
    in 

    mm_help plist 0 (GameState.curr_turn st)

  (**valid moves of all pieces of a given [color] *)
  let all_moves color st = 
    let board = GameState.curr_board st in
    let piece_lst = (GameState.remaining st color) in 

    let rec find_single 
        (pos : position) 
        (validmoves : position list) 
        (acc : move list) =
      match validmoves with 
      | [] -> acc
      | h::t -> find_single pos t ((pos, h)::acc) in 

    let rec findall plist (acc : move list) =
      match plist with 
      | [] -> acc
      | h::t -> findall t (acc @ find_single (ChessPiece.get_position h) 
                             (ChessBoard.valid_moves board h) []) in 

    findall piece_lst []



  let add_depth (n : t) = 
    match n with 
    | Nodes((s,m,mm), nil, parent) -> let st = s in

      let color = GameState.curr_turn st in 
      let movelist = all_moves color st in 

      let rec create_node_list lst (acc : t list) = 
        let currmove = try List.hd lst with Failure (hd) -> ((0,0),(0,0)) in
        let stcopy = GameState.state_from_board st 
            (ChessBoard.copyboard (GameState.curr_board st)) in
        let st' = try GameState.move stcopy (fst currmove) (snd currmove) with
            Invalid_argument (i) -> st; in 

        match lst with
        | [] -> acc
        | h::t -> create_node_list t 
                    ((Nodes ((st', currmove, minimax st'), [], Some n))::acc) in 

      let nodelist = create_node_list movelist [] in 

      match n with 
      | Nodes(s, nil, p) -> Nodes(s, nodelist, p)




  let next_move st = 
    let tr = st |> init_tree |> add_depth in


    (**finds the maximum possible minimax value *)
    let rec find_max (nodelst : t list) maxmm = 
      match nodelst with 
      | [] -> maxmm
      | Nodes((st,mo,mm),lst,parent)::t ->  if mm > maxmm then find_max t mm 
        else find_max t maxmm in 

    (**finds the list of moves that will achieve maximum minimax value*)
    let rec max_moves_lst max nodelst acc = 
      match nodelst with 
      | [] -> acc
      | Nodes((st,mo,mm),lst,parent)::t -> if mm = max 
        then max_moves_lst max t (mo::acc) else max_moves_lst max t acc in 

    (**select a random move from the list of best possible moves*)
    let rand_move maxmoves =
      Random.self_init ();
      let nd = List.map (fun c -> (Random.int 1000, c)) maxmoves in
      let sond = List.sort compare nd in
      let shuffled_list = List.map snd sond in
      List.hd shuffled_list in 

    let lst = match tr with | Nodes(s, children, p) -> children in 

    let max = find_max lst (-99999) in 
    let bestmoves = max_moves_lst max lst [] in 
    rand_move bestmoves





end

