open State
open Game
open Command
open Boardtree.Findmove

module type Brian = sig
  type piece = ChessPiece.t
  type player = Game.player
  type board = ChessBoard.t
  type t
  type possibility = ((Game.position*Game.position)*t)

  val init_state_black : t
  val init_state_white : t
  val get_state : t -> State.GameState.t
  val moves : player -> State.GameState.t -> (Game.position*Game.position) list
  val run : player -> int -> unit
end

module BrianState : Brian = struct
  type piece = ChessPiece.t
  type player = Game.player
  type board = ChessBoard.t

  type t = {
    color : player;
    state : GameState.t;
    moves : (position*position) list;
    difficulty : int
  }

  type possibility = ((Game.position*Game.position)*t)

  (** [single_piece piece st] returns the list of [position] pairs that can be
      selected and moved with [piece] in [st] *)
  let single_piece piece (st : GameState.t) =
    let head = (ChessPiece.get_position piece) in
    let lst = ChessBoard.valid_moves (GameState.curr_board st) piece in
    let rec iter head acc lst =
      match lst with
      | [] -> acc
      | h :: t -> t |> iter head ((head,h)::acc) in
    iter head [] lst

  (** [moves_help st accu pieces] folds the list of pieces using [single_piece]
      as a helper *)
  let rec moves_help (st : GameState.t) accu pieces =
    match pieces with
    | [] -> accu
    | h :: t -> t |> moves_help st ((single_piece h st)::accu)

  let moves (color : player) st =
    let pieces = GameState.remaining st color in
    List.flatten (moves_help st [] pieces)

  (** [update br] updates the list of valid moves based on the state in brian *)
  let update br = {
    color = br.color;
    state = br.state;
    moves = moves br.color br.state;
    difficulty = br.difficulty
  }

  (** Wrapper for GameState.move *)
  let move_help st pair =
    GameState.move st (fst pair) (snd pair)

  let get_state br =
    br.state

  (** Returns a new object of type br with new state after moving with
      respect to [pair] *)
  let move br pair = {
    color = br.color;
    state = move_help br.state pair;
    moves = br.moves;
    difficulty = br.difficulty
  }

  let init_state_black = {
    color = Black;
    state = GameState.init_state;
    moves = [];
    difficulty = 0
  }

  let init_state_white = {
    color = White;
    state = GameState.init_state;
    moves = moves White GameState.init_state;
    difficulty = 0
  }

  (** Returns a string visualisation of teh positions in [lst] *)
  let rec pos_to_rankfile lst acc = 
    match lst with
    | [] -> acc
    | (file, rank)::t ->
      let rank' = Char.chr(rank + 48) |> String.make 1  in 
      let file' = Char.chr(file + 96) |> String.make 1 in 
      let acc' = (file' ^ rank')::acc in pos_to_rankfile t acc'

  (** [print_list] prints the list inputted to it*)
  let rec print_list = function 
    | [] -> print_endline "\n"
    | h::t -> print_endline h; print_list t

  (** [constructor color] returns a state of Brian initialized to [color] *)
  let constructor color =
    match color with
    | White -> init_state_white
    | Black -> init_state_black

  (** [adjust difficulty br] sets the difficulty value in [br] to [difficulty]*)
  let adjust difficulty br = {
    color = br.color;
    state = br.state;
    moves = br.moves;
    difficulty = difficulty
  }

  (** [score item] returns the score value of [item] *)
  let score (item : piece) =
    match ChessPiece.get_name item with
    | Rook -> 525
    | Knight -> 350
    | Bishop -> 350
    | King -> 10000
    | Queen -> 1000
    | Pawn -> 100

  (** [compare_help obj obj'] compares the GameStates hidden in [obj] and [obj']
      with [GameState.compare]*) 
  let compare_help (obj:possibility) (obj':possibility) =
    GameState.compare (snd obj).state (snd obj').state

  (** [init_new st br] initializes a new state of brian with the color, moves,
      and difficulty of [br] but with the gamestate of [st] *)
  let init_new st br = {
    color = br.color;
    state = st;
    moves = moves br.color st;
    difficulty = br.difficulty
  }

  (** [pick_move moves] picks a random element from [moves] *)
  let rand_move br =
    Random.self_init ();
    let nd = List.map (fun c -> (Random.int 1000, c)) br.moves in
    let sond = List.sort compare nd in
    let shuffled_list = List.map snd sond in
    List.hd shuffled_list

  (** [capture br] returns the move that Brian can make that maximizes
      the board's minimax value *)
  let capture br =
    let st = br.state in
    Boardtree.Findmove.next_move st

  (** [pick_move br] calls a helper function to return a position based on
      [br]'s difficulty value *)
  let pick_move br =
    if br.difficulty = 1 then rand_move br
    else if br.difficulty = 2 then capture br
    else capture br

  (** [run_help color br] makes Brian's move if it's his turn else it prompts
      the human player to select a piece for moving *)
  let rec run_help color br =
    if (GameState.curr_turn br.state) = color then
      let br' = update br in
      (move br' (pick_move br')) |> run_help color else 
      br.state |> GameState.visualize |> print_string;
    let curr_player = GameState.curr_turn br.state in
    let prompt = "\n\n"^(ChessPiece.player_to_string curr_player) ^
                 " -- Select a piece to move with the 'select' command! \n> " in
    print_string prompt;
    match read_line () |> parse with
    | Select pos -> if (GameState.is_players_piece (get_state br) pos) then 
        select_run_help color br pos else run_help color br
    | Unselect pos
    | Move pos -> run_help color br
    | Score -> print_string "\nYour score is: "; 
      print_int 
        (GameState.score (get_state br) (GameState.curr_turn (get_state br))); 
      print_string "\n"; run_help color br
    | Draw -> ANSITerminal.(print_string [cyan] "Drew!\n"); exit 0
    | Quit -> ANSITerminal.(print_string [cyan] "Game Over!\n"); exit 0
    | exception Empty -> print_string "Please type a command\n"; 
      run_help color br
    | exception Malformed -> 
      let illegal_message = "\nIllegal Command: please try again \n" in 
      ANSITerminal.(print_string [yellow] illegal_message); run_help color br

  (** [select_run_help color br pos] prompt the player to select a position for
      moving after the player has selected [pos] *)
  and select_run_help color br pos =
    print_string
      "\n\nChoose a location from this list with the 'move' command. \n ";
    let val_moves = GameState.move_options br.state pos in
    pos_to_rankfile val_moves [] |> print_list;
    print_string "\n> ";
    match read_line () |> parse with
    | Select pos' -> if (GameState.is_players_piece br.state pos') then 
        select_run_help color br pos'
      else run_help color br
    | Unselect pos' ->
      if pos = pos' then run_help color br
      else print_endline "This position wasn't selected!"; 
      select_run_help color br pos
    | Move pos' -> 
      if not (GameState.is_players_piece br.state pos') && 
         List.mem pos' val_moves 
      then move br (pos,pos') |> run_help color
      else print_endline "Invalid Move -- Choose again"; 
      select_run_help color br pos
    | Score -> print_string "\nYour score is: "; 
      print_int 
        (GameState.score (get_state br) (GameState.curr_turn (get_state br))); 
      print_string "\n"; select_run_help color br pos
    | Draw -> ANSITerminal.(print_string [cyan] "Drew!\n"); exit 0
    | Quit -> ANSITerminal.(print_string [cyan] "Game Over!\n"); exit 0
    | exception Empty -> print_string "Please type a command\n"; 
      run_help color br
    | exception Malformed ->
      let illegal_message = "\nIllegal Command: please try again \n" in 
      ANSITerminal.(print_string [yellow] illegal_message); run_help color br

  (** [bad_time] prints a string and then exits the game *)
  let bad_time () =
    ANSITerminal.(print_string [red] 
                    "\nYour efforts are futile.\nYou are already dead.\n"); 
    exit 0

  (** [run color] runs Brian as player [color] *)
  let run (color : player) (difficulty : int) =
    let initial = color |> constructor |> adjust difficulty in
    run_help color initial
end