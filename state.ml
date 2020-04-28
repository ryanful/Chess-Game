open Game

module type State = sig
  type t
  type piece = ChessPiece.t
  type player = Game.player
  type board = ChessBoard.t
  val init_state : t
  val state_from_board : t -> board -> t
  val state_copy : t -> t
  val visualize : t -> string
  val captured : t -> player -> piece list
  val score : t -> player -> int
  val remaining : t -> player -> piece list
  val curr_turn : t -> player
  val curr_board : t -> board
  val change_turn : t -> t
  val check_win : t -> player -> bool
  val move : t -> position  -> position -> t
  val move_options : t -> position -> position list
  val is_players_piece : t -> position -> bool
  val compare : t -> t -> int
  val minimax : t -> int
end

module GameState : State = struct
  type piece = ChessPiece.t
  type player = Game.player
  type board = ChessBoard.t

  type t = {
    current_turn : player;
    board_state : board;
    white_captured : piece list;
    black_captured : piece list;
  }

  let init_state  = {
    current_turn = White;
    board_state = ChessBoard.init;
    white_captured = [];
    black_captured = []
  }

  let state_from_board st board = {
    current_turn = st.current_turn;
    board_state = board;
    white_captured = st.white_captured;
    black_captured = st.black_captured
  }

  let state_copy st = {
    current_turn = st.current_turn;
    board_state = ChessBoard.copyboard st.board_state;
    white_captured = st.white_captured;
    black_captured = st.black_captured
  }

  (** [piece_info piece] returns the string representation of [piece] *)
  let piece_info (piece : piece) =
    match ChessPiece.get_name piece with
    | Rook -> if ChessPiece.get_player piece = White then "[wR]" else "[bR]"
    | Knight -> if ChessPiece.get_player piece = White then "[wN]" else "[bN]"
    | Bishop -> if ChessPiece.get_player piece = White then "[wB]" else "[bB]"
    | King -> if ChessPiece.get_player piece = White then "[wK]" else "[bK]"
    | Queen -> if ChessPiece.get_player piece = White then "[wQ]" else "[bQ]"
    | Pawn -> if ChessPiece.get_player piece = White then "[wP]" else "[bP]"

  (** [square_to_string] returns the string representation of [square] *)
  let square_to_string (square : ChessSquare.t) =
    match ChessSquare.get_piece square with
    | None -> "[  ]"
    | Some a -> piece_info a

  (** [rank_to_string rank] returns the string representation of [rank] *)
  let rank_to_string (rank : ChessSquare.t array) =
    let string_array = Array.map square_to_string rank in
    Array.fold_right (^) string_array ""

  (** [rank_to_list rank] folds [rank] from an array into a list *)
  let rank_to_list (rank : ChessSquare.t array) =
    Array.fold_right List.cons rank []

  (** [board_to_list] creates a two-dimensional list constructed by appending
      the folded ranks *)
  let board_to_list (board : board) =
    rank_to_list (ChessBoard.get_rank board 0)@
    rank_to_list (ChessBoard.get_rank board 1)@
    rank_to_list (ChessBoard.get_rank board 2)@
    rank_to_list (ChessBoard.get_rank board 3)@
    rank_to_list (ChessBoard.get_rank board 4)@
    rank_to_list (ChessBoard.get_rank board 5)@
    rank_to_list (ChessBoard.get_rank board 6)@
    rank_to_list (ChessBoard.get_rank board 7)

  let visualize st = 
    "8 " ^ rank_to_string (ChessBoard.get_rank st.board_state 7)^"\n"^ 
    "7 " ^ rank_to_string (ChessBoard.get_rank st.board_state 6)^"\n"^
    "6 " ^ rank_to_string (ChessBoard.get_rank st.board_state 5)^"\n"^
    "5 " ^ rank_to_string (ChessBoard.get_rank st.board_state 4)^"\n"^
    "4 " ^ rank_to_string (ChessBoard.get_rank st.board_state 3)^"\n"^
    "3 " ^rank_to_string (ChessBoard.get_rank st.board_state 2)^"\n"^
    "2 " ^rank_to_string (ChessBoard.get_rank st.board_state 1)^"\n"^
    "1 " ^ rank_to_string (ChessBoard.get_rank st.board_state 0)^"\n" ^
    "   A   B   C   D   E   F   G   H    "

  let captured st player =
    match player with
    | White -> st.white_captured
    | Black -> st.black_captured

  (** [score_from_piece item] returns the corresponding score value based on
      which piece [item] is *)
  let score_from_piece (item : piece) =
    match ChessPiece.get_name item with
    | Rook -> 5
    | Knight -> 3
    | Bishop -> 3
    | King -> 0
    | Queen -> 9
    | Pawn -> 1

  (** [score_helper acc] returns the sum of the scores of the pieces passed in
      added to [acc] *)
  let rec score_helper acc = function
    | [] -> acc
    | h :: t -> t |> score_helper (acc + (score_from_piece h))

  let rec all_pieces acc = function
    | [] -> acc
    | h :: t -> begin
        match ChessSquare.get_piece h with
        | None -> t |> all_pieces acc
        | Some j -> t |> all_pieces (j::acc)
      end

  let rec remaining_help acc player = function
    | [] -> acc
    | h :: t -> if ChessPiece.get_player h = player then 
        t |> remaining_help (h::acc) player else 
        t |> remaining_help acc player

  let remaining st player = 
    remaining_help [] player 
      (all_pieces [] (board_to_list st.board_state))

  let score st player =
    let temp =
      match player with
      |White -> remaining st Black |> score_helper 0
      |Black -> remaining st White |> score_helper 0
    in
    39 - temp

  let curr_turn st = st.current_turn

  let curr_board st = st.board_state

  let change_turn st = {
    current_turn = if st.current_turn = White then Black else White;
    board_state = st.board_state;
    white_captured = st.white_captured;
    black_captured = st.black_captured
  }

  let check_win st color = 
    let board = curr_board st in
    match color with
    | White -> ChessBoard.checkmate board Black
    | Black -> ChessBoard.checkmate board White


  (** [capture_piece st player piece] is [captured st player] if it is player
      [player]'s turn or piece [piece] is None. Otherwise it is
      [captured st player] with the piece associated with [piece] added to it.*)
  let capture_piece st player piece =
    if curr_turn st = player then captured st player
    else
      match piece with 
      | None -> captured st player
      | Some p -> p :: (captured st player)

  let capture_help st player =
    let on_board = remaining st player in
    let total =
      match player with
      | White -> remaining (init_state) White
      | Black -> remaining (init_state) Black
    in
    let rec helper acc (brd:piece list) = function
      | [] -> acc
      | h :: t -> if (List.mem h brd) then
          t |> helper acc brd else t |> helper (h::acc) brd
    in
    helper [] on_board total

  let move st start finish = 
    let current_board = (curr_board st) in 
    let new_capture = ChessBoard.get_square current_board finish 
                      |> ChessSquare.get_piece in
    let st' = {
      current_turn = st |> curr_turn;
      board_state = ChessBoard.move current_board start finish;
      white_captured = capture_piece st White new_capture;
      black_captured = capture_piece st Black new_capture; 
    } in 
    let st_final = {
      current_turn = st'.current_turn;
      board_state = st'.board_state;
      white_captured = capture_help st' White;
      black_captured = capture_help st' Black
    } in
    change_turn st_final

  let move_options st pos =
    let board = curr_board st in
    let piece = ChessBoard.get_piece board pos in
    match piece with
    | None -> []
    | Some p -> ChessBoard.valid_moves board p

  let is_players_piece st pos = 
    let curr_player = curr_turn st in 
    let piece = ChessBoard.get_piece (curr_board st) pos in
    ChessPiece.is_players_piece curr_player piece 

  let minimax st = 
    let plist = (remaining st White) @ (remaining st Black) in 

    let rec mm_help lst acc =
      match lst with
      | [] -> acc
      | h::t -> match ChessPiece.get_name h with 
        | King -> if ChessPiece.get_player h = curr_turn st then
            mm_help t (acc + 900) else mm_help t (acc - 900)
        | Queen -> if ChessPiece.get_player h = curr_turn st then
            mm_help t (acc + 90) else mm_help t (acc - 90)
        | Bishop -> if ChessPiece.get_player h = curr_turn st then
            mm_help t (acc + 30) else mm_help t (acc - 30)
        | Rook -> if ChessPiece.get_player h = curr_turn st then
            mm_help t (acc + 50) else mm_help t (acc - 50)
        | Pawn -> if ChessPiece.get_player h = curr_turn st then
            mm_help t (acc + 10) else mm_help t (acc - 10)
        | Knight -> if ChessPiece.get_player h = curr_turn st then
            mm_help t (acc + 30) else mm_help t (acc - 30)
    in 

    mm_help plist 0

  let compare st st' =
    let int1 = minimax st in
    let int2 = minimax st' in
    if int1 < int2 then -1
    else if int1 > int2 then 1
    else 0

end