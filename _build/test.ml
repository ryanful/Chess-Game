open OUnit2
open Command
open State
open Game

(**  TEST PLAN

     OUnit test cases were created with Glass-Box testing in mind. We looked at
     the logic of our code and designed test cases to ensure that they functioned
     as designed

     Command Testing:
     Parsing commands mainly automated in this file with the CommandTests module 
     and play tested.

     Game Testing:
     Few tests for importants functions like moving pieces on a chess board,
      checking to see if the king is threatening by the opposing team, and 
      our getters to make sure that our Chess piece and chess square 
      abstraction worked properly.

     State Testing:
     A lot of playtesting was done using the [visualize] function in state
     which allowed us to see for ourselves that all of its functions were
     working as intended

     Brian & BoardTree Testing:
     Brian & BoardTree were also tested through playtesting to ensure that
     they worked as intended. We are satisfied with the experience of playing
     against Brian

     Our testing approach demonstrates the correctness of the system as early in
     the testing process we ensured that our preliminary methods functioned
     properly through OUnit testing. Once we had a game environment that we
     could interact with, we rigorously playtested scenarios such as check-mates
     in order to ensure that the users would have a fun chess experience.

*)

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists.  That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates.  Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  &&
  List.length lst2 = List.length uniq2
  &&
  uniq1 = uniq2

let cmp_piece p1 p2 =
  ChessPiece.(get_name p1 = get_name p2 && get_player p1 = get_player p2 && 
              get_position p1 = get_position p2)

let rec cmp_piece_list plist1 plist2 = 
  match plist1, plist2 with
  | [], [] -> true
  | p1::t, p2::t2 ->
    if cmp_piece p1 p2 then cmp_piece_list t t2 else false
  | _, _ -> false


(** 
   let pp_position pos = 
   let left = fst pos |> string_of_int in
   let right = snd pos |> string_of_int in
   "(" ^ left ^ ", " ^ right ^ ")"*)

let pp_piece_opt = function
  | None -> "no piece"
  | Some p -> ChessPiece.to_string p

let pp_square sq = 
  let pos = ChessSquare.get_position sq |> position_to_string in 
  let piece = ChessSquare.get_piece sq |> pp_piece_opt in
  "Square at " ^ pos ^ ": " ^ piece

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1::(h2::t as t') ->
        if n=100 then acc ^ "..."  (* stop printing long list *)
        else loop (n+1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"

module GameTests = struct

  let board = ChessBoard.init

  let get_piece_test name 
      (pos: position) 
      (board : ChessBoard.t) 
      (expected_output : ChessPiece.t option) =
    name >:: (fun _ -> 
        assert_equal ~printer:pp_piece_opt expected_output 
          (ChessBoard.get_piece board pos))

  let get_square_test 
      name
      (pos: position) 
      (board : ChessBoard.t)
      (expected_output : ChessSquare.t)
    = name >:: (fun _ -> 
        assert_equal ~printer:pp_square expected_output 
          (ChessBoard.get_square board pos))

  let move_test 
      name 
      (start : position) 
      (ending : position) 
      (board : ChessBoard.t) 
      (output_board : ChessBoard.t) = 
    name >:: (fun _ -> 
        assert_equal ~printer:pp_piece_opt 
          (ChessBoard.get_piece output_board ending)
          ((ChessBoard.move board start ending 
            |> ChessBoard.get_piece) ending))

  let valid_moves_test name board piece expected_output = 
    name >:: (fun _ -> assert_equal ~cmp:cmp_set_like_lists 
                 ~printer:(pp_list position_to_string) expected_output 
                 (ChessBoard.valid_moves board piece)
             )

  let check_test 
      name
      (board : ChessBoard.t) 
      (player : player) 
      (expected_output : bool) = name >:: (fun _ -> 
      assert_equal expected_output (ChessBoard.check board player) )

  (*eqality for positions*)
  let pos_compare (p1 : position) (p2 : position) = 
    if fst p1 = fst p2 && snd p1 = snd p2 then 0 else -1

  (**prints out a list of pieces*)
  let piece_list_tostring (plist : ChessPiece.t list) =
    let slist = List.map (ChessPiece.to_string) plist in
    let rec plist_help l acc = 
      match l with 
      | [] -> acc
      | h::t -> plist_help t (h ^ " || " ^ acc) in 

    plist_help slist ""


  let pos_list_tostring (plist : position list) =
    let slist = List.map (position_to_string) plist in
    let rec plist_help l acc = 
      match l with 
      | [] -> acc
      | h::t -> plist_help t (h ^ " || " ^ acc) in 

    plist_help slist ""


  let piece_list_test 
      name
      (board : ChessBoard.t)
      (player : player) 
      (expected_output : position list) = 
    name >:: (fun _ -> assert_equal ~cmp:cmp_set_like_lists 
                 ~printer:(pp_list Game.position_to_string)
                 expected_output 
                 ((*print_endline ((ChessBoard.piece_list board player) 
                    |> piece_list_tostring));*)
                   (List.sort_uniq (pos_compare) 
                      (List.map (ChessPiece.get_position) 
                         (ChessBoard.piece_list board player)))))

  let list_threatened_test 
      name
      (board : ChessBoard.t)
      (player : player) 
      (expected_output : position list) = name >:: (fun _ -> 
      assert_equal expected_output 
        (print_endline(pos_list_tostring 
                         (ChessBoard.list_threatened board player));
         ChessBoard.list_threatened board player))

  let nearbyknight_test name board pos player expected_output =
    name >:: (fun _ -> 
        assert_equal expected_output 
          (ChessBoard.nearby_knight_check board pos player) )

  let enemies_test name board pos player dir expected_output =
    name >:: (fun _ -> 
        assert_equal expected_output ~printer:string_of_bool
          (ChessBoard.enemies_around_king board pos player dir) )

  let kingsafe_test name board player start finish expected_output =
    name >:: (fun _ -> 
        assert_equal expected_output ~printer:string_of_bool
          (ChessBoard.king_is_safe board player start finish) )

  let initial = ChessBoard.init
  let init_knight = ChessPiece.build Knight (2,1) White
  let knight_moves = [(1,3);(3,3)]
  let pawn_moves = [(4,3);(4,4)]
  let init_pawn42 = ChessPiece.build Pawn (4,2) White
  let init_square42 = ChessSquare.build (4,2) (Some init_pawn42) 4 0

  let empty_b2 = ChessSquare.build (2,2) None 1 0
  let new_b3 = ChessSquare.build_with_piece (2,3) Pawn White 0 0
  let board0 = ChessBoard.init
  let rank2 = ChessBoard.get_rank board0 2
  let ()= Array.set (rank2) 2 empty_b2
  let rank3 = ChessBoard.get_rank board0 3
  let () = Array.set (rank3) 3 new_b3

  let pawnb3 = ChessPiece.build Pawn (2,3) White

  let moveb2b3 = ChessBoard.move ChessBoard.init (2,2) (2,3)
  let movee2e3 = ChessBoard.move ChessBoard.init (5,2) (5,3) 

  let initialboard = ChessBoard.init4testing ()
  let moveknight = ChessBoard.move initialboard (2,1) (1,3)
  let board4check = ChessBoard.copyboard initialboard
  let board4check' = ChessBoard.move board4check (2,8) (3,2)

  let game_tests = [

    piece_list_test "pieces of init board white" (ChessBoard.init4testing ()) 
      White 
      [(1,1); (2,1); (3,1); (4,1); (5,1); (6,1); (7,1); (8,1); 
       (1,2); (2,2); (3,2); (4,2); (5,2); (6,2); (7,2); (8,2)];

    (*piece_list_test "pieces of init board black" ChessBoard.init Black 
      (List.sort_uniq (pos_compare) 
      ([(1,8); (2,8); (3,8); (4,8); (5,8); (6,8); (7,8); (8,8); 
      (1,7); (2,7); (3,7); (4,7); (5,7); (6,7); (7,7); (8,7)]));*)

    (*list_threatened_test "test" initial White [];*)

    get_square_test "Getting Square D2 on init" (4,2) initial init_square42;

    get_piece_test "Get None for valid knight move on init" (1,3) initial None;
    get_piece_test "Get None for valid knight move on init" (3,3) initial None;
    get_piece_test "Get initial pawn at D2" (4,2) initial (Some init_pawn42);
    get_piece_test "Get piece at B3" (5,3) (ChessBoard.init4testing ()) (None);


    get_piece_test "Get b2 after pawn move to b3" (2,2) moveb2b3 None;
    get_piece_test "Get b3 after pawn move from b2" (2,3) moveb2b3 
      (Some pawnb3);
    get_piece_test "piece after wp from b2 to b3" (2,2) board0 None;

    valid_moves_test "Valid moves: left wK on init" 
      initial init_knight knight_moves;
    valid_moves_test "valid moves: leftmost pawn on init"
      initialboard init_pawn42 pawn_moves;

    move_test "Move: wP from b2 to b3" (2,2) (2,3) initial board0;
    move_test "Move: white knight from b1 to a3" (2,1) (1,3) 
      initialboard board0;

    enemies_test "white king enemies on init" initialboard (5,1) White 
      (0,0) false;
    enemies_test "white black enemies on init" initialboard (5,8) Black 
      (0,0) false;

    nearbyknight_test "nearby -- white king no nearby knight" initialboard (5,1) 
      White false;
    nearbyknight_test "nearby  -- white king in check by knight" board4check' 
      (5,1) White true;

    kingsafe_test "white king no nearby knight" 
      initialboard White (1,2) (1,3) true;

    kingsafe_test "white king in check by knight" 
      board4check' White (1,2) (1,3) false;
  ]




end

module CommandTests = struct

  let parse_test name input expected_output =
    name >:: (fun _ -> assert_equal expected_output (parse input))

  let parse_exc_test name input expected_exception =
    name >:: (fun _ -> assert_raises expected_exception (fun _ -> parse input))

  let command_tests = [
    (** Select tests *)
    parse_test "ParseTest: select" "select f7" (Select (6,7));
    parse_test "ParseTest: select" "select f7akfjdl" (Select (6,7));
    parse_test "ParseTest: valid select with whitespace" " select D8 " 
      (Select (4,8));
    parse_exc_test "ParseTest: Select with empty position" "select " Malformed;
    (** Unselect tests *)
    parse_test "ParseTest: unselect with position" "unselect a2" 
      (Unselect (1,2));
    parse_test "ParseTest: unselect with whitespace" " unselect d3" 
      (Unselect (4,3));
    parse_exc_test "ParseTest: unselect no position" "unselect" Malformed;
    (** Move tests *)
    parse_exc_test "ParseTest: move with no position" "move" Malformed;
    parse_exc_test "ParseTest: move with caps" "Move b2" Malformed;
    parse_test "ParseTest: move with position whitespace" " move b2 " 
      (Move (2,2));
    parse_test "ParseTest: move with position" "move B6" (Move (2,6));
    parse_test "ParseTest: move with position and extra" "move B6adfs" 
      (Move (2,6));
    (** Score tests *)
    parse_test "ParseTest: score with no position" "score" Score;
    parse_test "ParseTest: valid score with whitespace" " score " Score;
    parse_exc_test "ParseTest: score with position" "score B6" Malformed;
    (** Draw tests *)
    parse_test "ParseTest: draw with no position" "draw" Draw;
    parse_test "ParseTest: valid draw with whitespace" " draw " Draw;
    parse_exc_test "ParseTest: draw with position" "draw B6" Malformed;
    (** Quit tests *)
    parse_test "ParseTest: quit with no position" "quit" Quit;
    parse_test "ParseTest: valid quit with whitespace" " quit " Quit;
    parse_exc_test "ParseTest: quit with position" "quit B6" Malformed;
    (** Empty tests *)
    parse_exc_test "ParseTest: empty case" "" Empty;
    parse_exc_test "ParseTest: empty case" "               " Empty;
    parse_exc_test "ParseTest: empty case" "         \n      " Empty;
    parse_exc_test "ParseTest: empty case 2" "      " Empty;

    parse_exc_test "ParseTest: random" "hey" Malformed;

  ]

end 

module StateTests = struct
  let initial = GameState.init_state

  let make_captured_test
      (name : string)
      (st : GameState.t) 
      (player : Game.player)
      (expected_output : ChessPiece.t list) =
    name >:: (fun _ -> assert_equal ~cmp:cmp_piece_list 
                 ~printer:(pp_list ChessPiece.to_string) 
                 expected_output (GameState.captured st player))

  let make_score_test
      (name : string)
      (st : GameState.t)
      (player : Game.player)
      (expected_output : int) =
    name >:: (fun _ -> assert_equal expected_output (GameState.score st player))

  let make_visualize_test
      (name : string)
      (st : GameState.t)
      (expected_output : string) =
    name >:: (fun _ -> assert_equal expected_output (GameState.visualize st))

  let make_remaining_test
      (name : string)
      (st : GameState.t)
      (player : player)
      (expected_output : ChessPiece.t list) =
    name >:: (fun _ -> assert_equal ~cmp:cmp_piece_list 
                 ~printer:(pp_list ChessPiece.to_string) 
                 expected_output (GameState.remaining st player))

  let make_state_from_board_test
      (name : string)
      (st : GameState.t)
      (board : GameState.board)
      (expected_output : GameState.t) =
    name >:: (fun _ -> assert_equal expected_output
                 (GameState.state_from_board st board))


  (* white pawn at b2 can capture black pawn at a3 *)
  let some_state = GameState.move GameState.init_state (1,7) (1,3) (*[(1,3)] *)
  let some_state' = GameState.move GameState.init_state (2,2) (1,3) (*[(1,3)]*)
  let captured_pawn = ChessPiece.build Pawn (1,3) Black
  let white_remaining = 
    "[White Rook is at (1, 1); White Knight is at (2, 1); "
    ^ "White Bishop is at (3, 1); White Queen is at (4, 1); White King is at "
    ^ "(5, 1); White Bishop is at (6, 1); White Knight is at (7, 1); White Rook"
    ^ " is at (8, 1); White Pawn is at (1, 2); White Pawn is at (3, 2); White "
    ^ "Pawn is at (4, 2); White Pawn is at (6, 2); White Pawn is at (7, 2); " ^ 
    "White Pawn is at (8, 2); White Pawn is at (5, 3); White Pawn is at (2, 3)]"

  let state_tests = [
    (** [captured st player] tests *)
    make_captured_test "Testing initial board for White" initial White [];
    make_captured_test "Testing initial board for Black" initial Black [];

    make_captured_test "Somestate before capture" some_state Black [];
    make_captured_test "Somestate after capture" some_state' Black 
      [captured_pawn];

    (** [state_from_board] tests *)
    make_state_from_board_test "Testing copy" initial 
      (GameState.curr_board initial) initial
  ]

end

module BrianTests = struct

  let brian_tests = [

  ]
end



module Tester (S : State) = struct

  include GameTests
  include CommandTests
  include StateTests
  include BrianTests

  let game_tests = [

  ]

  let state_tests = [

  ]
end

(*module StartState : State = struct end*)

module Test = Tester(GameState)

let suite =
  "test suite for Chess Game"  >::: List.flatten [
    (*Test.game_tests;*)
    GameTests.game_tests;
    CommandTests.command_tests;
    StateTests.state_tests;
    BrianTests.brian_tests
  ]

let _ = run_test_tt_main suite
