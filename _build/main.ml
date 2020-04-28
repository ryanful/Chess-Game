open Command
open Game
open State
open Brian

(** *)
let rec pos_to_rankfile lst acc = 
  match lst with
  | [] -> acc
  | (file, rank)::t ->
    let rank' = Char.chr(rank + 48) |> String.make 1  in 
    let file' = Char.chr(file + 96) |> String.make 1 in 
    let acc' = (file' ^ rank')::acc in pos_to_rankfile t acc'

(** *)
let rec print_list = function 
  | [] -> print_endline "\n"
  | h::t -> print_endline h; print_list t

(** *)
let rec select_run st pos = 
  print_string 
    ("\n\nChoose a location from this list with the 'move' command "
     ^ "or select another piece: \n");
  let val_moves = GameState.move_options st pos in
  pos_to_rankfile val_moves [] |> print_list;
  print_string "\n> ";
  match read_line () |> parse with
  | Select pos' -> 
    if (GameState.is_players_piece st pos') then select_run st pos'
    else run st
  | Unselect pos' -> 
    if pos = pos' then run st
    else print_endline "This position wasn't selected!"; select_run st pos
  | Move pos' -> 
    if not (GameState.is_players_piece st pos') && List.mem pos' val_moves 
    then 
      let st' = GameState.move st pos pos' in
      if GameState.check_win st' (GameState.curr_turn st) 
      then let win_string = "\n\n\nWWOOOOOOO!! YOU WON!\n" in
        ANSITerminal.(print_string [green] win_string); exit 0
      else run st'
    else print_endline "Invalid Move -- Choose again"; select_run st pos
  | Score -> print_string "\nYour score is: "; 
    print_int (GameState.score st (GameState.curr_turn st)); 
    print_string "\n"; select_run st pos
  | Draw -> ANSITerminal.(print_string [cyan] "Drew!\n"); exit 0
  | Quit -> ANSITerminal.(print_string [cyan] "Game Over!\n"); exit 0
  | exception Empty -> print_string "Please type a command\n"; run st
  | exception Malformed -> 
    let illegal_message = "\nIllegal Command: please try again \n" in 
    ANSITerminal.(print_string [yellow] illegal_message); run st

and run st = 
  st |> GameState.visualize |> print_string;
  let curr_player = GameState.curr_turn st in
  let prompt = "\n\n" ^ (ChessPiece.player_to_string curr_player) ^
               " -- Select a piece to move with the 'select' command followed" 
               ^ " by a position! \n> " 
  in print_string prompt;
  match read_line () |> parse with
  | Select pos -> 
    if (GameState.is_players_piece st pos) then select_run st pos
    else run st
  | Unselect pos | Move pos -> run st
  | Score -> print_string "\nYour score is: ";
    print_int (GameState.score st (GameState.curr_turn st)); 
    print_string "\n"; run st
  | Draw -> ANSITerminal.(print_string [cyan] "Drew!\n"); exit 0
  | Quit -> ANSITerminal.(print_string [cyan] "Game Over!\n"); exit 0
  | exception Empty -> print_string "Please type a command\n"; run st
  | exception Malformed -> 
    let illegal_message = "\nIllegal Command: please try again \n" in 
    ANSITerminal.(print_string [yellow] illegal_message); run st

let rec player_prompt () =
  ANSITerminal.(print_string [red] 
                  ("\nI am Brian, an AI trying its best, would you " 
                   ^ "like to have an [easy] time or a [hard] time?\n\n> "));
  let difficulty = ref 0 in
  (match read_line () with
   | exception End_of_file -> ()
   | str -> let s = String.lowercase_ascii str in
     if (s = "easy" || s = "[easy]") then difficulty := 1
     else if (s = "hard" || s = "[hard]") then difficulty := 2
     else ANSITerminal.(print_string [red]
                          ("\nYou seem to be too stupid to choose a correct" 
                           ^ " difficulty, so I'll just go easy on you\n"));
     difficulty := 1);
  BrianState.run Black !difficulty

(** *)
let rec play_game () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to Chess\n");
  print_endline "Would you like to play against [Brian], or [a friend]?\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | str -> let s = String.lowercase_ascii str in
    if (s = "a friend" || s = "[a friend]") || (s = "friend" || s = "[friend]")
    then run GameState.init_state
    else if (s = "brian" || s = "[brian]") then
      player_prompt ()
    else (print_endline "Try again! Maybe you spelled something wrong?\n");
    play_game ()

(* Execute the game engine. *)
let () = play_game ()

