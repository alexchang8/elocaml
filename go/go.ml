(**The type corresponding to each space on the board.
   Star is equivalent to empty, but is added for printing the board
   nicely.*)
type cell = White | Black | Empty | Star

(**The abstract type representing a game state of go.
   Board: a [cell array array] representing the current board
   active_p: the active [p_id]
   b_captured: the number of pieces black has captured
   w_captured: the number of pieces white has captured
   b_error: error messages to be sent to the player playing black
   w_error: error messages to be sent to the player playing white
   last_passed: true iff in the last game state the active player passed*)
type t = {board: cell array array; active_p: int; b_captured: int; w_captured: int;
          b_error: string; w_error: string; score_str: string; last_passed : bool}

(**Go is a 2 player game. This is read by the server functor*)
let max_players = 2

(**The type corresponding to parsed strings entered by the player.
   Place (x,y): command corresponding to a player attempting to place a stone at (x,y)
   Pass: command corresponding to a player electing to skip their turn.
   Invalid: any command that did not parse*)
type command = Place of (int * int) | Pass | Invalid | Ignore

(**The list of points that are "stars" on the board. This is purely for printing*)
let stars = [(3,3);(3,9);(3,15);(9,3);(9,9);(9,15);(15,3);(15,9);(15,15)]

(**A 19 x 19 board filled with [Empty], and [Star] at the points in [stars]*)
let init_board =
  let b = Array.make_matrix 19 19 Empty in
  List.iter (fun (y,x) -> b.(y).(x) <- Star) stars;
  b

let terminal_size = (80,32)

(**The initial state of the game*)
let init_state = {board = Array.copy init_board; active_p = 0;
                  b_captured = 0; w_captured = 0; b_error = "";
                  w_error = ""; score_str = ""; last_passed = false}

(**escape sequence for the white stones*)
let white = "\x1B[43m\x1B[29m\226\151\143\x1B[0m\x1B[0m"
(**escape sequence for the black stones*)
(* let black = "\x1B[30m\226\151\139\x1B[0m" *)
let black = "\x1B[43m\x1B[30m\226\151\143\x1B[0m\x1B[0m"
(**escape sequence for the empty spaces*)
let empty = "\x1B[33;7m\194\183\x1B[0m"

(**[cell_string c] returns the printed symbol corresponding to [c]*)
let cell_string = function
  | White -> white
  | Black -> black
  | Empty -> empty
  | Star -> "\x1B[33;7m+\x1B[0m"

let name = "GOCAML"

(**[board_string b] is a stringified representation of the board*)
let board_string b =
  let col_label = "   A B C D E F G H J K L M N O P Q R S T " in
  let row_string row =
    Array.fold_left (fun acc c -> acc ^ "\x1B[33;7m \x1B[0m" ^ cell_string c ^ "") "" row in
  let str_int_2c i =
    if i >= 10 then string_of_int i
    else " " ^ string_of_int i in
  col_label ^
  (Array.fold_left (fun acc row -> row_string row::acc) [] b |>
   List.mapi (fun i s -> let x = str_int_2c (i + 1) in
               x ^ s ^ "\x1B[33;7m \x1B[0m" ^ x) |>
   List.rev |> List.fold_left (fun acc s -> acc ^ "\n" ^ s) "" )
  ^"\n" ^ col_label

(**[next_p p_id] returns 1 if p_id is 0, and 0 otherwise *)
let next_p p_id = if p_id = 0 then 1 else 0

(**[print_player_state t p_id] returns the string corresponding to the complete
   player view for player *)
let print_player_state t p_id =
  let p_id_symbol p_id = if p_id = 0 then black else white in
  let opening_msg = "  " ^ white ^ " has captured " ^ string_of_int t.w_captured
                    ^ " pieces\n" ^ "  " ^ black ^ " has captured " ^
                    string_of_int t.b_captured ^ " pieces\n" ^ "\n" in
  let e_msg = if p_id = 0 then t.b_error else t.w_error in
  let closing_msg = "\n  You are playing: " ^ p_id_symbol p_id ^ "\n  " ^
                    (if t.active_p = 0 then black else white) ^ " to play" in
  opening_msg ^ board_string t.board ^ closing_msg ^ e_msg ^ t.score_str

(**Returns [true] iff the character code of c is between the character code
   of lo and up inclusive*)
let char_in_range c lo up =
  Char.code c >= Char.code lo && Char.code c <= Char.code up

(**Returns [true] iff c is in A..H or c is in J..T *)
let valid_char_let c =
  char_in_range c 'A' 'H' || char_in_range c 'J' 'T'

(**Returns [true] iff c is in the character range 1..9*)
let valid_single_char_num c =
  char_in_range c '1' '9'

(**[num_let_tup x l] Returns the board position corresponding to the number
   x and letter l using standard Go notation. Note that the letter 'I' is skipped.*)
let num_let_tup x l =
  let col = if char_in_range l 'A' 'H' then Char.code l - Char.code 'A'
    else Char.code l - Char.code 'A' - 1 in
  (19 - x, col)

(**[parse s] returns the command corresponding to a string. A string corresponds
   to pass iff it is "pass". A string corresponding to [Place] is of the form
   ax, where [valid_char_let a] is [true] and x is a string corresponding to a number
   in 1..19 . Any other string is [Invalid].*)
let parse s =
  print_endline s;
  match String.split_on_char ' ' s with
  | x::cs::rs::[] when x = "mouse" -> begin
      match int_of_string_opt cs, int_of_string_opt rs with
      | Some c, Some r when c >= 36 && c <= 72 && r >= 7 && r <= 25 ->
        Place(r-5-2, (c-4-32)/2)
      | _ -> Ignore
    end
  | _ -> begin
      if s = "pass" then Pass else
      if s = "" || not (valid_char_let s.[0]) then Invalid else
      if String.length s = 2 then
        if valid_single_char_num s.[1] then
          Place (num_let_tup (Char.code s.[1] - Char.code '0') s.[0])
        else Invalid
      else if String.length s = 3 then
        match int_of_string_opt (String.sub s 1 2) with
        | Some x when x >=1 && x<=19 -> Place (num_let_tup x s.[0])
        | _ -> Invalid
      else Invalid
    end

(**Intpairs and PairsSet is taken directly from
   https://caml.inria.fr/pub/docs/manual-ocaml/libref/Set.html *)
module IntPairs =
struct
  type t = int * int
  let compare (x0,y0) (x1,y1) =
    match Pervasives.compare x0 x1 with
      0 -> Pervasives.compare y0 y1
    | c -> c
end
(**Intpairs and PairsSet is taken directly from
   https://caml.inria.fr/pub/docs/manual-ocaml/libref/Set.html *)
module PairsSet = Set.Make(IntPairs)

(**[is_empty c] returns true iff c is [Star] or [Empty]*)
let is_empty = function
  | White | Black -> false
  | Star | Empty -> true

(**[adj_cells c] returns a list of coordinates corresponding to cells that
   are adjacent to [c] in a 19x19 board. Adjacent cells are cells that are directly
   above or direcly below a cell.*)
let adj_cells (y, x) =
  let in_bounds x = x >= 0 && x < 19 in
  [(y + 1, x);(y - 1,x);(y, x + 1); (y, x - 1)] |>
  List.filter (fun z -> in_bounds (fst z) && in_bounds (snd z))

(**The same as Array.fold_left, except the first argument to [f] is the index
   of the element*)
let fold_lefti f acc a =
  let rec helper f acc a i =
    if i >= Array.length a then acc
    else helper f (f acc a.(i) i) a (i + 1) in
  helper f acc a 0

(**[stones b] returns a list of the boards equivalent stones. Equivalent stones
   are adjacent, (not diagonal), and have the same type (with [Star] and [Empty] equivalent).*)
let stones b =
  let cell_eq c1 c2 =
    match c1,c2 with
    | c1,c2 when c1 = c2 -> true
    | (Star, Empty) | (Empty, Star) -> true
    | _ -> false
  in
  let rec stone_helper (y, x) b searched =
    if PairsSet.mem (y,x) !searched then []
    else begin
      searched := PairsSet.add (y, x) !searched;
      List.fold_left (fun acc (y1, x1) ->
          if cell_eq b.(y).(x) b.(y1).(x1) then
            List.rev_append acc (stone_helper (y1, x1) b searched)
          else acc
        ) [(y, x)] (adj_cells (y,x))
    end
  in
  let s = ref PairsSet.empty in
  fold_lefti (fun acc row y ->
      List.rev_append acc (fold_lefti (fun r_acc _ x ->
          match stone_helper (x, y) b s with
          | [] -> r_acc
          | stones -> stones::r_acc
        ) [] row)
    ) [] b

(**[liberties b stone] returns the number of liberties of a given stone. A liberty
   is defined as an [Empty] cell adjacent to a stone. Equivalent stones share liberties.
   Note that this possibly double counts liberties, but if it returns 0 the number of
   liberties is gauranteed to be 0*)
let liberties b stone =
  List.fold_left (fun acc c ->
      acc + List.fold_left (fun c_libs (y,x) ->
          if is_empty b.(y).(x) then c_libs + 1 else c_libs)
        0 (adj_cells c)
    ) 0 stone

(**[remove_dead_stones color b stones] removes stones with 0 liberties of color
   [color] in a given board [b]. Returns the number of stones removed.*)
let remove_dead_stones color b stones =
  stones |> List.filter (fun stone ->
      let (y,x) = List.hd stone in b.(y).(x) = color) |>
  List.fold_left (fun acc stone ->
      if liberties b stone = 0 then begin
        List.iter (fun (y, x) -> b.(y).(x) <- Empty) stone;
        acc + List.length stone
      end
      else acc
    ) 0

(**[restore_stars b] mutates [b], restoring the star positions if they are
   [Empty]*)
let restore_stars b =
  List.iter (fun (y,x) -> if b.(y).(x) = Empty then b.(y).(x) <- Star) stars

(**[update_error t p_id e] returns the game state [t], but with an added
   error message for the player [p_id]*)
let update_error t p_id e =
  if p_id = 0 then {t with b_error = t.b_error ^ "\n" ^ e}
  else {t with w_error = t.w_error ^ "\n" ^ e}

(**[score t] returns a string containing the score of white and black in a given
   game t. Territory scoring is used. A color owns a territory if through any possible
   adjacent path of empty cells, only one color can be reached. The score is then the
   sum of a colors prisoners and the number of empty cells in their territories.*)
let score t =
  let color_reachable stone (color:cell) b =
    List.exists (fun coords -> adj_cells coords |>
                               List.exists (fun (y,x) -> b.(y).(x) = color)) stone in
  let empty_chains = stones t.board |>
                     List.filter (fun stone ->
                         let (y,x) = List.hd stone in is_empty t.board.(y).(x)) in
  let tagged_chains = List.map (fun stone ->
      match (color_reachable stone Black t.board, color_reachable stone White t.board) with
      | (true, true) | (false, false) -> (Empty, stone)
      | (true, false) ->  (Black, stone)
      | (false, true) -> (White, stone)
    ) empty_chains in
  let score color =
    List.fold_left (fun acc tag -> if (fst tag) = color then
                       (List.length (snd tag)) + acc else acc) 0 tagged_chains in
  let b_score = score Black + t.b_captured in
  let w_score = score White + t.w_captured in
  "\n " ^ black ^ "'s score is : " ^ string_of_int b_score ^
  "\n " ^ white ^ "'s score is : " ^ string_of_int w_score

(**[next_state t p_id command] returns the state of the game after
   [p_id] tries to play [command]. If [command] is [Invalid] or it is not that
   player's turn, only error messages are updated. If the command is [Pass], that
   player's turn is skipped. If the command is [Place (y,x)], a stone of the players
   color is then placed at the corresponding cell if it is empty or star. Then,
   dead stones of the opponent are removed, followed by dead stones of the player.
   If both players pass in a row then the game is scored and ends.*)
let next_state t p_id command =
  if t.active_p <> p_id then update_error t p_id "It isn't your turn!"
  else match command with
    (**TODO: implement scoring/endgame*)
    | Ignore -> t
    | Pass ->
      if t.last_passed then {t with active_p = -1; score_str = score t}
      else {t with active_p = next_p p_id; last_passed = true}
    | Invalid -> update_error t p_id "Invalid command!"
    (*TODO: simple ko rule*)
    | Place(y,x) ->
      if not (is_empty t.board.(y).(x)) then
        update_error t p_id "Invalid command (filled cell)!"
      else
        let color = if p_id = 0 then Black else White in
        t.board.(y).(x) <- color;
        let stones = stones t.board in
        let b_cap_lazy () = t.b_captured + (remove_dead_stones White t.board stones) in
        let w_cap_lazy () = t.w_captured + (remove_dead_stones Black t.board stones) in
        if p_id = 0 then
          let b_cap = b_cap_lazy () in
          let w_cap = w_cap_lazy () in
          restore_stars t.board;
          {t with active_p = next_p p_id; b_error = ""; b_captured = b_cap;
                  w_captured = w_cap; last_passed = false}
        else
          let w_cap = w_cap_lazy () in
          let b_cap = b_cap_lazy () in
          restore_stars t.board;
          {t with active_p = next_p p_id; w_error = ""; b_captured = b_cap;
                  w_captured = w_cap; last_passed = false}
