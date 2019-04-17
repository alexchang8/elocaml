type cell = White | Black | Empty | Star

type board = cell array array

type t = {board: board; active_p: int; b_captured: int; w_captured: int;
          b_error: string; w_error: string; score_str: string; last_passed : bool}

let max_players = 2

type command = Place of (int * int) | Pass | Invalid

let stars = [(3,3);(3,9);(3,15);(9,3);(9,9);(9,15);(15,3);(15,9);(15,15)]

let init_board =
  let b = Array.make_matrix 19 19 Empty in
  List.iter (fun (y,x) -> b.(y).(x) <- Star) stars;
  b

let init_state = {board = Array.copy init_board; active_p = 0;
                  b_captured = 0; w_captured = 0; b_error = "";
                  w_error = ""; score_str = ""; last_passed = false}

let white = "\226\151\143"
let black = "\226\151\139"
let empty = "\194\183"

let cell_string = function
  | White -> white
  | Black -> black
  | Empty -> empty
  | Star -> "+"

let col_label = "   A B C D E F G H J K L M N O P Q R S T "

let board_string b =
  let row_string row =
    Array.fold_left (fun acc c -> acc ^ " " ^ cell_string c) "" row in
  let str_int_2c i =
    if i >= 10 then string_of_int i
    else " " ^ string_of_int i in
  col_label ^
  (Array.fold_left (fun acc row -> row_string row::acc) [] b |>
   List.mapi (fun i s -> let x = str_int_2c (i + 1) in
               x ^ s ^ " " ^ x) |>
   List.rev |> List.fold_left (fun acc s -> acc ^ "\n" ^ s) "" )
  ^"\n" ^ col_label

let p_id_symbol p_id = if p_id = 0 then black else white

let next_p p_id = if p_id = 0 then 1 else 0

let print_player_state t p_id =
  let opening_msg = "  " ^ white ^ " has captured " ^ string_of_int t.w_captured
                    ^ " pieces\n" ^ "  " ^ black ^ " has captured " ^
                    string_of_int t.b_captured ^ " pieces\n" ^ "\n" in
  let e_msg = if p_id = 0 then t.b_error else t.w_error in
  let closing_msg = "\n  You are playing: " ^ p_id_symbol p_id ^ "\n  " ^
                    (if t.active_p = 0 then black else white) ^ " to play" in
  opening_msg ^ board_string t.board ^ closing_msg ^ e_msg ^ t.score_str

let char_in_range c lo up =
  Char.code c >= Char.code lo && Char.code c <= Char.code up

let valid_char_let c =
  char_in_range c 'A' 'H' || char_in_range c 'J' 'T'

let valid_single_char_num c =
  char_in_range c '1' '9'

let num_let_tup x l =
  let col = if char_in_range l 'A' 'H' then Char.code l - Char.code 'A'
    else Char.code l - Char.code 'A' - 1 in
  (19 - x, col)

let parse s =
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
module PairsSet = Set.Make(IntPairs)

let is_empty = function
  | White | Black -> false
  | Star | Empty -> true

let adj_cells (y, x) =
  let in_bounds x = x >= 0 && x < 19 in
  [(y + 1, x);(y - 1,x);(y, x + 1); (y, x - 1)] |>
  List.filter (fun z -> in_bounds (fst z) && in_bounds (snd z))

let cell_eq c1 c2 =
  match c1,c2 with
  | c1,c2 when c1 = c2 -> true
  | (Star, Empty) | (Empty, Star) -> true
  | _ -> false

let fold_lefti f acc a =
  let rec helper f acc a i =
    if i >= Array.length a then acc
    else helper f (f acc a.(i) i) a (i + 1) in
  helper f acc a 0

let stones b =
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

let liberties b stone =
  List.fold_left (fun acc c ->
      acc + List.fold_left (fun c_libs (y,x) ->
          if is_empty b.(y).(x) then c_libs + 1 else c_libs)
        0 (adj_cells c)
    ) 0 stone

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

let restore_stars b =
  List.iter (fun (y,x) -> if b.(y).(x) = Empty then b.(y).(x) <- Star) stars

let update_error t p_id e =
  if p_id = 0 then {t with b_error = t.b_error ^ "\n" ^ e}
  else {t with w_error = t.w_error ^ "\n" ^ e}

let color_reachable stone (color:cell) b =
  List.exists (fun coords -> adj_cells coords |>
                             List.exists (fun (y,x) -> b.(y).(x) = color)) stone

let score t =
  let empty_chains = stones t.board |> List.filter (fun stone ->
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


let next_state t p_id command =
  if t.active_p <> p_id then update_error t p_id "It isn't your turn!"
  else match command with
    (**TODO: implement scoring/endgame*)
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
