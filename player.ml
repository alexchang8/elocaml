
type cell = Empty|Ship|Miss|Hit|Sunk
type board = cell list list
type coord = Hit of int * int | Coord of int * int
type ship = {sunk :bool ; size:int; inserted:bool; coords: coord list}
type exc = Valid of coord list | Invalid of string

exception Diagonal_Ship
exception Out_of_Bounds
exception Invalid_Placement


(** RI: Board shape can be no larger than 26 by 26 *)
type player = {
  board : board;
  ships : ship list;
  shape : int * int;
  name : int
}

type t = player

type valid_board = Valid of t | Invalid of string
type game_over = Continue of t | Loss

(** [alphabet] is the alphabet allowed for the y axis coordinates of the 
    game board. *)
let alphabet = " ABCDEFGHIJKLMNOPQRSTUVWXYZ"

(** [index c] is the 0-based index of [c] in the alphabet.
    Requires: [c] is an uppercase letter in A..Z. *)
let index (c:char) : int =
  String.index alphabet c

(** [inv_index n] takes an index value and returns the corresponding letter
    in the alphabet.
    Requires: [n] is an int between 0-25. *)
let inv_index (n:int) : char = 
  String.get alphabet n

(** [set_inner acc y] is a helper function for initializing the empty board. 
    It tail recursively computes a list of Empty cells of length [y]. *)
let rec set_inner (acc:cell list) (y:int) = 
  if y > 0 
  then set_inner (Empty::acc) (y-1)
  else acc

(** [init_empty_board x y] is the function that initializes an empty board of
    shape ([x], [y]). An empty game board is a 2D array of all Empty cells.
    Requires: 0 < x < 26, 0 < y < 26 *)
let init_empty_board x y =
  let rec set_outer acc x y = 
    if x > 0
    then let new_arr = set_inner [] y in 
      set_outer (new_arr::acc) (x-1) y
    else acc
  in set_outer [] x y

(** [init_shape x y] initializes the shape of the game board given a row
    length of [x] and column length of [y]. *)
let init_shape x y = 
  (x, y)

(** [init_player x y name] initializes an new player containing an empty board 
    of size ([x], [y]), an empty ship array, and the board's shape ([x], [y]). 
*)
let init_player x y name : t = 
  {
    board = init_empty_board x y;
    ships = [];
    shape = init_shape x y;
    name = name
  }

(** [add_ship ships coords] adds a new ship of coordinates [coords] to the 
    list of current ships for the player: [ships].
    Requires: [coords] is a valid list of coordinates given the current
    game board. *)
let add_ship ships coords = 
  let new_ship = 
    {
      sunk = false;
      size = List.length coords;
      inserted = true;
      coords = coords
    } in
  new_ship::ships

(** [empty_cell c] is the boolean function for whether a given cell [c] is 
    Empty. *)
let empty_cell c = c = Empty

(** [add_ship_to_board board coords] adds a ship of coordinates [coords] 
    to the current game board [board] and returns a new game board. 
    Requires: [coords] is a valid list of coordinates given the current
    game board [board]. *)
let add_ship_to_board board coords = 
  let rec inner_loop acc row_pos col_pos row =
    match row with
    | [] -> acc
    | h::t -> 
      begin
        if List.mem (Coord (row_pos, col_pos)) coords
        then begin 
          if not (empty_cell h) 
          then raise Invalid_Placement (* TODO Add real exception *)
          else inner_loop (Ship::acc) row_pos (col_pos+1) t
        end
        else inner_loop (h::acc) row_pos (col_pos+1) t
      end in
  let rec outer_loop acc row_pos b =
    match b with
    | [] -> acc
    | h::t -> begin 
        outer_loop ((List.rev (inner_loop [] row_pos 1 h))::acc) (row_pos+1) t
      end
  in List.rev (outer_loop [] 1 board)

let match_coord c = 
  match c with 
  | Hit _ -> failwith "RI doesn't hold"
  | Coord (x,y) -> (x,y)

(* [get_all_cords start_cord end_cord] outputs the coordinate list of a ship
   based on a ship's start and end coordinates.
   Raises [Out_of_Bounds] if user creates a ship bigger than the board
   Raises [Diagonal_Ship] if user creates a diagonal ship **)
let get_all_cords (start_cord:coord) (end_cord:coord) : coord list  = 
  if 
    (start_cord |> match_coord |> fst > 25) || 
    (start_cord |> match_coord |> snd > 25) ||
    (end_cord |> match_coord |> fst > 25) || 
    (end_cord |> match_coord |> snd > 25)
  then raise Out_of_Bounds
  else match (match_coord start_cord), (match_coord end_cord) with 
    | (a1, b1), (a2, b2) when a1=a2 -> begin 
        let rec mid_coords (b1:int) (b2:int) (acc:coord list) : coord list = 
          if b2 >= b1 then let new_acc = Coord (a1, b2)::acc in
            mid_coords b1 (b2-1) new_acc
          else acc in mid_coords (snd (a1, b1)) (snd (a2, b2)) []
      end       
    | (a1, b1), (a2, b2) when b1=b2-> begin
        let rec midd_coords a1 a2 acc = 
          if a2 >= a1 then let new_acc = Coord ((a2-1),b1)::acc in
            midd_coords a1 (a2-1) new_acc
          else acc in midd_coords (fst (a1, b1)) (fst (a2, b2)) [] end
    | (_,_), (_, _) -> raise Diagonal_Ship


(** [insert_ship player start_cord end_cord] inserts a ship given the starting
    and ending coordinates of the ship ([start_cord] and [end_cord]) to the
    current players board and returns an updated player. *)
let insert_ship (player:t) start_cord end_cord size = 
  let cord_list = get_all_cords start_cord end_cord in
  if List.length cord_list <> size 
  then Invalid "Invalid size"
  else
    let new_ships = add_ship player.ships cord_list in
    let new_board = add_ship_to_board player.board cord_list in
    Valid {
      board=new_board;
      ships=new_ships;
      shape=player.shape;
      name=player.name
    }

(** [cell_spacing] is a constant representing the space between cells on a
    gameboard. Incorporated to display the game to the user. *)
let cell_spacing = " | "

(** [get_dash_space] computes the string of all white space the same size
    as [cell_spacing]. *)
let get_dash_space () = 
  let rec tr_helper acc s = 
    if s > 0
    then tr_helper (" " ^ acc) (s-1)
    else acc
  in tr_helper "" (String.length cell_spacing)

(** [dash_spacing] is a constant representing the space between cells on a
    gameboard. The space is the same size as cell_spacing except all white 
    space. Incorporated to display the game to the user. *)
let dash_space = get_dash_space ()

let miss_val = "0"
let hit_val = "X"
let sunk_val = "Z"
let empty_val = " "
let ship_val = "S"

(** [parse_cell c verbose] gives the string representation of the cell [c]. 
    If [verbose] is true then ships are displayed. Otherwise, they are skipped
    over and given the empty space. *)
let parse_cell c verbose = 
  match c with
  | Empty -> empty_val
  | Ship -> if verbose then ship_val else empty_val
  | Miss -> miss_val
  | Hit ->  hit_val
  | Sunk -> sunk_val

(** [get_row row_acc row verbose] computes the string representation of [row]. 
    Prepends the current row label to the row and spaces each cell by
    the [cell_spacing] variable. If [verbose] then the players ships will 
    also be displayed. *)
let get_row (row_acc:int) (row:cell list) verbose = 
  let rec tr_helper acc r = 
    match r with 
    | [] -> acc
    | h::t -> tr_helper (acc ^ (parse_cell h verbose ^ cell_spacing)) t
  in tr_helper ((row_acc |> inv_index |> Char.escaped) ^ cell_spacing) row

(** [print_row row_acc row verbose] prints the string representation of [row] 
    to the console. If [verbose] then the players ships will also be displayed. 
*)
let print_row row_acc row verbose = get_row row_acc row verbose |> print_endline

(** [print_dash row_size] prints a row of dashes seperated by a [dash_space]. *)
let rec print_dash row_size = 
  if row_size > 0
  then begin print_string ("-" ^ dash_space); print_dash (row_size - 1) end
  else print_endline ""

(** [print_top_labels s] prints the x-axis coordinates of the game board 
    seperated by [cell_spacing]. Begins with value of 1 and continues for
    [s] coordinate labels. *)
let print_top_labels s = 
  let () = print_string " " in (* Use as an offset for the row labels *)
  let rec helper acc size = 
    if size > 0
    then begin 
      cell_spacing ^ (string_of_int acc) |> print_string;
      helper (acc+1) (size-1)
    end
    else print_endline cell_spacing
  in helper 1 s

let temp_player = init_player 5 5 1

(** [print_board player verbose] prints the board of the current [player]. If 
    [verbose] then the players ships will also be displayed. *)
let print_board (player:t) verbose = 
  let shape = player.shape in
  let max_size = (snd shape) in (* change if we extend to unique boards *)
  let () = print_top_labels max_size in
  let rec intermediate (row_acc:int) (cur_board:board) =
    match cur_board with
    | [] -> begin (fst shape + 1) |> print_dash; print_endline "" end
    | h::t -> begin
        (fst shape + 1) |> print_dash; 
        print_row row_acc h verbose; 
        intermediate (row_acc+1) t
      end
  in intermediate 1 player.board

(** [print_my_board player] prints the board of the current [player] showing 
    them their own ships and which spots on the players board has been attacked.
*)
let print_my_board (player:t) = print_board player true

(** [print_opp_board player] prints the opposing players view of the current 
    [player]'s board. This view will hide the players ships from being printed. 
*)
let print_opp_board (player:t) = print_board player false

(** [is_hit c] returns true if the coordinate [c] is a hit. *)
let is_hit c = 
  match c with 
  | Hit _ -> true
  | Coord _ -> false

(** [is_sunk s] returns whether ship [s] is sunk. A ship is considered sunk when
    all of its coordinates have been hit. *)
let is_sunk (coords:coord list) = 
  let f acc b = acc && (is_hit b) in
  List.fold_left f true coords

(** [all_sunk s] returns whether all ships in [s] are sunk. A ship is 
    considered sunk when all of its coordinates have been hit. If every ship
    is sunk then the player has lost the game. *)
let all_sunk (s:ship list) = 
  let f acc b = acc && (is_sunk b.coords) in
  List.fold_left f true s

(** [already_guessed board (c1, c2)] is the function that determines if cell
    [(c1, c2)] has previously been guessed by a user. A cell has not been
    guessed if its value is Empty or a Ship. *)
let already_guessed board (c1, c2) : bool =
  let rec inner_loop row col_pos = 
    match row with 
    | [] -> false
    | h::t -> if col_pos = c2 
      then match h with
        | Empty | Ship -> false
        | Sunk | Hit | Miss -> true
      else inner_loop t (col_pos+1)
  in 
  let rec outer_loop b row_pos =
    match b with 
    | [] -> false
    | h::t -> if row_pos = c1
      then inner_loop h 1
      else outer_loop t (row_pos+1)
  in outer_loop board 1

(** [update_ships ships (c1, c2)] updates a ship list [ships] according to the
    guess (c1, c2). If a ship in the list has been hit, then it gets updated
    and if a ship has not been hit, then there are no changes to the ship list.
    Returns: tuple of the updated ship list and a list of coordinates to 
      update on the game board. 
    Requires: (c1, c2) has not previously been guessed. *)
let update_ships (ships:ship list) (c1, c2) = 
  let rec loop_ships acc ships update_coords update_val = 
    match ships with
    | [] -> acc, update_coords, update_val
    | h::t ->  
      if List.mem (Coord (c1, c2)) h.coords
      then begin
        let rec update_ship acc ship_coords = 
          match ship_coords with
          | [] -> acc
          | h::t -> if h = (Coord (c1, c2))
            then update_ship ((Hit (c1, c2)::acc)) t
            else update_ship (h::acc) t
        in 
        let new_coords = update_ship [] h.coords in 
        let sunk = is_sunk new_coords in
        let new_ship = 
          {sunk=sunk; size=h.size; inserted=h.inserted; coords=new_coords} in
        let update = if sunk then new_coords else [Hit (c1, c2)] in
        let up_val = if sunk then Sunk else Hit in
        loop_ships (new_ship::acc) t update up_val
      end
      else loop_ships (h::acc) t update_coords update_val
  in loop_ships [] ships [Hit (c1, c2)] Miss (* gross to initialize the update coordinate to a Hit but will work for now. *)

(** [update_board board update_coords v] computes a new board with all instances
    of update_coords replaced with the cell value [v] on the current board 
    [board]. *)
let update_board board update_coords v : board = 
  let rec loop_rows acc row_pos b =
    match b with
    | [] -> List.rev acc
    | h::t -> begin 
        let rec loop_row row_acc row_pos col_pos r = 
          match r with
          | [] -> List.rev row_acc
          | h::t -> if List.mem (Hit (row_pos, col_pos)) update_coords
            then loop_row (v::row_acc) row_pos (col_pos+1) t
            else loop_row (h::row_acc) row_pos (col_pos+1) t
        in let new_row = loop_row [] row_pos 1 h in
        loop_rows (new_row::acc) (row_pos+1) t
      end
  in loop_rows [] 1 board

(** [check (c1, c2)] returns the new player with the coordinate (c1, c2) 
    updated to represent the player's guess. 
    Requires: Cell (c1, c2) has not previously been guessed
    Returns the option of continue or loss
    TODO update this comment *)
let check (p:t) (c1, c2) = 
  (* 
    Sudo Code:
    Loop through the players ships:
      If no ship is hit
      then iterate over the board and change cell (c1, c2) to a miss
      else --> a ship is hit, then update the players ships.
        If the ship that just got hit is sunk
        then Update all cells on board to sunk
          If all ships are sunk
          then the game is over return Loss
          else return the player with updated ships and board
        else Update cell (c1, c2) to hit 
            and return player with updated ships and board
  *)
  let (new_ships, update_coords, v) = update_ships p.ships (c1, c2) in
  let over = all_sunk new_ships in
  if over 
  then Loss
  else begin
    if v = Miss (* Print the player missed and update the board *)
    then print_endline "Miss!"
    else if v = Hit then print_endline "Hit!"
    else print_endline "Hit! You sunk their ship!"; (* Ship must be sunk *)
    let new_board = update_board p.board update_coords v in
    Continue 
      ({
        board=new_board;
        ships=new_ships;
        shape=p.shape;
        name=p.name
      })
  end

(* If running in utop, uncomment lines below to see the functionality. 
    You can run print_my_board hit1 to print the board after the player
    has missed once and hit once.
*)
(* let res = insert_ship temp_player (Coord (1,1)) (Coord (1, 3)) 3;;

   let new_p = match res with | Valid p -> p | _ -> failwith "";;

   let miss1 = match (check new_p (3, 3)) with | Continue p -> p | _ -> failwith "";;

   print_my_board miss1;;

   let res  = check miss1 (1,2);;

   let hit1 =  match (check miss1 (1, 3)) with | Continue p -> p | _ -> failwith "";;

   let hit2  = match (check hit1 (1,1)) with | Continue p -> p | _ -> failwith "";; *)


