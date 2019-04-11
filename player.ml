
type cell = Empty|Ship|Miss|Hit
type board = cell list list
type coord = Hit | Coord of int * int
type ship = {sunk :bool ; size:int; inserted:bool; coords: coord list}

type player = {
  board : board;
  ships : ship list;
  shape : int * int
}

type  t = player



let rec set_inner (inner_acc:cell list) (y:int) = 
  if y > 0 
  then set_inner (Empty::inner_acc) (y-1)
  else inner_acc

let init_empty_board x y =
  let rec set_outer acc x y = 
    if x > 0
    then let new_arr = set_inner [] y in 
      set_outer (new_arr::acc) (x-1) y
    else acc
  in set_outer [] x y

let init_shape x y = 
  (x, y)

let get_row row = ""

let print_row row = get_row row |> print_endline

let rec print_dash row_size = 
  if row_size > 0
  then begin print_string "-"; print_dash (row_size - 1) end
  else ()

let temp_board = init_empty_board 5 5

let temp_shape = init_shape 5 5

let print_board () = 
  let rec intermediate (cur_board:board) =
    match cur_board with
    | [] -> print_endline ""
    | h::t -> begin
        (snd temp_shape + 1) |> print_dash; print_row h; intermediate t
      end
  in intermediate temp_board