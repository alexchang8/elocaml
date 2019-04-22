type t = string list

let n_builder x f init n =
  let rec helper n acc =
    if n <= 0 then acc
    else helper (n-1) (f x acc) in
  helper n init

let n_spacer = n_builder " " (^) ""

let n_list x = n_builder x (List.cons) []

let extend_string s =
  String.length s - 25 |> n_builder " " (^) s

let init_state:t = (extend_string "chat") ::n_list (n_spacer 25) 31

let next_state (s:string) (t:t) = extend_string s :: t

let list_take_n lst n =
  let rec helper lst n acc =
    if n <= 0 then acc
    else match lst with
      | h::t -> helper t (n-1) (h::acc)
      | [] -> acc
  in
  helper lst n [] |> List.rev

let print_chat (t:t) = list_take_n t 26 |> String.concat "\n"
