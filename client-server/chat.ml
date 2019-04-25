open Tools

type t = string list

(**[n_list x n] creates a list of [n] elements, where every element is
   [x]*)
let n_list x = n_builder x (fun lst z -> z::lst) []

(**[extend_string s] returns [s] if [s] has 25 or more characters,
   or otherwise adds whitespace at the end of [s] until it has exactly 25
   characters.*)
let extend_string s =
  25 - String.length s |> n_builder " " (^) s

let (init_state:t) = (extend_string "chat") ::n_list (n_spacer 25) 31

let rec next_state (s:string) (t:t) =
  if s= "" then t
  else if String.length s <= 25 then
    extend_string s :: t
  else
    let first25 = String.sub s 0 25 and
    left = String.sub s 25 (String.length s - 25) in
    next_state left (first25 :: t)

(**[list_take_n lst n] Returns the first [n] elements of [lst]. If
   there are less than [n] elements in [lst], then it returns [lst]*)
let list_take_n lst n =
  let rec helper lst n acc =
    if n <= 0 then acc
    else match lst with
      | h::t -> helper t (n-1) (h::acc)
      | [] -> acc
  in
  helper lst n []

let print_chat (t:t) = list_take_n t 26 |> String.concat "\n"
