let n_builder x f init n =
  let rec helper n acc =
    if n <= 0 then acc
    else helper (n-1) (f acc x) in
  helper n init

let n_spacer = n_builder " " (^) ""

let is_mouse_click s =
  match String.split_on_char ' ' s with
  | x::cs::rs::[] when x = "mouse" -> begin
      match int_of_string_opt cs, int_of_string_opt rs with
      | Some c, Some r -> Some (c,r)
      | _ -> None
    end
  | _ -> None

(**compiled regex for "\b \b"*)
let bregex = Str.regexp "\b \b"

(**[string_to_list s] returns [s] as a character list*)
let string_to_list s =
  let rec stl n acc =
    if n < 0 then acc
    else stl (n - 1) (s.[n]::acc)
  in stl (String.length s - 1) []

let parse_backspace s =
  let rec helper char_list acc =
    match char_list with
    | h::t -> begin
        if h = '\b' then
          match acc with
          | h'::t' -> helper t t'
          | [] -> helper t acc
        else helper t (h::acc)
      end
    | [] -> acc
  in
  helper (Str.global_replace bregex "\b" s |> string_to_list) [] |> List.rev |>
  List.fold_left (fun acc x -> acc ^ Char.escaped x) ""

(**compiled regex for the mouse string*)
let m_regex = Str.regexp "mouse [0-9]+ [0-9]+"

let remove_mouse = Str.global_replace m_regex ""

(**[pad_s_list n s s1] returns [s1], with [s] added to the tail until
   the list at least of length [n]. *)
let pad_s_list n s s1 =
  let rec helper n s1 =
    if List.length s1 >= n then s1
    else helper n (s::s1)
  in List.rev s1 |> helper n |> List.rev

(**[split_n s] splits a string on newlines*)
let split_n = String.split_on_char '\n'

let split_view_string s1 s2 =
  let s1',s2' =
    let s1_l = split_n s1 and
    s2_l = split_n s2 in
    let ns1_l = List.length s1_l and
      ns2_l = List.length s2_l in
    let n = ns1_l - ns2_l in
    if n > 0 then
      (s1_l, pad_s_list ns1_l "" s2_l)
    else if n < 0 then
      (pad_s_list ns2_l "                         " s1_l, s2_l)
    else (s1_l,s2_l)
  in List.map2 (fun x y -> x ^ "   \226\148\130   " ^ y) s1' s2' |> String.concat "\n"

let is_chat_click s =
  match String.split_on_char ' ' s with
  | x::cs::rs::[] when x = "mouse" -> begin
      match int_of_string_opt cs, int_of_string_opt rs with
      | Some c, Some r when c <= 28 -> `Chat_Click
      | _ -> `Non_Chat_Click
    end
  | _ -> `Message
