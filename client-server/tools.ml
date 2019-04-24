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

let bregex = Str.regexp "\b \b"

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

let m_regex = Str.regexp "mouse [0-9]+ [0-9]+"

let remove_mouse = Str.global_replace m_regex ""
