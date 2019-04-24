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
