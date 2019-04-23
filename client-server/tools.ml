let n_builder x f init n =
  let rec helper n acc =
    if n <= 0 then acc
    else helper (n-1) (f acc x) in
  helper n init

let n_spacer = n_builder " " (^) ""
