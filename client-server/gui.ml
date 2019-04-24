open Tools
let spacer = "                    "
let logo = "\x1B[38;2;255;0;0m \x1B[39m\x1B[38;2;255;7;7m \x1B[39m\x1B[38;2;255;13;13m \x1B[39m\x1B[38;2;255;20;20m_\x1B[39m\x1B[38;2;255;27;27m_\x1B[39m\x1B[38;2;255;34;34m_\x1B[39m\x1B[38;2;255;40;40m_\x1B[39m\x1B[38;2;255;47;47m_\x1B[39m\x1B[38;2;255;54;54m_\x1B[39m\x1B[38;2;255;60;60m \x1B[39m\x1B[38;2;255;67;67m \x1B[39m\x1B[38;2;255;74;74m \x1B[39m\x1B[38;2;255;81;81m_\x1B[39m\x1B[38;2;255;87;87m_\x1B[39m\x1B[38;2;255;94;94m_\x1B[39m\x1B[38;2;255;101;101m_\x1B[39m\x1B[38;2;255;107;107m \x1B[39m\x1B[38;2;255;114;114m \x1B[39m\x1B[38;2;255;121;121m_\x1B[39m\x1B[38;2;255;128;128m_\x1B[39m\x1B[38;2;255;134;134m_\x1B[39m\x1B[38;2;255;141;141m_\x1B[39m\x1B[38;2;255;148;148m_\x1B[39m\x1B[38;2;255;154;154m_\x1B[39m\x1B[38;2;255;161;161m_\x1B[39m\x1B[38;2;255;168;168m_\x1B[39m\x1B[38;2;255;174;174m \x1B[39m\x1B[38;2;255;181;181m \x1B[39m\x1B[38;2;255;188;188m \x1B[39m\x1B[38;2;255;195;195m_\x1B[39m\x1B[38;2;255;201;201m_\x1B[39m\x1B[38;2;255;208;208m \x1B[39m\x1B[38;2;255;215;215m \x1B[39m\x1B[38;2;255;221;221m_\x1B[39m\x1B[38;2;255;228;228m_\x1B[39m\x1B[38;2;255;235;235m_\x1B[39m\x1B[38;2;255;242;242m_\x1B[39m\x1B[38;2;255;248;248m_\x1B[39m\x1B[38;2;255;255;255m \x1B[39m\n\x1B[38;2;255;0;0m \x1B[39m\x1B[38;2;255;7;7m \x1B[39m\x1B[38;2;255;13;13m/\x1B[39m\x1B[38;2;255;20;20m \x1B[39m\x1B[38;2;255;27;27m_\x1B[39m\x1B[38;2;255;34;34m_\x1B[39m\x1B[38;2;255;40;40m/\x1B[39m\x1B[38;2;255;47;47m \x1B[39m\x1B[38;2;255;54;54m/\x1B[39m\x1B[38;2;255;60;60m \x1B[39m\x1B[38;2;255;67;67m \x1B[39m\x1B[38;2;255;74;74m/\x1B[39m\x1B[38;2;255;81;81m \x1B[39m\x1B[38;2;255;87;87m_\x1B[39m\x1B[38;2;255;94;94m_\x1B[39m\x1B[38;2;255;101;101m \x1B[39m\x1B[38;2;255;107;107m\\\x1B[39m\x1B[38;2;255;114;114m/\x1B[39m\x1B[38;2;255;121;121m \x1B[39m\x1B[38;2;255;128;128m_\x1B[39m\x1B[38;2;255;134;134m_\x1B[39m\x1B[38;2;255;141;141m_\x1B[39m\x1B[38;2;255;148;148m/\x1B[39m\x1B[38;2;255;154;154m \x1B[39m\x1B[38;2;255;161;161m_\x1B[39m\x1B[38;2;255;168;168m \x1B[39m\x1B[38;2;255;174;174m|\x1B[39m\x1B[38;2;255;181;181m \x1B[39m\x1B[38;2;255;188;188m/\x1B[39m\x1B[38;2;255;195;195m \x1B[39m\x1B[38;2;255;201;201m \x1B[39m\x1B[38;2;255;208;208m|\x1B[39m\x1B[38;2;255;215;215m/\x1B[39m\x1B[38;2;255;221;221m \x1B[39m\x1B[38;2;255;228;228m \x1B[39m\x1B[38;2;255;235;235m/\x1B[39m\x1B[38;2;255;242;242m \x1B[39m\x1B[38;2;255;248;248m/\x1B[39m\x1B[38;2;255;255;255m \x1B[39m\n\x1B[38;2;255;0;0m \x1B[39m\x1B[38;2;255;7;7m/\x1B[39m\x1B[38;2;255;13;13m \x1B[39m\x1B[38;2;255;20;20m_\x1B[39m\x1B[38;2;255;27;27m/\x1B[39m\x1B[38;2;255;34;34m/\x1B[39m\x1B[38;2;255;40;40m \x1B[39m\x1B[38;2;255;47;47m/\x1B[39m\x1B[38;2;255;54;54m_\x1B[39m\x1B[38;2;255;60;60m_\x1B[39m\x1B[38;2;255;67;67m/\x1B[39m\x1B[38;2;255;74;74m \x1B[39m\x1B[38;2;255;81;81m/\x1B[39m\x1B[38;2;255;87;87m_\x1B[39m\x1B[38;2;255;94;94m/\x1B[39m\x1B[38;2;255;101;101m \x1B[39m\x1B[38;2;255;107;107m/\x1B[39m\x1B[38;2;255;114;114m \x1B[39m\x1B[38;2;255;121;121m/\x1B[39m\x1B[38;2;255;128;128m_\x1B[39m\x1B[38;2;255;134;134m_\x1B[39m\x1B[38;2;255;141;141m/\x1B[39m\x1B[38;2;255;148;148m \x1B[39m\x1B[38;2;255;154;154m_\x1B[39m\x1B[38;2;255;161;161m_\x1B[39m\x1B[38;2;255;168;168m \x1B[39m\x1B[38;2;255;174;174m|\x1B[39m\x1B[38;2;255;181;181m/\x1B[39m\x1B[38;2;255;188;188m \x1B[39m\x1B[38;2;255;195;195m/\x1B[39m\x1B[38;2;255;201;201m|\x1B[39m\x1B[38;2;255;208;208m_\x1B[39m\x1B[38;2;255;215;215m/\x1B[39m\x1B[38;2;255;221;221m \x1B[39m\x1B[38;2;255;228;228m/\x1B[39m\x1B[38;2;255;235;235m \x1B[39m\x1B[38;2;255;242;242m/\x1B[39m\x1B[38;2;255;248;248m_\x1B[39m\x1B[38;2;255;255;255m_\x1B[39m\n\x1B[38;2;255;0;0m/\x1B[39m\x1B[38;2;255;7;7m_\x1B[39m\x1B[38;2;255;13;13m_\x1B[39m\x1B[38;2;255;20;20m_\x1B[39m\x1B[38;2;255;27;27m/\x1B[39m\x1B[38;2;255;34;34m_\x1B[39m\x1B[38;2;255;40;40m_\x1B[39m\x1B[38;2;255;47;47m_\x1B[39m\x1B[38;2;255;54;54m_\x1B[39m\x1B[38;2;255;60;60m/\x1B[39m\x1B[38;2;255;67;67m\\\x1B[39m\x1B[38;2;255;74;74m_\x1B[39m\x1B[38;2;255;81;81m_\x1B[39m\x1B[38;2;255;87;87m_\x1B[39m\x1B[38;2;255;94;94m_\x1B[39m\x1B[38;2;255;101;101m/\x1B[39m\x1B[38;2;255;107;107m\\\x1B[39m\x1B[38;2;255;114;114m_\x1B[39m\x1B[38;2;255;121;121m_\x1B[39m\x1B[38;2;255;128;128m_\x1B[39m\x1B[38;2;255;134;134m/\x1B[39m\x1B[38;2;255;141;141m_\x1B[39m\x1B[38;2;255;148;148m/\x1B[39m\x1B[38;2;255;154;154m \x1B[39m\x1B[38;2;255;161;161m|\x1B[39m\x1B[38;2;255;168;168m_\x1B[39m\x1B[38;2;255;174;174m/\x1B[39m\x1B[38;2;255;181;181m_\x1B[39m\x1B[38;2;255;188;188m/\x1B[39m\x1B[38;2;255;195;195m \x1B[39m\x1B[38;2;255;201;201m \x1B[39m\x1B[38;2;255;208;208m/\x1B[39m\x1B[38;2;255;215;215m_\x1B[39m\x1B[38;2;255;221;221m/\x1B[39m\x1B[38;2;255;228;228m_\x1B[39m\x1B[38;2;255;235;235m_\x1B[39m\x1B[38;2;255;242;242m_\x1B[39m\x1B[38;2;255;248;248m_\x1B[39m\x1B[38;2;255;255;255m/\x1B[39m"
let centered = String.split_on_char '\n' logo |>
               List.map(fun x -> spacer ^ x) |> String.concat "\n"

let set_cursor x y =
  let x' = string_of_int x in
  let y' = string_of_int y in
  "\x1B["^y'^";"^ x'^"f"

let erase_square x1 y1 x2 y2 =
  let empty_row = n_spacer (x2 - x1 + 1) in
  let rec build_string n acc =
    if n < y1 then acc
    else build_string (n-1) (acc ^ set_cursor x1 n ^empty_row)
  in
  "\x1B[s" ^  build_string y2 "" ^ "\x1B[u"


let draw_rect x1 y1 x2 y2 =
  let line = n_builder "─" (^) "" (x2 - x1 - 1) in
  let t_line = "┌" ^ line ^ "┐" in
  let b_line = "└" ^ line ^ "┘" in
  let row = "│" ^ n_spacer(x2 - x1 - 1) ^ "│" in
  let rec build_string n acc =
    if n <= y1 then acc
    else build_string (n-1) (acc ^ set_cursor x1 n ^ row) in
  "\x1B[s" ^ set_cursor x1 y1 ^ t_line ^ build_string (y2-1) "" ^
  set_cursor x1 y2 ^ b_line ^ "\x1B[u"

let rec print_object_tl x y s =
  let x' = string_of_int x in
  let rec helper y s_list acc =
    match s_list with
    | h::t -> begin
        let y' = string_of_int y in
        let acc' = acc ^ "\x1B["^y'^";"^ x'^"f"^h in
        helper (y+1) t acc'
      end
    | [] -> acc in
  "\x1B[s" ^ helper y (String.split_on_char '\n' s) "" ^ "\x1B[u"

let () =
  print_string "\x1Bc";
  flush stdout;
  print_endline "zzzzzzzzzzzzzzzzzzzzzzzzzzz";
  print_endline "zzzzzzzzzzzzzzzzzzzzzzzzzzz";
  print_endline "zzzzzzzzzzzzzzzzzzzzzzzzzzz";
  print_endline "zzzzzzzzzzzzzzzzzzzzzzzzzzz";
  print_endline "zzzzzzzzzzzzzzzzzzzzzzzzzzz";
  print_endline "zzzzzzzzzzzzzzzzzzzzzzzzzzz";
  print_endline "zzzzzzzzzzzzzzzzzzzzzzzzzzz";
  print_endline "zzzzzzzzzzzzzzzzzzzzzzzzzzz";
  print_endline "zzzzzzzzzzzzzzzzzzzzzzzzzzz";
  print_endline "zzzzzzzzzzzzzzzzzzzzzzzzzzz";
  print_endline "zzzzzzzzzzzzzzzzzzzzzzzzzzz";
  print_endline "zzzzzzzzzzzzzzzzzzzzzzzzzzz";
  print_endline "zzzzzzzzzzzzzzzzzzzzzzzzzzz";
  print_endline "zzzzzzzzzzzzzzzzzzzzzzzzzzz";
  draw_rect 2 2 20 8 |> print_string;
  flush stdout
