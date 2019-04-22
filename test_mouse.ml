let parse_click s =
  match String.split_on_char ';' s with
  | c::row::col::[] when c = "0" -> begin
      match int_of_string_opt row, int_of_string_opt col with
      | (Some x, Some y) -> Some(x,y)
      | (None, _) | (_, None) -> None
    end
  | _ -> None

let rec flush_mouse () =
  if input_char stdin = 'M' then
    ANSITerminal.set_cursor 1 1
  else flush_mouse ()

let rec filter_mouse x y =
  let c = input_char stdin in
  if c = '\027' then begin
    flush_mouse ();
    filter_mouse x y
  end
  else if c = '\127' then begin
    output_string stdout "\b \b";
    flush stdout;
    if x =1 then begin
      ANSITerminal.set_cursor 16 (y - 1);
      filter_mouse 16 (y-1)
    end
    else filter_mouse (x-1) y
  end
  else begin
    if x >= 16 then begin
      ANSITerminal.set_cursor 1 (y + 1);
      print_char c;
      flush stdout;
      filter_mouse 1 (y+1)
    end
    else begin
      print_char c;
      flush stdout;
      filter_mouse (x+1) y
    end
  end

let pad_s_list n s s1 =
  let rec helper n s1 =
    if List.length s1 >= n then s1
    else helper n (s::s1)
  in List.rev s1 |> helper n |> List.rev

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
      (pad_s_list ns2_l "                " s1_l, s2_l)
    else (s1_l,s2_l)
  in List.map2 (fun x y -> x ^ "   \226\148\130   " ^ y) s1' s2' |> String.concat "\n"

let _ =
  print_string "\x1Bc";
  print_string "\x1B[?9;1006;1015h";
  flush stdout;
  let termio = Unix.tcgetattr Unix.stdin in
  Unix.tcsetattr Unix.stdin Unix.TCSADRAIN { termio with Unix.c_echo = false; Unix.c_icanon = false };
  filter_mouse 1 1
