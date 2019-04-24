type connnection = {ic: in_channel; oc: out_channel; p_id:int}

let port = 1401

let get_new_connections sock n =
  (*we are assuming that sock has been set to nonblocking*)
  let rec helper acc n =
    try
      let (s, _) = Unix.accept sock in
      print_endline "socket accepted";
      Unix.set_nonblock s;
      let (ic, oc) = (Unix.in_channel_of_descr s, Unix.out_channel_of_descr s) in
      let new_con = {ic = ic; oc = oc; p_id = n} in
      helper (new_con::acc) (n + 1)
    with _ -> (acc, n)
  in
  helper [] n

let next_lobby_state s conn =
  match input_line conn.ic with
  | x -> Lobbyview.next_state s conn.p_id (Tools.parse_backspace x |> Tools.remove_mouse)
  | exception Sys_blocked_io -> s
  | exception End_of_file ->
    failwith "unimplemented"
(**TODO: IMPORTANT: deal with client disconnection *)

let rec game_loop (s:Lobbyview.t) (conns:connnection list) sock n =
  let new_conns, n' = get_new_connections sock n in
  let conns' = List.rev_append new_conns conns in
  let s' = (
    let init_new_c = List.fold_left (fun acc c -> Lobbyview.next_state s c.p_id "") s new_conns in
    List.fold_left next_lobby_state init_new_c conns')
  in
  List.iter(fun c -> Lobbyview.print_player_state c.p_id s' ^ "\nEND_OF_FILE\n" |> output_string c.oc;
             flush c.oc) conns';
  Unix.sleepf 0.1;
  game_loop s' conns' sock n'

let _ =
  (**todo: tell clients that we are still waiting for players*)
  let host_addr = (Unix.gethostbyname(Unix.gethostname())).Unix.h_addr_list.(0) in
  let s_addr = Unix.ADDR_INET(host_addr, port) in
  let sock = Unix.socket (Unix.domain_of_sockaddr s_addr) Unix.SOCK_STREAM 0 in
  Unix.bind sock s_addr;
  Unix.listen sock 20;
  Unix.set_nonblock sock;
  game_loop Lobbyview.init_state [] sock 0
