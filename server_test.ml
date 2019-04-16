let max_players = 2
let port = 1400


let init_state = ""
let next_state state command = state ^ command ^ "\n"

let wait_players sock =
  (*TODO: check for disconnections*)
  let rec client_helper sock acc =
    if List.length (fst acc) < max_players then
      let (s, _) = Unix.accept sock in
      print_endline "socket accepted";
      (*Lets us read multiple sockets in a single thread*)
      Unix.set_nonblock s;
      let (ic, oc) = (Unix.in_channel_of_descr s, Unix.out_channel_of_descr s) in
      if 1 + (fst acc |> List.length) < max_players then
        (output_string oc "waiting for more players\nEND_OF_FILE\n";
         flush oc);
      client_helper sock (ic::fst acc, oc::snd acc)
    else acc in
  client_helper sock ([],[])

let ic_next_state s ic =
  match input_line ic with
  | x -> next_state s x
  | exception Sys_blocked_io -> s
  | exception End_of_file ->
    (*TODO: wait for client reconnection*)
    failwith "unimplmented"

let rec game_loop s ics ocs =
  print_endline "beginning game loop";
  (*TODO: resolve when there are multiple endlines*)
  let s' = List.fold_left ic_next_state s ics in
  print_endline s';
  List.iter (fun oc -> output_string oc (s' ^ "END_OF_FILE\n"); flush oc) ocs;
  Unix.sleepf 0.1;
  game_loop s' ics ocs

let run () =
  (**todo: tell clients that we are still waiting for players*)
  let host_addr = (Unix.gethostbyname(Unix.gethostname())).Unix.h_addr_list.(0) in
  let s_addr = Unix.ADDR_INET(host_addr, port) in
  let sock = Unix.socket (Unix.domain_of_sockaddr s_addr) Unix.SOCK_STREAM 0 in
  Unix.bind sock s_addr;
  Unix.listen sock (max_players + 1);
  let (ics, ocs) = wait_players sock in
  game_loop init_state ics ocs
(*begin the game loop with collected clients*)

let _ = run ()
