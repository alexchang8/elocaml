open Game
module type Server = sig
  (**Runs the server for a game. The server accepts socket connections
     on the computers internal ip on port 1400.*)
  val run : unit -> unit
end

module MakeServer (G:Game) = struct

  type connection = {ic: in_channel; oc: out_channel; p_id: int}
  let port = 1400
  let wait_players sock =
    let rec client_helper sock acc =
      if List.length acc < G.max_players then
        let (s, _) = Unix.accept sock in
        print_endline "socket accepted";
        (*Lets us read multiple sockets in a single thread*)
        Unix.set_nonblock s;
        let (ic, oc) = (Unix.in_channel_of_descr s, Unix.out_channel_of_descr s) in
        if 1 + List.length acc < G.max_players then
          (output_string oc "waiting for more players\nEND_OF_FILE\n";
           flush oc);
        let player = {ic = ic; oc = oc; p_id = List.length acc} in
        client_helper sock (player::acc)
      else acc in
    client_helper sock []

  let c_next_state s conn =
    match input_line conn.ic with
    | x -> G.parse x |> G.next_state s conn.p_id
    | exception Sys_blocked_io -> s
    | exception End_of_file ->
      (*TODO: wait for client reconnection*)
      failwith "unimplmented"

  let rec game_loop (s:G.t) conns =
    (*TODO: resolve when there are multiple endlines*)
    let s' = List.fold_left c_next_state s conns in
    List.iter (fun c -> (G.print_player_state s c.p_id) ^ "\nEND_OF_FILE\n" |>
                        output_string c.oc; flush c.oc) conns;
    Unix.sleepf 0.1;
    game_loop s' conns

  let run () =
    (**todo: tell clients that we are still waiting for players*)
    let host_addr = (Unix.gethostbyname(Unix.gethostname())).Unix.h_addr_list.(0) in
    let s_addr = Unix.ADDR_INET(host_addr, port) in
    let sock = Unix.socket (Unix.domain_of_sockaddr s_addr) Unix.SOCK_STREAM 0 in
    Unix.bind sock s_addr;
    Unix.listen sock 5;
    game_loop G.init_state (wait_players sock)

end
