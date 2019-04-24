open Game
module type Server = sig
  (**Runs the server for a game. The server accepts socket connections
     on the computers internal ip on port 1400.*)
  val run : unit -> unit
end


module MakeServer (G:Game) = struct
  (**A type that holds the input channel, output channel, and player id
     of a single client*)
  type connection = {ic: in_channel; oc: out_channel; p_id: int; in_chat: bool ref}
  (**The port that the server will listen for players on*)
  let port = 1400

  (**[wait_players sock] blocks until [G.max_players] clients have connected
     to the server. Returns a [connection list] of [G.max_players] elements
     corresponding to each client.*)
  let wait_players sock =
    let rec client_helper sock acc =
      if List.length acc < G.max_players then
        let (s, _) = Unix.accept sock in
        print_endline "socket accepted";
        (*Lets us read multiple sockets in a single thread*)
        Unix.set_nonblock s;
        let (ic, oc) = (Unix.in_channel_of_descr s, Unix.out_channel_of_descr s) in
        if 1 + List.length acc <= G.max_players then begin
          let (x,y) = G.terminal_size in
          let rterm = "\x1B[8;" ^ string_of_int y ^ ";"^ string_of_int x ^"t" in
          output_string oc (rterm ^ "waiting for more players\nEND_OF_FILE\n");
          flush oc
        end;
        let player = {ic = ic; oc = oc; p_id = List.length acc; in_chat = ref false} in
        client_helper sock (player::acc)
      else acc in
    client_helper sock []

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

  (**[c_next_state s conn] reads an input line from a particular client,
     and if available returns the result of parsing the line and finding the next
     state. Returns [s] if no input line is available*)
  let c_next_state (s,chat) conn =
    match input_line conn.ic with
    | x -> begin
        match is_chat_click x with
        | `Chat_Click -> conn.in_chat := true; print_endline "chat click!"; (s, chat)
        | `Non_Chat_Click -> conn.in_chat := false; (G.parse x |> G.next_state s conn.p_id, chat)
        | `Message -> begin
            if !(conn.in_chat) then (s, Chat.next_state x chat)
            else (G.parse x |> G.next_state s conn.p_id, chat)
          end
      end
    | exception Sys_blocked_io -> (s, chat)
    | exception End_of_file ->
      (*TODO: wait for client reconnection*)
      failwith "unimplmented"

  let top_view_string = " \226\143\187 │                                     " ^ G.name ^ "\n───┴────────────────────────────────────────────────────────────────────────────\n"


  (**[game_loop s conns] continuously reads all of the input channels in [conns]
     for input lines, and parses each of them into a command, and storing the mutated
     game state in sequence. If there are multiple connections with pending buffers,
     the order of this sequence is not gauranteed. This function will never terminate properly.*)
  let rec game_loop (s:G.t) (chat:Chat.t) conns =
    (*TODO: resolve when there are multiple endlines*)
    (*TODO: use cursor movement instead of clearing the entire screen *)
    (*TODO: pass mouse click offset instead of absolute position *)
    List.iter (fun c -> top_view_string ^ split_view_string (Chat.print_chat chat)
                          (G.print_player_state s c.p_id) ^ "\nEND_OF_FILE\n" |>
                        output_string c.oc; flush c.oc) conns;
    let s',chat' = List.fold_left c_next_state (s,chat) conns in
    Unix.sleepf 0.1;
    game_loop s' chat' conns

  (**[run ()] initializes the server by binding a socket to the computers local ip
     and [port]. It then continuously runs game_loop to send updated states to clients*)
  let run () =
    (**todo: tell clients that we are still waiting for players*)
    let host_addr = (Unix.gethostbyname(Unix.gethostname())).Unix.h_addr_list.(0) in
    let s_addr = Unix.ADDR_INET(host_addr, port) in
    let sock = Unix.socket (Unix.domain_of_sockaddr s_addr) Unix.SOCK_STREAM 0 in
    Unix.bind sock s_addr;
    Unix.listen sock 5;
    game_loop G.init_state Chat.init_state (wait_players sock)

end
