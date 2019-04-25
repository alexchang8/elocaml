open Game
module type Server = sig
  (**Runs the server for a game. The server accepts socket connections
     on the computers internal ip on port 1400.*)
  val run : (in_channel * out_channel * int * string) ->
    (in_channel * out_channel * int * string) -> unit
end

module MakeServer (G:Game):Server = struct

  type connection = {ic: in_channel; oc: out_channel; p_id: int; in_chat: bool ref ;
                     username: string}

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
        let xparsed = Tools.parse_backspace x |> Tools.remove_mouse in
        match is_chat_click x with
        | `Chat_Click -> conn.in_chat := true; print_endline "chat click!"; (s, chat)
        | `Non_Chat_Click -> conn.in_chat := false; (G.parse x |> G.next_state s conn.p_id, chat)
        | `Message -> begin
            if !(conn.in_chat) then (s, Chat.next_state xparsed chat)
            else (G.parse xparsed |> G.next_state s conn.p_id, chat)
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
    List.iter (fun c -> top_view_string ^ split_view_string (Chat.print_chat chat)
                          (G.print_player_state s c.p_id) ^ "\n\nEND_OF_FILE\n" |>
                        output_string c.oc; flush c.oc) conns;
    let s',chat' = List.fold_left c_next_state (s,chat) conns in
    Unix.sleepf 0.1;
    game_loop s' chat' conns

  let run (ic1,oc1,id1,uname1) (ic2,oc2,id2,uname2) =
    Unix.set_nonblock (Unix.descr_of_in_channel ic1);
    Unix.set_nonblock (Unix.descr_of_in_channel ic2);
    let conns = [{ic=ic1; oc=oc1; p_id=id1; in_chat=ref false; username=uname1};
                 {ic=ic2; oc=oc2; p_id=id2; in_chat=ref false; username=uname2}] in
    game_loop (G.init_state) (Chat.init_state) conns
end
