open Game
open Tools

module type Server = sig
  val run : (in_channel * out_channel * int * string) ->
    (in_channel * out_channel * int * string) -> unit
end

module MakeServer (G:Game):Server = struct

  (**the type representing a client connection to this server*)
  type connection = {ic: in_channel; oc: out_channel; p_id: int; in_chat: bool ref ;
                     username: string; enabled: bool ref}

  (**[c_next_state s conn] reads an input line from a particular client,
     and if available returns the result of parsing the line and finding the next
     state. Returns [s] if no input line is available*)
  let c_next_state (s,chat) conn =
    match input_line conn.ic with
    | x  when x <> "" -> begin
        let xparsed = Tools.parse_backspace x |> Tools.remove_mouse in
        if !(conn.enabled) then begin
          if x = "__DISABLE__" then begin
            print_endline "disbaling!";
            conn.enabled := false;
            (s, chat)
          end
          else
            match is_chat_click x with
            | `Chat_Click -> conn.in_chat := true; print_endline "chat click!"; (s, chat)
            | `Non_Chat_Click -> conn.in_chat := false; (G.parse x |> G.next_state s conn.p_id, chat)
            | `Message -> begin
                if !(conn.in_chat) then (s, Chat.next_state (conn.username ^ ": " ^ xparsed) chat)
                else (G.parse xparsed |> G.next_state s conn.p_id, chat)
              end
        end
        else
          let () = if xparsed = "__ENABLE__" then (conn.enabled := true) in
          (s, chat)
      end
    | x -> (s, chat)
    | exception Sys_blocked_io -> (s, chat)
    | exception End_of_file ->
      (*TODO: wait for client reconnection*)
      failwith "unimplmented"

  (**The string which when printed resizes the terminal to the coordinates
     in G.terminal_size*)
  let rterm =
    let (x,y) = G.terminal_size  in
    "\x1B[8;" ^ string_of_int y ^ ";"^ string_of_int x ^"t"

  (**[game_loop s conns] continuously reads all of the input channels in [conns]
     for input lines, and parses each of them into a command, and storing the mutated
     game state in sequence. If there are multiple connections with pending buffers,
     the order of this sequence is not gauranteed. This function will never terminate properly.*)
  let rec game_loop (s:G.t) (chat:Chat.t) conns =
    List.iter (fun c ->
        let top_view_string = "                         | Home | Tab 1 | Tab 2 | Tab 3 |        user: "
                              ^ c.username ^ "\n" ^ "╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌\n" in
        let chat_tip = if !(c.in_chat) then "\n(chat): " else "\n(game command): " in
        if !(c.enabled) then (rterm ^ top_view_string ^ (split_view_string (Chat.print_chat chat)
                                                           (G.print_player_state s c.p_id)) ^ chat_tip ^ "\nEND_OF_FILE\n" |>
                              output_string c.oc; flush c.oc)) conns;
    let s',chat' = List.fold_left c_next_state (s,chat) conns in
    Unix.sleepf 0.1;
    game_loop s' chat' conns

  let run (ic1,oc1,id1,uname1) (ic2,oc2,id2,uname2) =
    Unix.set_nonblock (Unix.descr_of_in_channel ic1);
    Unix.set_nonblock (Unix.descr_of_in_channel ic2);
    let conns = [{ic=ic1; oc=oc1; p_id=id1; in_chat=ref false; username=uname1;
                  enabled = ref true};
                 {ic=ic2; oc=oc2; p_id=id2; in_chat=ref false; username=uname2;
                  enabled=ref true}] in
    game_loop (G.init_state) (Chat.init_state) conns
end
