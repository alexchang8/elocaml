(**TODO: implemented replay after win*)
let rec get_host () =
  print_endline "enter the host ip:";
  let host = read_line () in
  match Unix.inet_addr_of_string host with
  | x -> x
  | exception Failure(_) ->
    match (Unix.gethostbyname host).Unix.h_addr_list.(0) with
    | x -> x
    | exception Not_found ->
      print_endline "unknown server";
      get_host ()

let rec get_port () =
  print_endline "enter the host port:";
  match read_int () with
  | x -> x
  | exception Failure(_) ->
    print_endline "port must be a number!";
    get_port ()

let rec send_input oc =
  let m = read_line () in
  output_string oc (m ^ "\n");
  flush oc;
  send_input oc

let rec receive_state ic acc =
  match input_line ic with
  | "END_OF_FILE" -> acc
  | x -> receive_state ic (acc ^ x ^ "\n")
  | exception Sys_blocked_io ->
    (*input channel is blocked*)
    if acc = "" then ""
    else begin
      (*if we have a partial state, try to get it*)
      (*TODO: make sure at some point we discard partial states*)
      Unix.sleepf 0.5;
      receive_state ic acc
    end
  | exception End_of_file ->
    (*TODO: deal with disconnection to server*)
    failwith "unimplemented"

let rec update_view ic old_state =
  let new_state = receive_state ic "" in
  if new_state <> old_state && new_state <> "" then begin
    (*ANSITerminal.erase ANSITerminal.Screen;*)
    (*ANSITerminal.set_cursor 0 0;
      ANSITerminal.print_string [] new_state;*)
    print_string "\x1Bc";
    print_string new_state;
    flush stdout;
    Unix.sleepf 0.1;
    update_view ic new_state
  end
  else begin
    Unix.sleepf 0.1;
    update_view ic old_state
  end


let run () =  begin
  ANSITerminal.resize 80 32;
  let host = get_host () in
  let port = get_port () in
  let ic, oc = Unix.open_connection (Unix.ADDR_INET(host, port)) in
  Unix.set_nonblock (Unix.descr_of_in_channel ic);
  match Unix.fork () with
  | 0 -> update_view ic ""
  | _ -> send_input oc
end

let _ =
  print_string "\x1Bc";
  run ()
