(**[get_host ()] prompts the user for an ip address that is hosting the game.
   if the ip is invalid then they will be asked again. The valid ip they enter
   is returned as an abstract [Unix.inet_addr]*)
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

(**[get_host ()] prompts the user for an port for the hosting server. If the
   entered string is not an int the user will be prompted again*)
let rec get_port () =
  print_endline "enter the host port:";
  match read_int () with
  | x -> x
  | exception Failure(_) ->
    print_endline "port must be a number!";
    get_port ()

let parse_click s =
  match String.split_on_char ';' s with
  | c::row::col::[] when c = "[<0" -> Some ("mouse " ^ row ^ " " ^ col ^ "\n")
  | _ -> None

let rec flush_mouse () =
  let rec helper acc =
    let c = input_char stdin in
    if c = 'M' then (parse_click acc)
    else helper (acc ^ Char.escaped c)
  in
  helper ""

let rec send_input oc =
  let c = input_char stdin in
  if c = '\027' then begin
    match flush_mouse () with
    | Some x -> output_string oc x; flush oc; send_input oc
    | None -> send_input oc
  end
  else if c = '\127' then begin
    output_string stdout "\b \b";
    flush stdout;
    output_string oc "\b \b";
    flush oc;
    send_input oc
  end
  else begin
    print_char c;
    flush stdout;
    output_char oc c;
    flush oc;
    send_input oc
  end

(**[send_input oc] sends any input a player types into the console to [oc] after
    they press enter. The function will run continuously and never terminate
    properly.*)

(*let rec send_input oc =
  let m = read_line () in
  output_string oc (m ^ "\n");
  flush oc;
  send_input oc*)

(**[receive_state ic] returns a string corresponding to the complete
   game view the client should have based on the response from [ic]. If
   [ic] has no buffered game view, returns "". The end of a game view is marked
   by the string "END_OF_FILE" alone on a line.*)
let receive_state ic =
  let rec helper ic acc =
    match input_line ic with
    | "END_OF_FILE" -> acc
    | x -> helper ic (acc ^ x ^ "\n")
    | exception Sys_blocked_io ->
      (*input channel is blocked*)
      if acc = "" then ""
      else begin
        (*if we have a partial state, try to get it*)
        (*TODO: make sure at some point we discard partial states*)
        Unix.sleepf 0.5;
        helper ic acc
      end
    | exception End_of_file ->
      (*TODO: deal with disconnection to server*)
      failwith "unimplemented"
  in
  helper ic ""

(**[update_view ic old_state] updates the client terminal to be the
    string received from [receive_state ic] if the it is not empty
    and not the same as [old_state]. Continuously polls for new states
    to display to the client. This function will run continuously and
    never terminate properly.*)
let rec update_view ic old_state =
  let new_state = receive_state ic in
  if new_state <> old_state && new_state <> "" then begin
    print_string "\x1Bc";
    print_string "\x1B[?9;1006;1015h";
    print_string new_state;
    flush stdout;
    Unix.sleepf 0.1;
    update_view ic new_state
  end
  else begin
    Unix.sleepf 0.1;
    update_view ic old_state
  end

(**Initializes the overall client by first prompting a user for
   an ip and port, and then initializing a socket connection. The process
   is then forked, so one process continuously polls for client view updates
   and the other process sends any input lines the user writes to the socket*)
let run () =  begin
  ANSITerminal.resize 80 32;
  let host = get_host () in
  let port = get_port () in
  let termio = Unix.tcgetattr Unix.stdin in
  Unix.tcsetattr Unix.stdin Unix.TCSADRAIN { termio with Unix.c_echo = false; Unix.c_icanon = false };
  print_string "\x1B[?9;1006;1015h";
  flush stdout;
  let ic, oc = Unix.open_connection (Unix.ADDR_INET(host, port)) in
  Unix.set_nonblock (Unix.descr_of_in_channel ic);
  match Unix.fork () with
  | 0 -> update_view ic ""
  | _ -> send_input oc
end

let old_terminal_settings = Unix.tcgetattr Unix.stdin

let () = Sys.set_signal Sys.sigint (Sys.Signal_handle(fun x ->
    print_string "\x1B[?9;1006;1015l";
    Unix.tcsetattr Unix.stdin Unix.TCSANOW old_terminal_settings;
    exit 0
  ))

(**Dummy variable to initialize run*)
let _ =
  print_string "\x1Bc";
  print_string "\x1B[?9;1006;1015h";
  flush stdout;
  run ()
