type connnection = {ic: in_channel; oc: out_channel; p_id:int}

let port = 1401

let rec get_new_connections sock conns n =
  (*we are assuming that sock has been set to nonblocking*)
  try
    let (s, _) = Unix.accept sock in
    let (ic, oc) = (Unix.in_channel_of_descr s, Unix.out_channel_of_descr s) in
    let p_id = List.length conns in
    let conns' = ({ic = ic; oc = oc; p_id = p_id} :: conns) in
    get_new_connections sock conns' (n + 1)
  with _ -> (conns, n)

let rec game_loop (s:Lobbyview.t) (conns:connnection list) (n:int) =
  failwith "unimplemented"

let _ =
  (**todo: tell clients that we are still waiting for players*)
  let host_addr = (Unix.gethostbyname(Unix.gethostname())).Unix.h_addr_list.(0) in
  let s_addr = Unix.ADDR_INET(host_addr, port) in
  let sock = Unix.socket (Unix.domain_of_sockaddr s_addr) Unix.SOCK_STREAM 0 in
  Unix.bind sock s_addr;
  Unix.listen sock 20;
  Unix.set_nonblock sock
