type t

val next_state: t -> int -> out_channel -> string -> t

val remove_client_id: t -> int -> t

val get_dced: t -> int list

val print_player_state: int -> t -> string

val flush_dced: t -> t

val init_state: t
