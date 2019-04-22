type t

val init_state : t

val next_state : string -> t -> t

val print_chat : t -> string
