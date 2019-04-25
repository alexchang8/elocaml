(**The module holding gui strings and logic for the lobby interface*)

(**The abstract type representing a lobby view*)
type t

(**[next_state t p_id oc s] returns [t] after a client
   inputs [s].*)
val next_state: t -> int -> out_channel -> string -> t

(**[remove_client_id t p_id] returns [t] after the client corresponding
   to [p_id] is removed from the state.*)
val remove_client_id: t -> int -> t

(**[remove_client_id t p_id] gets a list of the p_ids which are disconnected
   from the lobby*)
val get_dced: t -> int list

(**[print_player_state p_id t] returns a string corresponding to a player
   [p_id]'s view of the lobby*)
val print_player_state: int -> t -> string

(**[get_dced] will return [] after this is called on the returned state*)
val flush_dced: t -> t

(**The state with no client connections*)
val init_state: t
