(**The module that holds chat logic*)

(**The abstract type representing a chat state*)
type t

(**the empty chat*)
val init_state : t

(**[next_state s t] returns the state of the chat after [s] is inserted
   into the chat*)
val next_state : string -> t -> t

(**[print_chat t] returns a string that represents a properly formatted
   view of chat*)
val print_chat : t -> string
