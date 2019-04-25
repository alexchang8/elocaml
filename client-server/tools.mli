val n_builder: 'a -> ('b -> 'a -> 'b) -> 'b -> int -> 'b

val n_spacer: int -> string

val is_mouse_click: string -> (int * int) option

val parse_backspace : string -> string

val remove_mouse: string -> string
