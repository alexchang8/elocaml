(**[n_builder x f init n] returns [f (f (... init) x) x] with
   a depth of n*)
val n_builder: 'a -> ('b -> 'a -> 'b) -> 'b -> int -> 'b

(**[n_spacer n] returns a string of [n] whitespaces*)
val n_spacer: int -> string

(**[is_mouse_click] returns [Some(x,y)] if s is
   "mouse x y" where x and y are integers. Otherwise
   returns none*)
val is_mouse_click: string -> (int * int) option


(**[sem_backs s] returns s, where the string sequence "\b \b" deletes the
   preceding character.
   Example: [rem_backs "\b \btest\b \babc\b \b\b \b" = "tesa"]*)
val parse_backspace : string -> string

(**[remove_mouse s] returns [s] with any substrings matching
   mouse [0-9]+ [0-9]+ removed.*)
val remove_mouse: string -> string

(**[split_view_string s1 s2] returns a single string where [s1] is printed on the
   left and [s2] is printed on the right. The strings are pretty printed so that
   newlines of each match with each other.
   Requires: every string in [s1] has the same length when printed. Otherwise will
   not show properly*)
val is_chat_click: string -> [> `Chat_Click | `Message | `Non_Chat_Click ]

(**[is_chat_click s] returns [`Chat_click] if [s] is a string representing
   a mouse click on the left side of the screen. Returns [`Non_Chat_Click] if
   s is a mouse click, but on the right side of the screen. Otherwise returns
   [`Message].*)
val split_view_string: string -> string -> string
