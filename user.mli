type t

type login_type = ValidUser of t | InvalidUser of string

(** [create_user name pswd] creates a new user for the server. 
    Requires: [name] is not already taken and [pswd] is valid. *)
val create_user : string -> string -> unit

(** [valid_name name] checks if the username is already taken. If so,
    then false is returned. Otherwise it is true. *)
val valid_name: string -> bool

(** [login name pswd] checks that the current username [name] and corresponding
    password [pswd] are in the system. If the username [name] or password [pswd]
    is not correct, then in an InvalidUser is returned. Otherwise, a ValidUser
    of type t is returned. *)
val login: string -> string -> login_type

(** [get_username user] returns the username of the [user] *)
val get_username: t -> string

(** [get_pswd user] returns the password of the [user] *)
val get_pswd: t -> string

(** [get_wins user] returns the number of wins for the [user] *)
val get_wins: t -> int

(** [get_losses user] returns the number of losses of the [user] *)
val get_losses: t -> int

(** [get_elo user] returns the elo rating of the [user] *)
val get_elo: t -> int

(** [incr_winner winner] increments the wins field of the username [winner] and
    updates the json file. *)
val incr_winner: string -> unit

(** [incr_loser loser] increments the losses field of the username [loser] 
    and updates the json file. *)
val incr_loser: string -> unit