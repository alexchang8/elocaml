(**the module that contains functions for updating usernames, passwords, and elo*)
type t

type login_type = ValidUser of t | InvalidUser of string

(** [create_user name pswd json_path] creates a new user to store in the
    specified path [json_path].
    Requires: [name] is not already taken and [pswd] is valid. *)
val create_user : string -> string -> string -> unit

(** [valid_name name json_path] checks if the username is already taken 
    in the file [json_path]. If so, then false is returned. 
    Otherwise it is true. *)
val valid_name: string -> string -> bool

(** [login name pswd json_path] checks that the current username [name] and 
    corresponding password [pswd] are in the database at [json_path]. If the 
    username [name] or password [pswd] is not correct, then in an InvalidUser 
    is returned. Otherwise, a ValidUser of type t is returned. *)
val login: string -> string -> string -> login_type

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

(** [incr_winner winner json_path] increments the wins field of the username 
    [winner] and updates the json file at [json_path]. *)
val incr_winner: string -> string -> unit

(** [incr_loser loser json_path] increments the losses field of the username
    [loser] and updates the json file at [json_path]. *)
val incr_loser: string -> string -> unit
