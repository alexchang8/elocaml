open Yojson.Basic
open Yojson.Basic.Util
open Sys

type t = {
  username:string;
  password:string;
  wins:int;
  losses:int;
  elo:int
}

type login_type = ValidUser of t | InvalidUser of string

let get_username user = user.username
let get_pswd user = user.password 
let get_wins user = user.wins
let get_losses user = user.losses 
let get_elo user = user.elo

(** [rewrite_file fname new_json] overwrites the previous file of name [fname]
    with the new json content [new_json]. *)
let rewrite_file fname new_json = 
  let () = Sys.remove fname in
  to_file fname new_json

(** [make_assoc name pswd w l elo] creates a Yojson.Basic.json representation
    of the user with username [name], password [pswd], wins [w], losses [l],
    and elo ranking [elo]. *)
let make_assoc name pswd w l elo : Yojson.Basic.json = 
  `Assoc [
    ("username", `String(name));
    ("password", `String (pswd));
    ("wins", `Int (w));
    ("losses", `Int (l));
    ("elo", `Int (elo))]

(** [update_json users] creates a new json representation for the total
    amount of users and the new user list [users]. *)
let update_json users : Yojson.Basic.json =
  let tot = List.length users in
  `Assoc [
    ("total users", `Int (tot));
    ("users", `List (users))
  ]

(** [create_user name pswd json_path] creates a new user to store in the
    specified path [json_path].
    Requires: [name] is not already taken and [pswd] is valid. *)
let create_user name pswd json_path = 
  let json = from_file json_path in
  let cur_users = json |> member "users" |> to_list in
  let new_user = make_assoc name pswd 0 0 (List.length cur_users + 1) in
  let new_json = update_json(new_user::cur_users) in
  rewrite_file json_path new_json


(** [valid_name name json_path] checks if the username is already taken 
    in the file [json_path]. If so, then false is returned. 
    Otherwise it is true. *)
let valid_name name json_path = 
  let json = from_file json_path in
  let user_list = json |> member "users" |> to_list in
  let f acc user = (user |> member "username" |> to_string)::acc in
  let names = List.fold_left f [] user_list in
  not (List.mem name names)

(** [get_assoc name json_path ] is the function that grabs the user in the 
    form of the type Yojson.Basic.json for the specified username [name] and
    file location [json_path].
    Requires: [name] is a valid username in the database. *)
let get_assoc name json_path = 
  let json = from_file json_path in
  let users = json |> member "users" |> to_list in
  let rec helper s = 
    match s with
    | [] -> failwith "RI does not hold"
    | h::t -> 
      if h |> member "username" |> to_string = name
      then h
      else helper t
  in helper users

(** [login name pswd json_path] checks that the current username [name] and 
    corresponding password [pswd] are in the database at [json_path]. If the 
    username [name] or password [pswd] is not correct, then in an InvalidUser 
    is returned. Otherwise, a ValidUser of type t is returned. *)
let login name pswd json_path  = 
  if valid_name name json_path (* valid_name is true if name does not exist *)
  then InvalidUser ("Username: " ^ name ^ " does not exist.")
  else let assoc = get_assoc name json_path in
    if assoc |> member "password" |> to_string <> pswd
    then InvalidUser ("Password does not match")
    else ValidUser (
        {
          username=name;
          password=pswd;
          wins=assoc |> member "wins" |> to_int;
          losses=assoc |> member "losses" |> to_int;
          elo=assoc |> member "elo" |> to_int;
        }
      )

(** [same_user h u] returns the boolean corresponding to if the two users
    [h] and [u] are the same. Two users are the same if they have the same 
    username.
    RI: All usernames are unique identifiers for the users. *)
let same_user h u = 
  h |> member "username" |> to_string = (u |> member "username" |> to_string)

(** [update_users users updated] replaces the instance of [updated] in the
    user list [users]. If no instance of [updated] is found, then [users] is
    unchanged. *)
let update_users users updated = 
  let rec helper acc lst = 
    match lst with
    | [] -> acc
    | h::t -> 
      if same_user h updated 
      then t @ (updated::acc)
      else helper (h::acc) t
  in helper [] users

(** [incr_user user won json_path] updates the [user] in the json at [json_path]
    by incrementing their wins count if [won] and incrementing their losses 
    count if not [won]. The updates rewrite the json file. *)
let incr_user user won json_path = 
  let assoc = get_assoc user json_path in
  let w, l = if won 
    then ((assoc |> member "wins" |> to_int |> (+) 1),
          (assoc |> member "losses" |> to_int))
    else ((assoc |> member "wins" |> to_int),
          (assoc |> member "losses" |> to_int |> (+) 1)) in
  let new_user = make_assoc
      (assoc |> member "username"|> to_string)
      (assoc |> member "password" |> to_string)
      w
      l
      (assoc |> member "elo" |> to_int) in
  let json = from_file json_path in
  let user_lst = update_users (json |> member "users" |> to_list) new_user in
  let new_json = update_json user_lst
  in rewrite_file json_path new_json

(** [incr_winner winner json_path] increments the wins field of the username 
    [winner] and updates the json file at [json_path]. *)
let incr_winner winner json_path = incr_user winner true json_path

(** [incr_loser loser json_path] increments the losses field of the username 
    [loser] and updates the json file at [json_path]. *)
let incr_loser loser json_path = incr_user loser false json_path
