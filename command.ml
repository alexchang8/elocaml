
(* Note: You may introduce new code anywhere in this file. *) 
open Str
type object_phrase = string list

type command = 
  | Place of (int*int)*(int*int)
  | Check of (int*int)
  | PrintMe
  | PrintOpp
  | Invalid
  | Quit

exception Empty

exception Malformed
(**[check_string_form t] returns true if the string has the form of 1 set of 
   coordinates, e. g.  "A1" **)
let check_string_form target =
  try String.length (target) = 2 && 
      let first = int_of_char (String.get target 0) in 
      (64 < first && first <= 90 ) && 
      let second = int_of_string (String.sub target 1 1) in 
      (1 <= second && second <= 26)
  with Failure _ -> false (* Catch int_of_string exceptions *)

(**[check_list_form t] returns true if the list has the form of either 1 or 2 
   sets of coordinates, e. g. ["A1", "A2"]**)
let check_list_form t =
  if List.length t = 1 then 
    let target = (List.hd t) in check_string_form target
  else if List.length t = 2 then
    let target1 = List.hd t in 
    let target2 = List.nth t 1  in 
    check_string_form target1  && check_string_form target2
  else false


let parse (str : string) :  command =
  if str = "" then Invalid
  else let strarray = String.split_on_char ' ' str in
    match strarray with
    |[] -> Invalid
    |h::t ->  if h = "quit" then Quit
      else if List.length t >= 1 then 
        if h = "print" then
          if List.length t = 1 then
            if List.hd t = "me" then PrintMe
            else if  List.hd t = "opponent" then PrintOpp
            else Invalid
          else Invalid

        else if h = "check" then 
          if check_list_form t 
          then try let test = List.hd t in 
              Check(int_of_string (String.sub test 1 1), 
                    int_of_char (String.get test 0) -64) 
            with Failure _ -> Invalid (* catch int_of_string errors. *)
          else Invalid
        else if h = "place" then
          if check_list_form t 
          then try let test1 = (List.hd t) in 
              let test2 = (List.nth t 1) in 
              Place((int_of_string (String.sub test1 1 1), 
                     (int_of_char (String.get test1 0) - 64)), 
                    ((int_of_string (String.sub test2 1 1)), 
                     int_of_char (String.get test2 0) - 64)) 
            with Failure _ -> Invalid (* catch int_of_string errors. *)
          else Invalid 
        else Invalid
      else Invalid


