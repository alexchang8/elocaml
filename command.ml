
(* Note: You may introduce new code anywhere in this file. *) 
open Str
type object_phrase = string list

type command = 
  | Place of (int*int)*(int*int)
  | Check of (int*int)
  | Invalid
  | Quit

exception Empty

exception Malformed
(**[parse str] takes string [str] and splits it at any space characters, returning empty for the empty
string, a command of value Quit if the user typed quit, or a Go command with value equal to a list of all strings following the first if the first string is go**)
let parse (str : string) :  command =
	if str = "" then Invalid
	else let strarray = String.split_on_char ' ' str in
		match strarray with
			|[] -> Invalid
			|h::t -> if h = "quit" then Quit else if 
			h = "check" then let test = (List.hd t) in if List.length t = 1 && Str.string_match (Str.regexp "[A-Z][1-26]") test 0 then Check(int_of_char (String.get test 0), int_of_string (String.sub test 1 2)) else Invalid
			 else if h = "place" then let test1 = (List.hd t) in let test2 = (List.nth t 1) in if List.length t = 2 && Str.string_match (Str.regexp "[A-Z][1-8]") test1 0 && Str.string_match (Str.regexp "[A-Z][1-26]") test2 0 then Place((int_of_char (String.get test1 0) - 64, int_of_string (String.sub test1 1 2)), (int_of_char (String.get test2 0) - 64, int_of_string (String.sub test2 1 2))) else Invalid else Invalid


