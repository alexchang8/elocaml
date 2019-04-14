ocamlc other options str.cma other files
ocamlopt other options str.cmxa other files
(* Note: You may introduce new code anywhere in this file. *) 

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
let parse str =
	if str = "" then failwith("Empty")
	else let strarray = String.split_on_char ' ' str in
		match strarray with
			|[] -> failwith("Empty")
			|h::t -> if h = "quit" then Quit else if 

				 h = "check" then let test = (List.hd t) in if List.length t = 1 && Str.string_match (Str.regexp "[A-Z][1-26]") test then Check((int_of_char (String.get test 0), int_of_string (String.sub test 1 2))
			 else if h = "place" then let test1 = (List.hd t) in let test2 = (List.tl t) in if List.length t = 2 && Str.string_match (Str.regexp "[A-Z][1-8]") test1 && Str.string_match (Str.regexp "[A-Z][1-26]") test2 then Place(((int_of_char (String.get test1 0) - 64, int_of_string (String.sub test1 1 2)), (int_of_char (String.get test2 0) - 64, int_of_string (String.sub test2 1 2))) else Invalid("Invalid Command")


