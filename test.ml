open OUnit2
open Player
open Command
open User
open Yojson.Basic

let init_player_test
    (name:string)
    (x:int)
    (y:int)
    (p_name:int)
    (player:Player.t) : test =
  name >:: (fun _ ->
      assert_equal p_name (get_name player);
      assert_equal (x, y) (get_shape player);
      assert_equal [] (get_ships player) )

let insert_ship_test
    (name:string)
    (board:board)
    (b:board) : test =
  name >:: (fun _ ->
      assert_equal b board;)

let already_guessed_test
    (name:string)
    (p:Player.t)
    (c:(int*int))
    (x:bool) :test =
  name >:: (fun _ ->
      assert_equal (already_guessed p c) x)

let p_1by1 = init_player 1 1 1
let p_4by3 = init_player 4 3 1
let p_5by5_2 = init_player 5 5 2
let p_5by5_1 = init_player 5 5 1

let init_tests = [
  init_player_test "Initializing empty player" 1 1 1 p_1by1;
  init_player_test "Initial Rectangle Board" 4 3 1 p_4by3;
  init_player_test "Not player 1" 5 5 2 p_5by5_2
]

let three_piece_v = [[Ship; Empty; Empty]; [Ship; Empty; Empty]; [Ship; Empty; Empty];
                     [Empty; Empty; Empty]]
let ins_3_ship = insert_ship p_4by3 (make_coord (1,1)) (make_coord (1,3)) 3
let four_piece_h = [[Empty; Empty; Empty; Empty; Empty]; [Empty; Ship; Ship; Ship; Ship];
                    [Empty; Empty; Empty; Empty; Empty]; [Empty; Empty; Empty; Empty; Empty];
                    [Empty; Empty; Empty; Empty; Empty]]
let ins_4_ship = insert_ship p_5by5_1 (make_coord (2,2)) (make_coord (5,2)) 4

let give_t v =
  match v with
  |ValidB t-> t
  |_-> failwith "not important"

let var1 = give_t ins_3_ship
let var2 = give_t ins_4_ship

let guess1 = check var2 (2,2)
let guess2 = check var2 (5,5)
let guess3 = check var1 (1,1)
let guess4 = check var1 (1,2)
let guess5 = check var1 (1,3)

let give_t_2 g =
  match g with
  |Continue (t, _) -> t
  |_->failwith "not important"

let var3 = give_t_2 guess1
let var4 = give_t_2 guess2
let var5 = give_t_2 guess5

let insert_tests = [
  insert_ship_test "Testing 3 piece ship on 4x3" (three_piece_v) (get_board var1);
  insert_ship_test "Testing 4 piece ship on 5x5" (four_piece_h) (get_board var2);
]

let already_guessed_tests = [
  already_guessed_test "Has not guessed A1" var1 (1,1) false;
  already_guessed_test "Has not guessed A2" var1 (1,2) false;
  already_guessed_test "Guessed B2" var3 (2,2) true;
  already_guessed_test "Guessed B2" var4 (2,2) false;
  already_guessed_test "Guesses and Sunk" var5 (3,3) false;
]

let string_form_test
    (name:string)
    (s:string)
    (expected:bool) : test =
  name >:: (fun _ ->
      assert_equal expected (check_string_form s) ~printer:string_of_bool;
    )

let string_list_test
    (name:string)
    (sl:string list)
    (exp:bool) : test =
  name >:: (fun _ ->
      assert_equal exp (check_list_form sl) ~printer:string_of_bool)

let parse_test
    (name:string)
    (s:string)
    (exp:Command.command) : test =
  name >:: (fun _ ->
      assert_equal exp (parse s))

let command_tests = [
  string_form_test "Valid form" "A1" true;
  string_form_test "Max Letter" "Z1" true;
  (* Update below test to A26 once numbers can reach double digits *)
  string_form_test "Max number" "A9" true;
  string_form_test "Lowercase letter" "a1" false;
  string_form_test "Using 0" "A0" false;
  string_form_test "Random values" "H4" true;
  string_form_test "Number out of range" "C42" false;
  string_form_test "Not a number" "Cc" false;
  string_form_test "Not a char" "11" false;
  (* BEGIN STRING LIST TESTS *)
  string_list_test "Valid 1 coordinate" ["A1"] true;
  string_list_test "Valid 2 coordinates" ["A1"; "B1"] true;
  string_list_test "Valid same coordinates" ["A1"; "A1"] true;
  string_list_test "Max Letter in coord" ["Z1"] true;
  (* Update below test to A26 once numbers can reach double digits *)
  string_list_test "Max number in coord" ["A9"] true;
  string_list_test "Lowercase letter" ["a1"] false;
  string_list_test "Using 0" ["A0"] false;
  string_list_test "Random values" ["H4"] true;
  string_list_test "Number too large" ["C42"] false;
  string_list_test "Not a number" ["Bb"] false;
  string_list_test "Not a char" ["11"] false;
  string_list_test "2 coords: Max Letter in coord" ["Z1"; "B3"] true;
  (* Update below test to A26 once numbers can reach double digits *)
  string_list_test "2 coords: Max number in coord" ["A9"; "B3"] true;
  string_list_test "2 coords: Lowercase letter in first coord" ["a1"; "B3"] false;
  string_list_test "2 coords: Lowercase letter in second coord" ["B3"; "a1"] false;
  string_list_test "2 coords: Using 0" ["A0"; "B3"] false;
  string_list_test "2 coords: Using 0 in second coord" ["B3"; "A0"] false;
  string_list_test "2 coords: Random values" ["H4"; "U4"] true;
  string_list_test "2 coords: Number too large first coord" ["C42"; "B3"] false;
  string_list_test "2 coords: Number too large second coord" ["B3"; "C42"] false;
  string_list_test "2 coords: Not a number" ["Bb"; "A2"] false;
  string_list_test "2 coords: Not a number" ["A2"; "Bb"] false;
  string_list_test "2 coords: Not a char" ["11"; "A5"] false;
  string_list_test "2 coords: Not a char" ["A5"; "11"] false;
  (* BEGIN PARSE TESTS *)
  parse_test "Empty string" "" Invalid;
  parse_test "Only white space" "    " Invalid;
  parse_test "Quitting" "quit" Quit;
  parse_test "Case sensitive quit" "Quit" Invalid;
  parse_test "Quit with extra space" "   quit   " Invalid;
  parse_test "Print with no label" "print" Invalid;
  parse_test "Print own board" "print me" PrintMe;
  parse_test "Case sensitive print me" "Print Me" Invalid;
  parse_test "Print opp board" "print opponent" PrintOpp;
  parse_test "Case sensitive print opp" "Print Opponent" Invalid;
  parse_test "Print extra spaces in middle" "print      me" Invalid;
  parse_test "Invalid print term" "print Google" Invalid;
  parse_test "Random invalid letters" "bdklhfewow" Invalid;
  parse_test "Check with no coord" "check" Invalid;
  parse_test "Valid check" "check A1" (Check (1,1));
  parse_test "Invalid coordinate" "check 11" Invalid;
  parse_test "Invalid coordinate (2)" "check C42" Invalid;
  parse_test "Case sensitive coordinate" "check a1" Invalid;
  parse_test "Check random coordinate" "check C5" (Check (5, 3));
  parse_test "Check case sensitive" "Check C2" Invalid;
  parse_test "Place normal" "place A1 A3" (Place ((1,1), (3,1)));
  parse_test "Place no coordinates" "place" Invalid;
  parse_test "Place one coordinate" "place D3" Invalid;
  parse_test "Place case sensitive" "Place A1 A3" Invalid;
  parse_test "Place invalid first coord" "place h3 H4" Invalid;
  parse_test "Place invalid second coord" "place C3 c4" Invalid;
  parse_test "Place both coords invalid" "place c1 c3" Invalid;
  parse_test "Place coordinate with zero" "place C0 C5" Invalid;
  parse_test "Place allow diagonal" "place C1 H4" (Place ((1, 3), (4, 8)));
  parse_test "Place first coord invalid size" "place C H4" Invalid
]

(* TODO Go tests commented out due to compilation errors. *)
(* let go_parse_test (name:string) (s: string) (e: Go.command) : test =
   name >:: (fun _ -> assert_equal (Go.parse s) e;)


   let next_state_test (name: string) (b: Go.cell array array) (e: Go.cell array array) : test =
   let t = {Go.init_state with board = b} in
   name >:: (fun _ -> assert_equal e ((Go.next_state t 0 (Go.Place(9,9)))).board)

   let go_command_tests = [
   go_parse_test "pass" "pass" Pass;
   go_parse_test "invalid test" "ksdjb" Invalid;
   go_parse_test "lowercase test" "a1" Invalid;
   go_parse_test "out of bounds test number" "a17" Invalid;
   go_parse_test "out of bounds test letter" "U7" Invalid;
   go_parse_test "normal test 1" "A19" (Go.Place (0,0));
   go_parse_test "normal test 2" "A1" (Go.Place (18,0));
   go_parse_test "normal test 3" "K10" (Go.Place (9,9));
   go_parse_test "out of bounds test letter" "I7" Invalid;
   go_parse_test "normal test 4" "Q16" (Go.Place (3,15));
   go_parse_test "double test" "A1 A1" Invalid;
   ]

   let b1 =
   let b1 = (Array.copy Go.init_board) in
   b1.(0).(0) <- Go.Black;
   b1.(0).(1) <- Go.Black;
   b1.(1).(0) <- Go.White;
   b1.(1).(1) <- Go.White;
   b1.(0).(2) <- Go.White;
   b1

   let b2 =
   let b2 = (Array.copy Go.init_board) in
   b2.(1).(0) <- Go.White;
   b2.(1).(1) <- Go.White;
   b2.(0).(2) <- Go.White;
   b2.(9).(9) <- Go.White;
   b2

   let remove_dead_stones_test (name: string) (b:Go.cell array array) (c:Go.cell) (e: int)
   : test =
   name >:: (fun _ -> assert_equal e (Go.remove_dead_stones c b (Go.stones b));)

   let go_board_tests = [
   remove_dead_stones_test "remove dead test" b1 Black 2;
   remove_dead_stones_test "no dead stones test1" b1 White 0;
   remove_dead_stones_test "no dead stones test2" b2 White 0;
   remove_dead_stones_test "no dead stones test3" b2 Black 0;
   next_state_test "remove test" b1 b2
   ]

   let go_suite = "Go Test Suite" >::: List.flatten [
    go_command_tests;
    go_board_tests
   ] *)
let rewrite_file fname new_json = 
  let () = Sys.remove fname in
  to_file fname new_json

let bjson = "btest.json"
let reset_json = from_file bjson
let user1 = match (User.login "Nate" "42" bjson) with ValidUser t -> t | _ -> failwith "Not true"
let user2 = match (User.login "Ocaml" "ocaml" bjson) with ValidUser t -> t | _ -> failwith "Not true"
let fake_name = User.login "foo" "42" bjson
let fake_pswd = User.login "Nate" "foo" bjson
let () = incr_winner "Nate" bjson
let nate43_0 = match (User.login "Nate" "42" bjson) with ValidUser t -> t | _ -> failwith "Not true"
let ocaml_after_incr_nate = match (User.login "Ocaml" "ocaml" bjson) with ValidUser t -> t | _ -> failwith "Not true"
let () = incr_loser "Ocaml" bjson
let ocaml0_43 = match (User.login "Ocaml" "ocaml" bjson) with ValidUser t -> t | _ -> failwith "Not true"
let nate_after_incr_ocaml = match (User.login "Nate" "42" bjson) with ValidUser t -> t | _ -> failwith "Not true"
let () = incr_loser "Nate" bjson
let nate43_1 = match (User.login "Nate" "42" bjson) with ValidUser t -> t | _ -> failwith "Not true"
let ()  = incr_winner "Ocaml" bjson
let ocaml1_43 = match (User.login "Ocaml" "ocaml" bjson) with ValidUser t -> t | _ -> failwith "Not true"
let () = rewrite_file bjson reset_json (* Reset for future tests. *)

let user_name_test
    (name:string)
    (user:User.t)
    (ex:string) : test =
  name >:: (fun _ ->
      assert_equal ex (User.get_username user))

let user_pswd_test
    (name:string)
    (user:User.t)
    (ex:string) : test =
  name >:: (fun _ ->
      assert_equal ex (User.get_pswd user))

let user_wins_test
    (name:string)
    (user:User.t)
    (ex:int) : test =
  name >:: (fun _ ->
      assert_equal ex (User.get_wins user))

let user_losses_test
    (name:string)
    (user:User.t)
    (ex:int) : test =
  name >:: (fun _ ->
      assert_equal ex (User.get_losses user))

let user_elo_test
    (name:string)
    (user:User.t)
    (ex:int) : test =
  name >:: (fun _ ->
      assert_equal ex (User.get_elo user))

let invalid_user_test
    (name:string)
    (user:User.login_type)
    (ex:User.login_type) : test = 
  name >:: (fun _ -> 
      assert_equal ex user)

let user_suite = [
  user_name_test "Valid name" user1 "Nate";
  user_name_test "Valid name" user2 "Ocaml";
  user_name_test "Name after incr_win" nate43_0 "Nate";
  user_name_test "Name after incr_loss" ocaml_after_incr_nate "Ocaml";
  user_name_test "Multiple incrs" nate43_1 "Nate";
  user_name_test "Multiple incrs" ocaml1_43 "Ocaml";
  invalid_user_test "Invalid name" fake_name (InvalidUser ("Username: foo does not exist."));
  invalid_user_test "Invalid password" fake_pswd (InvalidUser ("Password does not match"));
  user_pswd_test "Valid pswd" user1 "42";
  user_pswd_test "Valid pswd" user2 "ocaml";
  user_pswd_test "Pswd after incr_win" nate43_0 "42";
  user_pswd_test "Pswd after incr_loss" ocaml_after_incr_nate "ocaml";
  user_pswd_test "Multiple incrs" nate43_1 "42";
  user_pswd_test "Multiple incrs" ocaml1_43 "ocaml";
  user_wins_test "42 wins" user1 42;
  user_wins_test "0 wins" user2 0;
  user_wins_test "43 wins" nate43_0 43;
  user_wins_test "43 wins" nate43_1 43;
  user_wins_test "0 wins" ocaml_after_incr_nate 0;
  user_wins_test "0 wins" ocaml0_43 0;
  user_wins_test "1 win" ocaml1_43 1;
  user_losses_test "0 wins 43 losses" ocaml0_43 43;
  user_losses_test "1 win 43 losses" ocaml1_43 43;
  user_losses_test "0 losses" user1 0;
  user_losses_test "42 losses" user2 42;
  user_losses_test "0 losses after win" nate43_0 0;
  user_losses_test "1 loss" nate43_1 1;
  user_elo_test "Nate is #1" user1 1;
  user_elo_test "Ocaml elo" user2 2;
  user_elo_test "After many incrs" nate43_1 1;
  user_elo_test "After many incrs" ocaml1_43 2;
]

let suite = "Battleship Test Suite" >::: List.flatten [
    init_tests;
    insert_tests;
    already_guessed_tests;
    command_tests;
    user_suite
  ]

let _ =
  run_test_tt_main suite;
  (* run_test_tt_main go_suite *)