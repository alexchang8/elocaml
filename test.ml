open OUnit2
open Player

(* type cell = Empty|Ship|Miss|Hit|Sunk
   type board= cell list list *)

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

let insert_tests = [
  insert_ship_test "Testing 3 piece ship on 4x3" (three_piece_v) (get_board var1);
  insert_ship_test "Testing 4 piece ship on 5x5" (four_piece_h) (get_board var2);
]


let suite = "Battleship Test Suite" >::: List.flatten [
    init_tests;
    insert_tests;
  ]

let _ = run_test_tt_main suite