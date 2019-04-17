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
      assert_equal b board;
    )

let p_1by1 = init_player 1 1 1
let p_4by3 = init_player 4 3 1
let p_5by5 = init_player 5 5 2

let init_tests = [
  init_player_test "Initializing empty player" 1 1 1 p_1by1;
  init_player_test "Initial Rectangle Board" 4 3 1 p_4by3;
  init_player_test "Not player 1" 5 5 2 p_5by5
]

let three_piece = [[Ship; Empty; Empty]; [Ship; Empty; Empty]; [Ship; Empty; Empty];
                   [Empty; Empty; Empty]]


let ins_3_ship = insert_ship p_4by3 (make_coord (1,1)) (make_coord (1,3)) 3
let give_t v = 
  match v with
  |ValidB t-> t
  |_-> failwith "not important"

(* let convert (p:Player.board) : board = 
   match p with 
   | p -> get_board p *)

let insert_tests = [
  let var = give_t ins_3_ship in 
  (* let var2 = give_t  *)
  insert_ship_test "Testing 3 piece ship" (three_piece) (get_board var); 
]


let suite = "Battleship Test Suite" >::: List.flatten [
    init_tests;
    insert_tests;
  ]

let _ = run_test_tt_main suite