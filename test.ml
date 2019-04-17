open OUnit2
open Player

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
    (player:t)
    (ship:ship)
    (b:board)
    (start_coord:coord)
    (end_coord:coord) : test = 
  name >:: (fun _ -> 
      assert_equal b (get_board player))


let p_1by1 = init_player 1 1 1
let p_4by3 = init_player 4 3 1
let p_5by5 = init_player 5 5 2

let init_tests = [
  init_player_test "Initializing empty player" 1 1 1 p_1by1;
  init_player_test "Initial Rectangle Board" 4 3 1 p_4by3;
  init_player_test "Not player 1" 5 5 2 p_5by5
]

let getShip_fromList (sl:ship list) : ship  = 
  match sl with
  | [ship,_] -> ship

let ins_3_ship = insert_ship p_4by3 (make_coord (1,1)) (make_coord (1,3)) 3

let insert_tests = [
  insert_ship_test "Testing 3 piece ship" p_4by3 (get_ships p_4by3) |> getShip_fromList)
]

let suite = "Battleship Test Suite" >::: List.flatten [
    init_tests;
    insert_tests;
  ]

let _ = run_test_tt_main suite