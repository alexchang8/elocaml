module BattleshipServer = Server.MakeServer(Battleship)
let _ = BattleshipServer.run ()
