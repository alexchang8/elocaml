(**The module that when loaded runs the standalone battleship game server*)
module BattleshipServer = Server.MakeServer(Battleship)
(**Dummy variable to initialize the server*)
let _ = BattleshipServer.run ()
