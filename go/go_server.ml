(**The module that when loaded runs the standalone battleship game server*)
module GoServer = Server.MakeServer(Go)
(**the dummy variable to run the standalone server*)
let _ = GoServer.run ()
