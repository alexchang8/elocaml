module GoServer = Server.MakeServer(Go)
let _ = GoServer.run ()
