(**the module that holds logic for creating standalone servers from arbitrary games*)
open Game

module type Server = sig
  (**Runs the server for a game. The server accepts socket connections
     on the computers internal ip on port 1400.*)
  val run : unit -> unit
end

module MakeServer : functor (G:Game) -> Server
