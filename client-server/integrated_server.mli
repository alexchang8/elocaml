open Game

module type Server = sig
  (**Runs the server for a game. The server accepts socket connections
     on the computers internal ip on port 1400.*)
  val run : (in_channel * out_channel * int * string) ->
    (in_channel * out_channel * int * string) -> unit
end

module MakeServer : functor (G:Game) -> Server
(**TODO: functor to make client*)
