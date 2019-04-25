(**The module that holds servers integrated in the lobby system*)
open Game

(**The module which holds a server that can be run for a particular game*)
module type Server = sig
  (**[run (ic1,oc1,id1,uname1) (ic2,oc2,id2,uname2)] starts
     a game where commands are received from [ic1] and [ic2] and
     output to [oc1] and [oc2] between users.*)
  val run : (in_channel * out_channel * int * string) ->
    (in_channel * out_channel * int * string) -> unit
end

(**[MakeServer G] returns a [Server] using the game logic of [G]*)
module MakeServer : functor (G:Game) -> Server
