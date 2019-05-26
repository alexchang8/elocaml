
# Elocaml

Elocaml is a multiplayer client-server architecture capable of making an arbitrary game written in OCaml multiplayer. Key features include:
* A lobby system allowing many clients to join a single server, then create and join game lobbies for different games
* A tab system allowing users to play up to 3 games concurrently while connected to a single server
* Chat for each game tab
* A user login system including creating accounts
* An almost fully click-based GUI in terminal
* Passing mouse click events to game servers
* An authoritative server- a single client is capable of playing any game

## How to Run
To run the server: `make lobby`
The lobby will run on port 1400 on the local ip of the machine.


To run a client: `make client`
The client will then prompt for an ip and port.

## Dependencies
This project depends only on the Unix, Str, and YoJson modules.

## Documentation
Documentation was generated with ocamldoc.
[Public documentation (types and functions exposed through .mli)](https://alexchang8.github.io/elocaml/doc.public/index.html)
[Private documentation (all functions and types)](https://alexchang8.github.io/elocaml/doc.public/index.html)

