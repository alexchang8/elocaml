MLS=integrated_server.ml lobby.ml gui.ml integrated_server.ml lobbyview.ml authors.ml battleship_server.ml client.ml command.ml game.ml chat.ml tools.ml go_server.ml go.ml player.ml server.ml user.ml
MLS_BUILD=_build/client-server/gui.ml _build/client-server/integrated_server.ml _build/client-server/lobby.ml _build/client-server/gui.ml _build/client-server/lobbyview.ml authors.ml _build/battleship/battleship_server.ml _build/client-server/client.ml _build/battleship/command.ml game.ml _build/client-server/chat.ml _build/client-server/tools.ml _build/go/go_server.ml _build/go/go.ml _build/battleship/player.ml _build/client-server/server.ml _build/client-server/user.ml
MLIS= integrated_server.mli tools.mli lobbyview.mli authors.mli client.mli command.mli player.mli server.mli user.mli chat.mli
MLIS_BUILD=_build/client-server/integrated_server.mli _build/client-server/tools.mli _build/client-server/lobbyview.mli authors.mli _build/client-server/client.mli _build/battleship/command.mli _build/battleship/player.mli _build/client-server/server.mli _build/client-server/user.mli _build/client-server/chat.mli
TEST=test.byte
MAIN=main.byte
BSHIP_SERVER=battleship_server.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind
CLIENT=client.byte
GO_SERVER=go_server.byte
LOBBY=lobby.byte

default: build
	utop

build:
	$(OCAMLBUILD) go_server.cmo go.cmo battleship_server.cmo battleship.cmo command.cmo gui.cmo lobby.cmo integrated_server.cmo gui.cmo lobbyview.cmo chat.cmo tools.cmo authors.cmo client.cmo command.cmo player.cmo server.cmo user.cmo

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

lobby:
	$(OCAMLBUILD) $(LOBBY) && ./$(LOBBY)

play:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

battleship-server:
	$(OCAMLBUILD) $(BSHIP_SERVER) && ./$(BSHIP_SERVER)

client:
	$(OCAMLBUILD) $(CLIENT) && ./$(CLIENT)

go-server:
	$(OCAMLBUILD) $(GO_SERVER) && ./$(GO_SERVER)

zip:
	zip src.zip *.ml* *.json _tags Makefile

docs: docs-public docs-private

docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -I _build/go -I _build/battleship -I _build/client-server -package yojson,ANSITerminal -html -stars -d doc.public _build/*.mli _build/battleship/*.mli _build/client-server/*.mli

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -I _build/client-server -I _build/battleship -I _build/go -package yojson,ANSITerminal -html -stars -d doc.private -inv-merge-ml-mli -m A _build/*.ml _build/*.mli _build/go/*.ml _build/battleship/*.ml _build/battleship/*.mli _build/client-server/*.ml _build/client-server/*.mli

clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private src.zip
