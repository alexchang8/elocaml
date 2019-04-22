FILES=authors battleship_server client command game go_server go player server
MLS = $(FILES:=.ml)
MLIS=authors.mli client.mli command.mli player.mli server.mli
OBJECTS = $(FILES:=.cmo)
TEST=test.byte
MAIN=main.byte
BSHIP_SERVER=battleship_server.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind
CLIENT=client.byte
GO_SERVER=go_server.byte

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST)

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
	ocamlfind ocamldoc -I _build -I _build/go -I _build/battleship \
	-I _build/client-server -package yojson,ANSITerminal \
		-html -stars -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package yojson,ANSITerminal \
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A $(MLIS) $(MLS)

clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private src.zip
