MODULES=main simulator input authors game
MAIN=main.byte
GAME=game.byte
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind
PKGS=OUnit,Yojson,ANSITerminal,Unix,Str,QCheck,Bisect

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

run:
	$(OCAMLBUILD) $(MAIN) 2> /dev/null && ./$(MAIN)

play:
	$(OCAMLBUILD) $(GAME) 2> /dev/null && ./$(GAME)

test:
	$(OCAMLBUILD) -tag debug $(TEST) && ./$(TEST)

bisect-test:
	$(OCAMLBUILD) -package bisect -syntax camlp4o,bisect_pp \
	  $(TEST) && ./$(TEST) -runner sequential

bisect: clean bisect-test
	bisect-report -I _build -html report bisect0001.out

zip:
	zip search_src.zip *.ml* _tags Makefile analysis.pdf

docs: docs-public docs-private

docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A -hide-warnings $(MLIS) $(MLS)

clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private report search_src.zip bisect*.out
