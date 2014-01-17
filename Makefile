NAME = bencode
DOC = bencode.docdir/index.html
OCAMLBUILD = ocamlbuild -use-ocamlfind
TARGETS = bencode.cma bencode.cmxa bencode.cmi bencode.a
INSTALL_TARGETS = $(TARGETS) bencode.mli
LIB = $(addprefix _build/lib/, $(TARGETS)) 
INSTALL = $(LIB)

all:
	$(OCAMLBUILD) $(TARGETS) $(DOC)

install: all
	ocamlfind install $(NAME) META $(INSTALL)

uninstall:
	ocamlfind remove $(NAME)

clean:
	$(OCAMLBUILD) -clean

test:
	$(OCAMLBUILD) lib_test/test.native
	./test.native

ounit:
	$(OCAMLBUILD) -use-ocamlfind -package oUnit lib_test/test_ounit.native
	./test_ounit.native

qcheck:
	$(OCAMLBUILD) -use-ocamlfind -package qcheck lib_test/test_qcheck.native
	./test_qcheck.native

.PHONY: build clean install uninstall test all ounit qcheck
