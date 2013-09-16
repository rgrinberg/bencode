NAME = bencode
DOC = bencode.docdir/index.html
OCAMLBUILD = ocamlbuild -use-ocamlfind
TARGETS = bencode.cma bencode.cmxa bencode.cmi bencode.a
LIB = $(addprefix _build/, $(TARGETS)) 
INSTALL = $(LIB) bencode.mli

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

.PHONY: build clean install uninstall test
