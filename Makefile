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

.PHONY: build clean install uninstall test all
