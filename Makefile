
all:
	dune build @install -p bencode

clean:
	dune clean

doc:
	dune build @doc -p bencode

test:
	dune runtest -p bencode

.PHONY: build clean install uninstall test all doc
