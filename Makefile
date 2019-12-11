
all:
	dune build @all

clean:
	dune clean

doc:
	dune build @doc

test:
	dune runtest

.PHONY: build clean test all doc
