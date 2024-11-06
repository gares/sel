all:
	dune build

install:
	dune install

test:
	dune runtest

release:
	dune-release -p sel

.PHONY: test
