all:
	dune build

install:
	dune install

test:
	BISECT_FILE=$(shell pwd)/coverage dune runtest --instrument-with bisect_ppx --force
	bisect-ppx-report summary
	bisect-ppx-report html
	@echo Coverage report: python3 -m http.server --directory _coverage/

clean:
	dune clean
	rm -rf _coverage coverage*.coverage

release:
	dune-release -p sel

.PHONY: test
