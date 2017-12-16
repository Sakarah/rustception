all: build prustc

build:
	cargo build

prustc:
	ln -s target/debug/rustception prustc

test: all
	cd tests && ./test.sh -all ../prustc

clean:
	rm -r target
	rm prustc

.PHONY: all build test clean
