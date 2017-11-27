all: build prustc

build:
	cargo build

prustc:
	ln -s target/debug/rustception prustc

clean:
	rm -r target
	rm prustc

.PHONY: all build clean
