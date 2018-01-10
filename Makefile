all: build prustc

build: asm/prust_base.s
	cargo build --release

asm/mm.s: asm/mm.c
	gcc $^ -S -o $@

asm/prust_base.s: asm/sbrk.s asm/mm.s asm/start.s
	cat $^ > $@

prustc:
	ln -s target/release/rustception prustc

test: all
	cd tests && ./test.sh -all ../prustc

clean:
	-rm -r target
	-rm asm/mm.s asm/prust_base.s
	-rm prustc

.PHONY: all build test clean
