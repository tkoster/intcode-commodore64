.PHONY: all

all: test.prg test.d64

test.prg: test.s
	cl65 -t c64 -C c64-asm.cfg -u __EXEHDR__ -g -o test.prg -m test.map -Ln test.lbl test.s

test.d64: test.prg
	c1541 -format "intcode,1" d64 test.d64
	c1541 -attach test.d64 -write test.prg test.prg
	c1541 -attach test.d64 -list
