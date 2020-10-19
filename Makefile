test.prg: test.s
	vasm6502_oldstyle -Fbin -cbm-prg -dotdir -o test.prg test.s
	hexdump -C test.prg

test.d64: test.prg
	c1541 -format "intcode,1" d64 test.d64
	c1541 -attach test.d64 -write test.prg test.prg
	c1541 -attach test.d64 -list
