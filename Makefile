.PHONY: all

all: day2part1.prg day2part1.d64

day2part1.prg: day2part1.s
	cl65 -t c64 -C c64-asm.cfg -u __EXEHDR__ -g -o day2part1.prg -m day2part1.map -Ln day2part1.lbl day2part1.s

day2part1.d64: day2part1.prg
	c1541 -format "intcode,1" d64 day2part1.d64
	c1541 -attach day2part1.d64 -write day2part1.prg day2part1.prg
	c1541 -attach day2part1.d64 -list
