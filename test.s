; vim: tabstop=8 shiftwidth=8 textwidth=80 noexpandtab syntax=asmM6502

; Kernal routines

CHROUT = $ffd2

; ZP addresses (1 word each)
ARG0 = $fb
ARG1 = $fd

; Macros

	; Call println
	.macro println,s
	ldx #<\s
	ldy #>\s
	jsr println
	.endmacro

; Basic booter

	.org $0801
basic_start:
	.word basic_end, 10
	.byte $9e, " 2064", 0 ; SYS 2064 ($0810)
basic_end:
	.word 0

; Program

	.org $0810
main:
	ldx #<string_title
	ldy #>string_title
	jsr println

	ldx #<intcode_program
	ldy #>intcode_program
	jsr interpret
	rts

; Interpreter registers

INTCODE_MEMORY_MAX = $7fff

reg_pc:
	; program counter (index, not address)
	.word $5858
reg_op:
	; opcode
	.byte $58, $58, $58, $58
reg_a:
	; argument A
	.byte $58, $58, $58, $58
reg_b:
	; argument B
	.byte $58, $58, $58, $58
reg_c:
	; argument C
	.byte $58, $58, $58, $58

; interpret
;   Interpret the Intcode program in $2000-$7ffff
;
; Clobber:
;   A,X,Y,ZP
interpret:
	; Initialize.
	lda #0
	sta reg_pc
	sta reg_pc+1
	sta reg_op
	sta reg_op+1
	sta reg_op+2
	sta reg_op+3
	sta reg_a
	sta reg_a+1
	sta reg_a+2
	sta reg_a+3
	sta reg_b
	sta reg_b+1
	sta reg_b+2
	sta reg_b+3
	sta reg_c
	sta reg_c+1
	sta reg_c+2
	sta reg_c+3

interpret_loop:
	nop
	nop
	nop
	nop
	; Calculate the address of the opcode.
	; Store it in the argument of the LDA instruction that loads the opcode.
	clc
	lda reg_pc
	adc #<intcode_program
	sta interpret_lda+1
	lda reg_pc+1
	adc #>intcode_program
	sta interpret_lda+2

	; Load the 32-bit opcode into reg_op.
	ldy #3
interpret_lda:
	lda $5858,Y
	sta reg_op,Y
	dey
	bpl interpret_lda

interpret_decode:
	; Decode the opcode.
	lda reg_op
	cmp #1
	beq interpret_add
	cmp #2
	beq interpret_mul
	cmp #99
	beq interpret_hlt

	println string_invalid_opcode
interpret_end:
	rts

interpret_add:
	println string_add
	; Increment PC by 4.
	clc
	lda reg_pc
	adc #4
	sta reg_pc
	lda reg_pc+1
	adc #0
	sta reg_pc+1
	jmp interpret_loop

interpret_mul:
	println string_mul
	jmp interpret_end

interpret_hlt:
	println string_hlt
	jmp interpret_end

;interpret_pc:
;	.word $5858

; print
;   Print a string of characters, stopping at 0.
;
; Arguments:
;   X - address of the string to print (low byte)
;   Y - address of the string to print (high byte)
; Clobber:
;   A,X,Y,ZP
print:
	stx ARG0
	sty ARG0+1
	ldy #0
print_loop:
	lda (ARG0),y
	beq print_end
	jsr CHROUT
	iny
	bne print_loop
print_end:
	rts

; println - print a string of characters, stop at 0, followed by a CR
; See print.
println:
	jsr print
	lda #$0d
	jsr CHROUT
	rts

; Strings

string_title:
	.asciiz "INTCODE INTERPRETER - BY T. KOSTER"

string_add:
	.asciiz "ADD"

string_mul:
	.asciiz "MUL"

string_hlt:
	.asciiz "HLT"

string_invalid_opcode:
	.asciiz "INVALID OPCODE"

string_done:
	.asciiz "DONE"

; Data

	.org $0a00
intcode_program:
	.defl 1, 0, 0, 3 ; add 0 0 3
	.defl 99	 ; hlt

;gravity_assist_program:
;	.word 1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,10,1,19,1,6,19,23,2,23,6,27,1,5,27,31,1,31,9,35,2,10,35,39,1,5,39,43,2,43,10,47,1,47,6,51,2,51,6,55,2,55,13,59,2,6,59,63,1,63,5,67,1,6,67,71,2,71,9,75,1,6,75,79,2,13,79,83,1,9,83,87,1,87,13,91,2,91,10,95,1,6,95,99,1,99,13,103,1,13,103,107,2,107,10,111,1,9,111,115,1,115,10,119,1,5,119,123,1,6,123,127,1,10,127,131,1,2,131,135,1,135,10,0,99,2,14,0,0

;quine_program:
;	.word 109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99
