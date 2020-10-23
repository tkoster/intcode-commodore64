; vim: tabstop=8 shiftwidth=8 textwidth=80 noexpandtab syntax=asmM6502

; Kernal routines

CHROUT = $ffd2

; ZP addresses (1 word each)
ARG0 = $fb
ARG1 = $fd

; Macros

.macro println s
	ldx #<s
	ldy #>s
	jsr _println
.endmacro

; Program

main:
	println string_title
	jmp interpret

; Interpreter registers

INTCODE_MEMORY_MAX = $7fff

reg_start:
reg_pc:
	; Program counter (16 bits):
	; This register contains the offset (in bytes) into the program where
	; the current instruction is.
	.word $5858
reg_op:
	; Opcode (32 bits)
	; During decoding, this register contains the Intcode opcode.
	.byte $58, $58, $58, $58
reg_a:
	; Argument A (32 bits)
	.byte $58, $58, $58, $58
reg_b:
	; Argument B (32 bits)
	.byte $58, $58, $58, $58
reg_c:
	; Argument C (32 bits)
	.byte $58, $58, $58, $58
reg_end:

; interpret
;   Interpret the Intcode program.
;
; Clobber:
;   A,X,Y,ZP
interpret:
	; Zero all the interpreter registers.
	lda #0
	ldy #<(reg_end - reg_start)
@loop:
	dey
	sta reg_start,y
	bne @loop

interpret_loop:
	; Calculate the address of the opcode.
	; Store it in the argument of the LDA instruction that loads the opcode.
	clc
	lda reg_pc
	adc #<intcode_program
	sta interpret_lda+1
	lda reg_pc+1
	adc #>intcode_program
	sta interpret_lda+2

	; Copy the opcode into reg_op (32 bits)
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
	; Increment PC by 16 bytes.
	clc
	lda reg_pc
	adc #16
	sta reg_pc
	lda reg_pc+1
	adc #0
	sta reg_pc+1
	jmp interpret_loop

interpret_mul:
	println string_mul
	; TODO: multiply
	jmp interpret_end

interpret_hlt:
	println string_hlt
	jmp interpret_end

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
@loop:
	lda (ARG0),y
	beq @end
	jsr CHROUT
	iny
	bne @loop
@end:
	rts

; println
;   Print a string of characters, stopping at 0, followed by a CR.
;   See print.
_println:
	jsr print
	lda #$0d
	jsr CHROUT
	rts

; Strings

string_title:
	.asciiz "intcode interpreter - by t. koster"

string_add:
	.asciiz "add"

string_mul:
	.asciiz "mul"

string_hlt:
	.asciiz "hlt"

string_invalid_opcode:
	.asciiz "invalid opcode"

; Data

intcode_program:
	.dword 1, 0, 0, 3 ; add 0 0 3
	.dword 99         ; hlt

;gravity_assist_program:
;	.word 1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,10,1,19,1,6,19,23,2,23,6,27,1,5,27,31,1,31,9,35,2,10,35,39,1,5,39,43,2,43,10,47,1,47,6,51,2,51,6,55,2,55,13,59,2,6,59,63,1,63,5,67,1,6,67,71,2,71,9,75,1,6,75,79,2,13,79,83,1,9,83,87,1,87,13,91,2,91,10,95,1,6,95,99,1,99,13,103,1,13,103,107,2,107,10,111,1,9,111,115,1,115,10,119,1,5,119,123,1,6,123,127,1,10,127,131,1,2,131,135,1,135,10,0,99,2,14,0,0

;quine_program:
;	.word 109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99
