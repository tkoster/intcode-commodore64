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
	jmp interpret ; ends with rts

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
reg_r:
	; Math result register (64 bits)
	.byte $58, $58, $58, $58, $58, $58, $58, $58
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
	sta reg_start,Y
	bne @loop

interpret_loop:
	; Using the offset in the PC register, calculate the memory address of
	; the next Intcode instruction.

	; Store it in the argument of the LDA instruction below that loads the
	; instruction.

	clc
	lda reg_pc
	adc #<intcode_program
	sta @lda_op+1
	lda reg_pc+1
	adc #>intcode_program
	sta @lda_op+2

	; Copy the Intcode instruction into the instruction registers.
	; These are the opcode and 3 arguments, 32 bits each (16 bytes total).

	ldy #15
@loop:
@lda_op:
	lda $5858,Y
	sta reg_op,Y
	dey
	bpl @loop

	; Decode the opcode; jump to its code.

	lda reg_op
	cmp #1
	beq interpret_add
	cmp #2
	bne @cmp_hlt
	jmp interpret_mul
@cmp_hlt:
	cmp #99
	bne @invalid_opcode
	jmp interpret_hlt

@invalid_opcode:
	println string_invalid_opcode

interpret_end:
	rts

interpret_add:
	println string_add

	; The A register contains the Intcode address of the first operand.
	;
	; Replace it with the value at that address.

	jsr deref_a

	; The B register contains the Intcode address of the second operand.
	;
	; Replace it with the value at that address.

	jsr deref_b

	; Add the values in the A and B registers.
	; Store the result in the B register (32 bits).

	jsr add_a_b

	; The C register contains the Intcode address to write the result to.
	;
	; Copy the result value in register B to the Intcode memory at this
	; address.

	jsr store_b_in_c

	; Increment PC register by 16 bytes.

	jsr incpc16

	; Process next instruction.

	jmp interpret_loop

interpret_mul:
	println string_mul

	; The A register contains the Intcode address of the first operand.
	;
	; Replace it with the value at that address.

	jsr deref_a

	; The B register contains the Intcode address of the second operand.
	;
	; Replace is with the value at that addess.

	jsr deref_b

	; Multiply the values in the A and B registers.
	; Store the result in the B register (32 bits).

	jsr mul_a_b

	; The C register contains the Intcode address to write the result to.
	;
	; Copy the result value in register B to the Intcode memory at this
	; address.

	jsr store_b_in_c

	; Increment PC register by 16 bytes.

	jsr incpc16

	; Process next instruction.

	jmp interpret_loop

interpret_hlt:
	println string_hlt

	; Print the number in Intcode memory at address 0.

	jsr output_0

	; Done.

	jmp interpret_end

; Subroutines for interpreter operations like load and store

; deref_a - Replace an Intcode address in the A register with the value at
; that location in Intcode memory.
deref_a:
	; Calculate the memory address of the value of argument A.
	;
	; Store it in the argument of the LDA instruction below that loads the
	; value of argument A.

	asl reg_a
	rol reg_a+1
	asl reg_a
	rol reg_a+1
	clc
	lda reg_a
	adc #<intcode_program
	sta @lda_a+1
	lda reg_a+1
	adc #>intcode_program
	sta @lda_a+2

	; Copy the value of argument A into the A register (32 bits).

	ldy #3
@loop:
@lda_a:
	lda $5858,Y
	sta reg_a,Y
	dey
	bpl @loop

	rts

; deref_b - Replace an Intcode address in the B register with the value at
; that location in Intcode memory.
deref_b:
	; Calculate the memory address of the value of argument B.
	;
	; Store it in the argument of the LDA instruction below that loads the
	; value of argument B.

	asl reg_b
	rol reg_b+1
	asl reg_b
	rol reg_b+1
	clc
	lda reg_b
	adc #<intcode_program
	sta @lda_b+1
	lda reg_b+1
	adc #>intcode_program
	sta @lda_b+2

	; Copy the value of argument A into the B register (32 bits).

	ldy #3
@loop:
@lda_b:
	lda $5858,Y
	sta reg_b,Y
	dey
	bpl @loop

	rts

; store_b_in_c - Copy the value in register B to the Intcode memory at the
; Intcode address in register C.
;
; As the value of the register is an Intcode address, the high 16 bits of the
; value are ignored.
store_b_in_c:
	; Calculate the memory address of the result.
	;
	; Store it in the argument of the STA instruction below that saves the
	; result.

	asl reg_c
	rol reg_c+1
	asl reg_c
	rol reg_c+1
	clc
	lda reg_c
	adc #<intcode_program
	sta @sta_result+1
	lda reg_c+1
	adc #>intcode_program
	sta @sta_result+2

	; Copy the result from register B into Intcode memory (32 bits).

	ldy #3
@loop_r:
	lda reg_b,Y
@sta_result:
	sta $5858,Y
	dey
	bpl @loop_r

	rts

; incpc16
;   Increment the interpreter PC register by 16 bytes (4 cells).
;
; Arguments:
;   X - how much to increment the PC (number of bytes)
; Clobber:
;   A
incpc16:
	clc
	lda reg_pc
	adc #16
	sta reg_pc
	lda reg_pc+1
	adc #0
	sta reg_pc+1
	rts

; add_a_b
;   Add the values in the A and B registers, storing the sum in register B.
;
; Clobber: A
add_a_b:
	clc
	lda reg_a
	adc reg_b
	sta reg_b
	lda reg_a+1
	adc reg_b+1
	sta reg_b+1
	lda reg_a+2
	adc reg_b+2
	sta reg_b+2
	lda reg_a+3
	adc reg_b+3
	sta reg_b+3
	rts

; mul_a_b
;   Multiply the values in the A and B registers, storing the product in register B.
;
;   Register A is the multiplier and register B is the multiplicand.
;
;   After this subroutine returns, the value in the A register is undefined.
;
; Clobber:
;   A,X
mul_a_b:
	; Clear high product bits.

	lda #0
	sta reg_r+4
	sta reg_r+5
	sta reg_r+6
	sta reg_r+7

	ldx #32

	; Shift the multiplier right.

@shift:
	lsr reg_a+3
	ror reg_a+2
	ror reg_a+1
	ror reg_a

	; Skip additions if LSB was zero.

	bcc @rotate

	; Add multiplicand to product.

	clc
	lda reg_r+4
	adc reg_b
	sta reg_r+4
	lda reg_r+5
	adc reg_b+1
	sta reg_r+5
	lda reg_r+6
	adc reg_b+2
	sta reg_r+6
	lda reg_r+7
	adc reg_b+3

	; Rotate the product right.
@rotate:
	ror A
	sta reg_r+7
	ror reg_r+6
	ror reg_r+5
	ror reg_r+4
	ror reg_r+3
	ror reg_r+2
	ror reg_r+1
	ror reg_r

	; Next bit.

	dex
	bne @shift

	; Copy the low 32 bits of the product into register B.

	lda reg_r
	sta reg_b
	lda reg_r+1
	sta reg_b+1
	lda reg_r+2
	sta reg_b+2
	lda reg_r+3
	sta reg_b+3
	rts

output_0:
	ldy #3
@loop:
	; Print the high nibble.

	lda intcode_program,Y
	lsr A
	lsr A
	lsr A
	lsr A
	tax
	lda @digits,X
	jsr CHROUT

	; Print the low nibble.

	lda intcode_program,Y
	and #$0f
	tax
	lda @digits,X
	jsr CHROUT

	dey
	bpl @loop
	rts
@digits:
	.asciiz "0123456789abcdef"

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
	lda (ARG0),Y
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
	.dword 1,12,2,3,1,1,2,3,1,3,4,3,1,5,0,3,2,10,1,19,1,6,19,23,2,23,6,27,1,5,27,31,1,31,9,35,2,10,35,39,1,5,39,43,2,43,10,47,1,47,6,51,2,51,6,55,2,55,13,59,2,6,59,63,1,63,5,67,1,6,67,71,2,71,9,75,1,6,75,79,2,13,79,83,1,9,83,87,1,87,13,91,2,91,10,95,1,6,95,99,1,99,13,103,1,13,103,107,2,107,10,111,1,9,111,115,1,115,10,119,1,5,119,123,1,6,123,127,1,10,127,131,1,2,131,135,1,135,10,0,99,2,14,0,0
