; CpS 230 Team Project: Bill Alger (balge648) and Carter Shean (cshea892)
;---------------------------------------------------
; Final Release of the Kernel project that demonstrates task switching
;---------------------------------------------------
;16 bit mode
bits 16

;start the code at 0, since we are not using DOS anymore
org  0

; Where to find the INT 8 handler vector within the IVT [interrupt vector table]
IVT8_OFFSET_SLOT	equ	4 * 8			; Each IVT entry is 4 bytes; this is the 8th
IVT8_SEGMENT_SLOT	equ	IVT8_OFFSET_SLOT + 2	; Segment after Offset
;set up stacks for different tasks
stack_a TIMES 256 db 0
stack_b TIMES 256 db 0
stack_c TIMES 256 db 0
stack_d TIMES 256 db 0
rpn_stack TIMES 256 db 0
stack_pointer TIMES 32 dw 0

section .text
start:

mov	ax, cs
mov	ds, ax

; Set ES=0x0000 (segment of IVT)
mov	ax, 0x0000
mov	es, ax

;print out the original message for the rpn calculator with the cursor moved
call        switchcursorpos
mov      dx, fmt_prompt
call       puts
call       switchcursorposback


;move the 0 intothe number of the stack pointer
mov word[num_pointer], 0

;set up the rpn stack
mov sp, rpn_stack + STACK_SIZE
pushf
push cs
push .rpn
pusha
push ds
push es
mov [stack_pointer + 8], sp
		

;set up stack s
mov sp, stack_d + STACK_SIZE
pushf
push cs
push .task_d
pusha
push ds
push es
mov [stack_pointer + 6], sp

;set up stack c
mov sp, stack_c + STACK_SIZE
pushf
push cs
push .task_c
pusha
push ds
push es
mov [stack_pointer + 4], sp

;set up stack b
mov sp, stack_b + STACK_SIZE
pushf
push cs
push .task_b
pusha
push ds
push es
mov [stack_pointer + 2], sp

;set up stack a
mov sp, stack_a + STACK_SIZE
mov [stack_pointer], sp

	;redirect interrupts
	cli
	mov ax, [es:IVT8_OFFSET_SLOT]
	mov [ivt8_offset], ax
	mov ax, [es:IVT8_SEGMENT_SLOT]
	mov [ivt8_segment], ax
	mov word [es:IVT8_OFFSET_SLOT], timer_isr
	mov [es:IVT8_SEGMENT_SLOT], cs
	sti

; code for task a, prints message, then yields 
.task_a:
	mov dx, ATask  ;do task
	call puts
	jmp .task_a

; code for task b, prints message, then yields
.task_b: 
	mov dx, BTask  ;do task
	call puts 
	jmp .task_b 

; code for task a, prints message, then yields 
.task_c:
	mov dx, CTask
	call puts
	jmp .task_c

; code for task a, prints message, then yields 
.task_d:
	mov dx, DTask
	call puts
	jmp .task_d

;rpn calculator portion of the program
.rpn:
	
	;check which key was pressed
	mov ah, 0
	int    0x16
	cmp  al, 0
	jne   .calculate
	jmp .rpn
	
.calculate:
	;print out message after disabling interrupts (needs work)
	cli
	call       switchcursorpos
	mov      dx, fmt_prompt
	call       puts
	mov      dx, word[output]
	call        putc
	sti
	
        mov      byte[input_symbol], al
	
.number:

	;compare input_symbol to a number, jump if not equal to .nondigit
	;otherwise, add the number to the current_number variable
	cmp   byte[input_symbol], '0'
	jl       .nondigit
	cmp   byte[input_symbol], '9'
	jg      .nondigit
	
	sub     word[input_symbol], '0'
	mov    dx, 10
	imul    dx, word[current_number]
	add     dx, word[input_symbol]
	
	mov   word[current_number], dx
	mov   word[last_number_digit], 1
	call switchcursorposback
	jmp  .rpn
	
.nondigit:
	;check if the last number was a digit, jump if not equal to next comparson
	;otherwise, move the current number into the stack and reset variables
	cmp   word[last_number_digit], 1
	jne     .newline
	
	mov    bx, word[current_number]
	call    _push_function
	mov   word[current_number], 0
	mov   word[last_number_digit], 0
	inc     si
	
	jmp   .newline
	
.newline:
	;compare input_symbol to a newline character, jump if not equal to next test
	;otherwise, print the top of the stack and jump to top of loop
	cmp    word[input_symbol], 13
	jne     .plus
	
	dec    si
	call     _pop_function
	mov   dx, bx
	mov   word[operators], dx
	call     putc
	call   _push_function
	inc     si
	call   switchcursorposback
	jmp    .rpn
	
.plus:
	;compare input_symbol to a plus sign, jump if not equal to next test
	;otherwise, add the number latest two number on the stack
	cmp    byte[input_symbol], '+'
	jne     .minus
	
	dec    si
	call   _pop_function
	mov   dx, bx
	dec    si
	call   _pop_function
	add	  bx, dx
	call   _push_function
	inc   si
	call switchcursorposback
	jmp    .rpn
	
.minus:
	;compare input_symbol to a minus sign, jump if not equal to next test
	;otherwise, perform subtraction on the last two numbers on the stack
	cmp    byte[input_symbol], '-'
	jne      .negate
	
	dec   si
	call   _pop_function 
	mov   dx, bx	
	dec   si
	call   _pop_function
	sub	  bx, dx
        call    _push_function
	inc   si
	call switchcursorposback
	jmp    .rpn
.negate:
	;compare input_symbol to a tilde sign, jump if not equal to next test
	;otherwise, negate last number
	cmp     byte[input_symbol], '~'
	jne      .multiply
	
	dec      si
	call      _pop_function
	not      bx
        add     bx, 00000001B
	call     _push_function
	inc      si
	call switchcursorposback
	jmp    .rpn
.multiply:
	;compare input_symbol to a multiplication sign, jump if not equal to next test
	;otherwise, nultiply the last two numbers on the stack
	cmp     byte[input_symbol], '*'
	jne      .divide
	
	dec      si
	call      _pop_function
	mov    dx, bx  
	dec     si
	call      _pop_function
	imul    bx, dx
	call      _push_function
	inc      si
	call switchcursorposback
	jmp    .rpn
.divide:
	;compare input_symbol to a division sign, jump if not equal to next test
	;otherwise, divide the newest number on the stack from the next newest
	cmp     byte[input_symbol], '/'
	jne       .ignore
	
	dec     si
	call      _pop_function
	mov     cx, bx
	dec     si
	call      _pop_function
	mov    ax, bx
	cwd
	idiv     cx
	mov    bx, ax
	call     _push_function
	inc    si
	call switchcursorposback
	jmp      .rpn

.ignore:
	;ignore all other positions and switch the cursor's position back
	call switchcursorposback
	jmp .rpn
	

;push the value in bx onto the RPN stack
_push_function:
	push dx
	push bx
	mov ax, si
	mov bx, 2
	imul bx
	pop bx
	push si
	mov si, ax
	mov   word[operators + si], bx
	pop si
	pop dx
	ret
;pop the last value of the stack into bx, checking for
;stack underflow, and exiting the program if it occurs
_pop_function:
	push dx
	mov ax, si
	mov bx, 2
	imul bx
	push si
	mov si, ax
	mov   bx, word[operators + si]
	pop si
	cmp   si, 0
	jl      .error
	pop  dx
	ret
;handle stack underflow	
.error:
	ret

;handles a timer hit
timer_isr:
	pusha
	push ds
	push es	
	
	mov bx, [num_pointer]
	shl bx, 1
	mov [stack_pointer + bx], sp 
	mov bx, [num_pointer]
	add bx, 1

	cmp bx, 5
	jne .change_sp
	mov bx, 0
	
;changes the location of what sp points to
.change_sp:
	mov [num_pointer], bx
	shl bx, 1

	mov sp, [stack_pointer + bx]	
	
	pop es
	pop ds
	popa
	
	; Chain to the original INT 8 handler 
	jmp	far [cs:ivt8_offset]	; Use CS as the segment here, since who knows what DS is now

;switches the place on the screen where the cursor displays
switchcursorpos:
	;save old cursor position
	mov ah, 0x03
	int 0x10
	mov [cursor_row], dh
	mov [cursor_col], dl

	;move to new cursor position
	mov ah, 0x02
	mov dh, 20
	mov dl, 100
	mov bh, 0
	int 0x10
	ret

;switches cursor back to old placement
switchcursorposback:
	mov ah, 0x02
	mov dh, [cursor_row]
	mov dl, [cursor_col]
	mov bh, 0
	int 0x10
	ret

;changes the stacks back and forth, pushing all the general purpose registers
;flags etc.
yield: 

	pusha  ;
	pushf
	mov ax, [num_pointer]
	inc ax
	inc ax
	cmp ax, 8
	;check if the stack pointer went out of bounds
	jle .continue
	mov ax, 0
	.continue:
	mov bx, ax 
	mov sp, [stack_pointer + bx]
	mov [num_pointer], ax
	popf
	popa
	ret

;print method supplied in class code
puts:
	push	ax
	push	cx
	push	si
	
	mov	ah, 0x0e
	mov	cx, 1		; no repetition of chars
	
	mov	si, dx
.loop:	
    mov	al, [si]
	inc	si
	cmp	al, 0
	jz	.end
	int	0x10
	jmp	.loop
.end:
	pop	si
	pop	cx
	pop	ax
	ret

;homemade putchar method for numbers, divides numbers by ten, pushes them onto the
;stack, then prints them out in reverse, correct order
putc:
	push	ax
	push	cx
	push	si
	mov   cx, 10
	
.keepdividing:
	mov ax, dx
	mov dx, 0
	cwd
	idiv     cx
	inc   word[digits_to_pop]
	mov   word[num_to_div], ax
	add  dx, '0'
	push dx
	cmp word[num_to_div], 0
	je .printout
	mov dx, word[num_to_div]
	mov   cx, 10
	jmp .keepdividing
.printout:
	cmp word[digits_to_pop], 0
	je .exit
	pop ax
	mov    ah, 0x0E
	int      0x10
	dec word[digits_to_pop]
	jmp .printout
.exit:
	mov   al, 13
	mov    ah, 0x0E
	int      0x10
	pop   si
	pop	 cx
	pop 	 ax
	ret
	
;data section to hold saved stack pointer and char arrays for messages to be printed
section	.data
name_msg db "Cps 230 Team Project kernel: Bill Alger (balge648) and Carter Shean.", 13, 10, 0
address_sp dd 0
num_pointer dw 0
ATask	db	 "Four", 13, 10, 0
BTask	db	 "tasks", 13, 10, 0 
CTask        db     "at", 13, 10, 0
DTask        db     "once!", 13, 10, 0
STACK_SIZE equ 256
ivt8_offset	dw	0
ivt8_segment	dw	0

;numerical variables and arrays from rpn calculator
output dd 0
cursor_row dd 0
cursor_col dd 0
input_symbol  dd   0
digits_to_pop   dd  0
current_number dd 0
last_number_digit dd 0
num_to_print dd 0
num_to_div dd 0
operators	     dd   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

; String literals from rpn calculator
fmt_title   db   "RPN Calculator portion", 10, 0
fmt_prompt db "Top of the stack = ",13, 10, 0
fmt_invalid db "The input you entered was not a number or operator!",10,  0
fmt_error   db  "Error: stack underflow!", 10, 0


