; Hello World 1.0

INCLUDE "gbhw.inc" ; standard hardware definitions from devrs.com
INCLUDE "ibmpc1.inc" ; ASCII character set from devrs.com

; IRQs
SECTION	"Vblank",HOME[$0040]
	reti
SECTION	"LCDC",HOME[$0048]
	reti
SECTION	"Timer_Overflow",HOME[$0050]
	reti
SECTION	"Serial",HOME[$0058]
	reti
SECTION	"p1thru4",HOME[$0060]
	reti

; ****************************************************************************************
; boot loader jumps to here.
; ****************************************************************************************
SECTION	"start",HOME[$0100]
nop
jp	begin

; ****************************************************************************************
; ROM HEADER and ASCII character set
; ****************************************************************************************
; ROM header
	ROM_HEADER	ROM_NOMBC, ROM_SIZE_32KBYTE, RAM_SIZE_0KBYTE
INCLUDE "memory.asm"
TileData:
	chr_IBMPC1	1,8 ; LOAD ENTIRE CHARACTER SET

; ****************************************************************************************
; Main code Initialization:
; set the stack pointer, enable interrupts, set the palette, set the screen relative to the window
; copy the ASCII character table, clear the screen
; ****************************************************************************************
begin:
	nop
	di
	ld	sp, $ffff		; set the stack pointer to highest mem location + 1
init:
	ld	a, %11100100 	; Window palette colors, from darkest to lightest
	ld	[rBGP], a		; CLEAR THE SCREEN

	ld	a,0			; SET SCREEN TO TO UPPER RIGHT HAND CORNER
	ld	[rSCX], a
	ld	[rSCY], a
	call	StopLCD		; YOU CAN NOT LOAD $8000 WITH LCD ON
	ld	hl, TileData
	ld	de, _VRAM		; $8000
	ld	bc, 8*256 		; the ASCII character set: 256 characters, each with 8 bytes of display data
	call	mem_CopyMono	; load tile data
	ld	a, LCDCF_ON|LCDCF_BG8000|LCDCF_BG9800|LCDCF_BGON|LCDCF_OBJ16|LCDCF_OBJOFF
	ld	[rLCDC], a
	ld	a, 32		; ASCII FOR BLANK SPACE
	ld	hl, _SCRN0
	ld	bc, SCRN_VX_B * SCRN_VY_B
	call	mem_SetVRAM

; ****************************************************************************************
; Main code:
; Print a character string in the middle of the screen
; ****************************************************************************************
;	ld	hl, Title
;	;ld	de, _SCRN0+3+(SCRN_VY_B*7) ;  ~TA
;	ld	de, _SCRN0 + (SCRN_VY_B) ;  ~TA
;	ld	bc, TitleEnd - Title
;	call	mem_CopyVRAM

	ld	a, %00000000
	ld	b, %11111111



	;call	OneLeftShiftN
	ld	c, 2
	call	CopyBit

	add	a, $30

	ld	hl, _SCRN0 + (SCRN_VY_B * 7)
	ld	bc, 1
	call	mem_SetVRAM

; ****************************************************************************************
; Prologue
; Wait patiently 'til somebody kills you
; ****************************************************************************************
wait:
	halt
	nop
	jr wait

; ~TA

Divide:
	; inputs: a, b | b!= 0
	; output: a

;Np	SET $C000
;Dp	SET $C001
;Qp	SET $C002
;Rp	SET $C003
;Nip	SET $C004
;	ld [Np], a
;	ld [Dp], b





; ****************************************************************************************
OneLeftShiftN:
	; inputs: b
	; output: a

	di
	push	bc
	ld	a, b

	cp	a, 0
	jr	z, .zero

	ld	a, 1
.loop
	sla	a
	dec	b
	jr	nz, .loop
.loopEnd
	pop	bc
	reti

.zero
	pop	bc
	ld	a, 1
	reti

; ****************************************************************************************
ZeroLeftShiftN:
	; inputs: b
	; output: a

	di
	call OneLeftShiftN		; a = 00100000
	cpl				; a = 11011111
	reti


; ****************************************************************************************
IsSet:
	; inputs: a, b
	; output: a

	di

	push	bc
	push	af

	call	OneLeftShiftN
	ld	c, a		; c = 1 << b

	pop	af

	and	a, c
	jr	z, .zero
	jr	.one

.zero
	pop bc
	ld	a, 0
	reti
.one
	pop bc
	ld	a, 1
	reti

; ****************************************************************************************
CopyBit:
	; inputs: a, b, c
	; does  : a[c] = b[c]

	di
	push	de
	push	af
	push	bc

	ld	a, b			; a' = b
	ld	b, c			; b' = c
	call	IsSet			; a' = (b[c] == 1)
	ld	d, a			; d' = (b[c] == 1)

	cp	a, 0
	jr	z, .zero
	jr	.one

.zero
	call	ZeroLeftShiftN		; a' = 11011111
	pop	bc

	ld	d, a			; d' = 11011111
	pop	af
	ld	e, a			; e' = aaaaaaaa

	push	af
	ld	a, e			; a' = aaaaaaaa
	and	a, d			; a' = aa0aaaaa
	ld	d, a			; d' = aa0aaaaa
	pop	af

	; now we only have "de" on stack

	ld	a, d			; a  = aa0aaaaa
	pop	de
	reti
.one
	call	OneLeftShiftN		; a' = 00100000
	pop	bc

	ld	d, a			; d' = 00100000
	pop	af
	ld	e, a			; e' = aaaaaaaa

	push	af
	ld	a, e			; a' = aaaaaaaa
	or	a, d			; a' = aa1aaaaa
	ld	d, a			; d' = aa1aaaaa
	pop	af

	; now we only have "de" on stack

	ld	a, d			; a  = aa1aaaaa
	pop	de
	reti


; ****************************************************************************************
GTE:
	; inputs: a, b
	; output: a

	di
	sub	a, b
	jr	c, .isLess
	jp	.isGTE
.isGTE
	ld	a, $31
	reti
.isLess
	ld	a, $30
	reti


; ****************************************************************************************
; hard-coded data
; ****************************************************************************************
Title:
	DB	"tolga"
TitleEnd:

; ****************************************************************************************
; StopLCD:
; turn off LCD if it is on
; and wait until the LCD is off
; ****************************************************************************************
StopLCD:
        ld      a,[rLCDC]
        rlca                    ; Put the high bit of LCDC into the Carry flag
        ret     nc              ; Screen is off already. Exit.

; Loop until we are in VBlank

.wait:
        ld      a,[rLY]
        cp      145             ; Is display on scan line 145 yet?
        jr      nz,.wait        ; no, keep waiting

; Turn off the LCD

        ld      a,[rLCDC]
        res     7,a             ; Reset bit 7 of LCDC
        ld      [rLCDC],a

        ret
