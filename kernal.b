; cbm 600/700 Monitor by David Viner
; KERNAL.PRG
; comments+labels vossi 05/2020
; P500 patches vossi 05/2020
; fix01 changed ptr to same address in monitor
; fix02 default p500 irq - changeable by basic poke, reset+nmi vector to monitor break
; fix03 permanently changed primm to faster routine
!cpu 6502
!ct pet
!to "kernal.prg", cbm
!initmem $00
; switches

; BASIC: 600/700 = POKE 65098,123 / default P500 POKE 65098,130
; constants
irom	= $f		; System bank
; -------------------------------------------------------------------------------------------------
; zero page
e6509	= $00		; 6509 execution bank reg
i6509	= $01		; 6509 indirect bank reg

tmpbnk	= $a9		; temporaray ibank storage
ptr	= $aa		; 2 bytes pointer to message char
ipoint	= $ac		; tx routine usage
tpiptr	= $ae		; pointer to TPI
; -------------------------------------------------------------------------------------------------
; absolute

; System stack area
stack	= $0100		; Stack
stackp	= $01ff		; System Stack pointer transx code

mbreak	= $e003		; monitor break entry
mirq	= $e013		; monitor irq entry

tpi1	= $de00		; TPI1 active interrupt reg
air	= $7		; Active interrupt register
; -------------------------------------------------------------------------------------------------
; system entrys

bsout	= $ffd2
 
udtime	= $f980		; P500 update system time P500
;udtime	= $f979		; CBM2 600/700 update system time
nmi	= mbreak	; nmi P500
start	= mbreak 	; reset P500
; -------------------------------------------------------------------------------------------------
*= $fe00
; $fe00 interrupt handler
irq:	pha			; save regs
	txa
	pha
	tya
	pha
	lda #<(tpi1+air)	; init pointer to TPI1 irq reg
	sta tpiptr
	lda #>(tpi1+air)
	sta tpiptr+1
	tsx
	lda stack+4,x		; check break flag
	and #$10
	beq nobrk
	jmp mbreak		; jump to monitor break entry
nobrk:	lda i6509
	pha			; save ibank
	cld
	ldy #0
	lda #irom
	sta i6509		; switch to systembank
	lda (tpiptr),y		; and load TPI interrupt reg
	beq noirq			; skip if no irq
	and #$1e
	bne nothz		; skip if not 50/60Hz irq
	jsr monirq
	jsr timirq
nothz:	ldy #0
	sta (tpiptr),y		; clear interrupt
noirq:	pla
	sta i6509		; restore ibank, regs
	pla
	tay
	pla
	tax
	pla
	rti
; $fe3d
monirq:	lda #>mirq+2		; call monitor irq
	pha
	lda #<mirq+2
	pha
	jmp exnmi
; $fe46
timirq:	lda #>udtime+2		; call system timer irq
	pha
	lda #<udtime+2		; operand $fe4a = 65098
	pha
	jmp exnmi
; $fe4f
iprimm:				; print immediate (missing in cbm2/p500 kernal)
	pha			; ...zero-terminated text behind jsr
	txa
	pha
	tya
	pha
	ldx i6509
	stx tmpbnk		; remember ibank
	ldx e6509
	stx i6509		; switch to system bank
	tsx
	ldy #1			; set index to return address +1 for text
	lda stack+4,x		; get return address from stack
	sta ptr			; save to pointer
	lda stack+5,x
	sta ptr+1
msglp:	lda(ptr),y		; load char
	beq msgend		; exit if null
	jsr bsout		; print char
	iny
	bne msglp		; next char, exit after max. 255 bytes
msgend:	tya			; get index of null byte
	clc
	adc ptr			; add pointer
	sta stack+4,x		; store new return address to stack
	lda #0
	adc ptr+1
	sta stack+5,x
	ldx tmpbnk		; restore ibank
	stx i6509
	pla
	tay
	pla
	tax
	pla
	rts
; $fe8b
; -------------------------------------------------------------------------------------------------
*= $fe9d
; ##### transx #####
; txjmp - transfer-of-execution jumper
;   entry - .a=seg # .x=low .y=high
;   caller must be a jsr txjmp
;   all registers and i6509 destroyed
;   returns directly to caller...
txjmp	sta i6509		; bp routine
	txa
	clc
	adc #2			; add 2 to target address
	bcc txjmp1
	iny
txjmp1:	tax
	tya
	pha			; store target+2 to stack
	txa
	pha
	jsr ipinit		; go initilize ipoint
	lda #$fe
	sta (ipoint),y		; $fe to top of foreign stack
; 04/14/83 bp
; transfer exec routines for cbm2
; -------------------------------------------------------------------------------------------------
; FEB3 Support routine for cross bank calls
exsub:	php			; save status
	sei			; disable interrupts
	pha			; .a
exsub3:	txa
	pha			; .x
	tya
	pha			; .y
	jsr ipinit		; init ipoint and load stack from xfer seg
	tay			; .y is xfer seg stack pointer
	lda e6509		; push return segment to user stack
	jsr putas		; push .a to other stack
	lda #<excrt2		; xfer seg rts routn
	ldx #>excrt2		; xfer seg rts routn
	jsr putaxs		; put .a.x to xfer seg stack
	tsx
	lda stack+5,x		; .sp +5 is actual routn addr lo
	sec
	sbc #$03		; -3 for jsr to this routn
	pha			; save .a
	lda stack+6,x		; hi addr
	sbc #$00
	tax			; .x hi
	pla			; restore .a lo
	jsr putaxs		; save .a.x onto xfer seg stack
	tya			; xfer seg stack pointer
excomm:	sec
	sbc #$04		; 4 bytes .y.x.a.p
	sta stackp		; xfer seg new stack pointer temp storage
	tay			; use this as new pointer also
	ldx #$04		; 4 bytes .y.x.a.p
exsu10:	pla
	iny
	sta (ipoint),y		; push regs from this stack to xfer seg stack
	dex
	bne exsu10
	ldy stackp		; restore .y as stack pointer for xfer seg
	lda #<expul2		; pull regs and rts routn
	ldx #>expul2		; .hi prendn routn in xfer seg
	jsr putaxs		; put .a.x on xfer seg stack
	pla			; fix stack
	pla			; fix stack
exgby:	tsx
	stx stackp		; save current stack pointer this seg
	tya			; .y is stack pointer for xfer seg
	tax
	txs			; new stack for xfer seg
	lda i6509		; xfer seg #
	jmp gbye		; good bye
; -------------------------------------------------------------------------------------------------
	nop			; returns here if rti
; FF06 Return from call to foreign bank
excrts: php			; .p
	php			; .p
	sei             	; dis ints
	pha			; .a
	txa
	pha			; .x
	tya
	pha			; .y
	tsx
	lda stack+6,x		; sp +6 is return seg
	sta i6509		; restore i6509 to return seg
	jsr ipinit		; init ipoint and load stack from xfer seg
	jmp excomm
; -------------------------------------------------------------------------------------------------
; FF19 ipoint = $100, Y = $FF (stack)
ipinit: ldy #$01
	sty ipoint+1
	dey
	sty ipoint		; ipoint=$0100
	dey			; .y=$ff
	lda (ipoint),y		; load stack pointer from $001ff
	rts
; -------------------------------------------------------------------------------------------------
; FF24 Place X/A to ipoint (build stack in foreign bank)
putaxs: pha			; save .a
	txa
	sta (ipoint),y		; .x hi
	dey
	pla
; FF2A Place A to ipoint (build stack in foreign bank)
putas:  sta (ipoint),y		; .a lo
	dey
	rts
; -------------------------------------------------------------------------------------------------
; FF2E Pull registers after calling subroutine in foreign bank
expull: pla
	tay			; .y
	pla
	tax			; .x
	pla			; .a
	plp			; .p
	rts
;
excrt2=excrts-1
expul2=expull-1
; -------------------------------------------------------------------------------------------------
; FF35 Patched routine to route interrupts from foreign to system bank
exnmi:	php
	sei
	pha
	lda #irom
	sta i6509
	jmp exsub3		; 3 bytes later entry to exsub
; -------------------------------------------------------------------------------------------------
primm:	jmp iprimm
	jmp $0000
	jmp $0000
	jmp $0000
	jmp $0000
	jmp $0000
	jmp $0000
	jmp $0000
	jmp $0000
	jmp $0000
	jmp $0000
	jmp $0000
	jmp $0000
	jmp $0000
	jmp $0000
	jsr exnmi
	jsr exnmi
	jsr exnmi
	jsr exnmi
	jsr exnmi
	jsr exnmi
	jsr exnmi
	jsr exnmi
	jsr exnmi
	jsr exnmi
	jsr exnmi
	jsr exnmi
	jsr exnmi
	jsr exnmi
	jsr exnmi
	jsr exnmi
	jsr exnmi
	jsr exnmi
	jsr exnmi
	jsr exnmi
	jsr exnmi
	jsr exnmi
	jsr exnmi
	jsr exnmi
	jsr exnmi
	jsr exnmi
	jsr exnmi
	jsr exnmi
	jsr exnmi
	jsr exnmi
	jsr exnmi
	jsr exnmi
	jsr exnmi
	jsr exnmi
bsout:	jsr exnmi
	jsr exnmi
	jsr exnmi
	jsr exnmi
	jsr exnmi
	jsr exnmi
	jsr exnmi
	jsr exnmi
	jsr exnmi
	jsr exnmi
	jsr exnmi
	jsr exnmi
; -------------------------------------------------------------------------------------------------
; FFF6 Actual execution segment switch routine
gbye:	sta e6509		; goodbye...
	rts
	!byte $01
*= $fffa
; -------------------------------------------------------------------------------------------------
; FFFA Hardware vectors
hwnmi:  !word nmi		; FB3D Program defineable
hwres:	!word start		; F99E Initialization code
hwirq:	!word irq		; Interrupt handler