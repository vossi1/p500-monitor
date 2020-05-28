; cbm 600/700 Monitor by David Viner
; KERNAL.PRG
; comments+labels vossi 05/2020
!cpu 6502
!ct pet
!to "kernal.prg", cbm
!initmem $00

; constants

; Equates
irom	= $f		; System bank

; -------------------------------------------------------------------------------------------------
; zero page
e6509	= $00		; 6509 execution bank reg
i6509	= $01		; 6509 indirect bank reg
ipoint	= $ac		; tx routine usage
lstp	= $ce		; Screen editor start position
lsxp	= $cf		; Screen editor start row

tmpbnk	= $30		; temporaray ibank storage
tpiptr	= $41		; pointer to TPI
; -------------------------------------------------------------------------------------------------
; absolute

; System stack area
stack	= $0100		; Stack
stackp	= $01ff		; System Stack pointer transx code

jbreak	= $e003		; monitor break entry

tpi1	= $de00		; TPI1 active interrupt reg
air	= $7		; Active interrupt register
; -------------------------------------------------------------------------------------------------
; system entrys

bsout	= $ffd2

nmi	= $fb31		; nmi
start	= $f997 	; reset
;nmi	= $fb3d		; nmi P500
;start	= $f99e 	; reset P500
; -------------------------------------------------------------------------------------------------
*= $fe00
irq:	pha
	txa
	pha
	tya
	pha
	lda #<(tpi1+air)
	sta tpiptr
	lda #>(tpi1+air)
	sta tpiptr+1
	tsx
	lda stack+4,x
	and #$10
	beq +
	jmp jbreak
+	lda i6509
	pha
	cld
	ldy #$00
	lda #$0f
	sta i6509
	lda (tpiptr),y
	beq ++
	and #$1e
	bne +
	jsr sub1
	jsr sub2
+	ldy #$00
	sta (tpiptr),y
++	pla
	sta i6509
	pla
	tay
	pla
	tax
	pla
	rti

sub1:	lda #$e0
	pha
	lda #$15
	pha
	jmp exnmi

sub2:	lda #$f9
	pha
	lda #$7b
	pha
	jmp exnmi

iprimm:	pha
	txa
	pha
	tya
	pha
	ldy #$00
	ldx i6509
	stx tmpbnk
	ldx e6509
	stx i6509
-	tsx
	inc stack+4,x
	bne +
	inc stack+5,x
+	lda stack+4,x
	sta lstp
	lda stack+5,x
	sta lsxp
	lda (lstp),y
	beq +
	jsr bsout
	bcc -
+	ldx tmpbnk
	stx i6509
	pla
	tay
	pla
	tax
	pla
	rts
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
	adc #2
	bcc txjmp1
	iny
txjmp1:	tax
	tya
	pha
	txa
	pha
	jsr ipinit		; go initilize ipoint
	lda #$fe
	sta (ipoint),y
; 04/14/83 bp
; transfer exec routines for cbm2
; -------------------------------------------------------------------------------------------------
; FEB3 Support routine for cross bank calls
exsub:	php
	sei
	pha
exsub3:	txa			; entry from exnmi
	pha
	tya
	pha
	jsr ipinit
	tay
	lda e6509
	jsr putas
	lda #<excrt2		;xfer seg rts routn
	ldx #>excrt2		;xfer seg rts routn
	jsr putaxs
	tsx
	lda stack+5,x
	sec
	sbc #$03
	pha
	lda stack+6,x
	sbc #$00
	tax
	pla
	jsr putaxs
	tya
excomm:	sec
	sbc #$04
	sta stackp
	tay
	ldx #$04
exsu10:	pla
	iny
	sta (ipoint),y
	dex
	bne exsu10
	ldy stackp
	lda #<expul2
	ldx #>expul2
	jsr putaxs
	pla
	pla
exgby:	tsx
	stx stackp
	tya
	tax
	txs
	lda i6509
	jmp gbye
; -------------------------------------------------------------------------------------------------
	nop			; returns here if rti
; FF06 Return from call to foreign bank
excrts: php			; P
	php			; P
	sei             	; dis ints
	pha			; A
	txa
	pha			; X
	tya
	pha			; Y
	tsx
	lda stack+6,x		; sp +7 is return seg
	sta i6509		; restore i6509 to return seg
	jsr ipinit		; init ipoint and load stack from xfer seg
	jmp excomm
; -------------------------------------------------------------------------------------------------
; FF19 ipoint = $100, Y = $FF (stack)
ipinit: ldy #$01
	sty ipoint+1
	dey
	sty ipoint		; ipoint=$0100
	dey			; Y=$ff
	lda (ipoint),y		; load stack pointer from $001ff
	rts
; -------------------------------------------------------------------------------------------------
; FF24 Place X/A to ipoint (build stack in foreign bank)
putaxs: pha			; save A
	txa
	sta (ipoint),y		; X hi
	dey
	pla
; FF2A Place A to ipoint (build stack in foreign bank)
putas:  sta (ipoint),y		; A lo
	dey
	rts
; -------------------------------------------------------------------------------------------------
; FF2E Pull registers after calling subroutine in foreign bank
expull: pla
	tay			; Y
	pla
	tax			; X
	pla			; A
	plp			; P
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
	jmp exsub3		; 3bytes later entry to exsub
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