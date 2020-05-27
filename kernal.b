; cbm 600/700 Monitor by David Viner
; KERNAL.PRG
; disassembled by DASM6502b v.3.1 by Marat Fayzullin
; modified by Vossi 02/2019
!cpu 6502
!to "kernal.prg", cbm
*= $fe00
lfe00:  pha
	txa
	pha
	tya
	pha
	lda #$07
	sta $41
	lda #$de
	sta $42
	tsx
	lda $0104,x
	and #$10
	beq lfe18
	jmp $e003
lfe18:  lda $01
	pha
	cld
	ldy #$00
	lda #$0f
	sta $01
	lda ($41),y
	beq lfe34
	and #$1e
	bne lfe30
	jsr lfe3d
	jsr lfe46
lfe30:  ldy #$00
	sta ($41),y
lfe34:  pla
	sta $01
	pla
	tay
	pla
	tax
	pla
	rti
lfe3d:  lda #$e0
	pha
	lda #$15
	pha
	jmp lff35
lfe46:  lda #$f9
	pha
	lda #$7b
	pha
	jmp lff35
lfe4f:  pha
	txa
	pha
	tya
	pha
	ldy #$00
	ldx $01
	stx $30
	ldx $00
	stx $01
lfe5e:  tsx
	inc $0104,x
	bne lfe67
	inc $0105,x
lfe67:  lda $0104,x
	sta $ce
	lda $0105,x
	sta $cf
	lda ($ce),y
	beq lfe7a
	jsr lffd2
	bcc lfe5e
lfe7a:  ldx $30
	stx $01
	pla
	tay
	pla
	tax
	pla
	rts
	brk
	brk
	brk
	brk
	brk
	brk
	brk
	brk
	brk
	brk
	brk
	brk
	brk
	brk
	brk
	brk
	brk
	brk
	brk
	brk
	brk
	brk
	brk
	brk
	brk
	sta $01
	txa
	clc
	adc #$02
	bcc lfea6
	iny
lfea6:  tax
	tya
	pha
	txa
	pha
	jsr lff19
	lda #$fe
	sta ($ac),y
	php
	sei
	pha
lfeb5:  txa
	pha
	tya
	pha
	jsr lff19
	tay
	lda $00
	jsr lff2a
	lda #$04
	ldx #$ff
	jsr lff24
	tsx
	lda $0105,x
	sec
	sbc #$03
	pha
	lda $0106,x
	sbc #$00
	tax
	pla
	jsr lff24
	tya
lfedc:  sec
	sbc #$04
	sta $01ff
	tay
	ldx #$04
lfee5:  pla
	iny
	sta ($ac),y
	dex
	bne lfee5
	ldy $01ff
	lda #$2d
	ldx #$ff
	jsr lff24
	pla
	pla
	tsx
	stx $01ff
	tya
	tax
	txs
	lda $01
	jmp lfff6
	nop
	php
	php
	sei
	pha
	txa
	pha
	tya
	pha
	tsx
	lda $0106,x
	sta $01
	jsr lff19
	jmp lfedc
lff19:	ldy #$01
	sty $ad
	dey
	sty $ac
	dey
	lda ($ac),y
	rts
lff24:	pha
	txa
	sta ($ac),y
	dey
	pla
lff2a:	sta ($ac),y
	dey
	rts
	pla
	tay
	pla
	tax
	pla
	plp
	rts
lff35:	php
	sei
	pha
	lda #$0f
	sta $01
	jmp lfeb5
	jmp lfe4f
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
	jsr lff35
	jsr lff35
	jsr lff35
	jsr lff35
	jsr lff35
	jsr lff35
	jsr lff35
	jsr lff35
	jsr lff35
	jsr lff35
	jsr lff35
	jsr lff35
	jsr lff35
	jsr lff35
	jsr lff35
	jsr lff35
	jsr lff35
	jsr lff35
	jsr lff35
	jsr lff35
	jsr lff35
	jsr lff35
	jsr lff35
	jsr lff35
	jsr lff35
	jsr lff35
	jsr lff35
	jsr lff35
	jsr lff35
	jsr lff35
	jsr lff35
	jsr lff35
	jsr lff35
	jsr lff35
lffd2:	jsr lff35
	jsr lff35
	jsr lff35
	jsr lff35
	jsr lff35
	jsr lff35
	jsr lff35
	jsr lff35
	jsr lff35
	jsr lff35
	jsr lff35
	jsr lff35
lfff6:	sta $00
	rts
	!byte $01
hw_nmi:	!byte $31, $fb
hw_res:	!byte $97, $f9
hw_irq:	!byte $00, $fe
