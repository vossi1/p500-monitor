; cbm 600/700 Monitor by David Viner (converted from c128)
; MONITOR.PRG
; comments+labels vossi 05/2020
; P500 patches vossi 05/2020
!cpu 6502
!ct pet
; switches
;P500	= 1

!ifdef P500{
	!to "monitor500.prg", cbm
	!initmem $00
} else{
	!to "monitor.prg", cbm
	!initmem $ff
}
; constants
cr	= $0d
esc	= $1b
; -------------------------------------------------------------------------------------------------
; zero page stuff
i6509	= $01		; 6509 indirect bank reg

pcb	= $02
pch	= $03		;in basic's area
pcl	= $04
flgs	= $05
acc	= $06
xr	= $07
yr	= $08
sp	= $09

temp	= $5f		;use basic fac for monitor zp

t0	= $60		;three pointer
t1	= $63
t2	= $66

txtptr	= $7a

;fa	= $ba		;kernal definitions
;fnadr	= $bb
fnlen	= $b7
mode	= $d7
msgflg	= $9d
ndx	= $d0
;sa	= $b9
status	= $90
tmpc	= $9f
verck	= $93
;ba	= $c6
;fnbnk	= $c7

; -------------------------------------------------------------------------------------------------
; absolute monitor storage

bad	= $100		;fbuffr
buf	= $200		;input buffer
keyd	= $34a		;keyboard buffer

;exmon	= $32e		;indirect to command parser

stavec	= $2af+10	;'stash' indirect
cmpvec	= $2be+10	;'cmpare' indirect

	* = $0a80	;monitor's domain

xcnt	*=*+32		;compare buffer
hulp	*=*+10
format	*=*+1
length	*=*+1		;asm/dis
msal	*=*+3		;for assembler
sxreg	*=*+1		;1 byte temp used all over
syreg	*=*+1		;1 byte temp used all over
wrap	*=*+1		;1 byte temp for assembler
xsave	*=*+1		;save .x here during indirect subroutine calls
direction	= $03b3	;direction indicator for 'transfer'
count	*=*+1		;parse number conversion
number	*=*+1		;parse number conversion
shift	*=*+1		;parse number conversion
temps		= $03b7
; -------------------------------------------------------------------------------------------------
; system entrys

basin	= $ffcf		;kernal jump table
bsout	= $ffd2
chkin	= $ffc6
chkout	= $ffc9
close	= $ffc3
clrch	= $ffcc
open	= $ffc0
setlfs	= $ffba
setnam	= $ffbd
setbnk	= $ff68
stop	= $ffe1
_load	= $ffd5
_save	= $ffd8
primm	= $ff3f
_setmsg	= $ff90

hw_irq	= $fffe
; -------------------------------------------------------------------------------------------------
;///////////   M O N I T O R   J U M P   E N T R Y   P O I N T S   \\\\\\\\\\\\
*= $e000
monitor:
	jmp call	;'jmp' entry
mbreak:	jmp break	;'brk' entry
	jmp moncmd	;command parser
; $e009
break:  		;////// entry for 'brk'
	jsr primm
	!pet cr, "break", 7, 0
!ifndef P500{
	nop		; save 3 bytes for message "monitor500" ;)
	nop
	nop
}
	ldx #5
-	pla		;pull pc, registers & status off stack...
	sta pch,x	;...and preserve them for display
	dex
	bpl -		; (notice pc will be wrong- processor bug)
	bmi start
; $e021
call:			;////// entry for 'jmp' or 'sys'
	jsr setmod	; set 40/80 col mode, enable bell if cbm2
	lda #$00
	sta acc		;clear everything up for user
	sta xr
	sta yr
	sta flgs
	lda #<monitor	;init pc to ourselves
	ldy #>monitor
	sta pcl
	sty pch
	lda $00
	sta pcb
	jsr primm
!ifdef P500{
	!pet cr, "monitor500", 0
} else{

	!pet cr, "monitor", 0
}
; $e046
start:  
	cld
	tsx
	stx sp		;get stack pointer for register display
	lda #$c0
	jsr _setmsg	;enable kernal control & error messages
	cli		;start monitor: fall thru 'dspreg' to 'main'
;***********************************************************
;	Display contents of storage registers
;***********************************************************
; $e050
dspreg:
	jsr primm	;register legends
	!pet cr, "    pc  sr ac xr yr sp"
	!pet cr, "; ", esc, "q", 0
	lda pcb
	jsr makhex
	txa
	jsr bsout	;display bank # (just lower nybble)
	lda pch
	jsr puthex	;display pc high

	ldy #2
-	lda pcb,y
	jsr puthxs	;display rest, separated by spaces
	iny
	cpy #8
	bcc -
; $e08b
main:
	jsr crlf
	ldx #0
	stx txtptr

-	jsr basin	;read one line (up to <cr>) into buffer
	sta buf,x
	inx
	cpx #161
	bcs error	;...branch if 'line too long'
	cmp #cr
	bne -		;loop until end of line

	lda #0
	sta buf-1,x	;flag end of line

-	jsr gnc	  	;get a character from buffer
	beq main	;end of line
	cmp #' '	;skip leading spaces
	beq -
	nop
	nop
	nop
; $e0b2
moncmd:
	ldx #$15	;compare first char to list of valid commands
-	cmp cmdchr,x
	beq main1	;found it in list!
	dex
	bpl -
; $e0c2			;checked entire list, not found. fall into 'error'
error:
	jsr primm
	!pet $1d, "?", 0
	jmp main
; $e0c9
main1:
	cpx #cmdls	;is command 'L'oad, 'S'ave, or 'V'erify?
	bcs +		;...branch if so:  can't use parse!

	cpx #cmdno	;is it a number to evaluate?
	bcs ++		;...branch if so

	txa		;multiply index by two
	asl
	tax

	lda cmdtbl+1,x	;push address of command on stack
	pha
	lda cmdtbl,x
	pha
	jmp parse	;parse first arg, and rts to correct command routine

+	sta verck	;save copy of what command was,
	jmp lodsav	;...and jump to common load/save/verify routine

++	jmp convert	;simply evaluate number & print its value
; $e0e3
exit:
	rts
	!byte $00, $00
; $e0e6
cmdchr
	!byte 'a'	;assemble
	!byte 'c'	;compare
	!byte 'd'	;disassemble
	!byte 'f'	;fill
	!byte 'g'	;go (jmp to program)
	!byte 'h'	;hunt
	!byte 'j'	;jsr to subroutine
	!byte 'm'	;memory dump
	!byte 'r'	;display registers, pc & stack pointer
	!byte 't'	;transfer
	!byte 'x'	;exit (jmp to basic warm start)

	!byte '@'	;disk status/command
	!byte '.'	;alter assembly
	!byte '>'	;alter memory
	!byte ';'	;alter regs

cmdno	= *-cmdchr
cmdnum
	!byte '$'	;base-16 (hex)
	!byte '+'	;base-10 (dec)
	!byte '&'	;base-8  (oct)
	!byte '%'	;base-2  (bin)

cmdls	= *-cmdchr	;  (l,s & v must be last in table)

	!byte 'l'	;load memory
	!byte 's'	;save memory
	!byte 'v'	;verify memory

cmdqty	= *-cmdchr
; $e1fc
cmdtbl:
	!word assem-1
	!word compar-1
	!word disasm-1
	!word fill-1

	!word go-1
	!word hunt-1
	!word gosub-1
	!word dspmem-1
	!word dspreg-1
	!word trnsfr-1
	!word exit-1

	!word disk-1
	!word assem-1
	!word setmem-1
	!word setreg-1
; $e11a
fetch:	jsr ++
	lda (t2),y
	jmp +
stash:	jsr ++
	sta (t2),y
+	ldx $30
	stx i6509
	ldx $03b2
	rts

++	stx $03b2
	ldx i6509
	stx $30
	ldx t2+2
	stx i6509
	rts

le13b:	ldx i6509
	lda #$0f
	sta i6509
	ldy #$9c
	lda ($5d),y
	stx i6509
	rts
;********************************************
;	Display memory command
;********************************************
*= $e152
; $e152
dspmem:
	bcs dsp12l	;no range, do 1/2 screen
	jsr t0tot2	;else move 'from' value into place
	jsr parse
	bcc dspcalc	;got 'to', go dump

dsp12l:	lda #11	  	;do 12 lines
	sta t0
	bne dspdump	;always

; calculate # of lines
dspcalc:jsr sub0m2	;calculate # bytes, put result in t0 (msb goes in .a)
	bcc dsperr	;...branch if sa > ea

	ldx #$03
	bit mode	;divide by 8 if 40-col, 16 if 80-col
	bpl dspshft
	inx

dspshft:lsr t0+2	;shift msb right,
	ror t0+1	;..into middle byte,
	ror t0		;..and finally into lsb
	dex
	bne dspshft

dspdump:jsr stop	;is stop key down?
	beq dspexit	;..if so, exit.

	jsr dmpone
	lda #$08
	bit mode	;add 8 (16 if 80-col) to line start address
	bpl dsp40c
	asl

dsp40c	jsr addt2
	jsr dect0	;test if dump finished
	bcs dspdump	;loop until underflow
dspexit:jmp main

dsperr:	jmp error

;********************************************
;	Set register command
;********************************************
; $e194
setreg:
	jsr t0topc	;copy adr & bank to pcl,h & pcb,  if given
	ldy #$00
-	jsr parse
	bcs +		;quit anytime arg list is empty
	lda t0
	sta $0005,y
	iny
	cpy #$05
	bcc -
+	jmp main
;********************************************
;	Alter memory command
;********************************************
; $e1ab
setmem:
	bcs +++		;...branch if no arguments- just regurgitate existing data
	jsr t0tot2	;destination bank, addr in 't2'
	ldy #0

-	jsr parse	;scan for next argument
	bcs +++		;...branch if eol
	lda t0		;get the byte to stash
	jsr stash	;stash it
	iny
	bit mode
	bpl +
	cpy #16
	bcc -
+	cpy #8
	bcc -

+++	jsr primm	;clear all modes & cursor up
	!pet esc, "o", $91, 0

	jsr dmpone
	jmp main
;********************************************************************
;	Go command- start executing at either the supplied address,
;	   or (default) the current contents of the PC reg
;********************************************************************
; $e1d6
go:
	jsr t0topc	;copy adr & bank to pcl,h & pcb,  if given

	ldx sp
	txs		;set up stack pointer
	jmp goto
; gosub moved to the end
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
;********************************************
;	Subroutine to dump one line
;	of memory on screen
;********************************************
; $e1e8
dmpone:	
	jsr crlf
	lda #'>'	;print dump prompt
	jsr bsout
	jsr putt2	;print address, space.
	ldy #0
	beq dmpentr	;always (skip first space)

dmpbylp:jsr putspc
dmpentr:jsr fetch	;get a byte from memory
	jsr puthex	;print hex byte
	iny
	cpy #8		;8 bytes/line for 40-column mode
	bit mode
	bpl dmp40c
	cpy #16		;16 bytes/line for 80-column mode
dmp40c:	bcc dmpbylp

	jsr primm	;block off ascii dump & turn rvs on
	!pet ":", $12, 0

	ldy #0
dmpchlp:jsr fetch	;re-get byte from memory
	pha
	and #$7f	;mask control characters ($00-$1f and $80-$9f)
	cmp #$20
	pla
	bcs dmpskct
	lda #'.'	;print control characters as '.'

dmpskct:jsr bsout
	iny
	bit mode
	bpl dmp40c2
	cpy #16
	bcc dmpchlp
dmp40c2:cpy #8
	bcc dmpchlp
	rts
;********************************************
;	Transfer/Compare routines.
;
;	T starting-from,thru,to
;	C starting-from,thru,with
;********************************************
compar:
	lda #0	   	;flag 'compare'
	!byte $2c	;Sskip next
trnsfr:
	lda #$80	;flag 'transfer'
	sta verck
	lda #$00
	sta direction
	jsr range	;get source in t2, length in t1
	bcs trnerr
	jsr parse	;get destination in t0
	bcc trnok

trnerr:	jmp error

trnok:	bit verck
	bpl trnnxln		;...branch if compare (direction crap unimportant)

	sec		;determine direction of transfer (to avoid stepping on ourselves!)
	lda t2
	sbc t0		;source - destination (ignore banks...there might be bleed-thru)
	lda t2+1
	sbc t0+1
	bcs trnnxln	;branch if source >= destination

;	clc
	lda t1		;source < destination,   must work from back to front
	adc t0
	sta t0
	lda t1+1	;add length to destination
	adc t0+1
	sta t0+1
	lda t1+2
	adc t0+2
	sta t0+2
	ldx #2
trnsrc:	lda temps,x	;restore ea as source (saved @ 'range')
	sta t2,x
	dex
	bpl trnsrc

	lda #$80
	sta direction	;flag backwards direction

trnnxln:jsr crlf	;start with a new line
	ldy #0

trnlp:  jsr stop
	beq trnx	;...branch if user requests abort
	jsr fetch	;get a byte
	sta temp
	ldx i6509
	lda t0+2	;get bank and switch to
	sta i6509
	lda (t0),y	;get byte
	bit verck
	bpl trncmp	;branch if no copy
	lda temp
	sta (t0),y	;copy byte
trncmp:	stx i6509	;restore ibank
	cmp temp	;compare
	nop
	nop
	beq trnequ
	jsr putt2	;report mismatch
	jsr putspc	;make each number 8 bytes to look pretty
	jsr putspc

trnequ: bit direction
	bmi trndir	;test direction of transfers

	inc t0		;normal
	bne trnnext
	inc t0+1
	bne trnnext	;bra
	jmp error	;disallow bank-wrapping operations

trndir:	jsr dect0	;backwards
	jsr dect2
	jmp trnback

trnnext:jsr inct2
trnback:jsr dect1
	bcs trnlp

trnx:	jmp main
;******************************************************************
;	Hunt command - hunt for bytes or string
;
; syntax:  h 0000 1111 'ascii...   <or>   h 0000 1111 22 33 44 ...
;******************************************************************
; $e2ce
hunt:
	jsr range
	bcs le334
	ldy #$00
	jsr gnc
	cmp #$27
	bne le2f2
	jsr gnc
	cmp #$00
	beq le334
le2e3:  sta $0380,y
	iny
	jsr gnc
	beq le307
	cpy #$20
	bne le2e3
	beq le307
le2f2:  sty bad
	jsr le7a5
le2f8:  lda t0
	sta $0380,y
	iny
	jsr parse
	bcs le307
	cpy #$20
	bne le2f8
le307:  sty verck
	jsr crlf
le30c:  ldy #$00
le30e:  jsr fetch
	cmp $0380,y
	bne le324
	iny
	cpy verck
	bne le30e
	jsr putt2
	jsr putspc
	jsr putspc
le324:  jsr $ffe1
	beq le331
	jsr inct2
	jsr dect1
	bcs le30c
le331:  jmp main
le334:  jmp error
lodsav:  jsr lebae
le33a:  jsr gnc
	beq le3a3
	cmp #$20
	beq le33a
	cmp #$22
	bne le35c
	ldx txtptr
le349:  lda buf,x
	beq le3a3
	inx
	cmp #$22
	beq le35f
	sta ($90),y
	inc msgflg
	iny
	cpy #$11
	bcc le349
le35c:  jmp error
le35f:  stx txtptr
	jsr gnc
	beq le3a3
	jsr parse
	bcs le3a3
	lda t0
	sta $9f
	jsr parse
	bcs le3a3
	jsr t0tot2
	jsr parse
	bcs le3a9
	jsr crlf
	lda verck
	cmp #$53
	bne le35c
	lda #$00
	sta $a0
	ldx #$02
le38b:  lda t2,x
	sta $99,x
	lda t0,x
	sta $96,x
	dex
	bpl le38b
	jsr lebca
	ldx #$99
	ldy #$96
	jsr $ffd8
	jmp main
le3a3:  ldx #$ff
	stx t2
	stx t2+1
le3a9:  lda #$00
	sta $a0
	jsr lebca
	lda verck
	cmp #$56
	beq +
	cmp #$4c
	bne le35c
	lda #$00
	!byte $2c	; skip next
+	lda #$80
	ldx t2
	ldy t2+1
	ora t2+2
	jsr $ffd5
	jmp lebed

	!byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
	!byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff

fill:
	jsr range
	bcs le403
	lda t2+2
	cmp $03b9
	bne le403
	jsr parse
	bcs le403
	ldy #$00
le3ee:  lda t0
	jsr stash
	jsr $ffe1
	beq le400
	jsr inct2
	jsr dect1
	bcs le3ee
le400:  jmp main
le403:  jmp error

assem:
	bcs le442
	jsr t0tot2
le40b:  ldx #$00
	stx $03a1
	stx $03b4
le413:  jsr gnc
	bne le41f
	cpx #$00
	bne le41f
	jmp main
le41f:  cmp #$20
	beq le40b
	sta $03ac,x
	inx
	cpx #$03
	bne le413
le42b:  dex
	bmi le445
	lda $03ac,x
	sec
	sbc #$3f
	ldy #$05
le436:  lsr
	ror $03a1
	ror $03a0
	dey
	bne le436
	beq le42b
le442:  jmp error
le445:  ldx #$02
le447:  lda $03b4
	bne le47c
	jsr le7ce
	beq le47a
	bcs le442
	lda #$24
	sta $03a0,x
	inx
	lda t0+2
	bne le442
	ldy #$04
	lda $03b6
	cmp #$08
	bcc le46b
	cpy $03b4
	beq le471
le46b:  lda t0+1
	bne le471
	ldy #$02
le471:  lda #$30
le473:  sta $03a0,x
	inx
	dey
	bne le473
le47a:  dec txtptr
le47c:  jsr gnc
	beq le48f
	cmp #$20
	beq le447
	sta $03a0,x
	inx
	cpx #$0a
	bcc le447
	bcs le442
le48f:  stx t1
	ldx #$00
	stx $03b1
le496:  ldx #$00
	stx $9f
	lda $03b1
	jsr le659
	ldx $03aa
	stx t1+1
	tax
	lda $e761,x
	jsr le57f
	lda $e721,x
	jsr le57f
	ldx #$06
le4b4:  cpx #$03
	bne le4cc
	ldy $03ab
	beq le4cc
le4bd:  lda $03aa
	cmp #$e8
	lda #$30
	bcs le4e4
	jsr le57c
	dey
	bne le4bd
le4cc:  asl $03aa
	bcc le4df
	lda $e714,x
	jsr le57f
	lda $e71a,x
	beq le4df
	jsr le57f
le4df:  dex
	bne le4b4
	beq le4ea
le4e4:  jsr le57c
	jsr le57c
le4ea:  lda t1
	cmp $9f
	beq le4f3
	jmp le58b
le4f3:  ldy $03ab
	beq le52a
	lda t1+1
	cmp #$9d
	bne le521
	lda t0
	sbc t2
	tax
	lda t0+1
	sbc t2+1
	bcc le511
	bne le579
	cpx #$82
	bcs le579
	bcc le519
le511:  tay
	iny
	bne le579
	cpx #$82
	bcc le579
le519:  dex
	dex
	txa
	ldy $03ab
	bne le524
le521:  lda $005f,y
le524:  jsr stash
	dey
	bne le521
le52a:  lda $03b1
	jsr stash
	jsr le8ad
	jsr primm
	!pet "a ", esc, "q", 0
	jsr le5dc
	inc $03ab
	lda $03ab
	jsr addt2
	lda #$41
	sta keyd
	lda #$20
	sta $034b
	sta $0351
	lda t2+2
	jsr makhex
	stx $034c
	lda t2+1
	jsr makhex
	sta $034d
	stx $034e
	lda t2
	jsr makhex
	sta $034f
	stx $0350
	jsr leb87
	nop
	jmp main
le579:  jmp error
le57c:  jsr le57f
le57f:  stx $03af
	ldx $9f
	cmp $03a0,x
	beq le593
	pla
	pla
le58b:  inc $03b1
	beq le579
	jmp le496
le593:  inc $9f
	ldx $03af
	rts

disasm:
	bcs le5a3
	jsr t0tot2
	jsr parse
	bcc le5a9
le5a3:  lda #$14
	sta t0
	bne le5ae
le5a9:  jsr sub0m2
	bcc le5d1
le5ae:  jsr primm
	!pet cr, esc, "q", 0
	jsr $ffe1
	beq le5ce
	jsr le5d4
	inc $03ab
	lda $03ab
	jsr addt2
	lda $03ab
	jsr le924
	bcs le5ae
le5ce:  jmp main
le5d1:  jmp error
le5d4:  lda #$2e
	jsr bsout
	jsr putspc
le5dc:  jsr putt2
	jsr putspc
	ldy #$00
	jsr fetch
	jsr le659
	pha
	ldx $03ab
	inx
le5ef:  dex
	bpl le5fc
	jsr primm
	!pet "   ", 0
	jmp le602
le5fc:  jsr fetch
	jsr puthxs
le602:  iny
	cpy #$03
	bcc le5ef
	pla
	ldx #$03
	jsr le6a1
	ldx #$06
le60f:  cpx #$03
	bne le62a
	ldy $03ab
	beq le62a
le618:  lda $03aa
	cmp #$e8
	php
	jsr fetch
	plp
	bcs le641
	jsr puthex
	dey
	bne le618
le62a:  asl $03aa
	bcc le63d
	lda $e714,x
	jsr bsout
	lda $e71a,x
	beq le63d
	jsr bsout
le63d:  dex
	bne le60f
	rts
le641:  jsr le64d
	clc
	adc #$01
	bne le64a
	inx
le64a:  jmp le89f
le64d:  ldx t2+1
	tay
	bpl le653
	dex
le653:  adc t2
	bcc le658
	inx
le658:  rts
le659:  tay
	lsr
	bcc le668
	lsr
	bcs le677
	cmp #$22
	beq le677
	and #$07
	ora #$80
le668:  lsr
	tax
	lda nmode,x
	bcs le673
	lsr
	lsr
	lsr
	lsr
le673:  and #$0f
le675:  bne le67b
le677:  ldy #$80
	lda #$00
le67b:  tax
	lda $e707,x
	sta $03aa
	and #$03
	sta $03ab
	tya
	and #$8f
	tax
	tya
	ldy #$03
	cpx #$8a
	beq le69d
le692:  lsr
	bcc le69d
	lsr
le696:  lsr
	ora #$20
	dey
	bne le696
	iny
le69d:  dey
	bne le692
	rts
le6a1:  tay
	lda $e721,y
	sta t1
	lda $e761,y
	sta t1+1
le6ac:  lda #$00
	ldy #$05
le6b0:  asl t1+1
	rol t1
	rol
	dey
	bne le6b0
	adc #$3f
	jsr bsout
	dex
	bne le6ac
	jmp putspc
nmode:
	!byte $40,2,$45,3
	!byte $d0,8,$40,9
	!byte $30,$22,$45,$33
	!byte $d0,8,$40,9
	!byte $40,2,$45,$33
	!byte $d0,8,$40,9
	!byte $40,$02,$45,$b3
	!byte $d0,$08,$40,$09
	!byte 0,$22,$44,$33
	!byte $d0,$8c,$44,0
	!byte $11,$22,$44,$33
	!byte $d0,$8c,$44,$9a
	!byte $10,$22,$44,$33
	!byte $d0,8,$40,9
	!byte $10,$22,$44,$33
	!byte $d0,8,$40,9
	!byte $62,$13,$78,$a9
nmode2
	!byte 0,$21,$81,$82
	!byte 0,0,$59,$4d
	!byte $91,$92,$86,$4a
	!byte $85,$9d
char1
	!pet ",),#($"
char2
	!pet "y",0,"x$$",0
mneml
	!byte $1c,$8a,$1c,$23
	!byte $5d,$8b,$1b,$a1
	!byte $9d,$8a,$1d,$23
	!byte $9d,$8b,$1d,$a1
	!byte 0,$29,$19,$ae
	!byte $69,$a8,$19,$23
	!byte $24,$53,$1b,$23
	!byte $24,$53,$19,$a1
	!byte 0,$1a,$5b,$5b
	!byte $a5,$69,$24,$24
	!byte $ae,$ae,$a8,$ad
	!byte $29,0,$7c,0
	!byte $15,$9c,$6d,$9c
	!byte $a5,$69,$29,$53
	!byte $84,$13,$34,$11
	!byte $a5,$69,$23,$a0
mnemr
	!byte $d8,$62,$5a,$48
	!byte $26,$62,$94,$88
	!byte $54,$44,$c8,$54
	!byte $68,$44,$e8,$94
	!byte 0,$b4,8,$84
	!byte $74,$b4,$28,$6e
	!byte $74,$f4,$cc,$4a
	!byte $72,$f2,$a4,$8a
	!byte 0,$aa,$a2,$a2
	!byte $74,$74,$74,$72
	!byte $44,$68,$b2,$32
	!byte $b2,0,$22,0
	!byte $1a,$1a,$26,$26
	!byte $72,$72,$88,$c8
	!byte $c4,$ca,$26,$48
	!byte $44,$44,$a2,$c8
regk
	!byte cr,$20,$20,$20

le7a5:	dec txtptr
parse:  jsr le7ce
	bcs le7c2
	jsr le8e7
	bne le7ba
	dec txtptr
	lda $03b4
	bne le7c9
	beq le7c7
le7ba:  cmp #$20
	beq le7c9
	cmp #$2c
	beq le7c9
le7c2:  pla
	pla
	jmp error
le7c7:  sec
	!byte $24	; skip next
le7c9:	clc
	lda $03b4
	rts
le7ce:  lda #$00
	sta t0
	sta t0+1
	sta t0+2
	sta $03b4
	txa
	pha
	tya
	pha
le7dd:  jsr gnc
	bne le7e5
	jmp le87e
le7e5:  cmp #$20
	beq le7dd
	ldx #$03
le7eb:  cmp cmdnum,x
	beq le7f6
	dex
	bpl le7eb
	inx
	dec txtptr
le7f6:  ldy bases,x
	lda shifts,x
	sta $03b6
le7ff:  jsr gnc
	beq le87e
	sec
	sbc #$30
	bcc le87e
	cmp #$0a
	bcc le813
	sbc #$07
	cmp #$10
	bcs le87e
le813:  sta $03b5
	cpy $03b5
	bcc le87c
	beq le87c
	inc $03b4
	cpy #$0a
	bne le82e
	ldx #$02
le826:  lda t0,x
	sta temps,x
	dex
	bpl le826
le82e:  ldx $03b6
le831:  asl t0
	rol t0+1
	rol t0+2
	bcs le87c
	dex
	bne le831
	cpy #$0a
	bne le862
	asl temps
	rol $03b8
	rol $03b9
	bcs le87c
	lda temps
	adc t0
	sta t0
	lda $03b8
	adc t0+1
	sta t0+1
	lda $03b9
	adc t0+2
	sta t0+2
	bcs le87c
le862:  clc
	lda $03b5
	adc t0
	sta t0
	txa
	adc t0+1
	sta t0+1
	txa
	adc t0+2
	sta t0+2
	bcs le87c
	and #$f0
	bne le87c
	beq le7ff
le87c:  sec
	!byte $24	; skip next
le87e:	clc
	sty $03b6
	pla
	tay
	pla
	tax
	lda $03b4
	rts

bases:	!byte 16,10, 8, 2
shifts:	!byte  4, 3, 3, 1

; $8892 print t2 as 5 hex digits:	BHHLL
putt2:	lda t2+2
	jsr makhex
	txa
	jsr bsout
	lda t2
	ldx t2+1

le89f:  pha
	txa
	jsr puthex
	pla
puthxs:  jsr puthex
putspc:  lda #$20
	jmp bsout
le8ad:  jsr primm
	!pet cr, $91, 0
	rts
crlf:  lda #$0d
	jmp bsout
le8b9:  jsr primm
	!pet cr, esc, "q ", 0
	rts
puthex:  stx $03af
	jsr makhex
	jsr bsout
	txa
	ldx $03af
	jmp bsout
makhex:  pha
	jsr le8dc
	tax
	pla
	lsr
	lsr
	lsr
	lsr
le8dc:  and #$0f
	cmp #$0a
	bcc le8e4
	adc #$06
le8e4:  adc #$30
	rts
le8e7:  dec txtptr
gnc:  stx $03af
	ldx txtptr
	lda buf,x
	beq le8f9
	cmp #$3a
	beq le8f9
	cmp #$3f
le8f9:  php
	inc txtptr
	ldx $03af
	plp
	rts
t0tot2:  lda t0
	sta t2
	lda t0+1
	sta t2+1
	lda t0+2
	sta t2+2
	rts
sub0m2:  sec
	lda t0
	sbc t2
	sta t0
	lda t0+1
	sbc t2+1
	sta t0+1
	lda t0+2
	sbc t2+2
	sta t0+2
	rts
dect0:  lda #$01
le924:  sta $03af
	sec
	lda t0
	sbc $03af
	sta t0
	lda t0+1
	sbc #$00
	sta t0+1
	lda t0+2
	sbc #$00
	sta t0+2
	rts
dect1:  sec
	lda t1
	sbc #$01
	sta t1
	lda t1+1
	sbc #$00
	sta t1+1
	lda t1+2
	sbc #$00
	sta t1+2
	rts
inct2:  lda #$01
addt2:  clc
	adc t2
	sta t2
	bcc le95f
	inc t2+1
	bne le95f
	inc t2+2
le95f:  rts
dect2:  sec
	lda t2
	sbc #$01
	sta t2
	lda t2+1
	sbc #$00
	sta t2+1
	lda t2+2
	sbc #$00
	sta t2+2
	rts
t0topc:  bcs le982
	lda t0
	ldy t0+1
	ldx t0+2
	sta pcl
	sty pch
	stx pcb
le982:  rts
range:  bcs +
	jsr t0tot2
	jsr parse
	bcs +
	lda t0
	sta temps
	lda t0+1
	sta $03b8
	lda t0+2
	sta $03b9
	jsr sub0m2
	lda t0
	sta t1
	lda t0+1
	sta t1+1
	lda t0+2
	sta t1+2
	bcc +
	clc
	!byte $24	; skip next
+	sec
	rts
convert:  jsr le7a5
	jsr le8b9
	lda #$24
	jsr bsout
	lda t0+2
	beq le9c7
	jsr makhex
	txa
	jsr bsout
le9c7:  lda t0
	ldx t0+1
	jsr le89f
	jsr le8b9
	lda #$2b
	jsr bsout
	jsr lea07
	lda #$00
	ldx #$08
	ldy #$03
	jsr lea5d
	jsr le8b9
	lda #$26
	jsr bsout
	lda #$00
	ldx #$08
	ldy #$02
	jsr lea47
	jsr le8b9
	lda #$25
	jsr bsout
	lda #$00
	ldx #$18
	ldy #$00
	jsr lea47
	jmp main
lea07:  jsr t0tot2
	lda #$00
	ldx #$07
lea0e:  sta $03a0,x
	dex
	bpl lea0e
	inc $03a7
	ldy #$17
	php
	sei
	sed
lea1c:  lsr t2+2
	ror t2+1
	ror t2
	bcc lea33
	clc
	ldx #$03
lea27:  lda $03a4,x
	adc $03a0,x
	sta $03a0,x
	dex
	bpl lea27
lea33:  clc
	ldx #$03
lea36:  lda $03a4,x
	adc $03a4,x
	sta $03a4,x
	dex
	bpl lea36
	dey
	bpl lea1c
	plp
	rts
lea47:  pha
	lda t0
	sta $03a2
	lda t0+1
	sta $03a1
	lda t0+2
	sta $03a0
	lda #$00
	sta $03a3
	pla
lea5d:  sta $03b4
	sty $03b6
lea63:  ldy $03b6
	lda #$00
lea68:  asl $03a3
	rol $03a2
	rol $03a1
	rol $03a0
	rol
	dey
	bpl lea68
	tay
	bne lea84
	cpx #$01
	beq lea84
	ldy $03b4
	beq lea8c
lea84:  inc $03b4
	ora #$30
	jsr bsout
lea8c:  dex
	bne lea63
	rts

disk:
	bne +
	ldx #$08
	!byte $2c	; skip next
+	ldx t0
	cpx #$04
	bcc leb00
	cpx #$1f
	bcs leb00
	stx t0
	lda #$00
	sta t0+2
	sta fnlen
	tax
	jsr $ff68
	jsr gnc
	dec txtptr
	cmp #$24
	beq leb03
	lda #$00
	ldx t0
	ldy #$0f
	jsr $ffba
	jsr $ffc0
	bcs leaf4
	ldx #$00
	jsr $ffc9
	bcs leaf4
leac9:  ldx txtptr
	inc txtptr
	lda buf,x
	beq lead7
	jsr bsout
	bcc leac9
lead7:  jsr $ffcc
	jsr crlf
	ldx #$00
	jsr $ffc6
	bcs leaf4
leae4:  jsr basin
	jsr bsout
	cmp #$0d
	beq leaf4
	lda status
	and #$bf
	beq leae4
leaf4:  jsr $ffcc
	lda #$00
	sec
	jsr $ffc3
	jmp main
leb00:  jmp error
leb03:  ldy #$ff
	ldx txtptr
	dex
leb08:  iny
	inx
	lda buf,x
	bne leb08
	tya
	ldx txtptr
	ldy #$02
	jsr $ffbd
	lda #$00
	ldx t0
	ldy #t0
	jsr $ffba
	jsr $ffc0
	bcs leaf4
	ldx #$00
	jsr $ffc6
	jsr crlf
	ldy #$03
leb2f:  sty t1
leb31:  jsr basin
	sta t0
	lda status
	bne leaf4
	jsr basin
	sta t0+1
	lda status
	bne leaf4
	dec t1
	bne leb31
	jsr lea07
	lda #$00
	ldx #$08
	ldy #$03
	jsr lea5d
	lda #$20
	jsr bsout
leb58:  jsr basin
	beq leb66
	ldx status
	bne leaf4
	jsr bsout
	bcc leb58
leb66:  jsr crlf
	jsr $ffe1
	beq leaf4
	ldy #$02
	bne leb2f

setmod:
!ifdef P500{
	lda #$00
	sta mode
	rts
} else{
	lda hw_irq
	cmp #$00
	bne +
	lda #$ff
	!byte $2c	; skik next
+	lda #$00
	sta mode
	jsr primm
	!pet esc, "h", 0
	rts
}
*= $eb87
leb87:  lda #$ab
	sta $5d
	lda #$03
	sta $5e
	ldx i6509
	lda #$0f
	sta i6509
	ldy #$07
leb97:  lda keyd,y
	sta ($5d),y
	dey
	bpl leb97
	lda #$d1
	sta $5d
	ldy #$00
	sty $5e
	lda #$08
	sta ($5d),y
	stx i6509
	rts
lebae:  ldy #$01
	sty tmpc
	sty $a0
	dey
	sty $98
	sty $9b
	sty $9c
	sty msgflg
	lda #$80
	sta status
	lda #$03
	sta $91
	lda $00
	sta $92
	rts
lebca:  ldx i6509
	lda #$0f
	sta i6509
	lda #$00
	sta $5d
	sta $5e
	ldy #$a0
lebd8:  lda $0000,y
	sta ($5d),y
	dey
	cpy #$95
	bne lebe6
	ldy #$92
	bne lebd8
lebe6:  cpy #$8f
	bne lebd8
	stx i6509
	rts
lebed:  jsr le13b
	and #$10
	bne lebff
	jsr primm
	!pet cr, "ok", cr, 0
	jmp main
lebff:	jsr primm
	!pet " error", 0
	jmp main
;********************************************************************
;	Jsr command- start executing at either the supplied address,
;	   or (default) the current contents of the PC reg.
;	   return is to monitor 'main' loop.
;********************************************************************
gosub:
	jsr t0topc
	lda pcb
	cmp $00
	beq lec3a
	jsr lec1b
	jmp main
lec1b:  sei
	clc
	lda pcl
	adc #$02
	tax
	lda pch
	adc #$00
	pha
	txa
	pha
	lda flgs
	pha
	lda pcb
	sta i6509
	lda acc
	ldx xr
	ldy yr
	pha
	jmp $feb5
lec3a:  lda #$e0
	pha
	!byte $a9	;lda #$8a
goto:	
	txa
	pha
	lda pch
	sta t0+1
	lda pcl
	sta t0
	lda acc
	ldx xr
	ldy yr
	jmp (t0)
!ifndef P500{
*= $efff
	!byte $ff
}