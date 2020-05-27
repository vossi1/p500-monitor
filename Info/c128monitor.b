	.page
	.subttl  MONITOR DECLARATIONS  (03/01/85)

; constants

cr	= $0d

; zero page stuff

pcb	= $02
pch	= $03		;in basic's area
pcl	= $04
flgs	= $05
acc	= $06
xr	= $07
yr	= $08
sp	= $09

	* = $60		;use basic fac for monitor zp

t0	*=*+3
t1	*=*+3
t2	*=*+3

txtptr	= $7a


fa	= $ba		;kernal definitions
fnadr	= $bb
fnlen	= $b7
mode	= $d7
msgflg	= $9d
ndx	= $d0
sa	= $b9
status	= $90
tmpc	= $9f
verck	= $93
ba	= $c6
fnbnk	= $c7

	.page
; absolute monitor storage


bad	= $100		;fbuffr
buf	= $200		;input buffer
keyd	= $34a		;keyboard buffer

exmon	= $32e		;indirect to command parser

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
direction *=*+1		;direction indicator for 'transfer'
count	*=*+1		;parse number conversion
number	*=*+1		;parse number conversion
shift	*=*+1		;parse number conversion
temps

; system entrys

system_vector	= $0a00	;return vector to os
mmucr	= $ff00		;mmu configuration register

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
primm	= $ff7d
_fetch	= $ff74
_stash	= $ff77
_cmpar	= $ff7a
_gosub	= $ff6e
_goto	= $ff71
_setmsg	= $ff90

	.page
	.subttl monitor0 - main

;///////////   M O N I T O R   J U M P   E N T R Y   P O I N T S   \\\\\\\\\\\\

monitor
	jmp call	;'jmp' entry
	jmp break	;'brk' entry
	jmp moncmd	;command parser



break			;////// entry for 'brk'
	jsr primm
	.byte cr,'BREAK',7,0

	pla
	sta pcb		;pull bank off stack

	ldx #5

1$	pla	      	;pull pc, registers & status off stack...
	sta pch,x	;...and preserve them for display
	dex
	bpl 1$		; (notice pc will be wrong- processor bug)
	bmi start


call			;////// entry for 'jmp' or 'sys'
	lda #0
	sta mmucr	;must have system roms, ram0, & i/o in
	sta acc		;clear everything up for user
	sta xr
	sta yr
	sta flgs
	lda #<monitor	;init pc to ourselves
	ldy #>monitor
	sta pcl
	sty pch
	lda #$0f
	sta pcb		;init bank pointer to system roms, i/o, & ram0
	jsr primm
	.byte cr,'MONITOR',0
	
start	cld
	tsx
	stx sp		;get stack pointer for register display
	lda #$c0
	jsr _setmsg	;enable kernal control & error messages
	cli	      	;start monitor: fall thru 'dspreg' to 'main'

	.page
;***********************************************************
;
;	Display contents of storage registers
;
;***********************************************************

dspreg
	jsr primm	;register legends
	.byte cr,'    PC  SR AC XR YR SP'
	.byte cr,'; ',$1b,'Q',0

	lda pcb
	jsr makhex
	txa
	jsr bsout	;display bank # (just lower nybble)
	lda pch
	jsr puthex	;display pc high

	ldy #2
1$	lda pcb,y
	jsr puthxs	;display rest, separated by spaces
	iny
	cpy #8
	bcc 1$

main
	jsr crlf
	ldx #0
	stx txtptr

1$	jsr basin 	;read one line (up to <cr>) into buffer
	sta buf,x
	inx
	cpx #161
	bcs error	;...branch if 'line too long'
	cmp #cr
	bne 1$		;loop until end of line

	lda #0
	sta buf-1,x	;flag end of line

2$	jsr gnc	  	;get a character from buffer
	beq main	;end of line
	cmp #' '	;skip leading spaces
	beq 2$
	jmp (exmon)	;lookup command
	.page
moncmd
	ldx #cmdqty-1	;compare first char to list of valid commands
10$	cmp cmdchr,x
	beq main1	;found it in list!
	dex
	bpl 10$
			;checked entire list, not found. fall into 'error'


error
	jsr primm	;general error message: print <crsr-rt><question mark>
	.byte 29,'?',0
	jmp main



main1
	cpx #cmdls	;is command 'L'oad, 'S'ave, or 'V'erify?
	bcs 10$		;...branch if so:  can't use parse!

	cpx #cmdno	;is it a number to evaluate?
	bcs 20$		;...branch if so

	txa		;multiply index by two
	asl a
	tax

	lda cmdtbl+1,x	;push address of command on stack
	pha
	lda cmdtbl,x
	pha
	jmp parse	;parse first arg, and rts to correct command routine


10$	sta verck	;save copy of what command was,
	jmp lodsav	;...and jump to common load/save/verify routine


20$	jmp convert	;simply evaluate number & print its value




exit			;exit monitor, return to system
	jmp (system_vector)

	.page
cmdchr
	.byte 'A'	;assemble
	.byte 'C'	;compare
	.byte 'D'	;disassemble
	.byte 'F'	;fill
	.byte 'G'	;go (jmp to program)
	.byte 'H'	;hunt
	.byte 'J'	;jsr to subroutine
	.byte 'M'	;memory dump
	.byte 'R'	;display registers, pc & stack pointer
	.byte 'T'	;transfer
	.byte 'X'	;exit (jmp to basic warm start)

	.byte '@'	;disk status/command
	.byte '.'	;alter assembly
	.byte '>'	;alter memory
	.byte ';'	;alter regs

cmdno	= *-cmdchr
cmdnum
	.byte '$'	;base-16 (hex)
	.byte '+'	;base-10 (dec)
	.byte '&'	;base-8  (oct)
	.byte '%'	;base-2  (bin)

cmdls	= *-cmdchr	;  (l,s & v must be last in table)

	.byte 'L'	;load memory
	.byte 'S'	;save memory
	.byte 'V'	;verify memory

cmdqty	= *-cmdchr



cmdtbl
	.word assem-1
	.word compar-1
	.word disasm-1
	.word fill-1
	.word go-1
	.word hunt-1
	.word gosub-1
	.word dspmem-1
	.word dspreg-1
	.word trnsfr-1
	.word exit-1

	.word disk-1
	.word assem-1
	.word setmem-1
	.word setreg-1

	.page
fetch
	stx xsave	;save .x!
	ldx t2+2	;set up bank
	lda #t2		;set up address
	sei
	jsr _fetch	;get byte (protect from irq's: bank may not have vectors!)
	cli
	ldx xsave	;restore .x! (note: monitor doesn't need status here)
	rts



stash
	stx xsave	;save .x!
	ldx #t2
	stx stavec	;set up address
	ldx t2+2	;set up bank
	sei
	jsr _stash	;put byte (protect from irq's: bank may not have vectors!)
	cli
	ldx xsave	;restore .x!
	rts



cmpare
	stx xsave	;save .x!
	ldx #t2
	stx cmpvec	;set up address
	ldx t2+2	;set up bank
	sei
	jsr _cmpar	;compare bytes (protect from irq's: bank may not have vectors!)
	cli
	php
	ldx xsave	;restore .x!
	plp		;restore status!
	rts

;.end

	.page
	.subttl monitor1

;********************************************
;
;	Display memory command
;
;********************************************

dspmem
	bcs 1$		;no range, do 1/2 screen
	jsr t0tot2	;else move 'from' value into place
	jsr parse
	bcc 2$		;got 'to', go dump

1$	lda #11	  	;do 12 lines
	sta t0
	bne 4$		;always


;	calculate # of lines

2$	jsr sub0m2	;calculate # bytes, put result in t0 (msb goes in .a)
	bcc 7$		;...branch if sa > ea
	
	ldx #3
	bit mode	;divide by 8 if 40-col, 16 if 80-col
	bpl 3$
	inx

3$	lsr t0+2	;shift msb right,
	ror t0+1	;..into middle byte,
	ror t0		;..and finally into lsb
	dex
	bne 3$

4$	jsr stop	;is stop key down?
	beq 6$		;..if so, exit.

	jsr dmpone
	lda #8
	bit mode	;add 8 (16 if 80-col) to line start address
	bpl 5$
	asl a

5$	jsr addt2
	jsr dect0	;test if dump finished
	bcs 4$		;loop until underflow
6$	jmp main

7$	jmp error

	.page
;********************************************
;
;	Set register command
;
;********************************************

setreg
	jsr t0topc	;copy adr & bank to pcl,h & pcb,  if given

	ldy #0
1$	jsr parse
	bcs 2$		;quit anytime arg list is empty
	lda t0
	sta flgs,y
	iny
	cpy #5
	bcc 1$
2$	jmp main

	.page
;********************************************
;
;	Alter memory command
;
;********************************************

setmem
	bcs 3$		;...branch if no arguments- just regurgitate existing data
	jsr t0tot2	;destination bank, addr in 't2'
	ldy #0

1$	jsr parse	;scan for next argument
	bcs 3$		;...branch if eol
	lda t0		;get the byte to stash
	jsr stash	;stash it
	iny
	bit mode
	bpl 2$
	cpy #16
	bcc 1$
2$	cpy #8
	bcc 1$

3$	jsr primm	;clear all modes & cursor up
	.byte $1b,$4f,$91,0

	jsr dmpone
	jmp main

	.page
;********************************************************************
;
;	Go command- start executing at either the supplied address,
;	   or (default) the current contents of the PC reg
;
;********************************************************************

go
	jsr t0topc	;copy adr & bank to pcl,h & pcb,  if given

	ldx sp
	txs		;set up stack pointer

	jmp _goto	;goto <pcl,h,b>





;********************************************************************
;
;	Jsr command- start executing at either the supplied address,
;	   or (default) the current contents of the PC reg.
;	   return is to monitor 'main' loop.
;
;********************************************************************

gosub
	jsr t0topc	;copy adr & bank to pcl,h & pcb,  if given
	jsr _gosub	;execute 'go' as a subroutine
	jmp main	;we're back

	.pag
;********************************************
;
;	Subroutine to dump one line
;	of memory on screen
;
;********************************************

dmpone
	jsr crlf
	lda #'>'	;print dump prompt
	jsr bsout
	jsr putt2	;print address, space.
	ldy #0
	beq 2$		;always (skip first space)

1$	jsr putspc
2$	jsr fetch	;get a byte from memory
	jsr puthex	;print hex byte
	iny
	cpy #8		;8 bytes/line for 40-column mode
	bit mode
	bpl 3$
	cpy #16		;16 bytes/line for 80-column mode
3$	bcc 1$

	jsr primm	;block off ascii dump & turn rvs on
	.byte ':',18,0

	ldy #0

4$	jsr fetch	;re-get byte from memory
	pha
	and #$7f	;mask control characters ($00-$1f and $80-$9f)
	cmp #$20
	pla
	bcs 5$
	lda #'.'	;print control characters as '.'

5$	jsr bsout
	iny
	bit mode
	bpl 6$
	cpy #16
	bcc 4$
6$	cpy #8
	bcc 4$
	rts

	.page
;********************************************
;
;	Transfer/Compare routines.
;
;	T starting-from,thru,to
;	C starting-from,thru,with
;
;********************************************

compar
	lda #0	   	;flag 'compare'
	.byte $2c

trnsfr
	lda #$80	;flag 'transfer'
	sta verck
	lda #0
	sta direction
	jsr range	;get source in t2, length in t1
	bcs 99$
	jsr parse	;get destination in t0
	bcc 1$

99$	jmp error

1$	bit verck
	bpl 20$		;...branch if compare (direction crap unimportant)

	sec		;determine direction of transfer (to avoid stepping on ourselves!)
	lda t2
	sbc t0		;source - destination (ignore banks...there might be bleed-thru)
	lda t2+1
	sbc t0+1
	bcs 20$		;branch if source >= destination

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
10$	lda temps,x	;restore ea as source (saved @ 'range')
	sta t2,x
	dex
	bpl 10$

	lda #$80
	sta direction	;flag backwards direction

20$	jsr crlf	;start with a new line
	ldy #0

30$	jsr stop
	beq 90$		;...branch if user requests abort
	jsr fetch	;get a byte
	ldx #t0
	stx stavec
	stx cmpvec	;set up sta/cmp addresses
	ldx t0+2	;get bank

	sei
	bit verck
	bpl 40$
	jsr _stash	;copy byte

40$	ldx t0+2	;get bank
	jsr _cmpar	;compare bytes
	cli
	beq 50$

	jsr putt2	;report mismatch
	jsr putspc	;make each number 8 bytes to look pretty
	jsr putspc

50$	bit direction
	bmi 60$		;test direction of transfers

	inc t0		;normal
	bne 70$
	inc t0+1
	bne 70$		;bra
	jmp error	;disallow bank-wrapping operations

60$	jsr dect0	;backwards
	jsr dect2
	jmp 80$

70$	jsr inct2
80$	jsr dect1
	bcs 30$

90$	jmp main

;.end

	.page
	.subttl monitor2

;******************************************************************
;
;	Hunt command - hunt for bytes or string
;
; syntax:  h 0000 1111 'ascii...   <or>   h 0000 1111 22 33 44 ...
;
;******************************************************************

hunt
	jsr range	;get sa in t2, calculate length, put in t1
	bcs 9$		;...error if eol
	ldy #0
	jsr gnc		;get first char
	cmp #$27	;is it an <'>
	bne 2$		;no-  must be hex
	jsr gnc		;yes- get first string chr
	cmp #0
	beq 9$		;...branch if true eol (error)

1$	sta xcnt,y	;save in buffer
	iny
	jsr gnc		;get next
	beq 4$		;yes-end of string
	cpy #32		;no-32 char yet?
	bne 1$		;no-get more
	beq 4$		;yes-go look for it

2$	sty bad		;zero for rdob
	jsr pargot	;finish hex read

3$	lda t0
	sta xcnt,y
	iny
	jsr parse	;get next character
	bcs 4$		;no more -go look for bytes
	cpy #$20	;32 bytes yet?
	bne 3$		;no-get more

4$	sty verck	;yes-start search
	jsr crlf	;next line

5$	ldy #0

6$	jsr fetch	;get a byte from memory
	cmp xcnt,y
	bne 7$		;...branch if no match
	iny
	cpy verck	;checked full string?
	bne 6$		;no-check on

	jsr putt2	;print address found
	jsr putspc	;pretty up to 8 bytes
	jsr putspc

7$	jsr stop
	beq 8$		;...branch if user requests abort
	jsr inct2	;increment t2
	jsr dect1	;decrement byte counter
	bcs 5$		;loop if not done

8$	jmp main

9$	jmp error
	.page
;************************************************************
; load/save/verify
;
; l ["name"] [,device_number] [,alt_load_address]
; v ["name"] [,device_number] [,alt_load_address]
; s "name", device_number, starting_address, ending_address
;************************************************************

lodsav
	ldy #1
	sty fa		;setup defaults: tape, fixed_load, no filename
	sty sa
	dey		;(.y=0)
	sty ba		;default l/s/v from/to RAM 0
	sty fnlen	;default no filename
	sty fnbnk	;filename is always in RAM 0 for monitor
	sty status
	lda #>xcnt
	sta fnadr+1
	lda #<xcnt
	sta fnadr

1$	jsr gnc		;look for name
	beq 6$		;branch if no name (must be default load)
	cmp #' '
	beq 1$		;skip spaces
	cmp #'"'
	bne 3$		;jmp to error if no quoted name
	ldx txtptr

2$	lda buf,x	;get chr
	beq 6$		;eol, must be load
	inx
	cmp #'"'	;pass everything up to closing quote
	beq 4$
	sta (fnadr),y	;okay- always bank 0
	inc fnlen
	iny
	cpy #17		;check length of name (16 max.)
	bcc 2$
3$	jmp error

4$	stx txtptr
	jsr gnc		;trash delimitor
	beq 6$		;...eol, use default
	jsr parse	;get device #
	bcs 6$		;...eol, use default
	lda t0
	sta fa		;device # in 'fa' (let kernal catch invalid devices)

	jsr parse	;get starting address
	bcs 6$		;none, must be load
	jsr t0tot2	;save start_addr in t2
	sta ba		;set bank

	jsr parse	;get ending address
	bcs 8$		;none...must be 'alternate load'
	jsr crlf	;prep for 'saving...' msg
	ldx t0		;pickup end addr
	ldy t0+1
	lda verck
	cmp #'S'	;check that this is a save
	bne 3$
	lda #0
	sta sa
	lda #t2		;pointer to start. addr
	jsr _save	;do save
5$	jmp main

6$	lda verck	;check for load
	cmp #'V'	;..or verify
	beq 7$
	cmp #'L'
	bne 3$
	lda #0		;flag load

7$	jsr _load	;do load/verify
	lda status
	and #$10
	beq 5$		;ok to cont.
	lda verck	;l & v have diff. err. msgs
	beq 3$		;branch if zero (load)
	jsr primm
	.byte ' ERROR',0
	jmp main

8$	ldx t2		;set up new address for load (bank already put in 'ba')
	ldy t2+1
	lda #0		;flag 'non-default load'
	sta sa
	beq 6$		;go check for V or L, then doit.

	.page
;******************************************************************
;
;	Fill command - F starting-address ending-address value
;
;******************************************************************

fill
	jsr range	;sa in t2, len in t1
	bcs 3$		;error if eol

	lda t2+2
	cmp temps+2
	bne 3$		;prevent bank wraps

	jsr parse	;get fill value
	bcs 3$
	ldy #0

1$	lda t0
	jsr stash	;fill memory byte by byte (without comparison check)
	jsr stop
	beq 2$		;branch if user wants to abort
	jsr inct2
	jsr dect1
	bcs 1$

2$	jmp main

3$	jmp error

;.end

	.page
	.subttl  monitor3 - assembler

;**********************************************************
;  simple assembler
;  syntax:      a 1111 lda ($00,x)
; 	      a 1111 dex:		(':' = terminator)
;**********************************************************

assem
	bcs 7$		;...branch if missing sa
	jsr t0tot2	;save sa

1$	ldx #0
	stx hulp+1	;clear left mnemonic
	stx count	;clear operand

2$	jsr gnc	 	;get a char
	bne 3$		;check for eol
	cpx #0
	bne 3$
	jmp main	;if eol & no mnemonic, exit cleanly

3$	cmp #' '	;is it a space ?
	beq 1$		;yes - start again (passes over object code, if any)
	sta msal,x      ;no - save char
	inx
	cpx #3	  	;got three chars ?
	bne 2$		;no - loop

4$	dex	     	;squished all three ?
	bmi 10$		;yes
	lda msal,x     	;no - first in last out
	sec	     	;no borrow
	sbc #$3f	;normalize
	ldy #5	  	;set for 5 shift rights

5$	lsr a
	ror hulp+1	;left mnemonic
	ror hulp	;right mnemonic
	dey	     	;done 5 shifts?
	bne 5$		;no-loop
	beq 4$		;always


7$	jmp error	;error parsing input


10$	ldx #2	  	;move output buffer index past crunched mnemonic

15$	lda count	;after first number copy everything else to output buffer
	bne 50$
	jsr eval	;evaluate next parameter, if number crunch it
	beq 40$		;...branch if not a number
	bcs 7$		;...branch if illegal number

	lda #'$'
	sta hulp,x	;buffer a number, either '$00' or '$0000'
	inx
	lda t0+2
	bne 7$		;...branch if number too large
	ldy #4
	lda shift	;kludge to allow user to force absolute over zero page
	cmp #8
	bcc 18$		;...allow only hex or octal bases to force absolute
	cpy count
	beq 20$		;...branch to force absolute	
18$	lda t0+1
	bne 20$		;...branch if 2-byte field
	ldy #2		;else set up  1-byte field

20$	lda #'0'
30$	sta hulp,x
	inx
	dey
	bne 30$

40$	dec txtptr	;re-get last character from input buffer
50$	jsr gnc		;copy rest of input buffer to output buffer
	beq 60$		;...branch if eol
	cmp #' '
	beq 15$		;...squish out spaces
	sta hulp,x	;hopefully it's of one of these:   #,()
	inx
	cpx #10
	bcc 15$		;...loop until eol or
	bcs 7$		;...buffer overflow


60$	stx t1	  	;save input # of characters
	ldx #0
	stx wrap	;start trial at zero

as110	ldx #0
	stx tmpc	;disa index=0
	lda wrap	;get trial byte
	jsr dset	;digest it
	ldx format	;save format for later
	stx t1+1
	tax	     	;index into mnemonic table
	lda mnemr,x	;get compressed
	jsr tstrx	;mnemonic and test
	lda mneml,x
	jsr tstrx
	ldx #6	  	;six format bits
as210
	cpx #3
	bne as230
	ldy length
	beq as230	;skip-single byte instr
as220
	lda format
	cmp #$e8	;a relative instr?
	lda #'0'	;test zeros
	bcs as250	;no-3 byte
	jsr tst2	;test a byte,2 chars
	dey
	bne as220
as230
	asl format
	bcc as240
	lda char1-1,x
	jsr tstrx	;test syntax
	lda char2-1,x
	beq as240
	jsr tstrx	;test more syntax
as240
	dex
	bne as210
	beq as300
as250
	jsr tst2	;test a word,4 chars
	jsr tst2
as300
	lda t1	  	;check # chars of both
	cmp tmpc
	beq as310	;match, skip
	jmp tst05	;fail
as310
	ldy length
	beq as500	;if only 1 byte instr skip
	lda t1+1	;get saved format
	cmp #$9d	;a relative instr?
	bne as400	;no-skip

	lda t0	  	;calculate a relative
	sbc t2	  	;(.c=1 already)
	tax		;save 'diff'
	lda t0+1
	sbc t2+1
	bcc as320	;...taken if a negative branch

	bne aerr	;positive branch, out of range
	cpx #$82
	bcs aerr
	bcc as340

as320	tay		;negative branch
	iny	     	;out of range, y=$ff
	bne aerr
	cpx #$82
	bcc aerr
as340
	dex	     	;subtract 2 from 'diff' for instr
	dex
	txa
	ldy length	;set index to length
	bne as420	;branch always

as400
	lda t0-1,y      ;no-put byte out there
as420
	jsr stash
	dey
	bne as400
as500
	lda wrap	;get good op code
	jsr stash
	jsr cronly	;get ready to overstrike line
	jsr primm	;print 'A ' & erase eol
	.byte 'A ',27,'Q',0
	jsr dis400	;disassemble one line

	inc length
	lda length
	jsr addt2	;update address

	lda #'A'	;set up next line with 'a bnnnn ' for convenience
	sta keyd	;put it in the keyboard buffer
	lda #' '
	sta keyd+1
	sta keyd+7
	lda t2+2	;get the bank number
	jsr makhex	;get hi byte in .a (which we'll ignore), and lo in .x
	stx keyd+2
	lda t2+1	;next get mid byte of address
	jsr makhex
	sta keyd+3	;..and put in buffer,
	stx keyd+4
	lda t2		;then get the low byte of address,
	jsr makhex
	sta keyd+5	;..and put that in the buffer, too.
	stx keyd+6
	lda #8		;signal that we put 8 char's in the buffer
	sta ndx
	jmp main



aerr
	jmp error




;  test char in .a with char in hulp
;
tst2
	jsr tstrx	;test for '00' (do two tests)

tstrx
	stx sxreg
	ldx tmpc	;get current position
	cmp hulp,x	;same char
	beq tst10	;yes-skip
	pla	     	;pull jsr off stack
	pla

tst05
	inc wrap	;try next trial
	beq aerr	;=0 tried all,sorry
	jmp as110

tst10
	inc tmpc
	ldx sxreg	;restore x
	rts

;.end

	.page
	.subttl  monitor4 - disassembler

;***************************************************
;
;	Mini disassembler
;
;***************************************************

disasm
	bcs 1$		;use a default length from current sa
	jsr t0tot2
	jsr parse
	bcc 2$		;got sa,ea. use 'em

1$	lda #20	  	;guess at 1/2 page
	sta t0
	bne 3$

2$	jsr sub0m2      ;put ea-sa in t0
	bcc 5$		;...branch if sa > ea

3$	jsr primm	;print <cr> & delete to end of line for neatness
	.byte cr,$1b,'Q',0
	jsr stop
	beq 4$		;...branch if user requests abort
	jsr dis300      ;disassemble 1 line
	inc length
	lda length
	jsr addt2
	lda length
	jsr subt0
	bcs 3$
4$	jmp main

5$	jmp error


dis300
	lda #'.'
	jsr bsout
	jsr putspc
dis400
	jsr putt2
	jsr putspc
	ldy #0
	jsr fetch	;get a byte from memory
	jsr dset	;get instr & digest it

	pha	      	;dump (length+1) bytes
	ldx length      ;(.y=0 from 'dset' above)
	inx
pradr0
	dex
	bpl pradrl      ;pad non-printers
	jsr primm	;print 3 spaces
	.byte '   ',0
	jmp pradrm
pradrl
	jsr fetch
	jsr puthxs
pradrm
	iny
	cpy #3
	bcc pradr0
	pla

	ldx #3
	jsr prmne	;print mnemonic
	ldx #6	   	;6 format bits
pradr1
	cpx #3
	bne pradr3     	;if x=3 print adr val
	ldy length
	beq pradr3     	;no print if len=0
pradr2
	lda format
	cmp #$e8 	;relative addressing mode?
	php		;save carry
	jsr fetch
	plp
	bcs reladr
	jsr puthex
	dey
	bne pradr2
pradr3
	asl format	;test next format bit
	bcc pradr4	;no print if=0
	lda char1-1,x
	jsr bsout
	lda char2-1,x
	beq pradr4
	jsr bsout
pradr4
	dex
	bne pradr1
	rts


reladr
	jsr pcadj3	;pcl,h + disp + 1 into a,x
	clc	      	;add 1
	adc #1
	bne relad2
	inx
relad2
	jmp putwrd


pcadj3
	ldx t2+1
	tay
	bpl pcadj4
	dex
pcadj4
	adc t2
	bcc pcrts
	inx
pcrts
	rts



; disassembler digest routine
;
dset
	tay
	lsr a	    	;even/odd test
	bcc ieven
	lsr a	    	;test b1
	bcs err	  	;xxxxxx11 instr bad
	cmp #$22
	beq err	  	;10001001 instr bad
	and #$07	;mask 3 bits for adr mode
	ora #$80 	;add indexing offset
ieven
	lsr a	    	;left/right test
	tax
	lda nmode,x	;index into mode table
	bcs rtmode 	;if carry set use lsb for
	lsr a	    	;print format index
	lsr a
	lsr a	    	;if carry clr use msb
	lsr a
rtmode
	and #$0f	;mask for 4-bit index
	bne getfmt	;$0 for bad opcodes
err
	ldy #$80	;sub $80 for bad opcode
	lda #0	   	;set format index to zero
getfmt
	tax
	lda nmode2,x	;index into prt format tab
	sta format	;save for adr field format
	and #3	   	;mask 2-bit length. 0=1byte
	sta length	;1=2byte,2=3byte
	tya	      	;op code
	and #$8f	;mask for 1xxx1010 test
	tax	      	;save in x
	tya	      	;op code again
	ldy #3
	cpx #$8a
	beq mnndx3
mnndx1
	lsr a
	bcc mnndx3	;form index into mnemonic tab
	lsr a
mnndx2
	lsr a	    	;1xxx1010->00101xxx
	ora #$20	;xxxyyy01->00111xxx
	dey	      	;xxxyyy10->00110xxx
	bne mnndx2	;xxxyy100->00100xxx
	iny	      	;xxxxx000->000xxxxx
mnndx3
	dey
	bne mnndx1
	rts	      	;(.y=0 is assumed!)



; print mnemonic
; enter x=3 characters
;
prmne
	tay
	lda mneml,y	;fetch 3 char mnemonic
	sta t1
	lda mnemr,y
	sta t1+1
prmn1
	lda #0
	ldy #5
prmn2
	asl t1+1	;shift 5 bits of char
	rol t1	   	;into a
	rol a	    	;clear carry
	dey
	bne prmn2
	adc #$3f	;add '?' offset
	jsr bsout
	dex
	bne prmn1
	jmp putspc	;finish with space


nmode
	.byte $40,2,$45,3
	.byte $d0,8,$40,9
	.byte $30,$22,$45,$33
	.byte $d0,8,$40,9
	.byte $40,2,$45,$33
	.byte $d0,8,$40,9
	.byte $40,$02,$45,$b3
	.byte $d0,$08,$40,$09
	.byte 0,$22,$44,$33
	.byte $d0,$8c,$44,0
	.byte $11,$22,$44,$33
	.byte $d0,$8c,$44,$9a
	.byte $10,$22,$44,$33
	.byte $d0,8,$40,9
	.byte $10,$22,$44,$33
	.byte $d0,8,$40,9
	.byte $62,$13,$78,$a9
nmode2
	.byte 0,$21,$81,$82
	.byte 0,0,$59,$4d
	.byte $91,$92,$86,$4a
	.byte $85,$9d
char1
	.byte ',),#($'
char2
	.byte 'Y',0,'X$$',0
mneml
	.byte $1c,$8a,$1c,$23
	.byte $5d,$8b,$1b,$a1
	.byte $9d,$8a,$1d,$23
	.byte $9d,$8b,$1d,$a1
	.byte 0,$29,$19,$ae
	.byte $69,$a8,$19,$23
	.byte $24,$53,$1b,$23
	.byte $24,$53,$19,$a1
	.byte 0,$1a,$5b,$5b
	.byte $a5,$69,$24,$24
	.byte $ae,$ae,$a8,$ad
	.byte $29,0,$7c,0
	.byte $15,$9c,$6d,$9c
	.byte $a5,$69,$29,$53
	.byte $84,$13,$34,$11
	.byte $a5,$69,$23,$a0
mnemr
	.byte $d8,$62,$5a,$48
	.byte $26,$62,$94,$88
	.byte $54,$44,$c8,$54
	.byte $68,$44,$e8,$94
	.byte 0,$b4,8,$84
	.byte $74,$b4,$28,$6e
	.byte $74,$f4,$cc,$4a
	.byte $72,$f2,$a4,$8a
	.byte 0,$aa,$a2,$a2
	.byte $74,$74,$74,$72
	.byte $44,$68,$b2,$32
	.byte $b2,0,$22,0
	.byte $1a,$1a,$26,$26
	.byte $72,$72,$88,$c8
	.byte $c4,$ca,$26,$48
	.byte $44,$44,$a2,$c8
regk
	.byte cr,$20,$20,$20

;.end

	.page
	.subttl  monitor5 - routines

;  parse entry when 1st character has already been read
;



pargot
	dec txtptr



;  parse next item in buffer & put its value into  t0, t0+1 & t0+2.
;
;	.z=1 if no value found.
;	.c=1 if eol.
;	.x & .y are preserved, .a contains # digits read.
;
;	if error, call is popped & 'jmp error' performed.


parse
	jsr eval	;evaluate ascii input as a number
	bcs 10$		;...branch if error

	jsr glc		;re-get last character
	bne 5$		;...branch if not eol
	dec txtptr	;point to eol: make next 'gnc' see eol
	lda count
	bne 30$		;...valid number input, treat eol as a delimiter
	beq 20$		;...the well is very dry

5$	cmp #' '	;parse delimiters (only allow <space> or <comma>)
	beq 30$
	cmp #','
	beq 30$		;...fall into error if bad delimiter

10$	pla		;pop this call
	pla
	jmp error

20$	sec		;set .c=1 for eol
	.byte $24

30$	clc		;clear .c for not-eol
	lda count	;set .z=1 for valid number
	rts

	.page
;  evaluate next item in buffer & put its value into  t0, t0+1 & t0+2.
;
;	.c=0  normal return
;	.c=1  error  return
;	.z=1  null input
;	.x & .y are preserved.

eval
	lda #0
	sta t0		;clear value
	sta t0+1
	sta t0+2
	sta count	;reset digit counter (flags valid number vs. null input)
	txa
	pha		;preserve .x & .y
	tya
	pha


10$	jsr gnc		;get next character
	bne 15$
	jmp eval_ok	;...branch if end of line
15$	cmp #' '
	beq 10$		;...branch & ignore leading spaces

	ldx #3
20$	cmp cmdnum,x	;is first character a base prefix?
	beq 30$		;...yes
	dex
	bpl 20$

	inx		;...no: default base to hex
	dec txtptr	;to reget digit

30$	ldy bases,x	;this is the base
	lda shifts,x	;this is the # of shifts required for given base
	sta shift

40$	jsr gnc		;get next character
	beq eval_ok	;...branch if eol
	sec
	sbc #'0'	;convert ascii digit to binary value
	bcc eval_ok	;...branch if not a number  (assume delimiter)
	cmp #10
	bcc 50$		;...number 0-9
	sbc #7
	cmp #16		;...number a-f
	bcs eval_ok	;...branch if not a number  (assume delimiter)

50$	sta number	;binary value of current digit
	cpy number
	bcc eval_ng	;...branch if number out of range of given base
	beq eval_ng
	inc count	;flag valid digit
	cpy #10
	bne 70$		;...branch if not base-10

	ldx #2
60$	lda t0,x	;save a copy current total for base-10 calc
	sta temps,x
	dex
	bpl 60$

70$	ldx shift
80$	asl t0		;multiply current value by base using binary shifts
	rol t0+1
	rol t0+2
	bcs eval_ng	;...branch if overflow error
	dex
	bne 80$		;...next shift

	cpy #10
	bne 90$		;...branch if not base-10

	asl temps	;more base-10 calc: first one more shift
	rol temps+1
	rol temps+2
	bcs eval_ng	;...overflow
	lda temps
	adc t0		;add 'em up
	sta t0
	lda temps+1
	adc t0+1
	sta t0+1
	lda temps+2
	adc t0+2
	sta t0+2
	bcs eval_ng	;...overflow

90$	clc		;add current digit (all bases)
	lda number
	adc t0
	sta t0
	txa		;.x=0
	adc t0+1
	sta t0+1
	txa
	adc t0+2
	sta t0+2
	bcs eval_ng	;...overflow
	and #$f0
	bne eval_ng	;...overflow
	beq 40$		;...next character

	.page
eval_ng
	sec
	.byte $24

eval_ok
	clc
	sty shift	;return input base (used by 'assem')
	pla
	tay		;restore .x & .y
	pla
	tax
	lda count
	rts



bases	.byte 16,10, 8, 2
shifts	.byte  4, 3, 3, 1

	.page
;  print t2 as 5 hex digits:	BHHLL
;

putt2	lda t2+2	;get bank (a19-a16)
	jsr makhex	;make ascii:  msd in .a (ignored) and lsd in .x
	txa
	jsr bsout	;print lsd
	lda t2		;get address (a15-a0)
	ldx t2+1


putwrd	pha		;print address:  msb first, then lsb
	txa
	jsr puthex
	pla


puthxs	jsr puthex	;print byte in .a as two hex digits


putspc	lda #' '	;print <space>
	jmp bsout



cronly	jsr primm	;print <cr><crsr-up>
	.byte 13,145,0
	rts



crlf	lda #cr
	jmp bsout



new_line
	jsr primm	;print <cr><clear_eol><space>
	.byte 13,27,'Q ',0
	rts

	.page
;  print .a as 2 hex digits
;

puthex	stx sxreg
	jsr makhex
	jsr bsout
	txa
	ldx sxreg
	jmp bsout



;  convert .a to 2 hex digits & put msb in .a, lsb in .x
;

makhex	pha
	jsr 1$
	tax
	pla
	lsr a
	lsr a
	lsr a
	lsr a

1$	and #$0f
	cmp #$0a
	bcc 2$
	adc #6

2$	adc #'0'
	rts


;  get last character
;

glc	dec txtptr


;  get next character: return in .a  (return = if buffer empty or eol)
;

gnc	stx sxreg
	ldx txtptr
	lda buf,x
	beq 1$		;eol-return with z=1
	cmp #':'
	beq 1$		;eol-return with z=1
	cmp #'?'
1$	php
	inc txtptr
	ldx sxreg
	plp
	rts

	.page
;  move t0,t0+1,t0+2 to t2,t2+1,t2+2
;

t0tot2	lda t0
	sta t2
	lda t0+1
	sta t2+1
	lda t0+2
	sta t2+2
	rts


;  subtract t2 from t0, result in t0
;

sub0m2	sec
	lda t0
	sbc t2
	sta t0
	lda t0+1
	sbc t2+1
	sta t0+1
	lda t0+2
	sbc t2+2
	sta t0+2	;note .c=0 indicates t0 < t2, thus t0 is negative!
	rts


;  decrement t0
;

dect0	lda #1

subt0	sta sxreg	;subtract .a from t2
	sec
	lda t0
	sbc sxreg
	sta t0
	lda t0+1
	sbc #0
	sta t0+1
	lda t0+2
	sbc #0
	sta t0+2
	rts


;  decrement t1
;

dect1	sec
	lda t1
	sbc #1
	sta t1
	lda t1+1
	sbc #0
	sta t1+1
	lda t1+2
	sbc #0
	sta t1+2
	rts


; increment t2
;

inct2	lda #1

addt2	clc	      	;add .a to t2
	adc t2
	sta t2
	bcc 1$
	inc t2+1
	bne 1$
	inc t2+2
1$	rts


;  decrement t2
;

dect2	sec
	lda t2
	sbc #1
	sta t2
	lda t2+1
	sbc #0
	sta t2+1
	lda t2+2
	sbc #0
	sta t2+2
	rts



; copy t0 to pc registers
;

t0topc	bcs 1$		;no arg given, just exit
	lda t0
	ldy t0+1
	ldx t0+2
	sta pcl
	sty pch
	stx pcb
1$	rts
	.page
;  read a range - put sa in t2, count in t1   (save ea in 'temps')
;
;  returns .c=0 if okay, .c=1 if error (missing parameter or sa < ea)
;

range
	bcs 1$		;...branch if missing sa
	jsr t0tot2	;move sa from t0 to t2
	jsr parse	;get ea
	bcs 1$		;...branch if missing ea

	lda t0		;save ea (for 'transfer' cmd)
	sta temps
	lda t0+1
	sta temps+1
	lda t0+2
	sta temps+2

	jsr sub0m2	;calculate length = ea - sa  (.c=0 if sa<ea)

	lda t0	   	;move length from t0 to t1
	sta t1
	lda t0+1
	sta t1+1
	lda t0+2
	sta t1+2

	bcc 1$		;invert .c from subtraction above
	clc		;good stuff exits here
	.byte $24
1$	sec		;bad  stuff exits here
	rts

	.page
;  convert given number from its base to hex
;

convert
	jsr pargot	;parse number & put its value in t0
	jsr new_line
	lda #'$'	;print hexidecimal conversion of t0
	jsr bsout
	lda t0+2	;how big a number is it?
	beq 10$		;...branch if 'bank' nybble unnecessary
	jsr makhex
	txa
	jsr bsout	;print lsd of this byte
10$	lda t0
	ldx t0+1
	jsr putwrd	;print hex word

	jsr new_line
	lda #'+'	;print decimal conversion of t0
	jsr bsout
	jsr bindec	;convert t0 into packed BCD in hulp
	lda #0		;no leading zeros
	ldx #8		;number of digits
	ldy #3		;number of shifts-1
	jsr unpack	;convert packed BCD in hulp to ASCII & print it

	jsr new_line
	lda #'&'	;print octal conversion of t0
	jsr bsout
	lda #0		;no leading zeros
	ldx #8		;number of digits
	ldy #2		;number of shifts-1
	jsr unpack_t0	;convert binary in hulp to ASCII & print it

	jsr new_line
	lda #'%'	;print binary conversion of t0
	jsr bsout
	lda #0		;no leading zeros
	ldx #24		;number of digits
	ldy #0		;number of shifts-1
	jsr unpack_t0	;convert binary in hulp to ASCII & print it

	jmp main

	.page
;  convert from binary to decimal (BCD)
;
;	input :	binary value (3 bytes) in T0
;	output:	decimal value (4 bytes) in HULP as packed BCD
;

bindec
	jsr t0tot2		;copy from T0 to T2 (lo/mid/hi)

	lda #0
	ldx #7
1$	sta hulp,x	;initialize working registers
	dex
	bpl 1$
	inc hulp+7	;seed value_of_bit with 1

	ldy #23		;loop index (3 bytes = 24 bits)
	php		;save caller's processor mode
	sei		;disable IRQ's (but beware NMI's!)
	sed		;put processor into decimal mode (for ADC's & SBC's)


;  main loop. rotate bits right 1 at a time, and if
;  set add the current value_of_bit to the sum.


10$	lsr t2+2	;hi
	ror t2+1	;mid
	ror t2		;lo
	bcc 30$		;...branch if bit not set (its value is 0)

	clc
	ldx #3
20$	lda hulp+4,x	;add current value_of_bit (decimal arithmetic)
	adc hulp,x
	sta hulp,x
	dex
	bpl 20$

30$	clc
	ldx #3
40$	lda hulp+4,x	;calculate value of next bit (decimal arithmetic)
	adc hulp+4,x
	sta hulp+4,x
	dex
	bpl 40$
	dey
	bpl 10$		;loop until done conversion

	plp		;restore processor mode
	rts

	.page
;  unpack binary value in T0 by first copying it to HULP.
;

unpack_t0
	pha		;save .a
	lda t0
	sta hulp+2	;copy t0 (lo/mid/hi) to hulp (hi/mid/lo)
	lda t0+1
	sta hulp+1
	lda t0+2
	sta hulp
	lda #0
	sta hulp+3
	pla		;restore .a & fall into unpack...


;  unpack base encoded number, convert it to ascii & print it.
;
;  enter:  packed number in HULP  (hi/mid/lo)
;	   # digits in .x,  # shifts/digit in .y
;	   .a=0 to trim leading 0's, else .a>0 to print them.


unpack
	sta count
	sty shift
10$	ldy shift	;# bits per digit, .x is digit counter
	lda #0
20$	asl hulp+3	;lo
	rol hulp+2	;mid lo
	rol hulp+1	;mid hi
	rol hulp	;hi
	rol a		;shift a digit into .a
	dey
	bpl 20$

	tay		;set flags for .a
	bne 30$
	cpx #1
	beq 30$		;...print zero if it's the last digit
	ldy count
	beq 40$		;...else skip leading zeros

30$	inc count	;flag a non-zero digit
	ora #$30	;make it ascii
	jsr bsout	;print it

40$	dex
	bne 10$		;...loop until all digits printed
	rts
	
;.end

	.page
	.subttl  monitor6 - disk commands

;********************************************
;
;	Disk Command/Status Operations
;
;	@[device-number][,command-string]
;
;********************************************

disk
	bne 10$		;...branch if given device #
	ldx #8		;default device number
	.byte $2c

10$	ldx t0		;get given device #
	cpx #4
	bcc disk_err	;...branch if bad device #
	cpx #31
	bcs disk_err
	stx t0

	lda #0
	sta t0+2	;clear line # register (in case DIR cmd)
	sta fnlen
	tax
	jsr setbnk	;cmd string in in ram0 (in case DIR cmd)

	jsr gnc		;peek at first character of disk command
	dec txtptr	;backup so we will re-get this character later
	cmp #'$'
	beq disk_dir	;...branch if directory read


;  open disk command channel & pass it given command


	lda #0		;la
	ldx t0		;fa
	ldy #15		;sa
	jsr setlfs
	jsr open	;open disk command channel
	bcs disk_done	;...branch on error

	ldx #0
	jsr chkout	;make it an output channel
	bcs disk_done	;...branch on error

20$	ldx txtptr	;get next character
	inc txtptr
	lda buf,x
	beq disk_st	;...branch if eol
	jsr bsout	;xmit character to disk
	bcc 20$		;...loop until error or eol

	.page
disk_st
	jsr clrch	;read & report disk status
	jsr crlf
	ldx #0
	jsr chkin	;make it an input channel
	bcs disk_done	;...branch on error

10$	jsr basin	;get a character from disk
	jsr bsout	;print it
	cmp #cr
	beq disk_done	;...branch if eol
	lda status
	and #$bf	;strip eoi bit
	beq 10$		;...loop until error or eol



disk_done
	jsr clrch
	lda #0
	sec
	jsr close
	jmp main



disk_err
	jmp error

	.page
;  read & display the disk directory

disk_dir
	ldy #$ff	;determine directory string length
	ldx txtptr
	dex
	
10$	iny
	inx
	lda buf,x	;get a character
	bne 10$		;...loop until eol

	tya		;length
	ldx txtptr	;fnadr low
	ldy #>buf	;fnadr high
	jsr setnam
	lda #0		;la
	ldx t0		;fa
	ldy #$60	;sa
	jsr setlfs
	jsr open	;open directory channel
	bcs disk_done	;...branch on error
	ldx #0
	jsr chkin	;make it an input channel

	jsr crlf

	ldy #3		;first pass only- trash first two bytes read

20$	sty t1		;loop counter
25$	jsr basin
	sta t0		;get # blocks low
	lda status
	bne disk_done	;...branch if error
	jsr basin
	sta t0+1	;get # blocks high
	lda status
	bne disk_done	;...branch if error
	dec t1
	bne 25$		;...loop until done

	jsr bindec	;convert # blocks to decimal
	lda #0		;no leading zeros
	ldx #8		;max digits
	ldy #3		;required # shifts for decimal value
	jsr unpack	;print # blocks
	lda #' '
	jsr bsout	;print space  (to match loaded directory display)

30$	jsr basin	;read & print filename & filetype
	beq 40$		;...branch if eol
	ldx status
	bne disk_done	;...branch if error
	jsr bsout
	bcc 30$		;...loop always

40$	jsr crlf	;start a new line
	jsr stop
	beq disk_done	;...branch if user hit STOP
	ldy #2
	bne 20$		;...loop always

;.end
