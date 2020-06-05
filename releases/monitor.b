; cbm 600/700 Monitor by David Viner (converted from c128)
; MONITOR.PRG
; comments+labels vossi 05/2020
; P500 patches vossi 05/2020
; fix01 dv forgot to change original-adr $eaa8 fnlen $b7 -> $9d, 1by diff to org
; fix02 change 4x fa in assembler to temp, total 5by diff to org
; fix03 jump to goto was 2 bytes too short - one extra pha in go command! 
!cpu 6502
!ct pet
; ########################################### TODO ################################################
; ATTENTION: If basic is too long, jsr to this bank does not return !!!
; ########################################### TODO ################################################
; 
; -------------------------------------------------------------------------------------------------
; switches
P500	= 1
OPTI	= 1	;optimizations
!ifdef P500{
	!to "monitor500.prg", cbm
	!initmem $00
} else{
	!to "monitor.prg", cbm
	!initmem $ff
}
DEFDEV 	= 8		;default device	
!ifdef OPTI{
DISDEF	= 23		;bytes to disassemble by default
MEMDEF	= 8		;lines to display memory
} else{
DISDEF	= 20		;bytes to disassemble by default
MEMDEF	= 12		;lines to display memory
}
!ifdef P500{
DEFBANK	= 0		;if no address, load to this bank 
COLS	= 0		;40 columns
} else{
DEFBANK	= 1
COLS	= 1		;80 columns
}
; constants

irom	= $f		;System bank
cr	= $0d
esc	= $1b
; -------------------------------------------------------------------------------------------------
; zero page stuff
i6509	= $01		;6509 indirect bank reg
e6509	= $00		;6509 execution bank reg

;only in bank 15
ndx	= $d1		;kernal keyboard buffer index
bellmd	= $039f		;Bell on/off flag (80 column only)
keyd	= $03ab		;10by kernal keyboard buffer

; used by monitor-kernal
;tpiptr	= $41		;pointer to TPI (used in IRQ !)
;ipoint	= $ac		;tx routine usage

; used bey monitor + monitor-kernal
tmpbnk	= $30		;temporaray ibank storage
ptr	= $5d		;2by pointer keyd,status / primm pointer to message char


; monitor zp
pcb	= $02		;shadow regs
pch	= $03
pcl	= $04
flgs	= $05
acc	= $06
xr	= $07
yr	= $08
sp	= $09

temp	= $5f		;use basic fac for monitor zp

t0	= $60		;3by pointer
t1	= $63		;3by pointer
t2	= $66		;3by pointer

txtptr	= $7a
verck	= $93		;verify flag
mode	= $d7		;40/80 column mode

; kernal definitions - shadows of bank 15 
fnadr	= $90		;3by Address of file name string
			;  low, high, bank
; $93-$95 not used
eal	= $96		;3by End of load/save
eah	= $97		;  low, high, bank
eas	= $98
stal	= $99		;3by Start of load/save
stah	= $9a		;  low, high, bank
stas	= $9b
status	= $9c		;I/O operation status
fnlen	= $9d		;File name length
la	= $9e		;Current logical index
fa	= $9f		;Current first address
sa	= $a0		;Current secondary address

; -------------------------------------------------------------------------------------------------
; absolute monitor storage

!ifndef OPTI{bad=$0100}	;fbuffr
buf	= $0200		;160by input buffer

mkeyd	= $034a		;10by monitor keyboard buffer
xcnt	= $0380		;32by compare buffer
hulp	= $03a0		;10by buffer
format	= $03aa		;asm
length	= $03ab		;asm/dis
msal	= $03ac		;3by for assembler
sxreg	= $03af		;1 byte temp used all over
wrap	= $03b1		;1 byte temp for assembler
xsave	= $03b2		;save .x here during indirect subroutine calls
dir	= $03b3		;direction indicator for 'transfer'
count	= $03b4		;parse number conversion
number	= $03b5		;parse number conversion
shift	= $03b6		;parse number conversion
temps	= $03b7		;3 temp bytes
; -------------------------------------------------------------------------------------------------
; system entrys
primm	= $ff3f		;print message routine

exsub3	= $feb5		;kernal cross bank call

setbnk	= $ff68		;kernal jump table
_setmsg	= $ff90
setlfs	= $ffba
setnam	= $ffbd
open	= $ffc0
close	= $ffc3
chkin	= $ffc6
chkout	= $ffc9
clrch	= $ffcc
basin	= $ffcf
bsout	= $ffd2
_load	= $ffd5
_save	= $ffd8
stop	= $ffe1

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
	nop		;save 3 bytes for message "monitor500" ;)
	nop
	nop
}
	ldx #5
cpyregs:pla		;pull pc, registers & status off stack...
	sta pch,x	;...and preserve them for display
	dex
	bpl cpyregs	;(notice pc will be wrong- processor bug)
	bmi start
; $e021
call:			;////// entry for 'jmp' or 'sys'
!ifdef OPTI{
	!ifndef P500{
	lda #<bellmd	;set pointer to bell flag
	sta ptr
	lda #>bellmd
	sta ptr+1
	ldx i6509	;remember ibank
	lda #irom
	sta i6509	;switch to system bank
	ldy #$00
	sta (ptr),y	;disable bell (some value > 0)
	stx i6509	;restore ibank	
	tya
	}
} else{
!ifdef P500{
	lda #$00
	sta mode	;set 40 column mode
	nop
	} else{
	jsr setmod	;set 40/80 col mode, disable bell for m-command if b-machine
	lda #$00
	}
}
	sta acc		;clear everything up for user
	sta xr
	sta yr
	sta flgs
	lda #<monitor	;init pc to ourselves
	ldy #>monitor
	sta pcl
	sty pch
	lda e6509
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
	lda #$40+$80	;$40=kernal errors, $80=i/o messages to screen
	jsr _setmsg	;enable kernal i/o error messages (sets msgflg = .a)
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
dsplp:	lda pcb,y
	jsr puthxs	;display rest, separated by spaces
	iny
	cpy #8
	bcc dsplp
; $e08b main loop
main:
	jsr crlf	;print cr+lp
	ldx #0
	stx txtptr	;init ptr to textbuffer
; line input
mainlp:	jsr basin	;read one line (up to <cr>) into buffer
	sta buf,x
	inx
	cpx #161
	bcs error	;...branch if 'line too long'
	cmp #cr
	bne mainlp	;loop until end of line

	lda #0
	sta buf-1,x	;flag end of line

mgetchr:jsr gnc	  	;get a character from buffer
	beq main	;end of line
	cmp #' '	;skip leading spaces
	beq mgetchr
	nop
	nop
	nop
; $e0b2
moncmd:
	ldx #cmdqty-1	;compare first char to list of valid commands
mcmdlp:	cmp cmdchr,x
	beq main1	;found it in list!
	dex
	bpl mcmdlp
; $e0c2			;checked entire list, not found. fall into 'error'
error:
	jsr primm
	!pet $1d, "?", 0
	jmp main
; $e0c9
main1:
	cpx #cmdls	;is command 'L'oad, 'S'ave, or 'V'erify?
	bcs mcmdls	;...branch if so:  can't use parse!

	cpx #cmdno	;is it a number to evaluate?
	bcs mcmdno	;...branch if so

	txa		;multiply index by two
	asl
	tax

	lda cmdtbl+1,x	;push address of command on stack
	pha
	lda cmdtbl,x
	pha
	jmp parse	;parse first arg, and rts to correct command routine

mcmdls:	sta verck	;save copy of what command was,
	jmp lodsav	;...and jump to common load/save/verify routine

mcmdno	jmp convert	;simply evaluate number & print its value
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

; $e11a crossbank subroutines
fetch:	jsr swbnk
	lda (t2),y	;get byte 
	jmp restbnk

stash:	jsr swbnk
	sta (t2),y	;put byte
restbnk:ldx tmpbnk	;restore ibank
	stx i6509
	ldx xsave	;restore .x! 
	rts

swbnk:	stx xsave	;save .x!
	ldx i6509
	stx tmpbnk	;remember ibank
	ldx t2+2	;set up bank
	stx i6509
	rts

getstat:ldx i6509	;remember ibank
	lda #irom
	sta i6509	;switch to system bank
	ldy #status
	lda (ptr),y	;get status
	stx i6509	;restore ibank
	rts
!ifndef OPTI{
!fill 10,$ff
}
;********************************************
;	Display memory command
;********************************************
; $e152
dspmem:
	bcs dspdef	;no range, do default lines
	jsr t0tot2	;else move 'from' value into place
	jsr parse
	bcc dspcalc	;got 'to', go dump

dspdef:	lda #MEMDEF-1  	;do default lines
	sta t0
	bne dspdump	;always

; calculate # of lines
dspcalc:jsr sub0m2	;calculate # bytes, put result in t0 (msb goes in .a)
	bcc dsperr	;...branch if sa > ea
; shift 3/4 bits right for 8/16 bytes per line
!ifdef OPTI{
	ldx #$03+COLS	;add 1 if 80 columns
} else{
	ldx #$03
	bit mode	;divide by 8 if 40-col, 16 if 80-col
	bpl dspshft
	inx
}
dspshft:lsr t0+2	;shift msb right,
	ror t0+1	;..into middle byte,
	ror t0		;..and finally into lsb
	dex
	bne dspshft

dspdump:jsr stop	;is stop key down?
	beq dspexit	;..if so, exit.

	jsr dmpone
!ifdef OPTI{
	lda #8*(COLS+1)	;8/16 bytes per line for 40/80 columns
} else{
	lda #8
	bit mode	;add 8 (16 if 80-col) to line start address
	bpl dsp40c
	asl
}
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
	ldy #0
setrlp:	jsr parse
	bcs setrx	;quit anytime arg list is empty
	lda t0
	sta flgs,y
	iny
	cpy #5
	bcc setrlp
setrx:	jmp main
;********************************************
;	Alter memory command
;********************************************
; $e1ab
setmem:
	bcs setmx	;...branch if no arguments- just regurgitate existing data
	jsr t0tot2	;destination bank, addr in 't2'
	ldy #0

setmlp:	jsr parse	;scan for next argument
	bcs setmx	;...branch if eol
	lda t0		;get the byte to stash
	jsr stash	;stash it
	iny
!ifdef OPTI{
	cpy #8*(COLS+1)	;8/16 bytes per line for 40/80 columns
	bcc setmlp
} else{
	bit mode
	bpl setm40c
	cpy #16
	bcc setmlp
setm40c:cpy #8
	bcc setmlp
}
setmx:	jsr primm	;clear all modes & cursor up
	!pet esc, "o", $91, 0

	jsr dmpone
	jmp main
;********************************************************************
;	Go command- start executing at either the supplied address,
;	   or (default) the current contents of the PC reg
;********************************************************************
; $e1d6 go jumps only to same bank !!!
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
!ifdef OPTI{
	cpy #8*(COLS+1)	;8/16 bytes per line for 40/80 columns
	bcc dmpbylp
} else{
	cpy #8		;8 bytes/line for 40-column mode
	bit mode
	bpl dmp40c
	cpy #16		;16 bytes/line for 80-column mode
dmp40c:	bcc dmpbylp
}
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
!ifdef OPTI{
	cpy #8*(COLS+1)	;8/16 bytes per line for 40/80 columns
	bcc dmpchlp
} else{
	bit mode
	bpl dmp40c2
	cpy #16
	bcc dmpchlp
dmp40c2:cpy #8
	bcc dmpchlp
}
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
	lda #0
	sta dir
	jsr range	;get source in t2, length in t1
	bcs trnerr
	jsr parse	;get destination in t0
	bcc trnok

trnerr:	jmp error

trnok:	bit verck
	bpl trnnxln	;...branch if compare (direction crap unimportant)

	sec		;determine direction of transfer (to avoid stepping on ourselves!)
	lda t2
	sbc t0		;source - destination (ignore banks...there might be bleed-thru)
	lda t2+1
	sbc t0+1
	bcs trnnxln	;branch if source >= destination

	lda t1		;source < destination,   must work from back to front
	adc t0		;carry is already clear!
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
	sta dir		;flag backwards direction

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

trnequ: bit dir
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
	jsr range	;get sa in t2, calculate length, put in t1
	bcs hunerr	;...error if eol
	ldy #0
	jsr gnc		;get first char
	cmp #$27	;is it an <'>
	bne hunhex		;no-  must be hex
	jsr gnc		;yes- get first string chr
	cmp #0
	beq hunerr	;...branch if true eol (error)

hunnxch:sta xcnt,y
	iny
	jsr gnc		;get next
	beq hunstrt	;yes-end of string
	cpy #32		;no-32 char yet?
	bne hunnxch	;no-get more
	beq hunstrt	;yes-go look for it

hunhex:	
!ifdef OPTI{
;!fill 3, $ff
} else{
	sty bad		;zero for rdob
}
	jsr pargot	;finish hex read

hunnxhx:lda t0
	sta xcnt,y
	iny
	jsr parse	;get next character
	bcs hunstrt	;no more -go look for bytes
	cpy #$20	;32 bytes yet?
	bne hunnxhx	;no-get more

hunstrt:sty verck	;yes-start search
	jsr crlf	;next line

hunlp:	ldy #$00

hunstlp:jsr fetch	;get a byte from memory
	cmp xcnt,y
	bne hundiff	;...branch if no match
	iny
	cpy verck	;checked full string?
	bne hunstlp	;no-check on

	jsr putt2	;print address found
	jsr putspc	;pretty up to 8 bytes
	jsr putspc

hundiff:jsr stop
	beq hunx	;...branch if user requests abort
	jsr inct2
	jsr dect1
	bcs hunlp	;loop if not done

hunx:	jmp main

hunerr:	jmp error
;************************************************************
; load/save/verify
;
; l ["name"] [,device_number] [,alt_load_address]
; v ["name"] [,device_number] [,alt_load_address]
; s "name", device_number, starting_address, ending_address
;************************************************************
; $e337
lodsav:
	jsr lsinit	;init defaults, returns with .y = 0
lsspc:  jsr gnc		;look for name
!ifdef OPTI{
	beq lserr	;cbm2 non-tape system has no load without name
} else{
	beq lsload	;branch if no name (must be default load)
}
	cmp #' '
	beq lsspc	;skip spaces
	cmp #$22	;quote
	bne lserr	;jmp to error if no quoted name
	ldx txtptr

lsnxchr:lda buf,x	;get chr
	beq lsload	;eol, must be load
	inx
	cmp #$22	;pass everything up to closing quote
	beq lsdev
	sta (fnadr),y	;okay - always monitor bank
	inc fnlen
	iny
	cpy #17		;check length of name (16 max.)
	bcc lsnxchr
lserr:  jmp error

lsdev:	stx txtptr
	jsr gnc		;trash delimitor
	beq lsload	;...eol, use default
	jsr parse	;get device #
	bcs lsload	;...eol, use default
	lda t0
	sta fa		;device # in 'fa' (let kernal catch invalid devices)

	jsr parse	;get starting address
	bcs lsload	;none, must be load
	jsr t0tot2	;save start_addr in t2

	jsr parse	;get ending address
	bcs loadadr	;none...must be 'alternate load'
!ifndef OPTI{
	jsr crlf	;prep for 'saving...' msg
}
	lda verck
	cmp #'s'	;check that this is a save
	bne lserr
	lda #0
	sta sa
; copy start+end address to kernal vars
	ldx #2
lsadrcp:lda t2,x
	sta stal,x	;copy start adr
	lda t0,x
	sta eal,x	;copy end adr
	dex
	bpl lsadrcp

	jsr fparcpy	;copy file parameter to system bank
	ldx #stal	;load parameter addresses for kernal routine
	ldy #eal
	jsr _save	;do save
!ifdef OPTI{
	lda #0
	sta fnlen
	sta buf
	sta txtptr
	jmp chksave
} else{
	jmp main
}

lsload: ldx #$ff
	stx t2		;load to saved address
	stx t2+1
!ifdef OPTI{
	ldx #DEFBANK
	stx t2+2	;default is first ram bank 0/1
}
loadadr:lda #0		;flag 'non-default load'
	sta sa
	jsr fparcpy	;copy file parameter to system bank

	lda verck	;check for load
	cmp #'v'	;..or verify
	beq verify
	cmp #'l'
	bne lserr
	lda #0		;flag load
	!byte $2c	;skip next
verify	lda #$80	;flag for verify
	ldx t2		;set up new address for load
	ldy t2+1
	ora t2+2	;or verify flag to bank
	jsr _load	;do load/verify
!ifdef OPTI{
	bcs verifyx
	lda verck
	cmp #'v'	;check for verify
	beq verchk
verifyx:jmp main
}
verchk:	jmp lsstate	;print verify result
;******************************************************************
;	Fill command - F starting-address ending-address value
;******************************************************************
!ifndef OPTI{
!fill 16, $ff
}
; $e3db
fill:
	jsr range	;sa in t2, len in t1
	bcs filerr	;error if eol

	lda t2+2
	cmp temps+2
	bne filerr	;prevent bank wraps

	jsr parse	;get fill value
	bcs filerr
	ldy #0

fillp:  lda t0
	jsr stash	;fill memory byte by byte (without comparison check)
	jsr stop
	beq filx	;branch if user wants to abort
	jsr inct2
	jsr dect1
	bcs fillp

filx:	jmp main

filerr:	jmp error
;**********************************************************
;  simple assembler
;  syntax:      a 1111 lda ($00,x)
; 	      a 1111 dex:		(':' = terminator)
;**********************************************************
; $e406
assem:
	bcs aserr	;...branch if missing sa
	jsr t0tot2	;save sa

asspc:  ldx #0
	stx hulp+1	;clear left mnemonic
	stx count	;clear operand

asnxchr:jsr gnc		;get a char
	bne aspars	;check for eol
	cpx #0
	bne aspars
	jmp main	;if eol & no mnemonic, exit cleanly

aspars:	cmp #' '	;is it a space ?
	beq asspc	;yes - start again (passes over object code, if any)
	sta msal,x      ;no - save char
	inx
	cpx #3	  	;got three chars ?
	bne asnxchr	;no - loop

asnxmch:dex		;squished all three ? (3x5bits in hulp,hulp+1 bit#15-1)
	bmi asstart	;yes
	lda msal,x	;no - first in last out
	sec	     	;no borrow
	sbc #$3f	;normalize
	ldy #5	  	;set for 5 shift rights

ashftmn:lsr
	ror hulp+1	;left mnemonic
	ror hulp	;right mnemonic
	dey	     	;done 5 shifts?
	bne ashftmn	;no-loop
	beq asnxmch	;always

aserr:	jmp error

asstart:ldx #2		;move output buffer index past crunched mnemonic

asoplp:	lda count	;after first number copy everything else to output buffer
	bne asopend
	jsr eval	;evaluate next parameter, if number crunch it
	beq asoptxt	;...branch if not a number
	bcs aserr	;...branch if illegal number

	lda #'$'
	sta hulp,x	;buffer a number, either '$00' or '$0000'
	inx
	lda t0+2
	bne aserr	;...branch if number too large
	ldy #4
	lda shift	;kludge to allow user to force absolute over zero page
	cmp #8
	bcc asopchk	;...allow only hex or octal bases to force absolute
	cpy count
	beq asop2by	;...branch to force absolute	
asopchk:lda t0+1
	bne asop2by	;...branch if 2-byte field
	ldy #2		;else set up  1-byte field

asop2by:lda #'0'
asopzlp:sta hulp,x
	inx
	dey
	bne asopzlp

asoptxt:dec txtptr	;re-get last character from input buffer
asopend:jsr gnc		;copy rest of input buffer to output buffer
	beq asopeol	;...branch if eol
	cmp #' '
	beq asoplp	;...squish out spaces
	sta hulp,x	;hopefully it's of one of these:   #,()
	inx
	cpx #10
	bcc asoplp	;...loop until eol or
	bcs aserr	;...buffer overflow

asopeol:stx t1		;save input # of characters
	ldx #0
	stx wrap	;start trial at zero

as110:  ldx #0
	stx temp	;disa index=0
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

as210:  cpx #3
	bne as230
	ldy length
	beq as230	;skip-single byte instr

as220:  lda format
	cmp #$e8	;a relative instr?
	lda #'0'	;test zeros
	bcs as250	;no-3 byte
	jsr tst2	;test a byte,2 chars
	dey
	bne as220

as230:  asl format
	bcc as240
	lda char1-1,x
	jsr tstrx	;test syntax
	lda char2-1,x
	beq as240
	jsr tstrx	;test more syntax

as240:  dex
	bne as210
	beq as300

as250:  jsr tst2	;test a word,4 chars
	jsr tst2

as300:  lda t1	  	;check # chars of both
	cmp temp
	beq as310	;match, skip
	jmp tst05	;fail

as310:  ldy length
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
as340:  dex	     	;subtract 2 from 'diff' for instr
	dex
	txa
	ldy length	;set index to length
	bne as420	;branch always

as400:	lda t0-1,y      ;no-put byte out there

as420:	jsr stash
	dey
	bne as400

as500: 	lda wrap	;get good op code
	jsr stash
	jsr cronly	;get ready to overstrike line
	jsr primm	;print 'A ' & erase eol
	!pet "a ", esc, "q", 0
	jsr dis400	;disassemble one line

	inc length
	lda length
	jsr addt2	;update address

	lda #'a'	;set up next line with 'a bnnnn ' for convenience
	sta mkeyd	;put it in the keyboard buffer
	lda #' '
	sta mkeyd+1
	sta mkeyd+7
	lda t2+2	;get the bank number
	jsr makhex	;get hi byte in .a (which we'll ignore), and lo in .x
	stx mkeyd+2
	lda t2+1	;next get mid byte of address
	jsr makhex
	sta mkeyd+3	;..and put in buffer,
	stx mkeyd+4
	lda t2		;then get the low byte of address,
	jsr makhex
	sta mkeyd+5	;..and put that in the buffer, too.
	stx mkeyd+6
	jsr asprint	;print new assembler input line
	nop
	jmp main

aerr:	jmp error

;  test char in .a with char in hulp
;
tst2:	jsr tstrx	;test for '00' (do two tests)

tstrx:	stx sxreg
	ldx temp	;get current position
	cmp hulp,x	;same char
	beq tst10	;yes-skip
	pla	     	;pull jsr off stack
	pla

tst05:	inc wrap	;try next trial
	beq aerr	;=0 tried all,sorry
	jmp as110

tst10:	inc temp
	ldx sxreg	;restore x
	rts

;***************************************************
;	Mini disassembler
;***************************************************
; $e599
disasm:
	bcs dishpag	;use a default length from current sa
	jsr t0tot2
	jsr parse
	bcc disto	;got sa,ea. use 'em

dishpag:lda #DISDEF	;guess at 1/2 page
	sta t0
	bne dislp	;always

disto:	jsr sub0m2    	;put ea-sa in t0
	bcc diserr	;...branch if sa > ea

dislp:  jsr primm	;print <cr> & delete to end of line for neatness
	!pet cr, esc, "q", 0
	jsr stop
	beq disx	;...branch if user requests abort
	jsr dis300      ;disassemble 1 line
	inc length
	lda length
	jsr addt2
	lda length
	jsr subt0
	bcs dislp

disx:	jmp main

diserr:	jmp error

dis300:	lda #'.'
	jsr bsout
	jsr putspc

dis400:	jsr putt2
	jsr putspc
	ldy #0
	jsr fetch	;get a byte from memory
	jsr dset	;get instr & digest it

	pha	      	;dump (length+1) bytes
	ldx length      ;(.y=0 from 'dset' above)
	inx

pradr0:	dex
	bpl pradrl      ;pad non-printers
	jsr primm	;print 3 spaces
	!pet "   ", 0
	jmp pradrm

pradrl:	jsr fetch
	jsr puthxs

pradrm:	iny
	cpy #3
	bcc pradr0
	pla

	ldx #3
	jsr prmne	;print mnemonic
	ldx #6	   	;6 format bits

pradr1:	cpx #$03
	bne pradr3     	;if x=3 print adr val
	ldy length
	beq pradr3     	;no print if len=0

pradr2:	lda format
	cmp #$e8 	;relative addressing mode?
	php		;save carry
	jsr fetch
	plp
	bcs reladr
	jsr puthex
	dey
	bne pradr2

pradr3:	asl format	;test next format bit
	bcc pradr4	;no print if=0
	lda char1-1,x
	jsr bsout
	lda char2-1,x
	beq pradr4
	jsr bsout

pradr4:	dex
	bne pradr1
	rts

reladr: jsr pcadj3	;pcl,h + disp + 1 into a,x
	clc	      	;add 1
	adc #1
	bne relad2
	inx

relad2:	jmp putwrd

pcadj3:	ldx t2+1
	tay
	bpl pcadj4
	dex

pcadj4:	adc t2
	bcc pcrts
	inx

pcrts:	rts

; disassembler digest routine
; $e659
dset:	tay
	lsr	    	;even/odd test
	bcc ieven
	lsr	    	;test b1
	bcs err	  	;xxxxxx11 instr bad
	cmp #$22
	beq err	  	;10001001 instr bad
	and #$07	;mask 3 bits for adr mode
	ora #$80 	;add indexing offset

ieven:	lsr	    	;left/right test
	tax
	lda nmode,x	;index into mode table
	bcs rtmode 	;if carry set use lsb for
	lsr	    	;print format index
	lsr
	lsr	    	;if carry clr use msb
	lsr

rtmode:	and #$0f	;mask for 4-bit index
	bne getfmt	;$0 for bad opcodes

err:	ldy #$80	;sub $80 for bad opcode
	lda #0	   	;set format index to zero

getfmt:	tax
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

mnndx1:	lsr
	bcc mnndx3	;form index into mnemonic tab
	lsr

mnndx2:	lsr	    	;1xxx1010->00101xxx
	ora #$20	;xxxyyy01->00111xxx
	dey	      	;xxxyyy10->00110xxx
	bne mnndx2	;xxxyy100->00100xxx
	iny	      	;xxxxx000->000xxxxx

mnndx3:	dey
	bne mnndx1
	rts	      	;(.y=0 is assumed!)

; print mnemonic
; enter x=3 characters
; $e6a1
prmne:	tay
	lda mneml,y	;fetch 3 char mnemonic
	sta t1
	lda mnemr,y
	sta t1+1

prmn1:	lda #0
	ldy #5

prmn2:	asl t1+1	;shift 5 bits of char
	rol t1	   	;into a
	rol	    	;clear carry
	dey
	bne prmn2
	adc #$3f	;add '?' offset
	jsr bsout
	dex
	bne prmn1
	jmp putspc	;finish with space

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

;  parse entry when 1st character has already been read
pargot:
	dec txtptr

;  parse next item in buffer & put its value into  t0, t0+1 & t0+2.
;
;	.z=1 if no value found.
;	.c=1 if eol.
;	.x & .y are preserved, .a contains # digits read.
;
;	if error, call is popped & 'jmp error' performed.
; $e7a8
parse:  jsr eval	;evaluate ascii input as a number
	bcs parserr	;...branch if error
	jsr glc		;re-get last character
	bne parckdl	;...branch if not eol
	dec txtptr	;point to eol: make next 'gnc' see eol
	lda count
	bne parok	;...valid number input, treat eol as a delimiter
	beq pareol	;...the well is very dry

parckdl	cmp #' '	;parse delimiters (only allow <space> or <comma>)
	beq parok
	cmp #','
	beq parok	;...fall into error if bad delimiter

parserr:pla		;pop this call
	pla
	jmp error

pareol:	sec		;set .c=1 for eol
	!byte $24	;skip next

parok:	clc		;clear .c for not-eol
	lda count	;set .z=1 for valid number
	rts

;  evaluate next item in buffer & put its value into  t0, t0+1 & t0+2.
;
;	.c=0  normal return
;	.c=1  error  return
;	.z=1  null input
;	.x & .y are preserved.
; $e7cf
eval:	lda #0
	sta t0		;clear value
	sta t0+1
	sta t0+2
	sta count	;reset digit counter (flags valid number vs. null input)
	txa
	pha		;preserve .x & .y
	tya
	pha

evlspc:	jsr gnc		;get next character
	bne evnoeol
	jmp eval_ok	;...branch if end of line
evnoeol:cmp #' '
	beq evlspc	;...branch & ignore leading spaces

	ldx #3
evprelp:cmp cmdnum,x	;is first character a base prefix?
	beq evbase	;...yes
	dex
	bpl evprelp

	inx		;...no: default base to hex
	dec txtptr	;to reget digit

evbase:	ldy bases,x	;this is the base
	lda shifts,x	;this is the # of shifts required for given base
	sta shift

evnxchr:jsr gnc		;get next character
	beq eval_ok	;...branch if eol
	sec
	sbc #'0'	;convert ascii digit to binary value
	bcc eval_ok	;...branch if not a number  (assume delimiter)
	cmp #10
	bcc evnumbr		;...number 0-9
	sbc #7
	cmp #16		;...number a-f
	bcs eval_ok	;...branch if not a number  (assume delimiter)

evnumbr:sta number	;binary value of current digit
	cpy number
	bcc eval_ng	;...branch if number out of range of given base
	beq eval_ng
	inc count	;flag valid digit
	cpy #10
	bne evshift	;...branch if not base-10

	ldx #2
evdeclp:lda t0,x	;save a copy current total for base-10 calc
	sta temps,x
	dex
	bpl evdeclp

evshift:ldx shift
evshflp:asl t0		;multiply current value by base using binary shifts
	rol t0+1
	rol t0+2
	bcs eval_ng	;...branch if overflow error
	dex
	bne evshflp	;...next shift

	cpy #10
	bne evaddig	;...branch if not base-10

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

evaddig:clc		;add current digit (all bases)
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
	beq evnxchr	;...next character

eval_ng:
	sec
	!byte $24	;skip next
eval_ok:
	clc
	sty shift	;return input base (used by 'assem')
	pla
	tay		;restore .x & .y
	pla
	tax
	lda count
	rts

bases:	!byte 16,10, 8, 2
shifts:	!byte  4, 3, 3, 1

; $8892 print t2 as 5 hex digits:	BHHLL
putt2:	lda t2+2	;get bank (a19-a16)
	jsr makhex	;make ascii:  msd in .a (ignored) and lsd in .x
	txa
	jsr bsout	;print lsd
	lda t2		;get address (a15-a0)
	ldx t2+1

putwrd:	pha		;print address:  msb first, then lsb
	txa
	jsr puthex
	pla

puthxs:	jsr puthex	;print byte in .a as two hex digits

putspc:	lda #' '	;print <space>
	jmp bsout

cronly:	jsr primm	;print <cr><crsr-up>
	!pet cr, $91, 0
	rts

crlf:	lda #cr
	jmp bsout

new_line:
	jsr primm	;print <cr><clear_eol><space>
	!pet cr, esc, "q ", 0
	rts

puthex:	stx sxreg
	jsr makhex
	jsr bsout
	txa
	ldx sxreg
	jmp bsout

; $e8d2 convert .a to 2 hex digits & put msb in .a, lsb in .x
makhex:	pha
	jsr maknib	;convert nibble
	tax		;move low nibble to .x
	pla
	lsr		;sift high nibble right and convert it to .a
	lsr
	lsr
	lsr

maknib:	and #$0f
	cmp #$0a
	bcc mak0_9	;number 0-9
	adc #$06	;add 6+carry=7 for 'a-f
mak0_9	adc #'0'	;add petscii '0'
	rts

; $e8e7 get last character
glc:	dec txtptr

; $e8e9 get next character: return in .a  (return = if buffer empty or eol)
gnc:	stx sxreg
	ldx txtptr
	lda buf,x
	beq gnceol	;eol-return with z=1
	cmp #':'
	beq gnceol	;eol-return with z=1
	cmp #'?'
gnceol:	php
	inc txtptr
	ldx sxreg
	plp
	rts

; move t0,t0+1,t0+2 to t2,t2+1,t2+2
t0tot2: lda t0
	sta t2
	lda t0+1
	sta t2+1
	lda t0+2
	sta t2+2
	rts

; subtract t2 from t0, result in t0
sub0m2:	sec
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
; $e922 decrement t0
dect0:  lda #1

subt0:  sta sxreg	;subtract .a from t2
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

; decrement t1
dect1:  sec
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
inct2:  lda #1

addt2:  clc	      	;add .a to t2
	adc t2
	sta t2
	bcc addt2x
	inc t2+1
	bne addt2x
	inc t2+2
addt2x:	rts

;  decrement t2
dect2:  sec
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
t0topc:	bcs t0topcx	;no arg given, just exit
	lda t0
	ldy t0+1
	ldx t0+2
	sta pcl
	sty pch
	stx pcb
t0topcx:rts

;  read a range - put sa in t2, count in t1   (save ea in 'temps')
;
;  returns .c=0 if okay, .c=1 if error (missing parameter or sa < ea)
range:  bcs rangex	;...branch if missing sa
	jsr t0tot2	;move sa from t0 to t2
	jsr parse	;get ea
	bcs rangex	;...branch if missing ea

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
	bcc rangex	;invert .c from subtraction above
	clc		;good stuff exits here
	!byte $24	;skip next
rangex:	sec		;bad  stuff exits here
	rts

;  convert given number from its base to hex
convert:
	jsr pargot	;parse number & put its value in t0
	jsr new_line
	lda #'$'	;print hexidecimal conversion of t0
	jsr bsout
	lda t0+2	;how big a number is it?
	beq cvnobnk	;...branch if 'bank' nybble unnecessary
	jsr makhex
	txa
	jsr bsout	;print lsd of this byte
cvnobnk:lda t0
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

;  convert from binary to decimal (BCD)
;
;	input :	binary value (3 bytes) in T0
;	output:	decimal value (4 bytes) in HULP as packed BCD
bindec:	
	jsr t0tot2	;copy from T0 to T2 (lo/mid/hi)

	lda #0
	ldx #7
bindlp: sta hulp,x	;initialize working registers
	dex
	bpl bindlp
	inc hulp+7	;seed value_of_bit with 1

	ldy #23		;loop index (3 bytes = 24 bits)
	php		;save caller's processor mode
	sei		;disable IRQ's (but beware NMI's!)
	sed		;put processor into decimal mode (for ADC's & SBC's)

;  main loop. rotate bits right 1 at a time, and if
;  set add the current value_of_bit to the sum.
binmlp:	lsr t2+2	;hi
	ror t2+1	;mid
	ror t2		;lo
	bcc binbit0	;...branch if bit not set (its value is 0)

	clc
	ldx #3
binbtlp:lda hulp+4,x	;add current value_of_bit (decimal arithmetic)
	adc hulp,x
	sta hulp,x
	dex
	bpl binbtlp

binbit0:clc
	ldx #3
bincalp:lda hulp+4,x	;calculate value of next bit (decimal arithmetic)
	adc hulp+4,x
	sta hulp+4,x
	dex
	bpl bincalp
	dey
	bpl binmlp	;loop until done conversion

	plp		;restore processor mode
	rts

unpack_t0:
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
unpack:
	sta count
	sty shift
unplp:	ldy shift	;# bits per digit, .x is digit counter
	lda #0
unpshlp:asl hulp+3	;lo
	rol hulp+2	;mid lo
	rol hulp+1	;mid hi
	rol hulp	;hi
	rol		;shift a digit into .a
	dey
	bpl unpshlp

	tay		;set flags for .a
	bne binzero
	cpx #1
	beq binzero	;...print zero if it's the last digit
	ldy count
	beq binskzr	;...else skip leading zeros

binzero:inc count	;flag a non-zero digit
	ora #$30	;make it ascii
	jsr bsout	;print it

binskzr:dex
	bne unplp	;...loop until all digits printed
	rts

;********************************************
;	Disk Command/Status Operations
;
;	@[device-number][,command-string]
;********************************************
disk:
	bne dskdev	;...branch if given device #
	ldx #DEFDEV	;default device number
	!byte $2c	;skip next

dskdev: ldx t0		;get given device #
	cpx #4
	bcc baddev	;...branch if bad device #
	cpx #31
	bcs baddev
!ifdef OPTI{
	stx fa		;set first address
	lda e6509
	sta fnadr+2	;set filename bank (in case DIR cmd)
} else{
	stx t0
}

	lda #0
	sta t0+2	;clear line # register (in case DIR cmd)
	sta fnlen	;reset command string length for status
!ifdef OPTI{
	sta la		;set logical index
} else{
	tax
	jsr setbnk	;cmd string in ram0 (in case DIR cmd)
}
	
	jsr gnc		;peek at first character of disk command
	dec txtptr	;backup so we will re-get this character later
	cmp #'$'
	beq disk_dir	;...branch if directory read

; open disk command channel & pass it given command
!ifdef OPTI{
chksave:lda #15		;command channel
	sta sa		;set secondary address
	jsr fparcpy	;copy file parameter to system bank
	clc
} else{
	lda #0		;la
	ldx t0		;fa
	ldy #15		;sa
	jsr setlfs
}
	jsr open	;open disk command channel (c=0)
	bcs disk_done	;...branch on error

	ldx #0		;logical index
	jsr chkout	;make it an output channel
	bcs disk_done	;...branch on error

dsknxch:ldx txtptr	;get next character
	inc txtptr
	lda buf,x
	beq disk_st	;...branch if eol
	jsr bsout	;xmit character to disk
	bcc dsknxch	;...loop until error or eol
;read & report disk status
disk_st
	jsr clrch	;clear channel
	jsr crlf
	ldx #0
	jsr chkin	;make it an input channel
	bcs disk_done	;...branch on error

!ifdef OPTI{
	jsr basin	;get a character from disk
	cmp #cr
	beq nodev	;...no device? if first char cr
dskchlp:jsr bsout	;print it
	jsr basin	;get a character from disk
	cmp #cr
	beq disk_done	;...branch if eol
	bne dskchlp	;...loop until error or eol
} else{
dskchlp:jsr basin	;get a character from disk
	jsr bsout	;print it
	cmp #cr
	beq disk_done	;...branch if eol
	lda fnadr
	and #$bf	;strip eoi bit
	beq dskchlp	;...loop until error or eol
}

!ifdef OPTI{
baddev:
	jmp error	;bad device number error

dir_done:
	cpy #3
	bne disk_done
	jsr crlf

nodev:
jsr primm
!pet "no device?",0	;if only got cr, no dev present error
}

disk_done:
	jsr clrch	;clear channel
	lda #0		;la
	sec
	jsr close	;close device (c=1)
	jmp main

!ifndef OPTI{
baddev:
	jmp error
}
; read & display the disk directory
disk_dir:
	ldy #$ff	;determine directory string length
	ldx txtptr
	dex
	
dirchlp:iny
	inx
	lda buf,x	;get a character
	bne dirchlp	;...loop until eol

!ifdef OPTI{
	sty fnlen
	lda txtptr
	sta fnadr
	lda #>buf
	sta fnadr+1
	lda #$60
	sta sa
	jsr fparcpy	;copy file parameter to system bank
	clc
} else{
	tya		;length
	ldx txtptr	;fnadr low
	ldy #>buf	;fnadr high
	jsr setnam	
	lda #0		;la
	ldx t0		;fa
	ldy #$60	;sa
	jsr setlfs
}
	jsr open	;open directory channel
	bcs disk_done	;...branch on error
	ldx #0
	jsr chkin	;make it an input channel

!ifndef OPTI{
	jsr crlf	;start a new line
}

	ldy #3		;first pass only- trash first two bytes read

dirlp:  sty t1		;loop counter
dirbklp:jsr basin
	sta t0		;get # blocks low
!ifdef OPTI{
	jsr getdirstat
	lda status
	bne dir_done	;...branch if error
} else{
	lda fnadr
	bne disk_done	;...branch if error
}
	jsr basin
	sta t0+1	;get # blocks high
!ifdef OPTI{
	jsr getdirstat
	lda status
	bne dir_done	;...branch if error
} else{
	lda fnadr
	bne disk_done	;...branch if error
}
	dec t1
	bne dirbklp	;...loop until done

!ifdef OPTI{
	jsr crlf	;start a new line
}
	jsr bindec	;convert # blocks to decimal
	lda #0		;no leading zeros
	ldx #8		;max digits
	ldy #3		;required # shifts for decimal value
	jsr unpack	;print # blocks
	lda #' '
	jsr bsout	;print space  (to match loaded directory display)

dirfnlp:jsr basin	;read & print filename & filetype
	beq direol	;...branch if eol
!ifdef OPTI{
	jsr getdirstat
	ldx status
	bne disk_done	;...branch if error
} else{
	ldx fnadr
	bne disk_done	;...branch if error
}
	jsr bsout
	bcc dirfnlp	;...loop always

direol:
!ifndef OPTI{
	jsr crlf	;start a new line
}
	jsr stop

	beq disk_done	;...branch if user hit STOP
	ldy #2
	bne dirlp	;...loop always

!ifdef P500{
;!fill 21, $ff
} else{
setmod:
	lda hw_irq
	cmp #$00
	bne +
	lda #$ff
	!byte $2c	;skip next
+	lda #$00
	sta mode
	jsr primm
	!pet esc, "h", 0
	rts
}
; $eb87	outsourced from assem - copy new line to kernal keyboard buffer
asprint:lda #<keyd
	sta ptr		;set ptr to kernal keyboard buffer
	lda #>keyd
	sta ptr+1
	ldx i6509	;remember ibank
	lda #irom
	sta i6509	;switch to system bank

	ldy #7		;8 chars
asprtlp:lda mkeyd,y	;get from monitor key buffer
	sta (ptr),y	;copy one char to kernal buffer
	dey
	bpl asprtlp

	lda #<ndx	;set pointer to kernal keybuffer index
	sta ptr
	ldy #0
	sty ptr+1
	lda #8		;store 8 keys in buffer
	sta (ptr),y
	stx i6509	;restore ibank
	rts
; $ebae	setp load/save
lsinit:	
!ifdef OPTI{
	ldy #DEFDEV
	sty fa		;setup defaults: tape, fixed_load, no filename
	ldy #1
	sty sa
	dey		;(.y=0)
	sty status
	sty fnlen	;reset filename length
	lda #DEFBANK
	sta eas		;default l/s/v from/to first ram bank 0/1
	sta stas
} else{
	ldy #1
	sty fa		;setup defaults: tape, fixed_load, no filename
	sty sa
	dey		;(.y=0)
	sty eas		;default l/s/v from/to bank 0
	sty stas	;default l/s/v from/to bank 0
	sty status
	sty fnlen	;default no filename
}
	lda #<xcnt
	sta fnadr	;filename address in monitor bank
	lda #>xcnt
	sta fnadr+1
	lda e6509
	sta fnadr+2
	rts 		;.y has to be 0 for indirect in lodsav
; $ebca copy file parameter for kernal routine to system bank
fparcpy:
	ldx i6509
	lda #irom
	sta i6509	;switch to system bank
	lda #$00
	sta ptr		;init pointer
	sta ptr+1
	ldy #sa		;end of file params
fparlp:	lda $00,y
	sta (ptr),y	;copy file parm $90-$92, $96-$a0
	dey
	cpy #eal-1	;end of upper part list
	bne +
	ldy #fnadr+2	;lower part of list
	bne fparlp
+	cpy #fnadr-1	;start of file params
	bne fparlp
	stx i6509	;restore ibank
	rts
; $ebed print verify result 
lsstate:jsr getstat	;get verify state
	and #$10
	bne lssterr	;error detected
	jsr primm
!ifdef OPTI{
	!pet " ok", 0
} else{
	!pet cr, "ok", cr, 0
}
	jmp main

lssterr:jsr primm
	!pet " error", 0
	jmp main
;********************************************************************
;	Jsr command- start executing at either the supplied address,
;	   or (default) the current contents of the PC reg.
;	   return is to monitor 'main' loop.
;********************************************************************
gosub:
	jsr t0topc	;copy adr & bank to pcl,h & pcb,  if given
	lda pcb
	cmp e6509
	beq gosmbnk	;branch if adrress is in monitor bank
	jsr gofobnk	;gosub foreign bank
	jmp main

gofobnk:sei		;disable interrrupts
	clc
	lda pcl
	adc #$02	;load address and add +2
	tax
	lda pch
	adc #$00	;add hi
	pha		;push target +2 to stack
	txa
	pha
	lda flgs	;push regs to stack
	pha
	lda pcb
	sta i6509	;set bank
	lda acc
	ldx xr
	ldy yr
	pha
	jmp exsub3	;jump to address in foreign bank

gosmbnk:
	lda #>(main-1)	;store main as return address
	pha
	lda #<(main-1)
	pha
; goto routine continued
goto:
	lda pch
	sta t0+1	;store address to t0
	lda pcl
	sta t0
	lda acc		;load shadow regs
	ldx xr
	ldy yr
	jmp (t0)	;jump to address

!ifdef OPTI{
copyfnadr:
	tya		;length
	ldx i6509
	ldy #irom
	sty i6509	;switch to system bank

	ldy #fnlen
	sta (ptr),y
	lda txtptr	;fnadr low
	ldy #fnadr
	sta (ptr),y
	lda #>buf	;fnadr high
	iny
	sta (ptr),y
	lda e6509	;fnadr bank
	iny
	sta (ptr),y
	stx i6509	;restore ibank
	rts

getdirstat:
	pha
	txa
	pha
	tya
	pha
	ldx i6509	;remember ibank
	lda #irom
	sta i6509	;switch to system bank
	ldy #status
	lda (ptr),y	;get status
	sta status
	stx i6509	;restore ibank
	pla
	tay
	pla
	tax
	pla
	rts
}
!ifndef OPTI{
*= $efff
	!byte $ff
}