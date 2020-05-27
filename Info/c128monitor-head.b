	.subttl *** COPYRIGHT (C) 1985  by  COMMODORE BUSINESS MACHINES, INC. ***

;  ***************************************************************************
;  *                              //                                         *
;  *              CCCCCCC        //    111     2222222     8888888           *
;  *             CCC   CCC      //    1111    222   222   888   888          *
;  *            CCC            //      111         222    888   888          *
;  *            CCC           //       111       222        88888            *
;  *            CCC          //        111     222        888   888          *
;  *             CCC   CCC  //         111    222   222   888   888          *
;  *              CCCCCCC  //        1111111  222222222    8888888           *
;  *                      //                                                 *
;  *                                                                         *
;  * MMM   MMM    OOOOOOO    NNN    NN  III  TTTTTTTTT  OOOOOOO    RRRRRRRR  * 
;  * MMMM MMMM   OOO   OOO   NNNN   NN  III     TTT    OOO   OOO   RRR   RRR * 
;  * MMMMMMMMM  OOO     OOO  NNNNN  NN  III     TTT   OOO     OOO  RRR   RRR * 
;  * MMM M MMM  OOO     OOO  NNN NN NN  III     TTT   OOO     OOO  RRRRRRRR  * 
;  * MMM   MMM  OOO     OOO  NNN  NNNN  III     TTT   OOO     OOO  RRR RRR   * 
;  * MMM   MMM   OOO   OOO   NNN   NNN  III     TTT    OOO   OOO   RRR  RRR  * 
;  * MMM   MMM    OOOOOOO    NNN    NN  III     TTT     OOOOOOO    RRR   RRR * 
;  *									     * 
;  *									     * 
;  *        COPYRIGHT (C)1985  BY   COMMODORE BUSINESS MACHINES, INC.        *
;  *									     *
;  ***************************************************************************
  
  
  
;	****************************************************************** 
;	*                                                                * 
;	* THIS LISTING CONTAINS CONFIDENTIAL AND PROPRIETARY INFORMATION *
;	* OF CBM, INC.   REPRODUCTION,  DISSEMINATION  OR  DISCLOSURE TO *
;	* OTHERS WITHOUT EXPRESS WRITTEN PERMISSION IS PROHIBITED.  THIS *
;	* SOFTWARE IS INTENDED FOR USE IN  COMMODORE C/128 SYSTEMS ONLY. *
;	*                                                                * 
;	*  THE  INFORMATION  IN  THIS  DOCUMENT  IS  SUBJECT  TO CHANGE  * 
;	*                       WITHOUT NOTICE.                          * 
;	*                                                                * 
;	*  NO  RESPONSIBILITY  IS  ASSUMED  FOR THE RELIABILITY OF THIS  * 
;	*                          SOFTWARE.                             * 
;	*                                                                * 
;	****************************************************************** 


  
;.end

	.subttl  C/128 SYSTEM MEMORY MAPS  (11/07/86)
	.page
;				     COMPOSITE  SYSTEM  MEMORY  MAP
;
;	  C64 Cartridges	       C64		     C128 ROMs		 RAM Banks (0-3)
; $FFFF> _______________	 _______________	 _______________	 _______________
;	|		|	|		|	|		|	|		|
;	|     Game	|	|     KERNAL	|	|		|	|		|
;	|		|	|	&	|	|     KERNAL	|	|		|
;	|     Card	|	|     EDITOR	|	|		|	|		|
;	|		|	|		|	|		|	|		|
; $E000>|_______________|-------|_______________|-------|_______________|	|		|
;				| Color Nybbles,|	| Color Nyb,I/O,|	|		|
;				| I/O and CHARs	|	| CHARs & CP/M	|	|		|
; $D000>------------------------|_______________|-------|_______________|	|		|
;							|		|	|		|
;	 _______________	 _______________	|     EDITOR	|	|		|
; $C000>|		|-------|		|-------|_______________|-------|		|
;	|		|	|		|	|		|	|		|
;	|  Application	|	|		|	|    MONITOR	|	|		|
; $B000>|		|-------|     BASIC	|-------|_______________|	|		|
;	|   Card - HI	|	|		|	|		|	|		|
;	|		|	|		|	|		|	|	_	|
; $A000>|_______________|-------|_______________|	|		|	|      | SYSTEM	|
;	|		|				|     BASIC	|	| RAM0-| & BASIC|
;	|  Application	|				|		|	|      |_  TEXT	|
;	|		|				|       HI	|	|	_	|
;	|   Card - LO	|				|		|	|      | BASIC	|
;	|		|				|		|	| RAM1-|  VARs	|
; $8000>|_______________|-------------------------------|_______________|-------|      |_	|
;							|		|	|		|
;							|		|	|	_	|
;							|		|	| RAM2-| FUTURE |
;							|     BASIC	|	| RAM3-|_EXPAND |
;							|		|	|		|
;							|       LO	|	|		|
;							|		|	|		|
;							|		|	|		|
;							|		|	|		|
;							|		|	|		|
;							|		|	|		|
; $4000>------------------------------------------------|_______________|-------|		|
;										|		|
;										|		|
;										|		|
;										|		|
;										|		|
;										|		|
;										|		|
;										|		|
;										|		|
; $0400>------------------------------------------------------------------------|_______________|
;										|    COMMON	|
; $0000>------------------------------------------------------------------------|_______________|

	.page
; $0A00>|---------------| $1300>|---------------| $1C00>|---------------| $FFFF>|---------------|
;	|		|	|		|	|		| $FFFA>|  NMI RST IRQ  |
;	|		|	|     Basic	|	|		| $FFD0>| CP/M RAM Code	|
;	|		|	|    Absolute	|	|		|       | Krnl RAM Code |
;	|    Basic	|	|   Variables	|	|		| $FF05>|---------------|
;	|		|	|		|	|		|	|MMU Config Regs|
; $0900>|   Run-Time	| $1200>|---------------| $1B00>|		| $FF00>|---------------|
;	|		|	|     Basic	|	|		|	|		|
;	|    Stack	|	|   DOS / VSP	|	|		|	|		|
;	|		|	|   Variables	|	|		|	|		|
;	|		| $1108>|---------------|	|		|	|		|
;	|		|	|CP/M Reset Code|	|		|	|		|
; $0800>|---------------| $1100>|---------------| $1A00>|		|	|		|
;	|		|	|		|	|		|	|		|
;	|		|	|    Function	|	|		|	|		|
;	|		|	|      Key	|	|		|	|		|
;	|		|	|     Buffer	|	|		|	|		|
;	|      VIC	|	|		|	|		|	|		|
;	|		| $1000>|---------------| $1900>|		|	|		|
;	|     Text	|	|		|	|		|	|		|
;	~		~	|		|	|   Reserved	|	|     Basic	|
;	|    Screen	|	|		|	|		|	|		|
;	|		|	|    Sprite	|	|      for	|	|		|
;	|    (VM#1)	|	|		|	|		|	|		|
;	|		| $0F00>|   Definition	| $1800>| Applications	|	~     Text	~
;	|		|	|		|	|		|	|		|
;	|		|	|     Area	|	|   Software	|	|		|
;	|		|	|		|	|		|	|		|
;	|		|	|		|	|		|	|     Area	|
;	|		|	|		|	|		|	|		|
; $0400>|---------------| $0E00>|---------------| $1700>|		|	|		|
;	| Basic	RAM Code|	|		|	|		|	| (Basic Text	|
; $0380>| - - - - - - - |	|    RS-232	|	|		|	|   begins at	|
;	| Kernal Tables |	|    Output	|	|		|	|     $1C00	|
; $033C>| - - - - - - - |	|    Buffer	|	|		|	|  if Bit-Map	|
;	|   Indirects	|	|		|	|		|	|  unallocated)	|
; $02FC>|---------------| $0D00>|---------------| $1600>|		| $4000>|---------------|
;	|Kernal	RAM Code|	|		|	|		|	|		|
; $02A2>| - - - - - - - |	|    RS-232	|	|		|	|		|
;	|Basic & Monitor|	|    Input	|	|		|	|		|
;	|     Input	|	|    Buffer	|	|		|	|      VIC	|
;	|     Buffer	|	|		|	|		|	|		|
; $0200>|---------------| $0C00>|---------------| $1500>|		|	~    Bit-Map	~
;	| System Stack	|	|  (Disk Boot	|	|		|	|		|
; $0149>| - - - - - - - |	|     Page)	|	|		|	|     Screen	|
;	|Basic DOS,USING| $0BC0>| - - - - - - - |	|		|	|		|
; $0110>| - - - - - - - |	|   Cassette	|	|		|	|		|
;	|    FBUFFER	|	|    Buffer	|	|		|	|		|
; $0100>|---------------| $0B00>|---------------| $1400>|		| $2000>| - - - - - - - |
;	|		|	|   Monitor &	|	|		|	|		|
;	|  Kernal Z.P.	|	|    Kernal	|	|		|	|      VIC	|
; $0090>| - - - - - - - |	|   Absolute	|	|		|	~    Bit-Map	~
;	|  Basic  Z.P.	|	|   Variables	|	|		|	|     Color	|
; $0002>| _ _ _ _ _ _ _	|	|		|	|		|	|     (VM#2)	|
; $0000>|_______________| $0A00>|---------------| $1300>|---------------| $1C00>|---------------|


;			  	C/128  SYSTEM  ROM  LAYOUT
;
;	$FFFF _  _______________
;	$FF4D _	|_______________|__________ Kernal Jump Table & Hardware Vectors --------
;		|		|							|
;	$FF05 _ |_______________|__________ Kernal Interrupt Dispatch Code		|
;	$FF00 _	|///////////////|__________ MMU Configuration Registers			|
;		|		|							|
;		|		|							|
;	$FC80 _	|_______________|__________ ROM Reserved for Foreign Language Versions	|
;		|		|							|
;		|		|							|
;	$FA80 _	|_______________|__________ Editor Tables				|
;		|		|							|
;		|		|							|
;		/		/							-\  HIGH
;		|		|							-/  ROM
;	$E000 _ |_______________|__________ Kernal ROM Code				|
;		|///////////////|							|
;		|///////////////|							|
;		/////////////////							|
;		|///////////////|							|
;	$D000 _	|///////////////|__________ I/O Space or Character ROM			|
;		|		|							|
;		|		|							|
;		/		/							|
;		|		|							|
;	$C000 _ |_______________|__________ Editor ROM Code -----------------------------
;		|		|							|
;		|		|							|
;		/		/							|
;		|		|							|
;	$B000 _ |_______________|__________ Monitor ROM Code				-\  MID
;		|		|							-/  ROM
;		|		|							|
;		|		|							|
;		|		|							|
;		|		|							|
;	$8000 -	/		/ ------------------------------------------------------|
;		|		|							|
;		|		|							-\  LOW
;		|		|							-/  ROM
;		|		|							|
;	$4000 _	|_______________|---------- BASIC ROM Code -----------------------------|

	.page

;	         I/O  BLOCK  MAP		6526  CIA  REGISTER MAP  (typical)
;
;	$DFFF _  _______________		REG#	FUNCTION
;		|  (DMA Cntlr)	|      ________
;		|    I/O - 2	|     /		 F .... CRB   (control register B)
;	$DF00 _ |_______________|    /		 E .... CRA   (control register A)
;		|		|   /		 D .... ICR   (interrupt control register)
;		|    I/O - 1	|  /		 C .... SDR   (serial data register)
;	$DE00 _ |_______________|_/		 B .... TOD   (hours)
;		|		|		 A .... TOD   (minutes)
;		|    CIA - 2	|		 9 .... TOD   (seconds)
;	$DD00 _ |_______________|		 8 .... TOD   (tenths)
;		|		|		 7 .... TB-HI (timer B)
;		|    CIA - 1	|		 6 .... TB-LO
;	$DC00 _ |_______________|_		 5 .... TA-HI (timer A)
;		|		| \		 4 .... TA-LO
;		|      VIC	|  \		 3 .... DDRB  (data direction port B)
;		/     COLOR	/   \		 2 .... DDRA  (data direction port A)
;		|      RAM	|    \		 1 .... PRB   (port B)
;	$D800 _ |_______________|     \________	 0 .... PRA   (port A)
;		|		|
;		|  (RESERVED)	|
;	$D700 _ |_______________|
;		|		|
;		|   8563 VDC	| ..... (register map on following pages)
;	$D600 _ |_______________|
;		|		|
;		|      MMU	| ..... (register map on following pages)
;	$D500 _ |_______________|
;		|		|
;		|      SID	| ..... (register map on following pages)
;	$D400 _ |_______________|
;		|		|
;		|		|
;		/      VIC	/ ..... (register map on following pages)
;		|		|
;	$D000 _ |_______________|

	.page
;///////////////       T H E       C / 1 2 8       K E Y B O A R D       \\\\\\\\\\\\\\\

;	    -------------------------------------------------------------------     -------
;	    | C0  | C1  | C2  | C3  | C4  | C5  | C6  | C7  | K0  | K1  | K2  |     | GND |
;	    |PIN13|PIN19|PIN18|PIN17|PIN16|PIN15|PIN14|PIN20|PIN21|PIN22|PIN23|     |PIN-1|
;	    ---|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|---     ---|---
;	      \|/   \|/   \|/   \|/   \|/   \|/   \|/   \|/   \|/   \|/   \|/          |
; -------   -------------------------------------------------------------------        |
; | R0  |/__| INS |  #  |  %  |  '  |  )  |  +  | lb  |  !  | HELP| ESC | ALT |        |
; |PIN12|\  | DEL |  3  |  5  |  7  |  9  |     |     |  1  |     |     |     |        |
; -------   -------------------------------------------------------------------        |
; | R1  |/__| RET |  W  |  R  |  Y  |  I  |  P  |  *  | <-- |  8  |  +  |  0  |        |
; |PIN11|\  |     |     |     |     |     |     |     |     |     |     |     |        |
; -------   -------------------------------------------------------------------        |
; | R2  |/__| /_\ |  A  |  D  |  G  |  J  |  L  |  ]  | CTRL|  5  |  -  |  .  |        |
; |PIN10|\  | \ / |     |     |     |     |     |  ;  |     |     |     |     |        |
; -------   -------------------------------------------------------------------        |
; | R3  |/__| F8  |  $  |  &  |  (  |  0  |  -  | CLR |  "  | TAB | LINE| /|\ |        |
; |PIN-5|\  | F7  |  4  |  6  |  8  |     |     | HOM |  2  |     | FEED|  |  |        |
; -------   -------------------------------------------------------------------        |
; | R4  |/__| F2  |  Z  |  C  |  B  |  M  |  >  |RIGHT|SPACE|  2  |ENTER|  |  |        |
; |PIN-8|\  | F1  |     |     |     |     |  .  |SHIFT| BAR |     |     | \|/ |        |
; -------   -------------------------------------------------------------------        |
; | R5  |/__| F4  |  S  |  F  |  H  |  K  |  [  |  =  | C=  |  4  |  6  | /__ |        |
; |PIN-7|\  | F3  |     |     |     |     |  :  |     |     |     |     | \   |        |
; -------   -------------------------------------------------------------------        |
; | R6  |/__| F6  |  E  |  T  |  U  |  O  |  @  | pi  |  Q  |  7  |  9  | __\ |        |
; |PIN-6|\  | F5  |     |     |     |     |     |  ^  |     |     |     |   / |        |
; -------   -------------------------------------------------------------------        |
; | R7  |/__| /|\ |LEFT |  X  |  V  |  N  |  <  |  ?  | RUN |  1  |  3  |  NO |_____   |
; |PIN-9|\  | \|/ |SHIFT|     |     |     |  ,  |  /  | STOP|     |     | SCRL|    |   |
; -------   -------------------------------------------------------------------    |   |
;		     |								   |   |
;		  -------   /  (LOCKING)					   |   |
;		  |SHIFT|__o  o____________________________________________________|   |
;		  | LOCK|							       |
;		  -------							       |
; -------         -------   /							       |
; | NMI |/________|RESTR|__o  o________________________________________________________o
; |PIN-3|\        |     |							       |
; -------         -------							       |
; -------         -------   /  (LOCKING)                                               |
; |40/80|/________|40/80|__o  o________________________________________________________o
; |PIN24|\        |DSPLY|							       |
; -------         -------							       |
; -------         -------   /  (LOCKING)                                               |
; |P6510|/________|CAPS |__o  o________________________________________________________o
; |PIN25|\        |LOCK |
; -------         -------

;NOTES:	1/  THE 64 KEYS UNDER C0 THRU C7 ABOVE OCCUPY THE SAME MATRIX POSITION AS IN THE C/64,
;	    AS DOES THE 'RESTORE' KEY. INCLUDING THE 'SHIFT LOCK' KEY, THERE ARE 66 SUCH KEYS.
;	2/  THE 24 KEYS UNDER THE K0, K1 AND K2 OUTPUTS ARE PART  OF  THE  EXTENDED  KEYBOARD,
;	    AS ARE THE '40/80 DSPLY' AND 'CAPS LOCK' KEYS.  THERE ARE 26 NEW KEYS.  THEY  WILL
;	    NOT BE SCANNED IN C/64  MODE,  ALTHO THE USER MAY WRITE CODE TO SCAN THEM HIMSELF.
	.page
;			8722  MMU  REGISTER  MAP
;
;	REGISTER		DESCRIPTION
;				 _______
;	 11  (VR) ............. | 7 - 4 | .....	BANK Version
;	    Version Register	| 3 - 0 | .....	MMU  Version
;				|-------|
;				 _______               __
;	 10  (PAGE-1 HI) ...... | 7 - 4 | .....	???	 |
;	  8  (PAGE-0 HI)	| 3 | .........	A19	Swaps PAGE 0 and/or PAGE 1
;				| 2 | .........	A18	with any other page in the
;				| 1 | .........	A17	256K address space.
;				| 0 | ......... A16	 |
;				|---|			 |
;				 ___			 |
;	  9  (PAGE-1 LO) ......	| 7 | ......... A15	 |
;	  7  (PAGE-0 LO)	| 6 | ......... A14	 |
;				| 5 | ......... A13	 |
;				| 4 | ......... A12	 |
;				| 3 | ......... A11	 |
;				| 2 | ......... A10	 |
;				| 1 | ......... A9	 |
;				| 0 | ......... A8     __|
;				|---|
;				 ___
;	  6  (RCR)		| 7 | .........	VA17 \	VIC RAM Bank  (and DMA RAM Bank)
;				| 6 | .........	VA16 /
;	    RAM Configuration	| 5 | .........	\	RAM Block     (for future expansion)
;	      Register		| 4 | .........	/
;				| 3 | .........	\	SHARE Status  (none,bot,top,both)
;				| 2 | .........	/
;				| 1 | .........	\	SHARE Amount  (1k,4k,8k,16k)
;				| 0 | .........	/
;				|---|
;	  			 ___
;	  5  (MCR)		| 7 | .........	40/80 Key Sense
;				| 6 | .........	OS Mode      0 = 128,  1 = 64
;	   Mode Configuration	| 5 | .........	/EXROM line sense
;	     Register		| 4 | .........	/GAME  line sense
;				| 3 | .........	FSDIR        0 = IN,   1 = OUT
;				| 2 | .........	???
;				| 1 | .........	???
;				| 0 | .........	Processor:   0 = Z80,  1 = 85xx
;				|---|
;				 ___
;	  4  (PCR-D)		| 7 | .........	A17 \	RAM BANK (0-3, processor only)
;	  3  (PCR-C)		| 6 | .........	A16 /
;	  2  (PCR-B)		| 5 | .........	\	ROM-HI	(system,int,ext,RAM)
;	  1  (PCR-A)		| 4 | .........	/
;	  0  (CR)		| 3 | .........	\	ROM-MID	(system,int,ext,RAM)
;				| 2 | .........	/
;	    Pre-configuration	| 1 | .................	ROM-LO	(system,RAM)
;	      & Configuration	| 0 | ................. I/O	(I/O, else see ROM-HI)
;	          Registers	|---|
	.page
;		8726 DMA CONTROLLER REGISTER  MAP	(11/86)
;
;
; REG #	 _______________________________________________________
;  0_	| PEND |  EOB | FAULT| SIZE | VER3 | VER2 | VER1 | VER0 |______ Status Register
;  1_	| EXEC |  --  | LOAD | FF00 |  --  |  --  |    MODE     |______ Command Register
;  2_	|  A7  |  A6  |  A5  |  A4  |  A3  |  A2  |  A1  |  A0  |______ Host Address lo
;  3_	|  A15 |  A14 |  A13 |  A12 |  A11 |  A10 |  A9  |  A8  |______ Host Address hi
;  4_	| EX7  | EX6  | EX5  | EX4  | EX3  | EX2  | EX1  | EX0  |______ Expansion Address lo
;  5_	| EX15 | EX14 | EX13 | EX12 | EX11 | EX10 | EX9  | EX8  |______ Expansion Address hi
;  6_	|  --  |  --  |  --  |  --  |  --  | BA2  | BA1  | BA0  |______ Expansion Bank pointer
;  7_	|  L7  |  L6  |  L5  |  L4  |  L3  |  L2  |  L1  |  L0  |______ Transfer Length lo
;  8_	|  L15 |  L14 |  L13 |  L12 |  L11 |  L10 |  L9  |  L8  |______ Transfer Length hi
;  9_	| ENBL | IEOB |  VFY |  --  |  --  |  --  |  --  |  --  |______ Interrupt Mask
; 10_   |  AC1 |  AC0 |  --  |  --  |  --  |  --  |  --  |  --  |______ Address Control
;       |______|______|______|______|______|______|______|______|



;	MODE =>		00: Transfer FROM internal  TO  external
;			01: Transfer FROM external  TO  internal
;			10: Swap
;			11: Verify
;
;       SIZE =>         0:  128KB
;                       1:  512KB (or 256KB if 1764 type card)
;
;       Adr Contl =>    00: Increment both host & expansion addresses
;                       01: Fix expansion address
;                       10: Fix host address
;                       11: Fix both host & expansion addresses
;
;
;       NOTE:  Bits 7-5 of STATUS register are CLEARED when this register is READ



;	The  DMA  CONTROLLER  appears in the C/128 I/O memory map at IO2  ($DF00).

	.page
;			8564  VIC  REGISTER  MAP
;
; REG #	 _______________________________________________________
;  0_	| S0x7 | S0x6 | S0x5 | S0x4 | S0x3 | S0x2 | S0x1 | S0x0 |______ Sprite 0  X location
;  1_	|______|______|______|______|______|______|______|______|______ Sprite 0  Y location
;  2_	|      |      |      |      |      |      |      |      |______ Sprite 1  X location
;  3_	|______|______|______|______|______|______|______|______|______ Sprite 1  Y location
;  4_	|      |      |      |      |      |      |      |      |______ Sprite 2  X location
;  5_	|______|______|______|______|______|______|______|______|______ Sprite 2  Y location
;  6_	|      |      |      |      |      |      |      |      |______ Sprite 3  X location
;  7_	|______|______|______|______|______|______|______|______|______ Sprite 3  Y location
;  8_	|      |      |      |      |      |      |      |      |______ Sprite 4  X location
;  9_	|______|______|______|______|______|______|______|______|______ Sprite 4  Y location
; 10_	|      |      |      |      |      |      |      |      |______ Sprite 5  X location
; 11_	|______|______|______|______|______|______|______|______|______ Sprite 5  Y location
; 12_	|      |      |      |      |      |      |      |      |______ Sprite 6  X location
; 13_	|______|______|______|______|______|______|______|______|______ Sprite 6  Y location
; 14_	|      |      |      |      |      |      |      |      |______ Sprite 7  X location
; 15_	|______|______|______|______|______|______|______|______|______ Sprite 7  Y location
; 16_	| S7x8 | S6x8 | S5x8 | S4x8 | S3x8 | S2x8 | S1x8 | S0x8 |______ MSB of Sprite X loctn
; 17_	|  RC8 |  ECM |  BMM | Blank| ROWS |  Y2  |  Y1  |  Y0  |______ mode, #row, y-scroll
; 18_	|  RC7 |  RC6 |  RC5 |  RC4 |  RC3 |  RC2 |  RC1 |  RC0 |______ Raster latch
; 19_	| LPx8 | LPx7 | LPx6 | LPx5 | LPx4 | LPx3 | LPx2 | LPx1 |______ Lightpen X latch
; 20_	| LPy7 | LPy6 | LPy5 | LPy4 | LPy3 | LPy2 | LPy1 | LPy0 |______ Lightpen Y latch
; 21_	|  SD7 |  SD6 |  SD5 |  SD4 |  SD3 |  SD2 |  SD1 |  SD0 |______ Sprite Disable
; 22_	|  --  |  --  | Reset|  MCM | COLS |  X2  |  X1  |  X0  |______ mode, #col, x-scroll
; 23_	| S7Ey | S6Ey | S5Ey | S4Ey | S3Ey | S2Ey | S1Ey | S0Ey |______ Sprite Y expand
; 24_	| VM13 | VM12 | VM11 | VM10 | CB13 | CB12 | CB11 |  --  |______ VM Base, CHR Base
; 25_	|  IRQ |  --  |  --  |  --  |  LP  |  S/S |  S/B | RIRQ |______ IRQ source (wrt to clr)
; 26_	|  --  |  --  |  --  |  --  | _LP  | _S/S | _S/B | _IRQ |______ IRQ enable (0=disable)
; 27_	| BSP7 | BSP6 | BSP5 | BSP4 | BSP3 | BSP2 | BSP1 | BSP0 |______ Sprite/Bgnd Priority
; 28_	| MCS7 | MCS6 | MCS5 | MCS4 | MCS3 | MCS2 | MCS1 | MCS0 |______ Multicolor Sprite Sel
; 29_	| S7Ex | S6Ex | S5Ex | S4Ex | S3Ex | S2Ex | S1Ex | S0Ex |______ Sprite X expand
; 30_	|  SS7 |  SS6 |  SS5 |  SS4 |  SS3 |  SS2 |  SS1 |  SS0 |______ S/S Collision latch
; 31_	|  SB7 |  SB6 |  SB5 |  SB4 |  SB3 |  SB2 |  SB1 |  SB0 |______ S/B Collision latch
; 32_	|  --  |  --  |  --  |  --  |      |      |      |      |______ Border Color
; 33_	|  --  |  --  |  --  |  --  |      |      |      |      |______ Background Color #0
; 34_	|  --  |  --  |  --  |  --  |      |      |      |      |______ Background Color #1
; 35_	|  --  |  --  |  --  |  --  |      |      |      |      |______ Background Color #2
; 36_	|  --  |  --  |  --  |  --  |      |      |      |      |______ Background Color #3
; 37_	|  --  |  --  |  --  |  --  |      |      |      |      |______ Sprite Multicolor #0
; 38_	|  --  |  --  |  --  |  --  |      |      |      |      |______ Sprite Multicolor #1
; 39_	|  --  |  --  |  --  |  --  |      |      |      |      |______ Sprite 0 Color
; 40_	|  --  |  --  |  --  |  --  |      |      |      |      |______ Sprite 1 Color
; 41_	|  --  |  --  |  --  |  --  |      |      |      |      |______ Sprite 2 Color
; 42_	|  --  |  --  |  --  |  --  |      |      |      |      |______ Sprite 3 Color
; 43_	|  --  |  --  |  --  |  --  |      |      |      |      |______ Sprite 4 Color
; 44_	|  --  |  --  |  --  |  --  |      |      |      |      |______ Sprite 5 Color
; 45_	|  --  |  --  |  --  |  --  |      |      |      |      |______ Sprite 6 Color
; 46_	|  --  |  --  |  --  |  --  |      |      |      |      |______ Sprite 7 Color
; 47_	|  --  |  --  |  --  |  --  |  --  |  K2  |  K1  |  K0  |______ Keyboard lines (c128 keypad)
; 48_	|  --  |  --  |  --  |  --  |  --  |  --  | TEST | 2MHz |______ Clock speed
;       |-------------------------------------------------------|
	.page
;			6581  SID  REGISTER  MAP
;
; REG #	 _______________________________________________________
;  0_	|  F7  |  F6  |  F5  |  F4  |  F3  |  F2  |  F1  |  F0  |______ Frequency lo    Voice-1
;  1_	|  F15 |  F14 |  F13 |  F12 |  F11 |  F10 |  F9  |  F8  |______ Frequency hi
;  2_	|  PW7 |  PW6 |  PW5 |  PW4 |  PW3 |  PW2 |  PW1 |  PW0 |______ Pulse Width lo
;  3_	|  --  |  --  |  --  |  --  | PW11 | PW10 |  PW9 |  PW8 |______ Pulse Width hi
;  4_	| NOISE| PULSE|  SAW |  TRI | TEST | RING | SYNC | GATE |______ Control Register
;  5_	| ATK3 | ATK2 | ATK1 | ATK0 | DCY3 | DCY2 | DCY1 | DCY0 |______ Attack/Decay
;  6_	| STN3 | STN2 | STN1 | STN0 | RLS3 | RLS2 | RLS1 | RLS0 |______ Sustain/Release
;	|------|------|------|------|------|------|------|------|
;  7_	|  F7  |  F6  |  F5  |  F4  |  F3  |  F2  |  F1  |  F0  |______ Frequency lo    Voice-2
;  8_	|  F15 |  F14 |  F13 |  F12 |  F11 |  F10 |  F9  |  F8  |______ Frequency hi
;  9_	|  PW7 |  PW6 |  PW5 |  PW4 |  PW3 |  PW2 |  PW1 |  PW0 |______ Pulse Width lo
; 10_	|  --  |  --  |  --  |  --  | PW11 | PW10 |  PW9 |  PW8 |______ Pulse Width hi
; 11_	| NOISE| PULSE|  SAW |  TRI | TEST | RING | SYNC | GATE |______ Control Register
; 12_	| ATK3 | ATK2 | ATK1 | ATK0 | DCY3 | DCY2 | DCY1 | DCY0 |______ Attack/Decay
; 13_	| STN3 | STN2 | STN1 | STN0 | RLS3 | RLS2 | RLS1 | RLS0 |______ Sustain/Release
;	|------|------|------|------|------|------|------|------|
; 14_	|  F7  |  F6  |  F5  |  F4  |  F3  |  F2  |  F1  |  F0  |______ Frequency lo    Voice-3
; 15_	|  F15 |  F14 |  F13 |  F12 |  F11 |  F10 |  F9  |  F8  |______ Frequency hi
; 16_	|  PW7 |  PW6 |  PW5 |  PW4 |  PW3 |  PW2 |  PW1 |  PW0 |______ Pulse Width lo
; 17_	|  --  |  --  |  --  |  --  | PW11 | PW10 |  PW9 |  PW8 |______ Pulse Width hi
; 18_	| NOISE| PULSE|  SAW |  TRI | TEST | RING | SYNC | GATE |______ Control Register
; 19_	| ATK3 | ATK2 | ATK1 | ATK0 | DCY3 | DCY2 | DCY1 | DCY0 |______ Attack/Decay
; 20_	| STN3 | STN2 | STN1 | STN0 | RLS3 | RLS2 | RLS1 | RLS0 |______ Sustain/Release
;	|------|------|------|------|------|------|------|------|
; 21_	|  --  |  --  |  --  |  --  |  --  |  FC2 |  FC1 |  FC0 |______ Frequency lo    Filter
; 22_	| FC10 |  FC9 |  FC8 |  FC7 |  FC6 |  FC5 |  FC4 |  FC3 |______ Frequency hi
; 23_	| RES3 | RES2 | RES1 | RES0 |FILTEX| FILT3| FILT2| FILT0|______ Resonance/Filter
; 24_	| 3 OFF|  HP  |  BP  |  LP  | VOL3 | VOL2 | VOL1 | VOL0 |______ Mode/Volume
;	|------|------|------|------|------|------|------|------|
; 25_	|  PX7 |  PX6 |  PX5 |  PX4 |  PX3 |  PX2 |  PX1 |  PX0 |______ Pot X           Misc.
; 26_	|  PY7 |  PY6 |  PY5 |  PY4 |  PY3 |  PY2 |  PY1 |  PY0 |______ Pot Y
; 27_	|  O7  |  O6  |  O5  |  O4  |  O3  |  O2  |  O1  |  O0  |______ Oscillator 3
; 28_	|  E7  |  E6  |  E5  |  E4  |  E3  |  E2  |  E1  |  E0  |______ Envelope 3
;	|-------------------------------------------------------|
	.page
;		8563  VDC  REGISTER  MAP	(also 8568,  10/85)
;
; REG #	 _______________________________________________________
;  0_	|  HT7 |  HT6 |  HT5 |  HT4 |  HT3 |  HT2 |  HT1 |  HT0 |______ Horizontal Total
;  1_	|  HD7 |  HD6 |  HD5 |  HD4 |  HD3 |  HD2 |  HD1 |  HD0 |______ Horizontal Displayed
;  2_	|  HP7 |  HP6 |  HP5 |  HP4 |  HP3 |  HP2 |  HP1 |  HP0 |______ Horizontal Sync Position
;  3_	|  VW3 |  VW2 |  VW1 |  VW0 |  HW3 |  HW2 |  HW1 |  HW0 |______ Vert/Horz Sync Width
;  4_	|  VT7 |  VT6 |  VT5 |  VT4 |  VT3 |  VT2 |  VT1 |  VT0 |______ Vertical Total
;  5_	|  --  |  --  |  --  |  VA4 |  VA3 |  VA2 |  VA1 |  VA0 |______ Vertical Total Adjust
;  6_	|  VD7 |  VD6 |  VD5 |  VD4 |  VD3 |  VD2 |  VD1 |  VD0 |______ Vertical Displayed
;  7_	|  VP7 |  VP6 |  VP5 |  VP4 |  VP3 |  VP2 |  VP1 |  VP0 |______ Vertical Sync Position
;  8_	|  --  |  --  |  --  |  --  |  --  |  --  |  IM1 |  IM0 |______ Interlace Mode
;  9_	|  --  |  --  |  --  | CTV4 | CTV3 | CTV2 | CTV1 | CTV0 |______ Character Total Vertical
; 10_	|  --  |  CM1 |  CM0 |  CS4 |  CS3 |  CS2 |  CS1 |  CS0 |______ Cursor Mode, Start Scan
; 11_	|  --  |  --  |  --  |  CE4 |  CE3 |  CE2 |  CE1 |  CE0 |______ Cursor End Scan Line
; 12_	| DS15 | DS14 | DS13 | DS12 | DS11 | DS10 |  DS9 |  DS8 |______ Display Start Address hi
; 13_	|  DS7 |  DS6 |  DS5 |  DS4 |  DS3 |  DS2 |  DS1 |  DS0 |______ Display Start Address lo
; 14_	| CP15 | CP14 | CP13 | CP12 | CP11 | CP10 |  CP9 |  CP8 |______ Cursor Position hi
; 15_	|  CP7 |  CP6 |  CP5 |  CP4 |  CP3 |  CP2 |  CP1 |  CP0 |______ Cursor Position lo
; 16_	| LPV7 | LPV6 | LPV5 | LPV4 | LPV3 | LPV2 | LPV1 | LPV0 |______ Light Pen Vertical
; 17_	| LPH7 | LPH6 | LPH5 | LPH4 | LPH3 | LPH2 | LPH1 | LPH0 |______ Light Pen Horizontal
; 18_	| UA15 | UA14 | UA13 | UA12 | UA11 | UA10 |  UA9 |  UA8 |______ Update Address hi
; 19_	|  UA7 |  UA6 |  UA5 |  UA4 |  UA3 |  UA2 |  UA1 |  UA0 |______ Update Address lo
; 20_	| AA15 | AA14 | AA13 | AA12 | AA11 | AA10 |  AA9 |  AA8 |______ Attribute Start Adr hi
; 21_	|  AA7 |  AA6 |  AA5 |  AA4 |  AA3 |  AA2 |  AA1 |  AA0 |______ Attribute Start Adr lo
; 22_	| CTH3 | CTH2 | CTH1 | CTH0 | CDH3 | CDH2 | CDH1 | CDH0 |______ Character Tot(h), Dsp(v)
; 23_	|  --  |  --  |  --  | CDV4 | CDV3 | CDV2 | CDV1 | CDV0 |______ Character Dsp(v)
; 24_	| COPY |  RVS |CBRATE| VSS4 | VSS3 | VSS2 | VSS1 | VSS0 |______ Vertical smooth scroll
; 25_	| TEXT |  ATR | SEMI |  DBL | HSS3 | HSS2 | HSS1 | HSS0 |______ Horizontal smooth scroll
; 26_	|  FG3 |  FG2 |  FG1 |  FG0 |  BG3 |  BG2 |  BG1 |  BG0 |______ Foregnd/Bgnd Color
; 27_	|  AI7 |  AI6 |  AI5 |  AI4 |  AI3 |  AI2 |  AI1 |  AI0 |______ Address Increment / Row
; 28_	| CB15 | CB14 | CB13 |  RAM |  --  |  --  |  --  |  --  |______ Character Base Address
; 29_	|  --  |  --  |  --  |  UL4 |  UL3 |  UL2 |  UL1 |  UL0 |______ Underline scan line
; 30_	|  WC7 |  WC6 |  WC5 |  WC4 |  WC3 |  WC2 |  WC1 |  WC0 |______ Word Count
; 31_	|  DA7 |  DA6 |  DA5 |  DA4 |  DA3 |  DA2 |  DA1 |  DA0 |______ Data
; 32_	| BA15 | BA14 | BA13 | BA12 | BA11 | BA10 |  BA9 |  BA8 |______ Block Start Address hi
; 33_	|  BA7 |  BA6 |  BA5 |  BA4 |  BA3 |  BA2 |  BA1 |  BA0 |______ Block Start Address lo
; 34_	| DEB7 | DEB6 | DEB5 | DEB4 | DEB3 | DEB2 | DEB1 | DEB0 |______ Display Enable Begin
; 35_	| DEE7 | DEE6 | DEE5 | DEE4 | DEE3 | DEE2 | DEE1 | DEE0 |______ Display Enable End
; 36_	|  --  |  --  |  --  |  --  | DRR3 | DRR2 | DRR1 | DRR0 |______ DRAM Refresh rate
; 37_	| HSYNC| VSYNC|  --  |  --  |  --  |  --  |  --  |  --  |______ Horz, Vert Sync Polarity
;	|-------------------------------------------------------|
;
;	(Register 37 appears on device 8568 only)
;
;	Description of MAPPED registers:
;					 _______________________________________________________
; $D600 ->	address (write):	|  --  |  --  |  R5  |  R4  |  R3  |  R2  |  R1  |  R0  |
;		status  (read) :	|STATUS|  LP  |VBLANK|  --  |  --  | VER2 | VER1 | VER0 |
;					|      |      |      |      |      |      |      |      |
; $D601 ->	data	(r/w)  :	|  D7  |  D6  |  D5  |  D4  |  D3  |  D3  |  D2  |  D0  |
;					|-------------------------------------------------------|
;					 _______________________________________________________
; ATTRIBUTE byte layout			|  ALT |  RVS |  UL  | FLASH|   R  |   G  |   B  |   I  |
;					|-------------------------------------------------------|
	.page
;	EXPLANATION OF VARIOUS KERNAL/EDITOR FLAG BYTES, ETC.
;
;	Symbol			       Description
;		    7      6      5      4      3      2      1      0
;		_________________________________________________________
;	D6510	|  --  | (in) | (out)| (in) | (out)| (out)| (out)| (out)|
;		|      |      |      |      |      |      |      |      |
;	R6510	|  --  |CAPKEY|CASMTR|CASSEN|CASWRT|CHAREN|VICNYB| uPNYB|
;		|      |      |      |      |      |      |      |      |
;	LOCKS	| CASE | CTL-S|  --  |  --  |  --  |  --  |  --  |  --  |
;		|      |      |      |      |      |      |      |      |
;	SCROLL	|  OFF |LINKER|      |      |      |      |      |      |
;		|      |      |      |      |      |      |      |      |
;	SHFLAG	|  --  |  --  |  --  |  ALT | ALPHA| CTRL |  C=  | SHIFT|
;		|      |      |      |      |      |      |      |      |
;	RPTFLG	|  ALL | NONE |  --  |  --  |  --  |  --  |  --  |  --  |
;		|      |      |      |      |      |      |      |      |
;	BLNON	|  ON  | BLNK |  --  |  --  |  --  |  --  |  --  |  --  |
;		|      |      |      |      |      |      |      |      |
;	BEEPER	|  ON  |  --  |  --  |  --  |  --  |  --  |  --  |  --  |
;		|      |      |      |      |      |      |      |      |
;	GRAPHM	|  MCM | SPLIT|  BMM |  --  |  --  |  --  |  --  |  --  |
;		|      |      |      |      |      |      |      |      |
;	MODE	| 40/80|  --  |  --  |  --  |  --  |  --  |  --  |  --  |
;		|      |      |      |      |      |      |      |      |
;	INIT_	|CHRSET| CINT |  --  |  --  |  --  |  --  |  --  | BASIC|
;	 STATUS	|-------------------------------------------------------|

;	The following SYMBOLS are used by the c/128 EDITOR. Note that the
;	EDITOR irq VIC screen handler depends upon them. In most cases the
;	contents of these locations will be placed directly into the appropriate
;	register and should be used by the user instead of the actual register.

;	GRAPHM ....... See above. If = $FF then editor leaves VIC alone.
;	CHAREN ....... Mask for R6510 /charen bit.
;	VM1 .......... VIC text mode Video Matrix & Character Base pointer.
;	VM2 .......... VIC graphic mode Video Matrix & Bit Map pointer.
;	VM3 .......... 8563 text display base address.
;	VM4 .......... 8563 attribute base address.
;	SPLIT ........ If split screen mode, contains value for 'middle' raster IRQ.
;	CURMOD ....... 8563 cursor mode.
;	PAUSE ........ Control-S in effect flag. Is = $13 if so.

;	Some insight into KERNAL symbols:

;	INIT_STATUS .. See also above. Lets system know what has been initialized
;			and what hasn't. Set to $00 by a reset but untouched by NMI.
;	SYSTEM_VECTOR. Where the kernal goes when it has to go somewhere. It's set
;			to BASIC COLD at reset. BASIC itself sets it to BASIC WARM
;			after it has inited. The MONITOR respects it too.
;	SYSTEM ....... Vector in RAM1 at $FFF8. Set at powerup to C128MODE, user may
;			redirect it to his code. Taken at RESET always, providing
;			user with control (protection) from reset.

	.page
	.subttl  RELEASE NOTES:   BETA-5   03/05/85

; THE FOLLOWING MODIFICATIONS HAVE BEEN MADE TO THE BETA-4 CODE TO CREATE THE
; BETA-5 VERSION RELEASED ON 03/05/85. THE BETA-5 RELEASE IS A PRE-PRODUCTION
; VERSION INTENDED FOR SYSTEM TEST AND EVALUATION AND PRELIMINARY APPLICATION
; SOFTWARE DEVELOPMENT ONLY.
;
;
; The following changes have been made to the MONITOR:
;
;	1/	The MONITOR now accepts binary, octal, decimal and hexidecimal
;		values for any numeric field. This was accomplished by totally
;		re-coding PARSE and portions of ASSEM, and installing a new
;		routine called EVAL. Numbers prefixed by one of the characters
;		$ + & % are interpreted by EVAL as base 16, 10, 8, or 2 values
;		respectively. In the absence of a prefix, the base defaults to
;		hexidecimal always. ASSEM will use the zero-page form wherever
;		possible unless the value is preceeded by extra zeros to force
;		the absolute form (except binary notation).
;
;	2/	The MONITOR now performs limited number conversion.  Additions
;		were made to MAIN1 and CMDCHR and a new routine called CONVERT
;		was installed to handle the conversions. Any of the characters
;		$ + & % entered as a command and prefixing a numeric value are
;		PARSEd (see #1 above) and the hexidecimal value printed.  Full
;		conversion between bases may be added in a later release.
;
;
;
; The following changes have been made to the EDITOR:
;
;	1/	PLOT (SET)  now performs  range checking.  The cursor position
;		sought must lie within the currently defined window. If it does,
;		the position is changed and .C=0 returned, else NO CHANGE made
;		and .C=1 returned.
;
;	2/	PLOT (READ) now returns the current cursor position within the
;		current window  (with respect to the window's origin)  and NOT
;		necessarily the screen origin.
;
;	3/	SCRORG was changed in a previous release to return the current
;		window size in .X and .Y and NOT the screen size.  SCRORG will
;		now return the maximum screen width (39 or 79) as well in .A.
;
;	4/	The entire data file, ED7.SRC, has been relocated to  $FA00 to
;		allow a reserved area in ROM for foreign language systems. The
;		reserved area shall be $FC80 thru $FEFF. This affects the MODE
;		tables, COLor tables, BITS, local variable init table, and all
;		PKY function key tables.
;
;	5/	The default screen colors for the  80-column  screen have been
;		changed to BLACK background and CYAN characters. This was done
;		to make a more presentable screen for systems using monochrome
;		monitors and allow a full pallet of colors without  background
;		interference for systems using RGBI monitors.
;
;
;
; The following changes have been made to the KERNAL:
;
;	1/	IOINIT now initializes the 8563 chip from an updated table now
;		that the tower has been removed. Also changed were the default
;		screen colors (see #5 above) and adjustments for PAL systems.
;
;	2/	IOINIT now has a delay loop of approximately 300ms incorporated
;		into it to allow older versions of the VIC-1541 disk drives to
;		finish their reset sequence. The system previously hung in the
;		auto-boot routine if in 40-column mode with old 1541's.
;
	.page
	.subttl  RELEASE NOTES:   BETA-6   03/20/85

; THE FOLLOWING MODIFICATIONS HAVE BEEN MADE TO THE BETA-5 CODE TO CREATE THE
; BETA-6 VERSION RELEASED ON 03/20/85. THE BETA-6 RELEASE IS A PRE-PRODUCTION
; VERSION INTENDED FOR SYSTEM TEST AND EVALUATION AND PRELIMINARY APPLICATION
; SOFTWARE DEVELOPMENT ONLY. NOTE THE CHANGE TO THE CHARACTER ROM ALSO.
;
;
;
; The following changes have been made to the MONITOR:
;
;	1/	Number conversion from/to all bases (hex,dec,oct,bin) fully
;		functional. Display format different from beta-5 also.
;
;	2/	The monitor DOS parser now provides DIRECTORY display using
;		the syntax:  @[unit#],$[drive:filespec]
;
;
;
; The following changes have been made to the EDITOR:
;
;	1/	The KEYSCAN routine was corrected to properly leave the COLM
;		driver port energizing only PB7. (Previously if no keys were
;		down all lines were left energized.) Also the scan of the
;		CAPS LOCK key was moved to the beginning of the KEYSCAN loop
;		where it will always be scanned and SHFLAG correctly updated.
;		(Previously if no matrix keys were down it was not scanned.)
;
;	2/	The CLRLIN and MOVLIN routines have had all the 'kludge' code
;		removed as it is no longer necessary using 8563R7 parts. Two
;		absolute variables consequently have disappeared, SAV80C & D.
;
;	3/	The various tables used by the screen editor have been shifted
;		around to allow better utilization of free ROM. The MODEx tables
;		now start at $FA80. All other tables now float above editor code.
;
;
;
; The following changes have been made to the KERNAL:
;
;	1/	The interrupt handlers now clear decimal mode (via CLD). This
;		is done AFTER the indirect jumps are made. The correct mode is
;		restored upon return from the interrupt code as usual.
;
;	2/	The SERIAL & TAPE routines now work at either 1 or 2MHz speed.
;		This was accomplished by replacing 'SEI' and 'CLI' opcodes in
;		time sensitive routines with JSR's to new code which saves the
;		current states & forces sprites off & 1MHz speed, and then re-
;		stores them all when through. This required minor changes to the
;		TAPE routines, but the serial ACPTR routine was totally re-coded.
;		Several absolute variables to store the states have been added
;		as well as one called HOLD_OFF which, if B7 is set, will cause
;		these new SEI/CLI routines to be skipped & require the user to
;		assume full responsibility for controlling system speed & sprites
;		during tape or serial I/O operations.
;
;	3/	The space key now correctly skips the current 'found' file during
;		TAPE loads. (The C= key or <no> action will initiate program load.)
;
;	4/	The CLOSE routine now saves the CARRY status in SVXT and not VERCK.
;		This corrects a conflict with the LOAD routine's use of VERCK.
;
;	5/	The BURST routine now correctly reports a 'device not present'. It
;		previously reported 'illegal device number'.
;
;	6/	Improved TKSA (serial routine) turnaround by putting DEBPIA inline.
;
;	7/	The OPEN routine now JSR's to OPENI for serial operations. This
;		corrects a problem that occurred when the device was not present.
;		OPENI pops the return address and JMPs to ERROR5 at such time.
;
;	8/	The INSTALL routine now downloads to RAM the correct code for the
;		DMA Controller. Previously the wrong subroutine was downloaded.
;		Also, a NOP was added to the RAM code after the DMA request.
;
;
;
; The following changes have been made to the CHARACTER ROM:
;
;	1/	Part number 315010-02 has a minor error in the font definition
;		for the lower case reverse field 'v'. This has been corrected in
;		part number 315010-03.
;
;	2/	Part number 315010-03 also includes a modification to the lower
;		case 'm' character (both normal & reverse fields) due to several
;		complaints about its appearance.
;
	.page
	.subttl  RELEASE NOTES:   BETA-7   04/08/85

; THE FOLLOWING MODIFICATIONS HAVE BEEN MADE TO THE BETA-6 CODE TO CREATE THE
; BETA-7 VERSION RELEASED ON 04/08/85. THE BETA-7 RELEASE IS A PRE-PRODUCTION
; VERSION INTENDED FOR SYSTEM TEST AND EVALUATION AND PRELIMINARY APPLICATION
; SOFTWARE DEVELOPMENT ONLY. NOTE THE CHANGE TO THE CHARACTER ROM ALSO.
;
;
;
; The following changes have been made to the MONITOR:
;
;	1/	Previously the HUNT command treated <colon> and <question> in
;		ASCII strings as terminators, hence they could not be  hunted
;		for. This has been corrected.
;
; The following changes have been made to the KERNAL:
;
;	1/	Previously the GO64 routine redirected the soft  RESET vector
;		back to itself thus providing a degree of protection to  C/64
;		software as the system had to be powered down to  escape C/64
;		mode. Unfortunately DRAM devices are not as  volatile as they
;		used to be and many systems required as much as 30 seconds to
;		elapse before they would power up into C/128 mode.  The RESET
;		vector is still present but the GO64 code no longer redirects
;		it. Applications using RESET should note the problem and pro-
;		vide a friendly escape mechanism after protecting themselves.
;
;	2/	Previously it was possible to enter C64 mode running at 2MHz.
;		This has been corrected. Note it is possible from C64 mode to
;		access the 1-2MHz bit (VIC register 48) to speed  up  certain
;		operations if so desired.
;
;	3/	Several minor changes to SERIAL code.  Some inaccessable code
;		was removed from TALK/LISTEN routine.  TKSA now errors out if
;		the SA could not be sent.
;
;	4/	Previously the BOOT routine returned  without  reporting  any
;		errors.  BOOT now returns errors in the same manner as LOADs,
;		with the carry set and error code in the accumulator.
;
;	5/	Previously the BURST load mechanism caused open disk channels
;		to be flushed. This has been corrected and now preserves open
;		user channels as the C/64 LOAD had  (e.g. chaining programs).
;		Note however that  BOOT loads will cause all open  files  for
;		the given device to be closed (see CLOSE_ALL note below).
;
;	6/	BURST must  UNLISTEN the  1571 when a break occurs (STOP key)
;		to signal the drive to  abort the  operation.  Previously the
;		drive stayed in its burst routine waiting to  send  the  next
;		block of data. This fix will prompt the drive to abort too.
;
;	7/	The DMA controller routine (for RAMDISK) has been modified to
;		meet the latest RAMDISK specification.
;
;	8/	A new Kernal JUMP TABLE entry has been created for an all new
;		Kernal subroutine, CLOSE_ALL.   Unlike  CLALL,  which  simply
;		cleared the LAT, FAT, SAT tables and restored the default I/O
;		devices without properly closing files, CLOSE_ALL is passed a
;		device number (fa) and properly closes ALL open files on that
;		device. If that device was the current I/O device,  only then
;		will CLOSE_ALL restore the  appropriate default device.  This
;		new jump vector is located at $FF4A.
;
;	9/	The Kernal IOINIT routine will recognize 8563-R8 & initialize
;		register #25 (horz. smooth scroll) to $47 per 8563 spec mods.
;		Previously this register was  initialized to  $40.  Note that
;		the lower 3 bits of the 8563 status register will return  the
;		revision level of the 8563 part (0=R7, 1=R8, etc.)
;
;
; The following changes have been made to the CHARACTER ROM:
;
;	1/	The hardware now supports an 8K byte character ROM. The lower
;		4K contains the C/64 character set  (P/N 901225-01),  and the
;		upper 4K contains the  C/128  character set  (P/N 315010-03).
;		In domestic systems the switching mechanism  is  the  /64-128
;		MMU signal and is therefore transparent to the user.  It  has
;		been proposed that international units replace one of the  4K
;		character sets with their particular national  character  set
;		and utilize the CAPS_LOCK key to manually switch sets.
	.page
	.subttl  RELEASE NOTES:   BETA-8   04/15/85

; THE FOLLOWING MODIFICATIONS HAVE BEEN MADE TO THE BETA-7 CODE TO CREATE THE
; BETA-8 VERSION RELEASED ON 04/15/85. THE BETA-8 RELEASE IS A PRE-PRODUCTION
; VERSION INTENDED FOR SYSTEM TEST AND EVALUATION AND PRELIMINARY APPLICATION
; SOFTWARE DEVELOPMENT ONLY.
;
;	**************************************************************
;	* PENDING SYSTEM TEST, THIS SHALL BE THE FINAL BETA RELEASE. *
;	* ANY  PROBLEMS  OR  NEEDS  *MUST*  BE  REPORTED  TO  SYSTEM *
;	* ENGINEERING BEFORE  04/19/85  FOR RESOLUTION AND INCLUSION *
;	* INTO THE PRODUCTION ROM RELEASE!			     *
;	**************************************************************
; 
;
;
;
; The following changes have been made to the MONITOR:
;
;	1/	The  Monitor DOS interface  now  uses  the  reserved  logical
;		channel number 0, and no longer transmits a 'close' to the
;		disk. Previously it used channel 1 (which was often in use by
;		the user) and performed a full 'close' (which caused the disk
;		to close ALL open files).
;
;	2/	The  Monitor DOS interface no longer allows  access  to  unit
;		numbers 3 and 31. They are illegal serial bus devices.
;
;	3/	The  Monitor MEMORY DUMP facility correctly displays the full
;		CBM ASCII character set.  Previously the  ASCII was masked to
;		7 bits.
;
; The following changes have been made to the KERNAL:
;
;	1/	The  Kernal BOOT routine no longer transmits a 'close' to the
;		disk. The disk status will now reflect the last I/O operation
;		status similar to a 'load' operation.  A reduntant attempt to
;		transmit the disk warm start (UI) command was removed.
;
;	2/	The Kernal GO64 routine now insures that the 6510 port /HIRAM
;		and /LORAM  lines  are set  HIGH  before entering  C64  mode.
;		Previously entering C64 mode  (where /HIRAM and /LORAM select
;		RAM and ROM memory configurations)  from a C128 graphics mode
;		(where  /HIRAM and /LORAM  select  VIC  and  processor  color
;		nybble banks) would crash the C64 system  because its  system
;		ROM was not in context and entry was not via hardware reset.
;
;
; The following changes have been made to the EDITOR:
;
;	1/	The IRQ screen handler has been modified to accommodate 2 MHz
;		split screen operation. Previously the split screen operation
;		in 2 MHz mode caused the IRQ keyscan to be skipped.
;
;	2/	A difference between the C64 and C128 screen editors has been
;		noted but will NOT BE CHANGED because the same anomaly occurs
;		with the windowing editor on other products.  The  effect  is
;		illustrated by the following BASIC operation:
;
;			10 OPEN 1,0	:REM   OPEN CHANNEL TO KEYBOARD
;			20 INPUT#1,A$	:REM   INPUT A LINE
;			30 PRINT A$	:REM   DISPLAY LINE INPUT
;
;		The PRINT A$  will overwrite the last character of the string
;		entered in response to the 'INPUT'.  The C/64 editor does not
;		overwrite; in fact there are extra spaces found  between  the
;		input and output strings.  Any requests to change this should
;		be sent as soon as possible!
;
	.page
	.subttl  RELEASE NOTES:   BETA-8   04/15/85

; THE FOLLOWING MODIFICATIONS HAVE BEEN MADE TO THE BETA-8 CODE TO CREATE THE
; BETA-9 VERSION RELEASED ON 04/19/85. THE BETA-8 RELEASE IS A PRE-PRODUCTION
; VERSION INTENDED FOR SYSTEM TEST AND EVALUATION AND PRELIMINARY APPLICATION
; SOFTWARE DEVELOPMENT ONLY.
;
;	**************************************************************
;	* PENDING SYSTEM TEST, THIS SHALL BE THE FINAL BETA RELEASE. *
;	**************************************************************
; 
;
; The following changes have been made to the KERNAL:
;
;	1/	The Kernal TAPE handler previously returned the  wrong  error
;		code upon encountering the EOT. This has been fixed to report
;		'file not found'.
;
; The following changes have been made to the EDITOR:
;
;	1/	DOCUMENTATION ONLY:  Please note that  the  Editor  does  not
;		factor in 'VM1' when utilizing 'LDTBL1'.  This means that the
;		IRQ screen handler WILL move the screen per VM1 but that  all
;		printing will continue to update the screen at $400.  Because
;		LDTBL1 is in ROM it cannot be modified as in the  C/64.   Any
;		requests to change this mechanism must be made NOW.
;
;	2/	The Editor now includes the National keyboard  lookup  tables
;		and a National-to-ASCII conversion table.  While there is  no
;		National keyboard driver code in ROM, it is expected that the
;		inclusion of these tables will provide sufficient support for
;		RAM or Function Cartridge based drivers.  AN APPLICATION NOTE
;		WILL BE FORTHCOMING TO DETAIL THE IMPLEMENTATION OF A DRIVER.
;		
; The following changes reference ALL sections of code:
;		
;	1/	The three (3) C/128 ROMs now contain a revision  byte  and  a
;		byte reserved for 'rounding' the checksum. The locations  are
;		given below. Please note that the 'revision'  byte  described
;		here refers to  the  individual  ROM  contents,  NOT  to  the
;		software (e.g.: the KERNAL software revision byte is $FF80).
;
;			LOW (U33):	$7FFE - REVISION BYTE
;					$7FFF - CHECKSUM ADJUST
;
;			MID (U34):	$BFFE - REVISION BYTE
;					$BFFF - CHECKSUM ADJUST
;
;			HIGH (U35):	$CFFE - REVISION BYTE
;					$CFFF - CHECKSUM ADJUST
;
;		The 8-bit checksum is calculated over the ROM's  contents  by
;		adding the sequential bytes for each 16KB ROM, discarding any
;		carry between additions. Thus the sum of each ROM  should  be
;		equal to the MSB of its starting address (e.g.: LO=$40,  MID=
;		$80, HI=$C0).  Please note the checksum of the HI  (U35)  ROM
;		does NOT include the CP/M portion (address range $D000-$DFFF)
;		as this section of ROM cannot be read by the host processor. 
	.page
	.subttl  RELEASE NOTES:   318020-03   05/08/85

; THE FOLLOWING MODIFICATIONS HAVE BEEN MADE TO THE  318020-02 CODE TO CREATE
; THE 318020-03 VERSION RELEASED ON 05/08/85.  THIS RELEASE IS MADE TO CREATE
; MASKED ROMS FOR PRODUCTION.
;
; 
;
; The following changes have been made to the KERNAL:
;
;	1/	Previously the IOINIT routine improperly initialized PAL  and
;		'R8' versions of the 8563 80-column display device. This  has
;		been corrected. Testing of these modes has been very limited.
;
;	2/	Previously the fast SERIAL routine SPINP forced the  50/60 Hz
;		flag on D1CRA low (60Hz). This has been corrected to preserve
;		the status of this line.
;
;	3/	A new jump table entry has been created for the  fast  SERIAL
;		routines SPINP and SPOUT at $FF47.  Users must enter with the
;		carry flag clear to select SPINP and carry flag set for SPOUT
;
;	4/	Previously the SERIAL routine DISK_CLI enabled  IRQ's  before
;		restoring the sprite enable register. This caused  the  BASIC
;		IRQ sprite handler, under stress, to lose sprites.  This  has
;		been corrected. The fix required a change as well to the TAPE
;		routine TNIF which shared portions of the  DISK_CLI  routine.
;		The shared code has been eliminated by copying the  necessary
;		portions to TNIF.
;
;	5/	Previously the Kernal RAM code for  JMPFAR called the  GETCFG
;		ROM code directly.  To preserve code  modularity,  the direct
;		address was replaced by the Kernal jump table address.
;
; The following changes have been made to the EDITOR:
;
;	1/	Due to remaining problems with the 8563 block fill  function,
;		the so-called KLUDGE code has again been added to the  CLRLIN
;		routine.  This patch will clear up the occasional  characters
;		seen on the 80-column text screen after a SCNCLR, and will be
;		valid even after the 8563 bug is fixed.  Two (2) absolute RAM
;		variables were added to DECLARE without disturbing anything.
;
;	2/	Previously it was not possible to move the VIC 40-column text
;		screen and still utilize the screen editor  because  LDTB1 is
;		is ROM based (unlke the C64 which is RAM based).  To  provide
;		this capability a new variable, LDTB1_SA has been created and
;		may be used in conjunction with the variable VM1 to  move the
;		VIC text screen. VM1 should be written instead of VIC reg #24
;		(default value $14) and LDTB1_SA loaded with the high byte of
;		the new screen location (default value $04). Please note that
;		the placement of VIC video matrices is limited by  the  image
;		of the character ROM VIC sees at $1000-$1FFF and the presence
;		of ROM beginning at $4000 in the system's  memory  map.  This
;		change affected Editor routines SCRSET and MOVLIN which  used
;		LDTB1 directly, and CINT which now initializes LDTB1_SA.
;
;	3/	There was a problem with windows one (1) character wide in 80
;		column mode which affected areas outside the  window. It  was
;		been corrected by testing for this special case in CLRL80.
;
;	4/	The NATIONAL keyboard lookup tables and the National-to-ASCII
;		conversion table have been removed.  They were  found  to  be
;		unnecessary for Foreign-language systems and insufficent  for
;		other applications. AN APPLICATION NOTE IS FORTHCOMING.
;
;	5/	The handling of 8563s special attributes (ALT, RVS, UNDERLINE
;		and FLASH) has been  made  more  consistant  throughout.  The
;		BLANK and CLRLN routines now strip all attributes except ALT.
;		(this will affect, for example, INSERT and DELETE).   Various
;		other routines now preserve the RVS bit  (not to be  confused
;		with reverse-field mode; this hardware bit is  unused  by the
;		system but can be set by the user).
;
;	6/	A single <ESCAPE> no longer cancels all  editor  modes  (e.g.
;		quote, insert, underline, flash & reverse). A double <ESCAPE>
;		or <ESCAPE>-O is now required. 
;
	.page
	.subttl   RELEASE NOTES:   318018-04, 318019-04, 318020-05   11/07/86
;
;          C128 ROM RELEASE NOTES: 318018-04, 318019-04, 318020-05
;                                  318022-02, 318023-02
;
;
; THE FOLLOWING MODIFICATIONS HAVE BEEN MADE TO THE 318018-02, 318019-02, AND
; 318020-03 CODE TO CREATE A NEW ROM SET RELEASED ON 11/07/86.  THIS  RELEASE
; IS MADE TO CREATE MASKED ROMS FOR PRODUCTION.  THESE ROMS ARE TO BE USED AS
; A SET, REPLACING THE PREVIOUS ROM SET ENTIRELY.  THEY  ARE NOT TO BE MIXED.
; THIS RELEASE IS MADE SIMULTANEOUSLY IN THE FORM OF 16KB AND 32KB ROMS,  FOR
; USE IN THE FOLLOWING SYSTEMS: C128, C128D, C128CR, C128DCR, AS APPROPRIATE.
;
;
;
; PLEASE NOTE:  AN  11/18/85  ENGINEERING RELEASE OF A C128 ROM SET WAS NEVER
; RELEASED TO PRODUCTION,  AND ACCOUNTS FOR THE APPARENT JUMP IN ROM REVISION
; NUMBERS.  THE FOLLOWING INFORMATION INCLUDES THOSE DIFFERENCES AS WELL.
;
;
;
;        The following information represents a filtered condensation  of
;        all  known problems, complaints, and suggestions that pertain to
;        the C/128 system software to  date  (exclusive  of  CP/M).   The
;        current ("original" ROMs) system software consists of:
;
;
;
;
;        1.  Part No. 318018-02 --> BASIC LOW            ($4000-$7FFF)
;
;        2.  Part No. 318019-02 --> BASIC HIGH, MONITOR  ($8000-$BFFF)
;
;        3.  Part No. 318020-03 --> EDITOR, KERNEL, CP/M ($C000-$FFFF)
;
;        4.  Part No. 315078-02 --> DIN ED, KERNEL, CP/M ($C000-$FFFF)
;
;        5.  Part No. 318022-01 --> BASIC, MONITOR       ($4000-$BFFF)
;                                   (includes 1+2 above)
;
;        6.  Part No. 318023-01 --> ED, KERN, CP/M, C64  ($C000-$FFFF)
;                                   (includes 3+4 above)
;
;
;
;        Each 16KB ROM block contains a small patch area for changes, and
;        all patches described below have been accomplished such that any 
;        particular change will never affect more than one ROM. Similarly
;        each ROM contains a revision status byte  (at $7FFE, $BFFE,  and
;        $CFFE) which  software  can test to determine the version of the
;        host system.  The "original" ROMs contain $00 in these locations
;        and  the  "update"  ROMs described herein contain $01.  Each ROM
;        has had several changes, as summarized on the following pages.
;
;
;
;        Part Number 318018-04 --> BASIC LOW ($4000-$7FFF)
;
;        1.  LIST and DELETE commands.  Previously they did not report as
;            errors  certain  non-numeric characters passed as arguments,
;            such as 'LIST A'.  This has been corrected  totally  in-line
;            by  adjusting  an  erroneous  relative branch in the 'RANGE'
;            subroutine.
;
;        2.  CIRCLE  command.    Previously   an   unspecified   Y-radius
;            defaulted  to  the X-radius (as it should), but the X-radius
;            value had already been scaled for the  X-axis  and  not  the
;            Y-axis.   This has been corrected totally in-line by scaling
;            the radii after the defaults have been established.
;
;        3.  RS-232 STatus.  Previously accessing  ST  after  RS-232  I/O
;            resulted  in  an incorrect status being returned from, and a
;            zero written to, location $10A14,  possibly  corrupting  the
;            BASIC variable area.  This was a result of BASIC calling the
;            Kernel routine 'READSS'  with  the  incorrect  RAM  bank  in
;            context.    This  has  been  corrected  totally  in-line  by
;            substituting the correct BASIC subroutine call.
;
;        4.  CHAR command.  Previously using CHAR with the 80-column text
;            screen  (GRAPHIC  mode  5)  resulted  in  RAM  corruption at
;            locations $D600 and $D601 of RAM  bank  0  (the  BASIC  text
;            bank)  due  to BASIC calling the Editor PLOT routine without
;            the I/O block in context.  This has been corrected utilizing
;            two patch subroutines.
;
;        5.  RENUMBER command.  Previously the pass 2 routine, which  was
;            to  pre-scan  BASIC  text  and report 'out of memory' errors
;            prior to actually changing anything, was  seriously  flawed.
;            This has been corrected utilizing a patch subroutine.
;
;        6.  DELETE command.  Previously did not limit-check itself  when
;            moving  down  BASIC text, therefore it was possible to crash
;            when DELETEing lines at or near the top of memory (near  the
;            MMU  configuration  registers).   This  has  been  corrected
;+           utilizing  a  patch  subroutine.   Also,  DELETE  previously
;+           exited to MAIN via 'JMP',  effectively ending the evaluation
;+           of the current command string.  This  has  been corrected by
;+           substituting  an  'RTS',   allowing  direct   commands  like 
;+           'DELETE 10: PRINT"DELETED LINE 10"'  to work correctly.
;
;+       7.  PLAY command.  Previously the  SID frequency tables were not
;+           exactly NTSC concert pitch. Also, there was no provision for
;+           adjusting  the  frequency  for  PAL  systems.  This has been
;+           corrected by changing the (NTSC) frequency tables,  creating
;+           new PAL tables,  and utilizing patch code to select from the
;+           appropriate table as determined by the Kernel PAL_NTSC flag.
;
;+       8.  The BASIC  ERROR  handler previously failed to clear pending
;+           string temporaries when an error was TRAPed.  This  has been
;+           corrected via patch code to reset TEMPT to TEMPST.
;
;+       9.  The powerup copyright notice has been updated to 1986, which
;+           will serve as an immediate  visual  indication  of  the  ROM
;+           update status.  Also, a new notice has been placed at $7FC0.
;
;       10.  The ROM signature at location $7FFC  and  $7FFD  (lo/hi)  is
;            $8DEF.  (new since last release).
;
;       11.  The ROM revision byte at  location  $7FFE,  has  incremented
;            from $00 to $01.
;
;       12.  The ROM checksum byte at location $7FFF,  has  changed  from
;            $4C to $61.
;
;
;       Part Number 318019-04 --> BASIC HIGH, MONITOR ($8000-$BFFF)
;
;       13.  RSPRITE and RSPPOS functions.  Previously they  accepted  as
;            parameters  sprite  numbers  in  the  range  1-16,  which is
;            incorrect.  This  has  been  corrected  totally  in-line  by
;            limiting  the  range  check to 1-8, and reporting an illegal
;            quantity error for sprite numbers outside this range.
;
;       14.  PRINT  USING  command.   Previously  there  was  an  anomaly
;            involving  the  use  of  floating  money  symbols  ('$') and
;            commas.  The command 'PRINT USING "#,##$.##";  123.45',  for
;            example,   resulted  in  the  output  '$,123.45',  which  is
;            incorrect.  This has been fixed utilizing a patch subroutine
;            which  checks  specifically  for  the  '$,'  occurrence  and
;            substitutes a '_$' ('_' = fill character) whenever found.
;
;       15.  Relative  Coordinates  for  all  graphic  commands   (except
;            MOVSPR)   were   incorrectly  processed.   The  problem  was
;            apparent when negative relative coordinates were used, which
;            resulted  in  an  illegal  quantity  error.   This  has been
;            corrected  totally  in-line  by  substituting  a   different
;            subroutine  call  to pre-existing code.  This change affects
;            the BASIC commands LOCATE, DRAW, PAINT, BOX, CIRCLE, GSHAPE,
;            and  SSHAPE.   This  change  also  allows  negative absolute
;            coordinates to be accepted (previously they resulted  in  an
;            illegal  quantity error), although the legal range remains a
;            16-bit  value:   0-65535  (unsigned)  or  -32768  to   32767
;            (signed:  i.e., -1 is equivalent to 65535).
;
;       16.  DOPEN and APPEND commands.  Previously it  was  possible  to
;            open  two  or  more disk channels with the same logical file
;            number without incurring an error  report.   This  has  been
;            corrected totally in-line.
;
;+      17.  MATH package.  An original  bug  fix  (ref: double zero bug)
;+           to the (F)MULT routine has been found  to  result  in  small
;+           errors (such as 2^15 = 32768.0001).  This has been corrected
;+           totally in line by fixing the original (dbl-0) problem  in a
;+           different way.  
;
;+      18.  A copyright notice has been placed, starting at $BFC0.
;
;       19.  The ROM signature at location $BFFC  and  $BFFD  (lo/hi)  is
;            $CDC8.  (new since last release).
;
;       20.  The ROM revision byte at  location  $BFFE,  has  incremented
;            from $00 to $01.
;
;       21.  The ROM checksum byte at location $BFFF,  has  changed  from
;            $3A to $C5.
;
;
;       Part number 318020-05 --> EDITOR, KERNEL, CP/M ($C000-$FFFF)
;
;       22.  CAPS LOCK Q.  Previously an error in  a  key  matrix  decode
;            table caused a lower-case 'Q' to be passed when the keyboard
;            is in CAPS LOCK mode.   The  table  has  been  corrected  by
;            substituting the correct value for upper-case 'Q'.
;
;+      23.  FUNCTION KEYs.  Previously the funcion key handler,  part of
;+           the SCNKEY routine at CKIT2,  failed to detect a funtion key
;+           string pending.  This  has been corrected via patch routine,
;+           which  will  ignore  new  function key depressions until the
;+           string in progress has been output (i.e., KYNDX = 0).  Also,
;+           DOPFKY now exits via SCNRTS, instead of simply RTSing.
;
;       24.  IOINIT  system  initialization.    Previously   the   RS-232
;            pseudo-6551  registers  were  not  initialized because these
;            values are expected to be given by the user whenever  RS-232
;            channels  are  OPENed.  Apparently many C64 users have taken
;            advantage of the fact the  C64  'happened'  to  clear  these
;            locations  and  fail  to specify critical parameters.  These
;            RS-232 registers are now  initialized  to  default  to:   no
;            parity, full duplex, 3-line, 1-stop bit, 8-bit words and 300
;            baud, via a patch subroutine.
;
;       25.  IOINIT PAL system  initialization.   Adjustments  have  been
;            made to the 8563 initialization values for PAL systems.  The
;            PAL horizontal total (register 0) changes from $7E  to  $7F.
;            The PAL vertical total (register 4) changes from $27 to $26.
;            These  changes  shift  the  cycle  time  from  20.320us   to
;            20.032us.  The patch required a patch subroutine, as well as
;            a change to VDCTBL.
;
;       26.  BASIN system  call.   Previously  attempting  input  from  a
;            logical channel to the screen (e.g., via INPUT#) resulted in
;            line too long errors.  This has been corrected  utilizing  a
;            subroutine  patch to preserve bit-7 of CRSW, which serves as
;            a flag to the Editor that a (pseudo)  end-of-line  has  been
;+           reached.  Also, TBLX is copied to LINTMP to correctly locate
;+           the current cursor line for the  Editor.  Please  note  that
;+           switching between the 40 and 80-column text screens, opening
;+           and closing windows,  or  clearing  text screens can confuse
;+           logical  screen channels.  The Editor variable LINTMP ($A30)
;+           is a global,  not local,  variable  as  it should have been.
;+           Users can POKE LINTMP with the  logical screen  line  number
;+           before INPUT#'s as a work-around.
;
;       27.  OPEN RS-232 system call.   Previously  it  was  possible  to
;            receive  a  carry-set  status,  normally indicating a error,
;            when no error existed after OPENing an RS-232 channel.  This
;            has  been corrected totally in-line by a modification to the
;            code which checks for the proper X-line hardware states.
;
;       28.  LOAD system call.  Previously the normal (a.k.a.  SLOW) load
;            mechanism  did  not  preserve  the  starting  address of any
;            LOADs, which made  the  BASIC  'BOOT  "file"'  command  form
;            malfunction  unpredictably.  This is apparent only when used
;            with 1541 drives.  This  has  been  corrected  via  a  patch
;            subroutine,  which  saves the starting address of all LOADed
;            files at SAL and  SAH,  the  same  place  the  fast  (a.k.a.
;            BURST) load mechanism does.
;
;       29.  DMA system call.  Previously the Kernel forced the I/O block
;            into  the user's memory configuration at all times, which is
;            no longer necessary  and,  in  fact,  seriously  limits  the
;            functionality of the RAM expansion cartridge.  This has been
;            corrected by a ROM patch routine,  which affects  all Kernel
;            DMA system calls as well as the BASIC FETCH, STASH, and SWAP
;+           commands.  Also,  previously  it  was possible for an IRQ to
;+           occur  between  the  'arm DMA'  and 'trigger DMA' sequences,
;+           resulting  in  a DMA operation with the system configuration
;+           in  context regardless of desired configuration.   This  has
;+           been corrected by adding 'PHP/SEI...PLP' instructions around
;+           the JSR to DMA RAM code at $3F0.  Applications using the DMA
;+           RAM code at $3F0 should do likewise.  Finally, in this patch
;+           changes were made to enable DMA operations to all RAM  banks
;+           by correctly using the VIC bank pointer found in the MMU RAM
;+           configuration register ($D506,  VA16=bit-6 and  VA17=bit-7).
;+           Applications using the Kernel routine at $FF50 will  inherit
;+           these changes automatically. Please note that NMI interrupts
;+           can screw-up DMA operations, as they cannot be masked.
;
;+      30.  A copyright notice has been placed, starting at $CFC0.
;
;+      31.  The ROM location $CFF8 is reserved  for  national  character
;+           ROM checksums. This does not apply to US ROMs, which contain
;+           $FF here.  (new since last release).
;
;       32.  The ROM location $CFF9 is now reserved  for  country  codes.
;            The US ROMs contain $FF here.  (new since last release).
;
;       33.  The  ROM  location  $CFFA  and  $CFFB  (lo/hi)  contain  the
;            national character set signature.  This does not apply to US
;            ROMs, which contain $FFFF here.  (new since last release).
;
;       34.  The ROM signature at location $CFFC  and  $CFFD  (lo/hi)  is
;            $8F76.  (new since last release).
;
;       35.  The ROM revision byte at  location  $CFFE,  has  incremented
;            from $00 to $01.
;
;       36.  The ROM checksum byte at location $CFFF,  has  changed  from
;            $C3 to $3C.
;
;       37.  The Kernel revision byte at location $FF80  has  incremented
;            from $00 to $01.
;
;
;       Part number 315078-03 --> DIN ED, KERNEL, CP/M ($C000-$FFFF)
;
;+       There  have  been  no  revisions  to the national portions of the
;+       many national Kernel/Editor ROMs.  All revisions described  above
;+       for the US version have also been made to the national versions.
;
;
;+      38.  A copyright notice has been placed, starting at $CFC0.
;
;+      39.  The ROM location $CFF8 is reserved  for  national  character
;+           ROM checksums.  The German/Austrian (DIN) ROMs contain a $FF
;+           here (that really is the checksum!).
;
;       40.  The ROM location $CFF9 is now reserved  for  country  codes.
;            The German/Austrian (DIN) ROMs contain $00  here.   US  ROMs
;            contain $FF here.
;
;       41.  The  ROM  location  $CFFA  and  $CFFB  (lo/hi)  contain  the
;            national  character  set signature.  The  DIN  ROMs  contain
;            $91F3 here.  This is unused ($FFFF) for US ROMs.
;
;       42.  The ROM signature at location $CFFC  and  $CFFD  (lo/hi)  is
;            $EA3B.
;
;       43.  The ROM revision byte at  location  $CFFE,  has  incremented
;            from $00 to $01.
;
;       44.  The ROM checksum byte at location $CFFF,  has  changed  from
;            $C3 to $4C.
;
;       45.  The Kernel revision byte at location $FF80  has  incremented
;            from $00 to $01.
;
;
;        C/128 developer and documentation notes
;
;        There were quite a few items that, for one reason or another,  I
;        categorized   as   un-fixable   as   the  fix  would  compromise
;        compatibility  or  be  too  radical  given  the  work-around  or
;        relative  severity  of  the  problem.  They are included here to
;        document them along with any implications, work-arounds, etc.
;
;        1.  DMA interface.  It should also be noted that DMA hardware is
;            unreliable  at  2MHz  clock speeds and consequently the user
;            must  insure  1MHz  (SLOW)  mode  is  used  before  any  DMA
;+           operations are performed.  NMI interrupts  will  also  cause
;+           problems, and should be disabled or somehow avoided.  RS-232
;+           operations use NMIs;  the  remote  should  be  XOFFed or the
;+           channel disabled before DMA operations are performed.
;
;        2.  IRQ handler.  It is possible for the Kernel IRQ  handler  to
;            perform  a keyscan when the IRQ was not the Kernel's.  While
;            the fix is trivial,  I did not include it because  it  might
;            cause problems with existing software which  may  be  taking
;            advantage of the unintentional keyscans.
;
;        3.  IRQ and NMI handlers.  The Kernel  forces  the  system  bank
;            into  context  before taking the RAM indirect vectors to the
;            actual interrupt handler.  Much flexibility can be added  to
;            the  mechanism if the memory bank to be brought into context
;            at interrupt time could be read from a variable instead of a
;            ROM constant.  I am reluctant to include this change because
;            the RAM variable would have to be located  in  a  previously
;            'free' location, which may be used by existing applications.
;
;        4.  SAVE-to-disk.  It is not possible to SAVE the last  byte  of
;            any  memory  bank  (e.g.,  RAM  at  $FFFF), because the SAVE
;            routine requires you to specify the end of the  area  to  be
;            SAVEd  as  the  ending  address PLUS ONE ($FFFF+1 -> $0000).
;            This is a problem found on all CBM 65xx systems.
;
;        5.  SAVE-to-cassette.  It is not possible to save the last  page
;            of  any memory bank (e.g., RAM at $FF00-$FFFF) to tape.  The
;            tape handler hangs with the motor  running  until  the  user
;            STOPs  it.   This is a problem found on all CBM 65xx systems
;            except the Plus-4.
;
;        6.  SAVE and LOAD.   While  program  SAVEs  correctly  save  the
;            16-bit  starting  address  for  future  LOADs,  the existing
;            formats do  not  save  the  memory  bank.   This  cannot  be
;            accommodated  without  the  creation of special C128 program
;            headers  for  disk  and  tape,  which  is  undesirable   and
;            non-trivial.
;
;        7.  STOP/RESET monitor entry.  It is not possible to  enter  the
;            Monitor  directly via the STOP/RESET sequence from BASIC and
;            then eXit back to BASIC without  incurring  a  'cold'  BASIC
;            initialization.   The  alternative,  taking the BASIC 'warm'
;            start route, would result in a system crash if BASIC had not
;            been properly initialized and is therefore not a recommended
;            fix.  The work-around, assuming  BASIC  was  running  before
;            STOP/RESET,  is  for  the  user  to place the value $C1 into
;            location $A04 (INITSTATUS) and then eXit.
;
;        8.  Monitor 'H' (hunt) command.   Because  the  editor  performs
;            various translations on data read from the screen, it is not
;            possible to Hunt for certain CBM characters, such as pi  and
;            all reverse-field characters.
;
;        9.  BOX command.   BOX,  because  of  the  particular  algorithm
;            utilized,  has  a  restricted  range  of  -16384  to  +16383
;            (unscaled).  The algorithm uses parameters  that  are  twice
;            those  given  for  calculations  and divides down the result
;            before  plotting.   Thus  it  is  possible  for  very  large
;            (unscaled)  positive coordinates to result in large negative
;            plots.   The  work-around   is   to   use   SCALEing,   user
;            range-checking,  or  avoid BOX and use either DRAW or CIRCLE
;            commands instead.
;
;       10.  RDOT, PEN, and  RSPPOS  functions.   These  BASIC  functions
;            return   the  current  pixel  cursor,  lightpen  and  sprite
;            positions, respectively, but  the  values  they  return  are
;            unSCALEd.   Correcting  this is trivial, but would result in
;            problems  for  existing  applications  as  well   as   being
;            incompatible with C64 VSP and the PLUS-4.
;
;       11.  FNDEF and GRAPHIC modes.  After defining  a  user  function,
;            anything that results in program relocation must be avoided,
;            such as GRAPHIC  'n'  or  GRAPHIC  CLR.   There  is  no  fix
;            planned.  The work-around is a general rule:  define GRAPHIC
;            screens first (then SCALE), then define functions.
;
;       12.  HELP command.  HELP does not completely identify two type of
;            syntax errors:  'RUN n' where 'n' is an undefined statement,
;            and 'XXXX n' where 'XXXX' is any BASIC command which expects
;            no  argument.   In  the  RUN  case,  TXTPTR had already been
;            reset, but not OLDLIN, thus the correct  line  is  displayed
;            but  without  any  highlighting.  In the other case, because
;            XEQCM exits via CHRGET, TXTPTR  is  incremented  before  the
;            error  is  caught,  resulting in part of the error not being
;            highlighted.  The  fix  would  be  too  complex,  especially
;            considering  the  fact that the line containing the error is
;            reported, albeit without highlighting.
;
;       13.  GETKEY  function.   In  the  form  'GETKEY  A',  this  BASIC
;            function  should  wait  until  a numeric key is pressed, but
;            certain non-numeric keys are accepted, such as  'E',  colon,
;            comma,  period,  '+', and '-'.  The same anomaly occurs with
;            'GET A' for the same reason, as is a known CBM quirk.
;
;       14.  PUDEF and PRINT USING commands.  There is poor documentation
;            of the fact that USING format fields such as "$,###.##", the
;            leading '$' or commas are not interpreted per the PUDEFs  as
;            they  are considered not part of the numeric field.  This is
;            found to occur in all versions of CBM BASIC 3.5 or higher.
;
;       15.  RENUMBER command.  Because of the routine's use of LINNUM to
;            fetch  from BASIC text the statement number of an object, it
;            is possible to get a 'syntax error' when none existed within
;            the RENUMBER command line itself.  LINNUM itself reports any
;            line numbers it  reads  that  are  out  of  range  (>63999).
;            Actually,  you  could  use  this to 'protect' a program from
;            RENUMBERing.  Also note that RENUMBER  can not renumber line
;            ranges.   Therefore,  LIST  and  DELETE  statements within a
;            program are NOT changed nor will they generate any errors.
;
;       16.  OUT of MEMORY ERROR.  It is possible to hang the system with
;            this error from a running program when there is insufficient
;            memory to contain the string representation of the  original
;            line number where the error occurred.  One must STOP/RESTORE
;            the system and type CLR to recover.  This problem  is  found
;            on most CBM 65xx systems.
;
;+      17.  BASIN ($FFCF) calls return <space><return> to <return> input
;+           (i.e., null input line).  The culprit is an unnecessary  BEQ
;+           test in LOOP4, which was left in for  compatibility with the
;+           C64 and PLUS-4 editors, which behave in a similar manner.
;
;+      18.  MATH package.  The binding of operators is such  that  unary
;+           minuses are evaluated after powers. This results in NO error
;+           when  equations  of  the  form (-4^.5) are evaluated (square
;+           root of a negative number). Of course, the error is given if
;+           the sign  of  the number is known when the power is  applied
;+           (eg., X=-4 :PRINT X^.5). This is a known CBM quirk.
;
;+      19.  BASIC  DOS  commands,  such  as   DOPEN  and  APPEND,  limit
;+           filenames to 16 characters maximum.  However,  when the name
;+           string includes the filetype,  such as "LONGLONGLONGLONG,P",
;+           BASIC reports a FILENAME TOO LONG error when, in fact, it is
;+           valid.  This is a problem on all CBM systems > DOS 3.0.
;
;+      20.  DCLOSE  accepts both LFN and UNIT parameters simultaneously,
;+           which it should not (they should be mutually exclusive). The
;+           LFN is always used, and the UNIT ignored, if both are given.
;
;+      21.  AUTO command puts next line number into the  KEY buffer.  It
;+           should probably just print it.  This  only  causes a problem
;+           when used with function key strings containing <return>s.
;
;       22.  INSERT Editor  mode  is  not  canceled  by  specific  ESCAPE
;            sequences    which    move   the   cursor.    For   example,
;            <INS><INS><INS> <ESC>K <CRSR><CRSR><CRSR>  illustrates  that
;            staying in INSERT mode after moving the cursor to the EOL is
;            undesirable.
;
;       23.  CIRCLE  command  in  multicolor  mode   calculates   default
;            Y-radius   based   upon   twice   the   X-radius.   This  is
;            undesirable, but maintained to preserve compatibility  among
;            C64 VSP, PLUS-4, and C128.
;
;+      24.  TAPEs written in FAST mode are occasionally hard to read  in
;+           SLOW mode.  While I would not be surprised by this,  I could
;+           not repeat the problem.  The complaint comes  from  PAL/50Hz
;+           environments, but that may be the only place tapes are used!
;+           Users should take care to use tapes only in SLOW (1MHz) mode
;+           so that the tapes can be read on PETs, 8032s, C64s, etc.
;
;+      25.  VERIFY and DVERIFY will report 'verify error' when, in fact,
;+           the programs are identical.  This  occurs  when  the  LOADed
;+           version of the BASIC program has been relocated since it was
;+           SAVEd, usually the result of the GRAPHIC  command.  In  such
;+           cases, the MSB of line links differ,  and  (D)VERIFY  fails.
;+           Try GRAPHIC <n|CLR> as appropriate and try again.
;
;
;    The updated C/128 16KB ROM set bears the following  part  numbers. (The
;    PCB socket number is valid only for original PCBs).
;
;
;
;    1.  # 318018-04 --> BASIC LOW            ($4000-$7FFF, U33) cksum= 9A40
;
;    2.  # 318019-04 --> BASIC HIGH, MONITOR  ($8000-$BFFF, U34) cksum= 6F80
;
;    3.  # 318020-05 --> EDITOR, KERNEL, CP/M ($C000-$FFFF, U35) cksum= EEC4
;
;    4.  # 315078-03 --> DIN ED, KERNEL, CP/M ($C000-$FFFF, U35) cksum= E4C4
;
;
;
;    The new C/128 32KB ROM set bears the following part numbers.
;
;
;
;    1.  # 318022-02 --> BASIC, MONITOR       ($4000-$BFFF, U34) cksum= 09C0
;
;    2.  # 318023-02 --> EDITOR, KERNEL, CP/M ($C000-$FFFF, U32) cksum= F324
;
;        (Note that #2 above also contains the C/64 ROM code.)
;
;
;
;
;        Fred Bowen        11/07/86

	.page
	.subttl  MONITOR DOCUMENTATION

;	I. SUMMARY OF C128 MONITOR COMMANDS
;
;
;
; 1/  A <start_address> <opcode> [operand]			:Assemble
;
; 2/  C <start_address> <end_address> <to_start_address>	:Compare
;
; 3/  D [<start_address> [end_address]]				:Disassemble
;
; 4/  F <start_address> <end_address> <byte>			:Fill
;
; 5/  G [address]						:Goto (JMP)
;
; 6/  H <start_address> <end_address> <byte1> [<byte_n>...]	:Hunt
;     H <start_address> <end_address> '<ascii_string>
;
; 7/  J [address]						:gosub (JSR)
;
; 8/  L "<filename>" [,<device_#> [,<load_address>]]		:Load
;
; 9/  M [<start_address> [end_address]]				:Memory dump
;
; 10/ R								:Register dump
;
; 11/ S "<filename>",<device_#>,<start_address> <last_address+1>:Save
;
; 12/ T <start_address> <end_address> <to_start_address>	:Transfer
;
; 13/ V "<filename>" [,<device_#> [,<load_address>]]		:Verify
;
; 14/ X								:eXit monitor
;
; 15/ @[device_#]						:disk status
;     @[device_#][,<command_string>]				:disk command
;     @[device_#],$[[<drive>][:<file_spec>]]			:disk catalog
;
;
;
;
;
;	NOTES:		<> enclose required parameters.
;			[] enclose optional parameters.
;
	.PAGE
;
;	II. SUMMARY OF MONITOR FIELD DESCRIPTORS
;
;
;	The follow designators precede monitor data fields (e.g. memory
;	dumps). When encountered as a command they instruct the monitor
;	to alter memory or register contents using the given data.
;
;
;	1/  .	:<period> precedes lines of disassembled code.
;
;	2/  >	:<right_angle> precedes lines of a memory dump.
;
;	3/  ;	:<semicolon> precedes line of a register dump.
;
;
;
;	The following designators precede number fields  (e.g. address)
;	and specify the  radix (number base) of the  value.  Entered as
;	commands they instruct the monitor simply to  display the given
;	value in each of the four radices.
;
;
;	1/  	:<null> (default) precedes hexidecimal values.
;
;	2/  $	:<dollar> precedes hexidecimal (base-16) values.
;
;	3/  +	:<plus> precedes decimal (base-10) values.
;
;	4/  &	:<ampersand> precedes octal (base-8) values.
;
;	5/  %	:<percent> precedes binary (base-2) values.
;
;
;
;	The  following  characters  are  used by the  monitor as  field
;	delimiters  or  line terminators  (unless encountered within an
;	ascii string).
;
;
;	1/	:<space> delimiter- separates two fields.
;
;	2/  ,	:<comma> delimiter- separates two fields.
;
;	3/  :	:<colon> terminator- logical end of line.
;
;	4/  ?	:<question> terminator- logical end of line.
;
	.page
;
;	III. MONITOR COMMAND DESCRIPTIONS
;
;
;
;	This section of documentation will be expanded at a later date.
;	In the meantime please refer to the C128 SOFTWARE SPECIFICATION
;	for complete descriptions and examples of the Monitor commands.
;	The following additions should be noted however.
;	
;
;	Except as noted earlier,  there are  no changes at this time to
;	the functionality of the monitor commands.  Please note however
;	that any number field (e.g. addresses, device numbers, and data
;	bytes) may now be specified as a based number. This affects the
;	operand field of the  Assemble  command as well.  Also note the
;	addition of the directory syntax to the disk command.
;
;
;	As a  further  aid to  programmers, the  Kernal  error  message
;	facility has been  automatically  enabled while in the Monitor.
;	This means the Kernal will display  'I/O ERROR #' and the error
;	code should  there be any  failed I/O attempt from the Monitor.
;	The  message  facility  is turned off when exiting the Monitor.
;
;.end

