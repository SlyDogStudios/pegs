a_punch				=	$01			; Assign names of the buttons to
b_punch				=	$02			; the bits that they correspond to
select_punch		=	$04			; in control_pad. This makes it easier
start_punch			=	$08			; to reference each button by throwing
up_punch			=	$10			; the pound sign in front of the button
down_punch			=	$20			; to use in each routine. Example:
left_punch			=	$40			; #a_punch
right_punch			=	$80
top_left_cursor		=	$200		; Give names to the most commonly used
top_right_cursor	=	$204		; sprite addresses for ease of use as
bot_left_cursor		=	$208		; well. The name is only given to the
bot_right_cursor	=	$20c		; first byte that describes each sprite.
left_num_moves		=	$210		; For accessing the tile, attribute, and
right_num_moves		=	$214		; Y-pos bytes, we will just be using
top_left_place		=	$218		; a number added to it. Example:
top_right_place		=	$21c		; lda top_left_cursor+2 would load A with
bot_left_place		=	$220		; the attribute byte from that given
bot_right_place		=	$224		; sprite.
left_num_peg		=	$228
right_num_peg		=	$22c
init				=	$898a		; Address of the music engines init routine
play				=	$8977		; Address of the music engines play routine

.segment "ZEROPAGE"

control_pad:		.res 1			; Used for the state of the buttons

control_old:		.res 1			; Used for the prior state of the buttons

no_control:			.res 1			; Used in NMI to shut off controls. Write
									; something other than zero to do this.

no_music:			.res 1			; Used in NMI to shut off music. Write
									; something other than zero to do this.

nmi_num:	 		.res 1			; This is incremented by one during each
									; NMI. We use this byte to make delays
									; and also to have the main loop run once
									; per frame.

top_board:			.res 1			; This byte keeps track of the top half of
									; the peg board. The items marked with the
									; bit numbers are what this byte keeps track
									; of. Bit 7 is unused.   
									;     6
									;    5 4
									;   3 2 1
									;  0 x x x
									; x x x x x

bottom_board:		.res 1			; This byte keeps track of the bottom half of
									; the peg board. The items marked with the
									; bit numbers are what this byte keeps track
									; of.
									;     x
									;    x x
									;   x x x
									;  x 7 6 5
									; 4 3 2 1 0

the_tiles:			.res 1			; These three bytes are used to decompress
number_of_tiles:	.res 1			; one of the nametables that is stored as
odd_even:			.res 1			; an RLE file.

hscroll:			.res 1			; Kind of a waste. This doesn't really
vscroll:			.res 1			; need named, but hey. We do use vscroll
									; to scroll the credits screen. At least
									; that's helpful :)

nmi_scroll_off:		.res 1			; Use this byte to NOT write zeros to the
									; scroll register at the end of NMI. We
									; use this byte when scrolling on the
									; credits screen.

peg_latch:			.res 1			; Used to determine if the player left 1 peg
									; on the board at the end of the game

temp1:				.res 1			; These two bytes are used for various things
temp2:				.res 1			; within the program

.segment "CODE"
.incbin "s\pegs.nsf"

reset:
	jsr vblank_wait
	jsr vblank_wait					; Here we do our basic intializing of
	sei								; the NES. Clearing RAM and making sure
	ldx #$00						; the stack is set up and ready, to try
	stx $4015						; and avoid any inconsistencies when
	ldx #$40						; first booting the game.
	stx $4017
	ldx #$ff
	txs
	inx
	stx $2000
	stx $2001
	txa
:	sta $000,x
	sta $100,x
	sta $200,x
	sta $300,x
	sta $400,x
	sta $500,x
	sta $600,x
	sta $700,x
	inx
	bne :-
	jsr vblank_wait

	ldx #$ff
	txs

; *********************************************************
; Load the title screen and send that into a loop         *
; *********************************************************

	jsr song_first
	jsr delay_with_nmi_count2
title_screen:
	lda #$00						; First zero out temp2 for the DUD B
	sta temp2						; stuff, and zero out nmi_scroll_off
	sta nmi_scroll_off				; here in case the user goes to the
	jsr vblank_wait					; credits screen and comes back.
	jsr PPU_off						; The scroll will be fine then.

	lda #$3F						; Load the palette that will be used
	ldx #$00						; for the whole game (small game, so
	sta $2006						; this is the only time we really
	stx $2006						; have to bother with doing this).
:	lda palette,x
	sta $2007
	inx
	cpx #$20
	bne :-

	ldy #$00						; This is to load the nametable. This
	ldx #$04						; similar type of routine is used
	lda #<title						; throughout the program to load the
	sta $f0							; backgrounds.
	lda #>title
	sta $f1
	jsr fill_nametable

	jsr vblank_wait
	ldx #$0							; Pull in bytes for sprites and their
:	lda the_sprites+$30,x			; attributes which are stored in the
	sta top_left_cursor,x			; 'the_sprites' table. Use X as an index
	inx								; to load and store each byte, which
	cpx #$08						; get stored starting in $200, where
	bne :-							; 'top_left_cursor' is located at.
	jsr stabilize
	jsr PPU_with_sprites

title_loop:
	lda temp2						; First test for temp2 to be #$0f. If
	cmp #$0f						; it is, the user has found DUD B, and
	bne :++							; we jump to the easter egg screen. If
	beq :+							; they haven't, we continue on with the
:	lda #$00						; title screen loop, and start by reading
	sta temp2						; controller input.
	jmp egg_screen
:	lda control_pad					; Up button check/routine
	eor control_old					; This section is probably going to look
	and control_pad					; a bit confusing, but I figured since
	and #up_punch					; I'm releasing the source, I might as well
	beq :++++++++					; make the easter egg something to find for
	lda temp2						; both the gamer and the coders who are 
	cmp #$07						; looking at this : ) Have fun with all of
	bne :++							; the unnamed labels that plague DUD B!!
	beq :+
:	lda #$00
	sta temp2
:	lda temp2
	cmp #$03
	bne :++
	beq :+
:	lda #$00
	sta temp2	
:	lda temp2
	cmp #$01
	bne :++
	beq :+
:	lda temp2
	eor #%00000010
	sta temp2
:	lda top_left_cursor				; While the DUD B stuff is going on, we
	cmp #$87						; also keep track of where the sprites
	bne :+							; are onscreen. In this case it's the
	lda #$9f						; movement of the menu selection sprites.
	sta top_left_cursor
	sta top_right_cursor
	jmp not_start
:	sec
	lda top_left_cursor
	sbc #$08
	sta top_left_cursor
	sec
	lda top_right_cursor
	sbc #$08
	sta top_right_cursor
:	lda control_pad					; Down button check/routine
	eor control_old
	and control_pad
	and #down_punch
	beq :++++++++++
	lda temp2
	cmp #$07
	bne :++
	beq :+
:	lda #$00
	sta temp2
	beq :+++++
:	lda temp2
	cmp #$01
	bne :++
	beq :+
:	lda #$00
	sta temp2
	beq :+++++
:	lda temp2
	cmp #$00
	bne :++
	beq :+
:	lda temp2
	eor #%00000001
	sta temp2
	bne :+++
:	lda temp2
	cmp #$03
	beq :+
	bne :++
:	lda temp2
	eor #%00000100
	sta temp2
;	bne :+

:	lda top_left_cursor				; Here is the down routine for the sprites
	cmp #$9f						; that are on screen. Once again, the menu
	bne :+							; selection sprites.
	lda #$87
	sta top_left_cursor
	sta top_right_cursor
	jmp not_start
:	clc
	lda #$08
	adc top_left_cursor
	sta top_left_cursor
	clc
	lda #$08
	adc top_right_cursor
	sta top_right_cursor
:	lda control_pad					; Start button check/routine
	eor control_old
	and control_pad
	and #start_punch
	beq not_start
	lda top_left_cursor				; When start is pressed, we test the 
	cmp #$87						; position of the sprite 'top_left_cursor'.
	bne :+							; Depending on where it is, depends on 
	lda #$01						; what happens next in the program.
	sta no_music					; The first test is for going to the actual
	ldx #%00000001					; game itself....
	stx $4015
	ldx #$00
	jsr sq1_sfx
	jsr delay_with_nmi_count
	jmp load_pegs_board
:	lda top_left_cursor				; ...the next test is for the how-to
	cmp #$8f						; screen....
	bne :+
	jmp manual_screen
:	lda top_left_cursor				; ... this is for the credits screen....
	cmp #$97
	bne :+
	jmp credits_screen
:	lda top_left_cursor				; ... and this is for the background music
	cmp #$9f						; test.
	bne not_start
	jmp sound_screen
not_start:
	lda control_pad					; Select button check/routine
	eor control_old
	and control_pad
	and #select_punch
	beq not_select
	lda top_left_cursor				; The select button can also be used for
	cmp #$9f						; the menu selection sprite. When pressed,
	bne :+							; it makes the cursor go downwards. It wraps
	lda #$87						; back to the top of the menu after it reaches
	sta top_left_cursor				; the bottom.
	sta top_right_cursor
	jmp not_select
:	lda top_left_cursor
	clc
	adc #$08
	sta top_left_cursor
	lda top_right_cursor
	clc
	adc #$08
	sta top_right_cursor
not_select:
	lda control_pad					; B button check/routine
	eor control_old
	and control_pad
	and #b_punch
	beq :++
	lda temp2						; This is the final check for DUD B.
	cmp #$07						; I think it's probably fairly obvious
	bne :++							; by now what DUD B stands for :P If
	beq :+							; the user enters the final button in
:	lda temp2						; the correct sequence, then the final
	eor #%00001000					; fourth bit is EORed, and we go back
	sta temp2						; to the beginning of the loop for the
:	jsr wait_for_nmi				; initial DUD B test.
	jmp title_loop

; *********************************************************
; Below is the routine to load the credits and loop it    *
; *********************************************************
credits_screen:
	lda #$00						; Zero temp2 so DUD B checks start over
	sta temp2						; when the user gets back to the title
	jsr vblank_wait					; screen.
	jsr PPU_off

	ldy #$00						; Load the nametable for the credits.
	ldx #$04
	lda #<credits1
	sta $f0
	lda #>credits1
	sta $f1
	jsr fill_nametable
	jsr vblank_wait
	jsr stabilize
	jsr PPU_no_sprites

	jsr vblank_wait
	jsr PPU_off
	ldy #$00						; Load the second nametable for the credits
	ldx #$04						; at $2800
	lda #<credits2
	sta $f0
	lda #>credits2
	sta $f1
	lda #$28
	sta $2006
	lda #$00
	sta $2006
:	lda ($f0),y
	sta $2007
	iny
	bne :-
	inc $f1
	dex
	bne :-
	jsr vblank_wait
	jsr stabilize
	jsr PPU_no_sprites
	
	lda #$00						; Here we set the scroll up to prepare for
	sta hscroll						; scrolling. We also put a #$01 in
	lda #$00						; 'nmi_scroll_off' so we don't write zeroes
	sta vscroll						; to the scroll during every NMI. Throw a
	lda #$01						; #$5c in temp1 which is to be used for the 
	sta nmi_scroll_off				; delay at the top and bottom of the scrolling.
	lda #$5c
	sta temp1

:	lda control_pad					; B button check/routine
	eor control_old
	and control_pad
	and #b_punch
	beq :+							; Use this to check for someone that wants
	jmp title_screen				; to immediately go back to the title screen
:	jsr wait_for_nmi				; when they first enter the credits.
	dec temp1
	bne :--
@loopz:
	
	lda control_pad					; B button check/routine
	eor control_old
	and control_pad
	and #b_punch					; Once the loop starts, we need to check for
	beq :+							; the user trying to leave the screen when
	jmp title_screen				; the scrolling is happening. If not, we
:	bit $2002						; do the scroll stuff to make things happen!
	lda hscroll
	sta $2005
	lda vscroll
	sta $2005
	lda temp2						; First test temp2 to see if the scroll is
	cmp #$01						; at the top or the bottom of the screen.
	bcc :++++						; If it is, jump four unnamed labels down.
	lda vscroll						; If not, we subtract one from vscroll during
 	sec								; each frame. We do this until vscroll is
 	sbc #$01						; equal to zero, in which case we store zero
	sta vscroll						; in temp2, and go into the holding loop...
	cmp #$00
	beq :+
	bne @this_nmi
:	lda #$00
	sta temp2

	lda #$5c
	sta temp1

:	lda control_pad					; B button check/routine
	eor control_old
	and control_pad					; ... which is here. Test for the user
	and #b_punch					; pressing B again during the holding phase
	beq :+							; of the scroll.
	jmp title_screen
:	lda hscroll
	sta $2005
	lda vscroll
	sta $2005
	jsr wait_for_nmi
	dec temp1
	bne :--

	jmp @this_nmi
:	lda vscroll						; This is for scrolling the other direction
 	clc								; which has the same set up for the other
 	adc #$01						; scroll, just reversed.
	sta vscroll
	cmp #$ef
	beq :+
	bne @this_nmi
:	lda #$01
	sta temp2
	lda #$5c
	sta temp1

:	lda control_pad					; B button check/routine
	eor control_old
	and control_pad
	and #b_punch					; One more B button routine to test for 
	beq :+							; the user trying to exit the screen while
	jmp title_screen				; the delay is going on.
:	lda hscroll
	sta $2005
	lda vscroll
	sta $2005
	jsr wait_for_nmi
	dec temp1
	bne :--
@this_nmi:
	lda hscroll
	sta $2005
	lda vscroll
	sta $2005
	jsr wait_for_nmi
	jmp @loopz
; *********************************************************
; Below is the routine to load the sound test and loop it *
; *********************************************************
sound_screen:
	lda #$00						; DUD B reset for going back to the title
	sta temp2						; screen.
	jsr vblank_wait
	jsr PPU_off

	ldy #$00						; Fill the tracks nametable
	ldx #$04
	lda #<soundtest
	sta $f0
	lda #>soundtest
	sta $f1
	jsr fill_nametable
	jsr vblank_wait
	lda #$97						; Move the menu sprites used on the
	sta top_left_cursor				; title screen to the new position to
	lda #$97						; be used on the BGM test screen.
	sta top_right_cursor
	lda #$37
	sta top_left_cursor+3
	lda #$3f
	sta top_right_cursor+3
	jsr stabilize
	jsr PPU_with_sprites
	lda #%11100000					; Shut off the music when first coming
	sta $4015						; to the sound screen. 'no_music' is tested
	lda #$01						; in NMI.
	sta no_music
sound_loop:
	lda control_pad					; Up button check/routine
	eor control_old
	and control_pad
	and #up_punch					; Up/Down/Select are all used to move the
	beq :++							; menu selection sprites. It's pretty
	lda top_left_cursor				; much the same as on the title screen.
	cmp #$97
	bne :+
	lda #$c7
	sta top_left_cursor
	sta top_right_cursor
	jmp @controls_done
:	sec
	lda top_left_cursor
	sbc #$10
	sta top_left_cursor
	sec
	lda top_right_cursor
	sbc #$10
	sta top_right_cursor
:	lda control_pad					; Down button check/routine
	eor control_old
	and control_pad
	and #down_punch
	beq :++
	lda top_left_cursor
	cmp #$c7
	bne :+
	lda #$97
	sta top_left_cursor
	sta top_right_cursor
	jmp @controls_done
:	clc
	lda #$10
	adc top_left_cursor
	sta top_left_cursor
	clc
	lda #$10
	adc top_right_cursor
	sta top_right_cursor
:	lda control_pad					; Select button check/routine
	eor control_old
	and control_pad
	and #select_punch
	beq :++
	lda top_left_cursor
	cmp #$c7
	bne :+
	lda #$97
	sta top_left_cursor
	sta top_right_cursor
	jmp @controls_done
:	clc
	lda #$10
	adc top_left_cursor
	sta top_left_cursor
	clc
	lda #$10
	adc top_right_cursor
	sta top_right_cursor	
:	lda control_pad					; B button check/routine
	eor control_old
	and control_pad
	and #b_punch					; If the user goes back to the title screen,
	beq :+							; turn the music back on and load the title
	lda #$00						; screen track.
	sta no_music
	jsr song_first
	jmp	title_screen
:	lda control_pad					; A button check/routine
	eor control_old
	and control_pad
	and #a_punch					; Test where the cursor is at when A is
	beq @controls_done				; pressed, and play the song that is listed
	lda top_left_cursor				; next to the cursor.
	cmp #$97						; Load the first song...
	bne :+
	lda #$00
	sta no_music
	jsr song_first
:	lda top_left_cursor				; ... load the second song...
	cmp #$a7
	bne :+
	lda #$00
	sta no_music
	jsr song_second
:	lda top_left_cursor				; ... load the third song...
	cmp #$b7
	bne :+
	lda #$00
	sta no_music
	jsr song_third
:	lda top_left_cursor				; ... load the fourth song.
	cmp #$c7
	bne @controls_done
	lda #$00
	sta no_music
	jsr song_fourth
	
@controls_done:						; Loop!
	jsr wait_for_nmi
	jmp sound_loop

; *********************************************************
; Below is the routine to load the egg and loop it        *
; *********************************************************
egg_screen:
	lda #$00						; DUB B reset stuff
	sta temp2
	jsr song_third					; Load the song for this screen
	jsr vblank_wait
	jsr PPU_off

	ldy #$00						; Nametable filling
	ldx #$04
	lda #<pegegg
	sta $f0
	lda #>pegegg
	sta $f1
	jsr fill_nametable
	jsr vblank_wait
	jsr stabilize
	jsr PPU_no_sprites
loop_eggy:
	lda control_pad					; B button check/routine
	eor control_old
	and control_pad					; When the user decides they are done with
	and #b_punch					; the egg screen and they press B, just
	beq :+							; reset the game.
	jmp	reset
:	jsr wait_for_nmi
	jmp loop_eggy

; *********************************************************
; Load the manual screen and send it into a loop          *
; *********************************************************
manual_screen:						; Same kid of stuff as before.
	jsr PPU_off
	jsr vblank_wait
	ldy #$00
	ldx #$04
	lda #<manual
	sta $f0
	lda #>manual
	sta $f1
	jsr fill_nametable
	jsr vblank_wait
	jsr stabilize
	jsr PPU_no_sprites
manual_loop:
	lda control_pad					; B button check/routine
	eor control_old
	and control_pad
	and #b_punch					; Go back to the title screen
	beq :+
	jmp	title_screen
:	jsr wait_for_nmi
	jmp manual_loop

; *********************************************************
; Here we actually load the playing field of the game     *
; *********************************************************
load_pegs_board:
	jsr vblank_wait
	jsr PPU_off

	ldy #$00
	ldx #$04
	lda #<pegs_board
	sta $f0
	lda #>pegs_board
	sta $f1
	jsr fill_nametable

	jsr vblank_wait
; *********************************************************
; A quick hack to clarify stuff. This just says "Press    *
; Start To Begin Again". It's getting towards the end of  *
; the project, and I really just want to get this         *
; finished up.                                            *
; *********************************************************
	lda #$20
	sta $2006
	lda #$76
	sta $2006
	lda #$00
	sta $2007
	sta $2007
	sta $2007
	sta $2007
	sta $2007
	lda #$20
	sta $2006
	lda #$96
	sta $2006
	lda #$00
	sta $2007
	sta $2007
	sta $2007
	sta $2007
	sta $2007
	lda #$20
	sta $2006
	lda #$b7
	sta $2006
	lda #$00
	sta $2007
	sta $2007
	lda #$20
	sta $2006
	lda #$d6
	sta $2006
	lda #$00
	sta $2007
	sta $2007
	sta $2007
	sta $2007
	sta $2007
	lda #$20
	sta $2006
	lda #$f6
	sta $2006
	lda #$00
	sta $2007
	sta $2007
	sta $2007
	sta $2007
	sta $2007

	ldx #$0							; Pull in bytes for sprites and their
:	lda the_sprites,x				; attributes which are stored in
	sta top_left_cursor,x			; 'the_sprites'. Use X as an index
	inx								; to load and store each byte, which
    cpx #$30						; get stored starting in $200, where
    bne :-							; 'top_left_cursor' is located at. This 
	jsr stabilize					; is for the cursor to select an empty slot.
	jsr PPU_with_sprites

	lda #$0f						; Jeremiah saying "Choose a slot!"
	sta $4010
	lda #$3f
	sta $4011
    lda #$a4
	sta $4012
	lda #$8f
	sta $4013
	lda #$0f
	sta $4015
	lda #$1f
	sta $4015
	jsr delay_with_nmi_count
wait_input:
	jsr pegs_controls
	lda control_pad					; A button check/routine
	eor control_old
	and control_pad
	and #a_punch
	beq :+
	bne :++
:	jmp @no_input
:	lda top_left_cursor				; Test for top-most to be empty
	cmp #$1f
	bne :+
	lda top_left_cursor+3
	cmp #$78
	bne :+
	lda top_board
	eor #%01000000
	sta top_board
	jmp test_empty_slot
:	lda top_left_cursor				; Test for second row down, right
	cmp #$3f						; peg to be empty
	bne :+
	lda top_left_cursor+3
	cmp #$88
	bne :+
	lda top_board
	eor #%00010000
	sta top_board
	jmp test_empty_slot
:	lda top_left_cursor				; Test for second row down, left
	cmp #$3f						; peg to be empty
	bne :+
	lda top_left_cursor+3
	cmp #$68
	bne :+
	lda top_board
	eor #%00100000
	sta top_board
	jmp test_empty_slot
:	lda top_left_cursor				; Test for third row down, left
	cmp #$5f						; peg to be empty
	bne :+
	lda top_left_cursor+3
	cmp #$58
	bne :+
	lda top_board
	eor #%00001000
	sta top_board
	jmp test_empty_slot
:	lda top_left_cursor				; Test for third row down, middle
	cmp #$5f						; peg to be empty
	bne :+
	lda top_left_cursor+3
	cmp #$78
	bne :+
	lda top_board
	eor #%00000100
	sta top_board
	jmp test_empty_slot
:	lda top_left_cursor				; Test for third row down, right
	cmp #$5f						; peg to be empty
	bne :+
	lda top_left_cursor+3
	cmp #$98
	bne :+
	lda top_board
	eor #%00000010
	sta top_board
	jmp test_empty_slot
:	lda top_left_cursor				; Test for fourth row down, left
	cmp #$7f						; peg to be empty
	bne :+
	lda top_left_cursor+3
	cmp #$48
	bne :+
	lda top_board
	eor #%00000001
	sta top_board
	jmp test_empty_slot
:	lda top_left_cursor				; Test for fourth row down, second
	cmp #$7f						; from left peg to be empty
	bne :+
	lda top_left_cursor+3
	cmp #$68
	bne :+
	lda bottom_board
	eor #%10000000
	sta bottom_board
	jmp test_empty_slot
:	lda top_left_cursor				; Test for fourth row down, second
	cmp #$7f						; from right peg to be empty
	bne :+
	lda top_left_cursor+3
	cmp #$88
	bne :+
	lda bottom_board
	eor #%01000000
	sta bottom_board
	jmp test_empty_slot
:	lda top_left_cursor				; Test for fourth row down, right
	cmp #$7f						; peg to be empty
	bne :+
	lda top_left_cursor+3
	cmp #$a8
	bne :+
	lda bottom_board
	eor #%00100000
	sta bottom_board
	jmp test_empty_slot
:	lda top_left_cursor				; Test for fifth row down, left
	cmp #$9f						; peg to be empty
	bne :+
	lda top_left_cursor+3
	cmp #$38
	bne :+
	lda bottom_board
	eor #%00010000
	sta bottom_board
	jmp test_empty_slot
:	lda top_left_cursor				; Test for fifth row down, second
	cmp #$9f						; from left peg to be empty
	bne :+
	lda top_left_cursor+3
	cmp #$58
	bne :+
	lda bottom_board
	eor #%00001000
	sta bottom_board
	jmp test_empty_slot
:	lda top_left_cursor				; Test for fifth row down, middle
	cmp #$9f						; peg to be empty
	bne :+
	lda top_left_cursor+3
	cmp #$78
	bne :+
	lda bottom_board
	eor #%00000100
	sta bottom_board
	jmp test_empty_slot
:	lda top_left_cursor				; Test for fifth row down, second
	cmp #$9f						; from right peg to be empty
	bne :+
	lda top_left_cursor+3
	cmp #$98
	bne :+
	lda bottom_board
	eor #%00000010
	sta bottom_board
	jmp test_empty_slot
:	lda top_left_cursor				; Test for fifth row down, right
	cmp #$9f						; peg to be empty
	bne :+
	lda top_left_cursor+3
	cmp #$b8
	bne :+
	lda bottom_board
	eor #%00000001
	sta bottom_board
	jmp test_empty_slot
:
@no_input:
	jsr wait_for_nmi
	jmp wait_input

test_empty_slot:

	lda #$f8
	sta top_left_cursor
	sta top_right_cursor
	sta bot_left_cursor
	sta bot_right_cursor
	lda #$00
	sta top_left_cursor+3
	sta top_right_cursor+3
	sta bot_left_cursor+3
	sta bot_right_cursor+3
	jsr vblank_wait

; *********************************************************
; Hackish code follows                                    *
; *********************************************************
	lda #$20
	sta $2006
	lda #$63
	sta $2006
	lda #$00
	sta $2007
	sta $2007
	sta $2007
	sta $2007
	sta $2007
	sta $2007
	lda #$20
	sta $2006
	lda #$a4
	sta $2006
	lda #$00
	sta $2007
	sta $2007
	sta $2007
	sta $2007
	sta $2007
	sta $2007
	sta $2007
	sta $2007
; *********************************************************
; /END hackish code (besides the rest of the code that is)*
; *********************************************************
	jsr stabilize
	jsr PPU_with_sprites
	jsr PPU_off
	lda top_board
	sta temp1
	rol top_board
	rol top_board
	bcs :+
	ldx #$00						; Load X and Y to fill the top
	ldy #$00						; peg with a peg. Subroutine
	jsr peg_in_20					; peg_in_20 for $2006 address
	ldx #$07
	jsr sq1_sfx
	jsr routines_betwixt
:	rol top_board
	bcs :+
	ldx #$04						; Load X and Y to fill the second
	ldy #$00						; left peg with a peg. Subroutine
	jsr peg_in_21					; peg_in_21 for $2006 address
	ldx #$07
	jsr sq1_sfx
	jsr routines_betwixt
:	rol top_board
	bcs :+
	ldx #$08						; Load X and Y to fill the second
	ldy #$00						; right peg with a peg. Subroutine
	jsr peg_in_21					; peg_in_21 for $2006 address
	ldx #$07
	jsr sq1_sfx
	jsr routines_betwixt
:	rol top_board
	bcs :+
	ldx #$0c						; Load X and Y to fill the third
	ldy #$00						; left peg with a peg. Subroutine
	jsr peg_in_21					; peg_in_21 for $2006 address
	ldx #$07
	jsr sq1_sfx
	jsr routines_betwixt
:	rol top_board
	bcs :+
	ldx #$10						; Load X and Y to fill the third
	ldy #$00						; middle peg with a peg. Subroutine
	jsr peg_in_21					; peg_in_21 for $2006 address
	ldx #$07
	jsr sq1_sfx
	jsr routines_betwixt
:	rol top_board
	bcs :+
	ldx #$14						; Load X and Y to fill the third
	ldy #$00						; right peg with a peg. Subroutine
	jsr peg_in_21					; peg_in_21 for $2006 address
	ldx #$07
	jsr sq1_sfx
	jsr routines_betwixt
:	rol top_board
	bcs :+
	ldx #$18						; Load X and Y to fill the fourth
	ldy #$00						; left peg with a peg. Subroutine
	jsr peg_in_22					; peg_in_22 for $2006 address
	ldx #$07
	jsr sq1_sfx
	jsr routines_betwixt
:	lda temp1
	sta top_board
;	jsr vblank_wait
	lda bottom_board
	sta temp1
	rol bottom_board
	bcs :+
	ldx #$1c						; Load X and Y to fill the fourth
	ldy #$00						; second peg with a peg. Subroutine
	jsr peg_in_22					; peg_in_22 for $2006 address
	ldx #$07
	jsr sq1_sfx
	jsr routines_betwixt
:	rol bottom_board
	bcs :+	
	ldx #$20						; Load X and Y to fill the fourth
	ldy #$00						; third peg with a peg. Subroutine
	jsr peg_in_22					; peg_in_22 for $2006 address
	ldx #$07
	jsr sq1_sfx
	jsr routines_betwixt
:	rol bottom_board
	bcs :+
	ldx #$24						; Load X and Y to fill the fourth
	ldy #$00						; right peg with a peg. Subroutine
	jsr peg_in_22					; peg_in_22 for $2006 address
	ldx #$07
	jsr sq1_sfx
	jsr routines_betwixt
:	rol bottom_board
	bcs :+
	ldx #$28						; Load X and Y to fill the fifth
	ldy #$00						; left peg with a peg. Subroutine
	jsr peg_in_22					; peg_in_22 for $2006 address
	ldx #$07
	jsr sq1_sfx
	jsr routines_betwixt
:	rol bottom_board
	bcs :+
	ldx #$0c						; Load X and Y to fill the fifth
	ldy #$00						; second peg with a peg. Subroutine
	jsr peg_in_22					; peg_in_22 for $2006 address
	ldx #$07
	jsr sq1_sfx
	jsr routines_betwixt
:	rol bottom_board
	bcs :+
	ldx #$10						; Load X and Y to fill the fifth
	ldy #$00						; middle peg with a peg. Subroutine
	jsr peg_in_22					; peg_in_22 for $2006 address
	ldx #$07
	jsr sq1_sfx
	jsr routines_betwixt
:	rol bottom_board
	bcs :+
	ldx #$14						; Load X and Y to fill the fifth
	ldy #$00						; fourth peg with a peg. Subroutine
	jsr peg_in_22					; peg_in_22 for $2006 address
	ldx #$07
	jsr sq1_sfx
	jsr routines_betwixt
:	rol bottom_board
	bcs :+
	ldx #$2c						; Load X and Y to fill the fifth
	ldy #$00						; right peg with a peg. Subroutine
	jsr peg_in_22					; peg_in_22 for $2006 address
	ldx #$07
	jsr sq1_sfx
	jsr routines_betwixt
:	lda temp1
	sta bottom_board

	jsr stabilize
	jsr PPU_with_sprites

	jsr delay_with_nmi_count
	jsr vblank_wait

	ldx #$0							; Pull in bytes for sprites to say
:	lda the_sprites+$38,x			; 3
	sta top_left_cursor,x
	inx
	cpx #$10
	bne :-							
	lda #%00000100
	sta $4015
	ldx #$04
	jsr triangle_sfx

	jsr delay_with_nmi_count
	jsr vblank_wait
	ldx #$00							; Pull in bytes for sprites to say
:	lda the_sprites+$48,x				; 2
	sta top_left_cursor,x
	inx
	cpx #$10
	bne :-
	ldx #%00000100
	stx $4015
	ldx #$04
	jsr triangle_sfx

	jsr delay_with_nmi_count
	jsr vblank_wait
	ldx #$0								; Pull in bytes for sprites to say
:	lda the_sprites+$58,x				; 1
	sta top_left_cursor,x
	inx
	cpx #$10
	bne :-
	ldx #%00000100
	stx $4015
	ldx #$04
	jsr triangle_sfx

	jsr delay_with_nmi_count
	jsr vblank_wait
	ldx #$0								; Pull in bytes for sprites to say
:	lda the_sprites+$68,x				; GO!
	sta top_left_cursor,x
	inx
	cpx #$10
	bne :-

	lda #$0f						; Jeremiah saying "Go!"
	sta $4010
	lda #$3f
	sta $4011
    lda #$c8
	sta $4012
	lda #$3f
	sta $4013
	lda #$0f
	sta $4015
	lda #$1f
	sta $4015

	jsr delay_with_nmi_count
	jsr vblank_wait
	jsr PPU_off
	lda #$20
	sta $2006
	lda #$76
	sta $2006
	ldx #$00
	lda press_word
@press:
	sta $2007
	inx
	lda press_word,x
	bne @press

	lda #$20
	sta $2006
	lda #$96
	sta $2006
	ldx #$00
	lda start_word
@start:
	sta $2007
	inx
	lda start_word,x
	bne @start

	lda #$20
	sta $2006
	lda #$b7
	sta $2006
	ldx #$00
	lda to_word
@to:
	sta $2007
	inx
	lda to_word,x
	bne @to

	lda #$20
	sta $2006
	lda #$d6
	sta $2006
	ldx #$00
	lda begin_word
@begin:
	sta $2007
	inx
	lda begin_word,x
	bne @begin

	lda #$20
	sta $2006
	lda #$f6
	sta $2006
	ldx #$00
	lda again_word
@again:
	sta $2007
	inx
	lda again_word,x
	bne @again
	jsr stabilize
	jsr PPU_with_sprites

	jsr song_second
	jsr vblank_wait
	ldx #$0							; Pull in bytes for sprites to be the main
:	lda the_sprites,x				; cursor and the like
	sta top_left_cursor,x
	inx
	cpx #$10
	bne :-
	lda #%00001111
	sta $4015
	lda #$00
	sta no_music

; *********************************************************
; The first thing we do in the main game loop is test the *
; board to see if any more moves are available            *
; *********************************************************
pegs_loop:
	lda top_board					;   0  <--- Top Peg
	and #%01101000					;  0
	cmp #$08						; 1
	beq :+
	bne :++
:	jmp @control_stuff
:	lda top_board					;   1  <--- Top Peg
	and #%01101000					;  0
	cmp #$40						; 0
	beq :+
	bne :++
:	jmp @control_stuff
:	lda top_board					;   0  <--- Second Left Peg
	and #%00101001					;  0
	cmp #$01						; 1
	beq :+
	bne :++
:	jmp @control_stuff
:	lda top_board					;   1  <--- Second Left Peg
	and #%00101001					;  0
	cmp #$20						; 0
	beq :+
	bne :++
:	jmp @control_stuff
:	lda top_board					;   0  <--- Third Left Peg
	and #%00001001					;  0
	cmp #$00						; 1
	beq :+
	bne :+++
:	lda bottom_board
	and #%00010000
	cmp #$10
	beq :+
	bne :++
:	jmp @control_stuff
:	lda top_board					;   1  <--- Third Left Peg
	and #%00001001					;  0
	cmp #$08						; 0
	beq :+
	bne :+++
:	lda bottom_board
	and #%00010000
	cmp #$00
	beq :+
	bne :++
:	jmp @control_stuff
:	lda top_board					;   1  <--- Second Right Peg
	and #%00010100					;  0
	cmp #$10						; 0
	beq :+
	bne :+++
:	lda bottom_board
	and #%10000000
	cmp #$00
	beq :+
	bne :++
:	jmp @control_stuff
:	lda top_board					;   0  <--- Second Right Peg
	and #%00010100					;  0
	cmp #$00						; 1
	beq :+
	bne :+++
:	lda bottom_board
	and #%10000000
	cmp #$80
	beq :+
	bne :++
:	jmp @control_stuff
:	lda top_board					;   1  <--- Third Middle Peg
	and #%00000100					;  0
	cmp #$04						; 0
	beq :+
	bne :+++
:	lda bottom_board
	and #%10001000
	cmp #$00
	beq :+
	bne :++
:	jmp @control_stuff
:	lda top_board					;   0  <--- Third Middle Peg
	and #%00000100					;  0
	cmp #$00						; 1
	beq :+
	bne :+++
:	lda bottom_board
	and #%10001000
	cmp #$08
	beq :+
	bne :++
:	jmp @control_stuff
:	lda top_board					;   1  <--- Third Right Peg
	and #%00000010					;  0
	cmp #$02						; 0
	beq :+
	bne :+++
:	lda bottom_board
	and #%01000100
	cmp #$00
	beq :+
	bne :++
:	jmp @control_stuff
:	lda top_board					;   0  <--- Third Right Peg
	and #%00000010					;  0
	cmp #$00						; 1
	beq :+
	bne :+++
:	lda bottom_board
	and #%01000100
	cmp #$04
	beq :+
	bne :++
:	jmp @control_stuff
:	lda top_board					;   1  <--- Top Peg
	and #%01010010					;    0
	cmp #$40						;     0
	beq :+
	bne :++
:	jmp @control_stuff
:	lda top_board					;   0  <--- Top Peg
	and #%01010010					;    0
	cmp #$02						;     1
	beq :+
	bne :++
:	jmp @control_stuff
:	lda top_board					;   1  <--- Second Right Peg
	and #%00010010					;    0
	cmp #$10						;     0
	beq :+
	bne :+++
:	lda bottom_board
	and #%00100000
	cmp #$00
	beq :+
	bne :++
:	jmp @control_stuff
:	lda top_board					;   0  <--- Second Right Peg
	and #%00010010					;    0
	cmp #$00						;     1
	beq :+
	bne :+++
:	lda bottom_board
	and #%00100000
	cmp #$20
	beq :+
	bne :++
:	jmp @control_stuff
:	lda top_board					;   1  <--- Third Right Peg
	and #%00000010					;    0
	cmp #$02						;     0
	beq :+
	bne :+++
:	lda bottom_board
	and #%00100001
	cmp #$00
	beq :+
	bne :++
:	jmp @control_stuff
:	lda top_board					;   0  <--- Third Right Peg
	and #%00000010					;    0
	cmp #$00						;     1
	beq :+
	bne :+++
:	lda bottom_board
	and #%00100001
	cmp #$01
	beq :+
	bne :++
:	jmp @control_stuff
:	lda top_board					;   1  <--- Second Left Peg
	and #%00100100					;    0
	cmp #$20						;     0
	beq :+
	bne :+++
:	lda bottom_board
	and #%01000000
	cmp #$00
	beq :+
	bne :++
:	jmp @control_stuff
:	lda top_board					;   0  <--- Second Left Peg
	and #%00100100					;    0
	cmp #$00						;     1
	beq :+
	bne :+++
:	lda bottom_board
	and #%01000000
	cmp #$40
	beq :+
	bne :++
:	jmp @control_stuff
:	lda top_board					;   1  <--- Third Middle Peg
	and #%00000100					;    0
	cmp #$04						;     0
	beq :+
	bne :+++
:	lda bottom_board
	and #%01000010
	cmp #$00
	beq :+
	bne :++
:	jmp @control_stuff
:	lda top_board					;   0  <--- Third Middle Peg
	and #%00000100					;    0
	cmp #$00						;     1
	beq :+
	bne :+++
:	lda bottom_board
	and #%01000010
	cmp #$02
	beq :+
	bne :++
:	jmp @control_stuff
:	lda top_board					;   1  <--- Third Left Peg
	and #%00001000					;    0
	cmp #$08						;     0
	beq :+
	bne :+++
:	lda bottom_board
	and #%10000100
	cmp #$00
	beq :+
	bne :++
:	jmp @control_stuff
:	lda top_board					;   0  <--- Third Left Peg
	and #%00001000					;    0
	cmp #$00						;     1
	beq :+
	bne :+++
:	lda bottom_board
	and #%10000100
	cmp #$04
	beq :+
	bne :++
:	jmp @control_stuff
:	lda top_board					;   1 0 0
	and #%00001110					;   ^------- Third Left Peg
	cmp #$08
	beq :+
	bne :++
:	jmp @control_stuff
:	lda top_board					;   0 0 1
	and #%00001110					;   ^------- Third Left Peg
	cmp #$02
	beq :+
	bne :++
:	jmp @control_stuff
:	lda top_board					;   1 0 0
	and #%00000001					;   ^------- Fourth Left Peg
	cmp #$01
	beq :+
	bne :+++
:	lda bottom_board
	and #%11000000
	cmp #$00
	beq :+
	bne :++
:	jmp @control_stuff
:	lda top_board					;   0 0 1
	and #%00000001					;   ^------- Fourth Left Peg
	cmp #$00
	beq :+
	bne :+++
:	lda bottom_board
	and #%11000000
	cmp #$40
	beq :+
	bne :++
:	jmp @control_stuff
:	lda bottom_board				;   1 0 0
	and #%11100000					;   ^------- Fourth Second Peg
	cmp #$80
	beq :+
	bne :++
:	jmp @control_stuff
:	lda bottom_board				;   0 0 1
	and #%11100000					;   ^------- Fourth Second Peg
	cmp #$20
	beq :+
	bne :++
:	jmp @control_stuff
:	lda bottom_board				;   1 0 0
	and #%00011100					;   ^------- Fifth Left Peg
	cmp #$10
	beq :+
	bne :++
:	jmp @control_stuff
:	lda bottom_board				;   0 0 1
	and #%00011100					;   ^------- Fifth Left Peg
	cmp #$04
	beq :+
	bne :++
:	jmp @control_stuff
:	lda bottom_board				;   1 0 0
	and #%00001110					;   ^------- Fifth Second Peg
	cmp #$08
	beq :+
	bne :++
:	jmp @control_stuff
:	lda bottom_board				;   0 0 1
	and #%00001110					;   ^------- Fifth Second Peg
	cmp #$02
	beq :+
	bne :++
:	jmp @control_stuff
:	lda bottom_board				;   1 0 0
	and #%00000111					;   ^------- Fifth Middle Peg
	cmp #$04
	beq :+
	bne :++
:	jmp @control_stuff
:	lda bottom_board				;   0 0 1
	and #%00000111					;   ^------- Fifth Middle Peg
	cmp #$01
	beq :+
	bne :++

; *********************************************************
; If all of the tests fail, that means there are no more  *
; moves for us to make on the board. This is a quick set  *
; up to go towards the ending screen(s)                   *
; *********************************************************

:	jmp @control_stuff
:	lda #$01
	sta no_music
	ldx #%00000001
	stx $4015
	ldx #$00
	stx top_board
	stx bottom_board
	jsr sq1_sfx
	jsr delay_with_nmi_count
	jsr delay_with_nmi_count
	jmp game_done

; *********************************************************
; If at least one test is passed, this means there are    *
; still some moves we can make. This is where it picks up *
; after all testing                                       *
; *********************************************************
@control_stuff:
	jsr pegs_controls
	lda control_pad					; B button check/routine
	eor control_old
	and control_pad
	and #start_punch
	beq :+
	lda #$01
	sta no_music
	ldx #%00000001
	stx $4015
	ldx #$00
	stx top_board
	stx bottom_board
	ldx #$0b
	jsr sq1_sfx
	jsr delay_with_nmi_count
	jmp load_pegs_board
:	lda control_pad					; B button check/routine
	eor control_old
	and control_pad
	and #b_punch
	beq :+
	jsr no_place_latch
	jmp @nmi_jump
:	lda control_pad					; A button check/routine
	eor control_old
	and control_pad
	and #a_punch					; If A is pushed, go down below and find
	beq :+							; out where the user's cursor was located
	bne :++							; when they pushed the button.
:	jmp @nmi_jump
:
	lda top_left_cursor				; Begin testing of cursor locations
	cmp #$1f
	bne @go_second_left
	lda top_left_cursor+3
	cmp #$78
	bne @go_second_left
	lda top_board					; If a peg is empty, don't allow the button
	and #%01000000					; to take action.
	bne @go_second_left
	jsr place_placeholder			; If it does, this is where we hold the peg in
	jsr third_left_cursor			; place and jump to the corresponding routine
	jsr delay_with_nmi_count2		; WAY down in the code. The same thing happens
	jmp top_peg_held				; in each test from here down, so this will
@go_second_left:					; be the only comment on how this portion works.
	lda top_left_cursor
	cmp #$3f
	bne @go_second_right
	lda top_left_cursor+3
	cmp #$68
	bne @go_second_right
	lda top_board
	and #%00100000
	bne @go_second_right
	jsr place_placeholder
	jsr fourth_left_cursor
	jsr delay_with_nmi_count2
	jmp second_left_peg_held
@go_second_right:
	lda top_left_cursor
	cmp #$3f
	bne @go_third_left
	lda top_left_cursor+3
	cmp #$88
	bne @go_third_left
	lda top_board
	and #%00010000
	bne @go_third_left
	jsr place_placeholder
	jsr fourth_second_cursor
	jsr delay_with_nmi_count2
	jmp second_right_peg_held
@go_third_left:
	lda top_left_cursor
	cmp #$5f
	bne @go_third_middle
	lda top_left_cursor+3
	cmp #$58
	bne @go_third_middle
	lda top_board
	and #%00001000
	bne @go_third_middle
	jsr place_placeholder
	jsr fifth_left_cursor
	jsr delay_with_nmi_count2
	jmp third_left_peg_held
@go_third_middle:
	lda top_left_cursor
	cmp #$5f
	bne @go_third_right
	lda top_left_cursor+3
	cmp #$78
	bne @go_third_right
	lda top_board
	and #%00000100
	bne @go_third_right
	jsr place_placeholder
	jsr fifth_second_cursor
	jsr delay_with_nmi_count2
	jmp third_middle_peg_held
@go_third_right:
	lda top_left_cursor
	cmp #$5f
	bne @go_fourth_left
	lda top_left_cursor+3
	cmp #$98
	bne @go_fourth_left
	lda top_board
	and #%00000010
	bne @go_fourth_left
	jsr place_placeholder
	jsr fifth_middle_cursor
	jsr delay_with_nmi_count2
	jmp third_right_peg_held
@go_fourth_left:
	lda top_left_cursor
	cmp #$7f
	bne @go_fourth_second
	lda top_left_cursor+3
	cmp #$48
	bne @go_fourth_second
	lda top_board
	and #%00000001
	bne @go_fourth_second
	jsr place_placeholder
	jsr second_left_cursor
	jsr delay_with_nmi_count2
	jmp fourth_left_peg_held
@go_fourth_second:
	lda top_left_cursor
	cmp #$7f
	bne @go_fourth_third
	lda top_left_cursor+3
	cmp #$68
	bne @go_fourth_third
	lda bottom_board
	and #%10000000
	bne @go_fourth_third
	jsr place_placeholder
	jsr second_right_cursor
	jsr delay_with_nmi_count2
	jmp fourth_second_peg_held
@go_fourth_third:
	lda top_left_cursor
	cmp #$7f
	bne @go_fourth_right
	lda top_left_cursor+3
	cmp #$88
	bne @go_fourth_right
	lda bottom_board
	and #%01000000
	bne @go_fourth_right
	jsr place_placeholder
	jsr second_left_cursor
	jsr delay_with_nmi_count2
	jmp fourth_third_peg_held
@go_fourth_right:
	lda top_left_cursor
	cmp #$7f
	bne @go_fifth_left
	lda top_left_cursor+3
	cmp #$a8
	bne @go_fifth_left
	lda bottom_board
	and #%00100000
	bne @go_fifth_left
	jsr place_placeholder
	jsr second_right_cursor
	jsr delay_with_nmi_count2
	jmp fourth_right_peg_held
@go_fifth_left:
	lda top_left_cursor
	cmp #$9f
	bne @go_fifth_second
	lda top_left_cursor+3
	cmp #$38				; HERE ROB
	bne @go_fifth_second
	lda bottom_board
	and #%00010000
	bne @go_fifth_second
	jsr place_placeholder
	jsr third_left_cursor
	jsr delay_with_nmi_count2
	jmp fifth_left_peg_held
@go_fifth_second:
	lda top_left_cursor
	cmp #$9f
	bne @go_fifth_middle
	lda top_left_cursor+3
	cmp #$58
	bne @go_fifth_middle
	lda bottom_board
	and #%00001000
	bne @go_fifth_middle
	jsr place_placeholder
	jsr third_middle_cursor
	jsr delay_with_nmi_count2
	jmp fifth_second_peg_held
@go_fifth_middle:
	lda top_left_cursor
	cmp #$9f
	bne @go_fifth_fourth
	lda top_left_cursor+3
	cmp #$78
	bne @go_fifth_fourth
	lda bottom_board
	and #%00000100
	bne @go_fifth_fourth
	jsr place_placeholder
	jsr third_left_cursor
	jsr delay_with_nmi_count2
	jmp fifth_middle_peg_held
@go_fifth_fourth:
	lda top_left_cursor
	cmp #$9f
	bne @go_fifth_right
	lda top_left_cursor+3
	cmp #$98
	bne @go_fifth_right
	lda bottom_board
	and #%00000010
	bne @go_fifth_right
	jsr place_placeholder
	jsr third_middle_cursor
	jsr delay_with_nmi_count2
	jmp fifth_fourth_peg_held
@go_fifth_right:
	lda top_left_cursor
	cmp #$9f
	bne @nmi_jump
	lda top_left_cursor+3
	cmp #$b8
	bne @nmi_jump
	lda bottom_board
	and #%00000001
	bne @nmi_jump
	jsr place_placeholder
	jsr third_right_cursor
	jsr delay_with_nmi_count2
	jmp fifth_right_peg_held
@nmi_jump:
	jsr wait_for_nmi
	jmp pegs_loop

; *********************************************************
; Here we have some routines that we will be using        *
; throughout the program.                                 *
; *********************************************************
PPU_off:							; Used to shut the PPU
	lda #$00						; off.
	sta $2000
	sta $2001
	rts

PPU_with_sprites:					; We use this to turn
	lda #%10000000					; on the PPU with sprites
	sta $2000						; enabled.
	lda #%00011110
	sta $2001
	rts

PPU_no_sprites:						; We use this to turn the
	lda #%10000000					; PPU on and have sprites
	sta $2000						; disabled.
	lda #%00001110
	sta $2001
	rts

stabilize:							; This routine seemed to work
	lda #$00						; better if I commented out
	sta $2006						; the two $2005 writes, and
	sta $2006						; got rid of the $2006 writes
;	sta $2005						; at the end of NMI. I'm not
;	sta $2005						; sure why it's better, but
	rts								; it ended up being so.

wait_for_nmi:						; A routine that we can use
	lda nmi_num						; in the main loop to make 
@check:								; sure we are doing a frame
	cmp nmi_num						; through each loop.
	beq @check
	rts

delay_with_nmi_count:				; This routine allows us to 
	clc								; add a one second delay (NTSC)
	lda nmi_num						; for whatever we need. Controls
	adc #$3c						; are not accessible while in
	sta temp1						; this loop, as the controls are
@wait:								; set up in the main loop, and they
	lda nmi_num						; are only strobed in NMI. If we
	cmp temp1						; wanted more or less of a delay,
	bne @wait						; the value #$3c could be changed.
	rts								; That would require another routine,
									; so maybe a macro could help.
delay_with_nmi_count2:				; This routine is basically the same
	clc								; as the above one, but it only delays
	lda nmi_num						; for 1/60th of a second. This routine
	adc #$01						; is used in cases where we switch
	sta temp1						; screens, and control schemes, and we
@wait:								; don't want the controls to overlap
	lda nmi_num						; into the next control scheme.
	cmp temp1
	bne @wait
	rts
delay_with_nmi_count3:				; This routine is used to lay the pegs
	clc								; sequentially. It is just a delay to 
	lda nmi_num						; put in between each one.
	adc #$08
	sta temp2
@wait:
	lda nmi_num
	cmp temp2
	bne @wait
	rts

vblank_wait:						; I think we all know what this is!
:	bit $2002
	bpl :-
	rts

fill_nametable:						; This is to fill up the background
	lda #$20						; You must first load X to 0 and Y
	sta $2006						; to 4 to get the whole nametable
	lda #$00						; to fill using this method.
	sta $2006
:	lda ($f0),y
	sta $2007
	iny
	bne :-
	inc $f1
	dex
	bne :-
	rts
clear_nametable:					; A routine to simply clear
	lda #$20						; the entire nametable.
	sta $2006
	lda #$00
	sta $2006
	lda #$00
	ldx #$00
:	sta $2007
	sta $2007
	sta $2007
	sta $2007
	dex
	bne :-
	rts
moves_routine:						; This is for keeping track of the
	lda right_num_moves+1			; number of moves the player has
	cmp #$39						; made. First, checks the number
	beq :+							; on the right side to see if it's
	bne :++							; 9 (tile #$39). If it is, it changes
:	lda #$30						; to a zero, then bumps the left
	sta right_num_moves+1			; number up one. If it's not a 9,
	inc left_num_moves+1			; it just bumps the right side number
	rts								; up one.
:	inc right_num_moves+1
	rts
pegs_left_routine:					; This follows the same principle
	lda right_num_peg+1				; as moves_routine, but in reverse
	cmp #$30						; to keep track of the number of pegs
	beq :+							; left. These sprites are kept off
	bne :++							; screen, and are only shown during 
:	lda #$39						; the final ranking screen.
	sta right_num_peg+1
	dec left_num_peg+1
	rts
:	dec right_num_peg+1
	rts

song_first:
	lda #$00						; Load song 1
	ldx #$00
	jsr init
	lda #%10000000
	sta $2000
	rts
song_second:
	lda #$01						; Load song 2
	ldx #$00
	jsr init
	lda #%10000000
	sta $2000
	rts
song_third:
	lda #$02						; Load song 3
	ldx #$00
	jsr init
	lda #%10000000
	sta $2000
	rts
song_fourth:
	lda #$03						; Load song 4
	ldx #$00
	jsr init
	lda #%10000000
	sta $2000
	rts
routines_betwixt:					; These are a bunch of routines crammed
	jsr stabilize					; into one routine. We use this between
	jsr PPU_with_sprites			; each time we change a peg on the
	jsr delay_with_nmi_count3		; screen.
	jsr vblank_wait
	jsr PPU_off
	rts
sq1_sfx:							; This routine grabs the selected
	lda sound_fx,x					; bytes in the sound_fx table, as
	sta $4000						; assigned by X, and feeds them
	inx								; into each register of the first
	lda sound_fx,x					; square channel.
	sta $4001
	inx
	lda sound_fx,x
	sta $4002
	inx
	lda sound_fx,x
	sta $4003
	rts

triangle_sfx:		                ; This routine grabs the selected
	lda sound_fx,x                  ; bytes in the sound_fx table, as
	sta $4008                       ; assigned by X, and feeds them
	inx                             ; into each register of the
	lda sound_fx,x					; triangle channel.
	sta $400a
	inx
	lda sound_fx,x
	sta $400b
	rts
start_over:							; This is what routine to use when
	lda #$01						; the user decides to reset the board
	sta no_music					; and choose a new slot.
	ldx #%00000001
	stx $4015
	ldx #$00
	stx top_board
	stx bottom_board
	ldx #$0b
	jsr sq1_sfx
	jsr delay_with_nmi_count
	rts

; *********************************************************
; All the routines below for the held pegs are pretty     *
; much the exact same thing. I'll comment the first one   *
; pretty heavily, and the rest of them, well, the same    *
; comments can be applied there                           *
; *********************************************************
top_peg_held:
	lda control_pad					; Left button check/routine
	eor control_old
	and control_pad
	and #left_punch					; When left is pushed, jump to the corresponding
	beq :+							; routine that will move the cursor. This will
	jsr third_left_cursor			; also apply in the next one, the right button
									; check...
:	lda control_pad					; ... right button check/routine
	eor control_old
	and control_pad
	and #right_punch
	beq :+
	jsr third_right_cursor

:	lda control_pad					; Start button check/routine
	eor control_old					; Add in a routine that allows the user to 
	and control_pad					; press start while a peg is held in place.
	and #start_punch				; The start button will start the board over,
	beq :+							; allowing the user to choose a new empty slot.
	jsr start_over
	jmp load_pegs_board

:	lda control_pad					; B button check/routine
	eor control_old
	and control_pad					; This will allow the user to stop having the
	and #b_punch					; peg held that they do at the current time.
	beq :+
	jsr no_place_latch
	jmp pegs_loop

:	lda control_pad					; A button check/routine
	eor control_old
	and control_pad
	and #a_punch					; When A is pushed, we first have to test exactly
	beq :+							; where the cursor is on screen. In this case, if
	bne :++							; we are at #$58 in the X-pos of top_left_cursor,
:	jmp @to_nmi						; we will keep going on with the code. If it's not,
:	lda top_left_cursor+3			; however, we will jump to @right_position.
	cmp #$58
	bne :+
	beq :++
:	jmp @right_position
:	lda top_board					; Load the parts of the board that will need
	and #%00100000					; to be tested to see if we can make the move or
	bne :+							; not. As long as the space next to us has a peg, 
	beq :++							; and the one after that is empty, we can move on.
:	jmp @to_nmi						; referr back to the reserved bytes of top_board
:	lda top_board					; and bottom_board to see which bits correspond
	and #%00001000					; to the pegs on the board.
	beq :+
	bne :++
:	jmp @to_nmi
:	lda top_board					; If the test succeeds, then we eor all the bits
	eor #%01101000					; that are affected by the move we just made.
	sta top_board					; We then put the peg held latch stuff off screen. 
	jsr no_place_latch				; and change how the pegs look on the board.
	jsr vblank_wait
	jsr PPU_off
	ldx #$00						; Load X and Y to get rid of the
	ldy #$00						; top peg. no_peg_in_20 for $2006
	jsr no_peg_in_20				; address
	jsr routines_betwixt
	ldx #$04						; Load X and Y to get rid of the
	ldy #$00						; peg in second left slot. no_peg_in_21
	jsr no_peg_in_21				; for $2006 address
	jsr routines_betwixt
	ldx #$0c						; Load X and Y to put a peg in
	ldy #$00						; third left slot. peg_in_21 for
	jsr peg_in_21					; $2006 address
	jsr stabilize
	jsr PPU_with_sprites
	jsr moves_routine				; Increment the amount of moves made.
	jsr pegs_left_routine			; Decrement the amount of pegs left.
	jsr delay_with_nmi_count2		; Wait one extra frame so the buttons
	jmp pegs_loop					; don't overlap.
@right_position:
	lda top_board
	and #%00010000
	bne :+
	beq :++
:	jmp @to_nmi
:	lda top_board
	and #%00000010
	beq :+
	bne :++
:	jmp @to_nmi
:	lda top_board
	eor #%01010010
	sta top_board
	jsr no_place_latch
	jsr vblank_wait
	jsr PPU_off
	ldx #$00						; Load X and Y to get rid of the
	ldy #$00						; top peg. no_peg_in_20 for $2006
	jsr no_peg_in_20				; address
	jsr routines_betwixt
	ldx #$08						; Load X and Y to get rid of the
	ldy #$00						; peg in second right slot. no_peg_in_21
	jsr no_peg_in_21				; for $2006 address
	jsr routines_betwixt
	ldx #$14						; Load X and Y to put a peg in
	ldy #$00						; third right slot. peg_in_21 for
	jsr peg_in_21					; $2006 address
	jsr stabilize
	jsr PPU_with_sprites
	jsr moves_routine
	jsr pegs_left_routine
	jsr delay_with_nmi_count2
	jmp pegs_loop
@to_nmi:
	jsr wait_for_nmi
	jmp top_peg_held

second_left_peg_held:
	lda control_pad					; Left button check/routine
	eor control_old
	and control_pad
	and #left_punch
	beq :+
	jsr fourth_left_cursor

:	lda control_pad					; Right button check/routine
	eor control_old
	and control_pad
	and #right_punch
	beq :+
	jsr fourth_third_cursor

:	lda control_pad					; Start button check/routine
	eor control_old
	and control_pad
	and #start_punch
	beq :+
	jsr start_over
	jmp load_pegs_board

:	lda control_pad					; B button check/routine
	eor control_old
	and control_pad
	and #b_punch
	beq :+
	jsr no_place_latch
	jmp pegs_loop

:	lda control_pad					; A button check/routine
	eor control_old
	and control_pad
	and #a_punch
	beq :+
	bne :++
:	jmp @to_nmi
:	lda top_left_cursor+3
	cmp #$48
	bne :+
	beq :++
:	jmp @right_position
:	lda top_board
	and #%00001000
	bne :+
	beq :++
:	jmp @to_nmi
:	lda top_board
	and #%00000001
	beq :+
	bne :++
:	jmp @to_nmi
:	lda top_board
	eor #%00101001
	sta top_board
	jsr no_place_latch
	jsr vblank_wait
	jsr PPU_off
	ldx #$04						; Load X and Y to get rid of the
	ldy #$00						; second left peg. no_peg_in_21 for
	jsr no_peg_in_21				; $2006 address
	jsr routines_betwixt
	ldx #$0c						; Load X and Y to get rid of the
	ldy #$00						; peg in third left slot. no_peg_in_21
	jsr no_peg_in_21				; for $2006 address
	jsr routines_betwixt
	ldx #$18						; Load X and Y to put a peg in
	ldy #$00						; fourth left slot. peg_in_22 for
	jsr peg_in_22					; $2006 address
	jsr stabilize
	jsr PPU_with_sprites
	jsr moves_routine
	jsr pegs_left_routine
	jsr delay_with_nmi_count2
	jmp pegs_loop
@right_position:
	lda top_board
	and #%00000100
	bne :+
	beq :++
:	jmp @to_nmi
:	lda bottom_board
	and #%01000000
	beq :+
	bne :++
:	jmp @to_nmi
:	lda top_board
	eor #%00100100
	sta top_board
	lda bottom_board
	eor #%01000000
	sta bottom_board
	jsr no_place_latch
	jsr vblank_wait
	jsr PPU_off
	ldx #$04						; Load X and Y to get rid of the
	ldy #$00						; second left peg. no_peg_in_21 for
	jsr no_peg_in_21				; $2006 address
	jsr routines_betwixt
	ldx #$10						; Load X and Y to get rid of the
	ldy #$00						; peg in third middle slot. no_peg_in_21
	jsr no_peg_in_21				; for $2006 address
	jsr routines_betwixt
	ldx #$20						; Load X and Y to put a peg in
	ldy #$00						; fourth third slot. peg_in_22 for
	jsr peg_in_22					; $2006 address
	jsr stabilize
	jsr PPU_with_sprites
	jsr moves_routine
	jsr pegs_left_routine
	jsr delay_with_nmi_count2
	jmp pegs_loop
@to_nmi:
	jsr wait_for_nmi
	jmp second_left_peg_held

second_right_peg_held:
	lda control_pad					; Left button check/routine
	eor control_old
	and control_pad
	and #left_punch
	beq :+
	jsr fourth_second_cursor

:	lda control_pad					; Right button check/routine
	eor control_old
	and control_pad
	and #right_punch
	beq :+
	jsr fourth_right_cursor

:	lda control_pad					; Start button check/routine
	eor control_old
	and control_pad
	and #start_punch
	beq :+
	jsr start_over
	jmp load_pegs_board

:	lda control_pad					; B button check/routine
	eor control_old
	and control_pad
	and #b_punch
	beq :+
	jsr no_place_latch
	jmp pegs_loop

:	lda control_pad					; A button check/routine
	eor control_old
	and control_pad
	and #a_punch
	beq :+
	bne :++
:	jmp @to_nmi
:	lda top_left_cursor+3
	cmp #$68
	bne :+
	beq :++
:	jmp @right_position
:	lda top_board
	and #%00000100
	bne :+
	beq :++
:	jmp @to_nmi
:	lda bottom_board
	and #%10000000
	beq :+
	bne :++
:	jmp @to_nmi
:	lda top_board
	eor #%00010100
	sta top_board
	lda bottom_board
	eor #%10000000
	sta bottom_board
	jsr no_place_latch
	jsr vblank_wait
	jsr PPU_off
	ldx #$08						; Load X and Y to get rid of the
	ldy #$00						; second left peg. no_peg_in_21 for
	jsr no_peg_in_21				; $2006 address
	jsr routines_betwixt
	ldx #$10						; Load X and Y to get rid of the
	ldy #$00						; peg in third left slot. no_peg_in_21
	jsr no_peg_in_21				; for $2006 address
	jsr routines_betwixt
	ldx #$1c						; Load X and Y to put a peg in
	ldy #$00						; fourth left slot. peg_in_22 for
	jsr peg_in_22					; $2006 address
	jsr stabilize
	jsr PPU_with_sprites
	jsr moves_routine
	jsr pegs_left_routine
	jsr delay_with_nmi_count2
	jmp pegs_loop
@right_position:
	lda top_board
	and #%00000010
	bne :+
	beq :++
:	jmp @to_nmi
:	lda bottom_board
	and #%00100000
	beq :+
	bne :++
:	jmp @to_nmi
:	lda top_board
	eor #%00010010
	sta top_board
	lda bottom_board
	eor #%00100000
	sta bottom_board
	jsr no_place_latch
	jsr vblank_wait
	jsr PPU_off
	ldx #$08						; Load X and Y to get rid of the
	ldy #$00						; second left peg. no_peg_in_21 for
	jsr no_peg_in_21				; $2006 address
	jsr routines_betwixt
	ldx #$14						; Load X and Y to get rid of the
	ldy #$00						; peg in third middle slot. no_peg_in_21
	jsr no_peg_in_21				; for $2006 address
	jsr routines_betwixt
	ldx #$24						; Load X and Y to put a peg in
	ldy #$00						; fourth third slot. peg_in_22 for
	jsr peg_in_22					; $2006 address
	jsr stabilize
	jsr PPU_with_sprites
	jsr moves_routine
	jsr pegs_left_routine
	jsr delay_with_nmi_count2
	jmp pegs_loop
@to_nmi:
	jsr wait_for_nmi
	jmp second_right_peg_held
	rts
third_left_peg_held:
	lda control_pad					; Up button check/routine
	eor control_old
	and control_pad
	and #up_punch
	beq :+
	jsr top_cursor

:	lda control_pad					; Down button check/routine
	eor control_old
	and control_pad
	and #down_punch
	beq :+
	jsr fifth_middle_cursor

:	lda control_pad					; Left button check/routine
	eor control_old
	and control_pad
	and #left_punch
	beq :+
	jsr fifth_left_cursor

:	lda control_pad					; Right button check/routine
	eor control_old
	and control_pad
	and #right_punch
	beq :+
	jsr third_right_cursor

:	lda control_pad					; Start button check/routine
	eor control_old
	and control_pad
	and #start_punch
	beq :+
	jsr start_over
	jmp load_pegs_board

:	lda control_pad					; B button check/routine
	eor control_old
	and control_pad
	and #b_punch
	beq :+
	jsr no_place_latch
	jmp pegs_loop

:	lda control_pad					; A button check/routine
	eor control_old
	and control_pad
	and #a_punch
	beq :+
	bne :++
:	jmp @to_nmi
:	lda top_left_cursor+3
	cmp #$38
	bne :+
	beq :++
:	jmp @position2
:	lda top_board
	and #%00000001
	bne :+
	beq :++
:	jmp @to_nmi
:	lda bottom_board
	and #%00010000
	beq :+
	bne :++
:	jmp @to_nmi
:	lda top_board
	eor #%00001001
	sta top_board
	lda bottom_board
	eor #%00010000
	sta bottom_board
	jsr no_place_latch
	jsr vblank_wait
	jsr PPU_off
	ldx #$0c						; Load X and Y to get rid of the
	ldy #$00						; top peg. no_peg_in_21 for $2006
	jsr no_peg_in_21				; address
	jsr routines_betwixt
	ldx #$18						; Load X and Y to get rid of the
	ldy #$00						; peg in second left slot. no_peg_in_22
	jsr no_peg_in_22				; for $2006 address
	jsr routines_betwixt
	ldx #$28						; Load X and Y to put a peg in
	ldy #$00						; third left slot. peg_in_22 for
	jsr peg_in_22					; $2006 address
	jsr stabilize
	jsr PPU_with_sprites
	jsr moves_routine
	jsr pegs_left_routine
	jsr delay_with_nmi_count2
	jmp pegs_loop
@position2:
	lda top_left_cursor+3
	cmp #$98
	bne :+
	beq :++
:	jmp @position3
:	lda top_board
	and #%00000100
	bne :+
	beq :++
:	jmp @to_nmi
:	lda top_board
	and #%00000010
	beq :+
	bne :++
:	jmp @to_nmi
:	lda top_board
	eor #%00001110
	sta top_board
	jsr no_place_latch
	jsr vblank_wait
	jsr PPU_off
	ldx #$0c						; Load X and Y to get rid of the
	ldy #$00						; top peg. no_peg_in_21 for $2006
	jsr no_peg_in_21				; address
	jsr routines_betwixt
	ldx #$10						; Load X and Y to get rid of the
	ldy #$00						; peg in second right slot. no_peg_in_21
	jsr no_peg_in_21				; for $2006 address
	jsr routines_betwixt
	ldx #$14						; Load X and Y to put a peg in
	ldy #$00						; third right slot. peg_in_21 for
	jsr peg_in_21					; $2006 address
	jsr stabilize
	jsr PPU_with_sprites
	jsr moves_routine
	jsr pegs_left_routine
	jsr delay_with_nmi_count2
	jmp pegs_loop
@position3:
	lda top_left_cursor
	cmp #$1f
	bne :+
	beq :++
:	jmp @position4
:	lda top_board
	and #%00100000
	bne :+
	beq :++
:	jmp @to_nmi
:	lda top_board
	and #%01000000
	beq :+
	bne :++
:	jmp @to_nmi
:	lda top_board
	eor #%01101000
	sta top_board
	jsr no_place_latch
	jsr vblank_wait
	jsr PPU_off
	ldx #$0c						; Load X and Y to get rid of the
	ldy #$00						; top peg. no_peg_in_21 for $2006
	jsr no_peg_in_21				; address
	jsr routines_betwixt
	ldx #$04						; Load X and Y to get rid of the
	ldy #$00						; peg in second right slot. no_peg_in_21
	jsr no_peg_in_21				; for $2006 address
	jsr routines_betwixt
	ldx #$00						; Load X and Y to put a peg in
	ldy #$00						; third right slot. peg_in_20 for
	jsr peg_in_20					; $2006 address
	jsr stabilize
	jsr PPU_with_sprites
	jsr moves_routine
	jsr pegs_left_routine
	jsr delay_with_nmi_count2
	jmp pegs_loop
@position4:
	lda top_left_cursor
	cmp #$9f
	bne :+
	beq :++
:	jmp @to_nmi
:	lda top_right_cursor+3
	cmp #$80
	bne :+
	beq :++
:	jmp @to_nmi
:	lda bottom_board
	and #%10000000
	bne :+
	beq :++
:	jmp @to_nmi
:	lda bottom_board
	and #%00000100
	beq :+
	bne :++
:	jmp @to_nmi
:	lda top_board
	eor #%00001000
	sta top_board
	lda bottom_board
	eor #%10000100
	sta bottom_board
	jsr no_place_latch
	jsr vblank_wait
	jsr PPU_off
	ldx #$0c						; Load X and Y to get rid of the
	ldy #$00						; top peg. no_peg_in_21 for $2006
	jsr no_peg_in_21				; address
	jsr routines_betwixt
	ldx #$1c						; Load X and Y to get rid of the
	ldy #$00						; peg in second right slot. no_peg_in_22
	jsr no_peg_in_22				; for $2006 address
	jsr routines_betwixt
	ldx #$10						; Load X and Y to put a peg in
	ldy #$00						; third right slot. peg_in_22 for
	jsr peg_in_22					; $2006 address
	jsr stabilize
	jsr PPU_with_sprites
	jsr moves_routine
	jsr pegs_left_routine
	jsr delay_with_nmi_count2
	jmp pegs_loop
@to_nmi:
	jsr wait_for_nmi
	jmp third_left_peg_held
	rts
third_middle_peg_held:
	lda control_pad					; Left button check/routine
	eor control_old
	and control_pad
	and #left_punch
	beq :+
	jsr fifth_second_cursor

:	lda control_pad					; Right button check/routine
	eor control_old
	and control_pad
	and #right_punch
	beq :+
	jsr fifth_fourth_cursor

:	lda control_pad					; Start button check/routine
	eor control_old
	and control_pad
	and #start_punch
	beq :+
	jsr start_over
	jmp load_pegs_board

:	lda control_pad					; B button check/routine
	eor control_old
	and control_pad
	and #b_punch
	beq :+
	jsr no_place_latch
	jmp pegs_loop

:	lda control_pad					; A button check/routine
	eor control_old
	and control_pad
	and #a_punch
	beq :+
	bne :++
:	jmp @to_nmi
:	lda top_left_cursor+3
	cmp #$58
	bne :+
	beq :++
:	jmp @right_position
:	lda bottom_board
	and #%10000000
	bne :+
	beq :++
:	jmp @to_nmi
:	lda bottom_board
	and #%00001000
	beq :+
	bne :++
:	jmp @to_nmi
:	lda top_board
	eor #%00000100
	sta top_board
	lda bottom_board
	eor #%10001000
	sta bottom_board
	jsr no_place_latch
	jsr vblank_wait
	jsr PPU_off
	ldx #$10						; Load X and Y to get rid of the
	ldy #$00						; top peg. no_peg_in_21 for $2006
	jsr no_peg_in_21				; address
	jsr routines_betwixt
	ldx #$1c						; Load X and Y to get rid of the
	ldy #$00						; peg in second left slot. no_peg_in_22
	jsr no_peg_in_22				; for $2006 address
	jsr routines_betwixt
	ldx #$0c						; Load X and Y to put a peg in
	ldy #$00						; third left slot. peg_in_22 for
	jsr peg_in_22					; $2006 address
	jsr stabilize
	jsr PPU_with_sprites
	jsr moves_routine
	jsr pegs_left_routine
	jsr delay_with_nmi_count2
	jmp pegs_loop
@right_position:
	lda bottom_board
	and #%01000000
	bne :+
	beq :++
:	jmp @to_nmi
:	lda bottom_board
	and #%00000010
	beq :+
	bne :++
:	jmp @to_nmi
:	lda top_board
	eor #%00000100
	sta top_board
	lda bottom_board
	eor #%01000010
	sta bottom_board
	jsr no_place_latch
	jsr vblank_wait
	jsr PPU_off
	ldx #$10						; Load X and Y to get rid of the
	ldy #$00						; top peg. no_peg_in_21 for $2006
	jsr no_peg_in_21				; address
	jsr routines_betwixt
	ldx #$20						; Load X and Y to get rid of the
	ldy #$00						; peg in second right slot. no_peg_in_22
	jsr no_peg_in_22				; for $2006 address
	jsr routines_betwixt
	ldx #$14						; Load X and Y to put a peg in
	ldy #$00						; third right slot. peg_in_22 for
	jsr peg_in_22					; $2006 address
	jsr stabilize
	jsr PPU_with_sprites
	jsr moves_routine
	jsr pegs_left_routine
	jsr delay_with_nmi_count2
	jmp pegs_loop
@to_nmi:
	jsr wait_for_nmi
	jmp third_middle_peg_held
	rts
third_right_peg_held:
	lda control_pad					; Up button check/routine
	eor control_old
	and control_pad
	and #up_punch
	beq :+
	jsr top_cursor

:	lda control_pad					; Down button check/routine
	eor control_old
	and control_pad
	and #down_punch
	beq :+
	jsr fifth_middle_cursor

:	lda control_pad					; Left button check/routine
	eor control_old
	and control_pad
	and #left_punch
	beq :+
	jsr third_left_cursor

:	lda control_pad					; Right button check/routine
	eor control_old
	and control_pad
	and #right_punch
	beq :+
	jsr fifth_right_cursor

:	lda control_pad					; Start button check/routine
	eor control_old
	and control_pad
	and #start_punch
	beq :+
	jsr start_over
	jmp load_pegs_board

:	lda control_pad					; B button check/routine
	eor control_old
	and control_pad
	and #b_punch
	beq :+
	jsr no_place_latch
	jmp pegs_loop

:	lda control_pad					; A button check/routine
	eor control_old
	and control_pad
	and #a_punch
	beq :+
	bne :++
:	jmp @to_nmi
:	lda top_left_cursor
	cmp #$1f
	bne :+
	beq :++
:	jmp @position2
:	lda top_board
	and #%00010000
	bne :+
	beq :++
:	jmp @to_nmi
:	lda top_board
	and #%01000000
	beq :+
	bne :++
:	jmp @to_nmi
:	lda top_board
	eor #%01010010
	sta top_board
	jsr no_place_latch
	jsr vblank_wait
	jsr PPU_off
	ldx #$14						; Load X and Y to get rid of the
	ldy #$00						; top peg. no_peg_in_21 for $2006
	jsr no_peg_in_21				; address
	jsr routines_betwixt
	ldx #$08						; Load X and Y to get rid of the
	ldy #$00						; peg in second left slot. no_peg_in_21
	jsr no_peg_in_21				; for $2006 address
	jsr routines_betwixt
	ldx #$00						; Load X and Y to put a peg in
	ldy #$00						; third left slot. peg_in_20 for
	jsr peg_in_20					; $2006 address
	jsr stabilize
	jsr PPU_with_sprites
	jsr moves_routine
	jsr pegs_left_routine
	jsr delay_with_nmi_count2
	jmp pegs_loop
@position2:
	lda top_left_cursor
	cmp #$5f
	bne :+
	beq :++
:	jmp @position3
:	lda top_board
	and #%00000100
	bne :+
	beq :++
:	jmp @to_nmi
:	lda top_board
	and #%00001000
	beq :+
	bne :++
:	jmp @to_nmi
:	lda top_board
	eor #%00001110
	sta top_board
	jsr no_place_latch
	jsr vblank_wait
	jsr PPU_off
	ldx #$14						; Load X and Y to get rid of the
	ldy #$00						; top peg. no_peg_in_21 for $2006
	jsr no_peg_in_21				; address
	jsr routines_betwixt
	ldx #$10						; Load X and Y to get rid of the
	ldy #$00						; peg in second right slot. no_peg_in_21
	jsr no_peg_in_21				; for $2006 address
	jsr routines_betwixt
	ldx #$0c						; Load X and Y to put a peg in
	ldy #$00						; third right slot. peg_in_21 for
	jsr peg_in_21					; $2006 address
	jsr stabilize
	jsr PPU_with_sprites
	jsr moves_routine
	jsr pegs_left_routine
	jsr delay_with_nmi_count2
	jmp pegs_loop
@position3:
	lda bot_right_cursor+3
	cmp #$c0
	bne :+
	beq :++
:	jmp @position4
:	lda bottom_board
	and #%00100000
	bne :+
	beq :++
:	jmp @to_nmi
:	lda bottom_board
	and #%00000001
	beq :+
	bne :++
:	jmp @to_nmi
:	lda top_board
	eor #%00000010
	sta top_board
	lda bottom_board
	eor #%00100001
	sta bottom_board
	jsr no_place_latch
	jsr vblank_wait
	jsr PPU_off
	ldx #$14						; Load X and Y to get rid of the
	ldy #$00						; top peg. no_peg_in_21 for $2006
	jsr no_peg_in_21				; address
	jsr routines_betwixt
	ldx #$24						; Load X and Y to get rid of the
	ldy #$00						; peg in second right slot. no_peg_in_22
	jsr no_peg_in_22				; for $2006 address
	jsr routines_betwixt
	ldx #$2c						; Load X and Y to put a peg in
	ldy #$00						; third right slot. peg_in_22 for
	jsr peg_in_22					; $2006 address
	jsr stabilize
	jsr PPU_with_sprites
	jsr moves_routine
	jsr pegs_left_routine
	jsr delay_with_nmi_count2
	jmp pegs_loop
@position4:
	lda top_left_cursor
	cmp #$9f
	bne :+
	beq :++
:	jmp @to_nmi
:	lda top_right_cursor+3
	cmp #$80
	bne :+
	beq :++
:	jmp @to_nmi
:	lda bottom_board
	and #%01000000
	bne :+
	beq :++
:	jmp @to_nmi
:	lda bottom_board
	and #%00000100
	beq :+
	bne :++
:	jmp @to_nmi
:	lda top_board
	eor #%00000010
	sta top_board
	lda bottom_board
	eor #%01000100
	sta bottom_board
	jsr no_place_latch
	jsr vblank_wait
	jsr PPU_off
	ldx #$14						; Load X and Y to get rid of the
	ldy #$00						; top peg. no_peg_in_22 for $2006
	jsr no_peg_in_21				; address
	jsr routines_betwixt
	ldx #$20						; Load X and Y to get rid of the
	ldy #$00						; peg in second right slot. no_peg_in_22
	jsr no_peg_in_22				; for $2006 address
	jsr routines_betwixt
	ldx #$10						; Load X and Y to put a peg in
	ldy #$00						; third right slot. peg_in_22 for
	jsr peg_in_22					; $2006 address
	jsr stabilize
	jsr PPU_with_sprites
	jsr moves_routine
	jsr pegs_left_routine
	jsr delay_with_nmi_count2
	jmp pegs_loop
@to_nmi:
	jsr wait_for_nmi
	jmp third_right_peg_held
	rts
fourth_left_peg_held:
	lda control_pad					; Up button check/routine
	eor control_old
	and control_pad
	and #up_punch
	beq :+
	jsr second_left_cursor

:	lda control_pad					; Down button check/routine
	eor control_old
	and control_pad
	and #down_punch
	beq :+
	jsr fourth_third_cursor

:	lda control_pad					; Start button check/routine
	eor control_old
	and control_pad
	and #start_punch
	beq :+
	jsr start_over
	jmp load_pegs_board

:	lda control_pad					; B button check/routine
	eor control_old
	and control_pad
	and #b_punch
	beq :+
	jsr no_place_latch
	jmp pegs_loop

:	lda control_pad					; A button check/routine
	eor control_old
	and control_pad
	and #a_punch
	beq :+
	bne :++
:	jmp @to_nmi
:	lda top_left_cursor+3
	cmp #$68
	bne :+
	beq :++
:	jmp @right_position
:	lda top_board
	and #%00001000
	bne :+
	beq :++
:	jmp @to_nmi
:	lda top_board
	and #%00100000
	beq :+
	bne :++
:	jmp @to_nmi
:	lda top_board
	eor #%00101001
	sta top_board
	lda bottom_board
	jsr no_place_latch
	jsr vblank_wait
	jsr PPU_off
	ldx #$18						; Load X and Y to get rid of the
	ldy #$00						; top peg. no_peg_in_22 for $2006
	jsr no_peg_in_22				; address
	jsr routines_betwixt
	ldx #$0c						; Load X and Y to get rid of the
	ldy #$00						; peg in second left slot. no_peg_in_21
	jsr no_peg_in_21				; for $2006 address
	jsr routines_betwixt
	ldx #$04						; Load X and Y to put a peg in
	ldy #$00						; third left slot. peg_in_21 for
	jsr peg_in_21					; $2006 address
	jsr stabilize
	jsr PPU_with_sprites
	jsr moves_routine
	jsr pegs_left_routine
	jsr delay_with_nmi_count2
	jmp pegs_loop
@right_position:
	lda bottom_board
	and #%10000000
	bne :+
	beq :++
:	jmp @to_nmi
:	lda bottom_board
	and #%01000000
	beq :+
	bne :++
:	jmp @to_nmi
:	lda top_board
	eor #%00000001
	sta top_board
	lda bottom_board
	eor #%11000000
	sta bottom_board
	jsr no_place_latch
	jsr vblank_wait
	jsr PPU_off
	ldx #$18						; Load X and Y to get rid of the
	ldy #$00						; top peg. no_peg_in_22 for $2006
	jsr no_peg_in_22				; address
	jsr routines_betwixt
	ldx #$1c						; Load X and Y to get rid of the
	ldy #$00						; peg in second right slot. no_peg_in_22
	jsr no_peg_in_22				; for $2006 address
	jsr routines_betwixt
	ldx #$20						; Load X and Y to put a peg in
	ldy #$00						; third right slot. peg_in_22 for
	jsr peg_in_22					; $2006 address
	jsr stabilize
	jsr PPU_with_sprites
	jsr moves_routine
	jsr pegs_left_routine
	jsr delay_with_nmi_count2
	jmp pegs_loop
@to_nmi:
	jsr wait_for_nmi
	jmp fourth_left_peg_held
	rts
fourth_second_peg_held:
	lda control_pad					; Up button check/routine
	eor control_old
	and control_pad
	and #up_punch
	beq :+
	jsr second_right_cursor

:	lda control_pad					; Down button check/routine
	eor control_old
	and control_pad
	and #down_punch
	beq :+
	jsr fourth_right_cursor

:	lda control_pad					; Start button check/routine
	eor control_old
	and control_pad
	and #start_punch
	beq :+
	jsr start_over
	jmp load_pegs_board

:	lda control_pad					; B button check/routine
	eor control_old
	and control_pad
	and #b_punch
	beq :+
	jsr no_place_latch
	jmp pegs_loop

:	lda control_pad					; A button check/routine
	eor control_old
	and control_pad
	and #a_punch
	beq :+
	bne :++
:	jmp @to_nmi
:	lda top_left_cursor+3
	cmp #$88
	bne :+
	beq :++
:	jmp @right_position
:	lda top_board
	and #%00000100
	bne :+
	beq :++
:	jmp @to_nmi
:	lda top_board
	and #%00010000
	beq :+
	bne :++
:	jmp @to_nmi
:	lda top_board
	eor #%00010100
	sta top_board
	lda bottom_board
	eor #%10000000
	sta bottom_board
	jsr no_place_latch
	jsr vblank_wait
	jsr PPU_off
	ldx #$1c						; Load X and Y to get rid of the
	ldy #$00						; top peg. no_peg_in_22 for $2006
	jsr no_peg_in_22				; address
	jsr routines_betwixt
	ldx #$10						; Load X and Y to get rid of the
	ldy #$00						; peg in second left slot. no_peg_in_21
	jsr no_peg_in_21				; for $2006 address
	jsr routines_betwixt
	ldx #$08						; Load X and Y to put a peg in
	ldy #$00						; third left slot. peg_in_21 for
	jsr peg_in_21					; $2006 address
	jsr stabilize
	jsr PPU_with_sprites
	jsr moves_routine
	jsr pegs_left_routine
	jsr delay_with_nmi_count2
	jmp pegs_loop
@right_position:
	lda bottom_board
	and #%01000000
	bne :+
	beq :++
:	jmp @to_nmi
:	lda bottom_board
	and #%00100000
	beq :+
	bne :++
:	jmp @to_nmi
:	lda bottom_board
	eor #%11100000
	sta bottom_board
	jsr no_place_latch
	jsr vblank_wait
	jsr PPU_off
	ldx #$1c						; Load X and Y to get rid of the
	ldy #$00						; top peg. no_peg_in_22 for $2006
	jsr no_peg_in_22				; address
	jsr routines_betwixt
	ldx #$20						; Load X and Y to get rid of the
	ldy #$00						; peg in second right slot. no_peg_in_22
	jsr no_peg_in_22				; for $2006 address
	jsr routines_betwixt
	ldx #$24						; Load X and Y to put a peg in
	ldy #$00						; third right slot. peg_in_22 for
	jsr peg_in_22					; $2006 address
	jsr stabilize
	jsr PPU_with_sprites
	jsr moves_routine
	jsr pegs_left_routine
	jsr delay_with_nmi_count2
	jmp pegs_loop
@to_nmi:
	jsr wait_for_nmi
	jmp fourth_second_peg_held
	rts
fourth_third_peg_held:
	lda control_pad					; Up button check/routine
	eor control_old
	and control_pad
	and #up_punch
	beq :+
	jsr second_left_cursor

:	lda control_pad					; Down button check/routine
	eor control_old
	and control_pad
	and #down_punch
	beq :+
	jsr fourth_left_cursor

:	lda control_pad					; Start button check/routine
	eor control_old
	and control_pad
	and #start_punch
	beq :+
	jsr start_over
	jmp load_pegs_board

:	lda control_pad					; B button check/routine
	eor control_old
	and control_pad
	and #b_punch
	beq :+
	jsr no_place_latch
	jmp pegs_loop

:	lda control_pad					; A button check/routine
	eor control_old
	and control_pad
	and #a_punch
	beq :+
	bne :++
:	jmp @to_nmi
:	lda top_left_cursor+3
	cmp #$68
	bne :+
	beq :++
:	jmp @right_position
:	lda top_board
	and #%00000100
	bne :+
	beq :++
:	jmp @to_nmi
:	lda top_board
	and #%00100000
	beq :+
	bne :++
:	jmp @to_nmi
:	lda top_board
	eor #%00100100
	sta top_board
	lda bottom_board
	eor #%01000000
	sta bottom_board
	jsr no_place_latch
	jsr vblank_wait
	jsr PPU_off
	ldx #$20						; Load X and Y to get rid of the
	ldy #$00						; top peg. no_peg_in_22 for $2006
	jsr no_peg_in_22				; address
	jsr routines_betwixt
	ldx #$10						; Load X and Y to get rid of the
	ldy #$00						; peg in second left slot. no_peg_in_21
	jsr no_peg_in_21				; for $2006 address
	jsr routines_betwixt
	ldx #$04						; Load X and Y to put a peg in
	ldy #$00						; third left slot. peg_in_21 for
	jsr peg_in_21					; $2006 address
	jsr stabilize
	jsr PPU_with_sprites
	jsr moves_routine
	jsr pegs_left_routine
	jsr delay_with_nmi_count2
	jmp pegs_loop
@right_position:
	lda bottom_board
	and #%10000000
	bne :+
	beq :++
:	jmp @to_nmi
:	lda top_board
	and #%00000001
	beq :+
	bne :++
:	jmp @to_nmi
:	lda top_board
	eor #%00000001
	sta top_board
	lda bottom_board
	eor #%11000000
	sta bottom_board
	jsr no_place_latch
	jsr vblank_wait
	jsr PPU_off
	ldx #$20						; Load X and Y to get rid of the
	ldy #$00						; top peg. no_peg_in_22 for $2006
	jsr no_peg_in_22				; address
	jsr routines_betwixt
	ldx #$1c						; Load X and Y to get rid of the
	ldy #$00						; peg in second right slot. no_peg_in_22
	jsr no_peg_in_22				; for $2006 address
	jsr routines_betwixt
	ldx #$18						; Load X and Y to put a peg in
	ldy #$00						; third right slot. peg_in_22 for
	jsr peg_in_22					; $2006 address
	jsr stabilize
	jsr PPU_with_sprites
	jsr moves_routine
	jsr pegs_left_routine
	jsr delay_with_nmi_count2
	jmp pegs_loop
@to_nmi:
	jsr wait_for_nmi
	jmp fourth_third_peg_held
	rts
fourth_right_peg_held:
	lda control_pad					; Up button check/routine
	eor control_old
	and control_pad
	and #up_punch
	beq :+
	jsr second_right_cursor

:	lda control_pad					; Down button check/routine
	eor control_old
	and control_pad
	and #down_punch
	beq :+
	jsr fourth_second_cursor

:	lda control_pad					; Start button check/routine
	eor control_old
	and control_pad
	and #start_punch
	beq :+
	jsr start_over
	jmp load_pegs_board

:	lda control_pad					; B button check/routine
	eor control_old
	and control_pad
	and #b_punch
	beq :+
	jsr no_place_latch
	jmp pegs_loop

:	lda control_pad					; A button check/routine
	eor control_old
	and control_pad
	and #a_punch
	beq :+
	bne :++
:	jmp @to_nmi
:	lda top_left_cursor+3
	cmp #$88
	bne :+
	beq :++
:	jmp @right_position
:	lda top_board
	and #%00000010
	bne :+
	beq :++
:	jmp @to_nmi
:	lda top_board
	and #%00010000
	beq :+
	bne :++
:	jmp @to_nmi
:	lda top_board
	eor #%00010010
	sta top_board
	lda bottom_board
	eor #%00100000
	sta bottom_board
	jsr no_place_latch
	jsr vblank_wait
	jsr PPU_off
	ldx #$24						; Load X and Y to get rid of the
	ldy #$00						; top peg. no_peg_in_22 for $2006
	jsr no_peg_in_22				; address
	jsr routines_betwixt
	ldx #$14						; Load X and Y to get rid of the
	ldy #$00						; peg in second left slot. no_peg_in_21
	jsr no_peg_in_21				; for $2006 address
	jsr routines_betwixt
	ldx #$08						; Load X and Y to put a peg in
	ldy #$00						; third left slot. peg_in_21 for
	jsr peg_in_21					; $2006 address
	jsr stabilize
	jsr PPU_with_sprites
	jsr moves_routine
	jsr pegs_left_routine
	jsr delay_with_nmi_count2
	jmp pegs_loop
@right_position:
	lda bottom_board
	and #%01000000
	bne :+
	beq :++
:	jmp @to_nmi
:	lda bottom_board
	and #%10000000
	beq :+
	bne :++
:	jmp @to_nmi
:	lda bottom_board
	eor #%11100000
	sta bottom_board
	jsr no_place_latch
	jsr vblank_wait
	jsr PPU_off
	ldx #$24						; Load X and Y to get rid of the
	ldy #$00						; top peg. no_peg_in_22 for $2006
	jsr no_peg_in_22				; address
	jsr routines_betwixt
	ldx #$20						; Load X and Y to get rid of the
	ldy #$00						; peg in second right slot. no_peg_in_22
	jsr no_peg_in_22				; for $2006 address
	jsr routines_betwixt
	ldx #$1c						; Load X and Y to put a peg in
	ldy #$00						; third right slot. peg_in_22 for
	jsr peg_in_22					; $2006 address
	jsr stabilize
	jsr PPU_with_sprites
	jsr moves_routine
	jsr pegs_left_routine
	jsr delay_with_nmi_count2
	jmp pegs_loop
@to_nmi:
	jsr wait_for_nmi
	jmp fourth_right_peg_held
	rts
fifth_left_peg_held:
	lda control_pad					; Up button check/routine
	eor control_old
	and control_pad
	and #up_punch
	beq :+
	jsr third_left_cursor

:	lda control_pad					; Down button check/routine
	eor control_old
	and control_pad
	and #down_punch
	beq :+
	jsr fifth_middle_cursor

:	lda control_pad					; Start button check/routine
	eor control_old
	and control_pad
	and #start_punch
	beq :+
	jsr start_over
	jmp load_pegs_board

:	lda control_pad					; B button check/routine
	eor control_old
	and control_pad
	and #b_punch
	beq :+
	jsr no_place_latch
	jmp pegs_loop

:	lda control_pad					; A button check/routine
	eor control_old
	and control_pad
	and #a_punch
	beq :+
	bne :++
:	jmp @to_nmi
:	lda top_left_cursor+3
	cmp #$58
	bne :+
	beq :++
:	jmp @right_position
:	lda top_board
	and #%00000001
	bne :+
	beq :++
:	jmp @to_nmi
:	lda top_board
	and #%00001000
	beq :+
	bne :++
:	jmp @to_nmi
:	lda top_board
	eor #%00001001
	sta top_board
	lda bottom_board
	eor #%00010000
	sta bottom_board
	jsr no_place_latch
	jsr vblank_wait
	jsr PPU_off
	ldx #$28						; Load X and Y to get rid of the
	ldy #$00						; top peg. no_peg_in_22 for $2006
	jsr no_peg_in_22				; address
	jsr routines_betwixt
	ldx #$18						; Load X and Y to get rid of the
	ldy #$00						; peg in second left slot. no_peg_in_22
	jsr no_peg_in_22				; for $2006 address
	jsr routines_betwixt
	ldx #$0c						; Load X and Y to put a peg in
	ldy #$00						; third left slot. peg_in_21 for
	jsr peg_in_21					; $2006 address
	jsr stabilize
	jsr PPU_with_sprites
	jsr moves_routine
	jsr pegs_left_routine
	jsr delay_with_nmi_count2
	jmp pegs_loop
@right_position:
	lda bottom_board
	and #%00001000
	bne :+
	beq :++
:	jmp @to_nmi
:	lda bottom_board
	and #%00000100
	beq :+
	bne :++
:	jmp @to_nmi
:	lda bottom_board
	eor #%00011100
	sta bottom_board
	jsr no_place_latch
	jsr vblank_wait
	jsr PPU_off
	ldx #$28						; Load X and Y to get rid of the
	ldy #$00						; top peg. no_peg_in_22 for $2006
	jsr no_peg_in_22				; address
	jsr routines_betwixt
	ldx #$0c						; Load X and Y to get rid of the
	ldy #$00						; peg in second right slot. no_peg_in_22
	jsr no_peg_in_22				; for $2006 address
	jsr routines_betwixt
	ldx #$10						; Load X and Y to put a peg in
	ldy #$00						; third right slot. peg_in_22 for
	jsr peg_in_22					; $2006 address
	jsr stabilize
	jsr PPU_with_sprites
	jsr moves_routine
	jsr pegs_left_routine
	jsr delay_with_nmi_count2
	jmp pegs_loop
@to_nmi:
	jsr wait_for_nmi
	jmp fifth_left_peg_held
	rts
fifth_second_peg_held:
	lda control_pad					; Up button check/routine
	eor control_old
	and control_pad
	and #up_punch
	beq :+
	jsr third_middle_cursor

:	lda control_pad					; Down button check/routine
	eor control_old
	and control_pad
	and #down_punch
	beq :+
	jsr fifth_fourth_cursor

:	lda control_pad					; Start button check/routine
	eor control_old
	and control_pad
	and #start_punch
	beq :+
	jsr start_over
	jmp load_pegs_board

:	lda control_pad					; B button check/routine
	eor control_old
	and control_pad
	and #b_punch
	beq :+
	jsr no_place_latch
	jmp pegs_loop

:	lda control_pad					; A button check/routine
	eor control_old
	and control_pad
	and #a_punch
	beq :+
	bne :++
:	jmp @to_nmi
:	lda top_left_cursor+3
	cmp #$78
	bne :+
	beq :++
:	jmp @right_position
:	lda bottom_board
	and #%10000000
	bne :+
	beq :++
:	jmp @to_nmi
:	lda top_board
	and #%00000100
	beq :+
	bne :++
:	jmp @to_nmi
:	lda top_board
	eor #%00000100
	sta top_board
	lda bottom_board
	eor #%10001000
	sta bottom_board
	jsr no_place_latch
	jsr vblank_wait
	jsr PPU_off
	ldx #$0c						; Load X and Y to get rid of the
	ldy #$00						; top peg. no_peg_in_22 for $2006
	jsr no_peg_in_22				; address
	jsr routines_betwixt
	ldx #$1c						; Load X and Y to get rid of the
	ldy #$00						; peg in second left slot. no_peg_in_22
	jsr no_peg_in_22				; for $2006 address
	jsr routines_betwixt
	ldx #$10						; Load X and Y to put a peg in
	ldy #$00						; third left slot. peg_in_21 for
	jsr peg_in_21					; $2006 address
	jsr stabilize
	jsr PPU_with_sprites
	jsr moves_routine
	jsr pegs_left_routine
	jsr delay_with_nmi_count2
	jmp pegs_loop
@right_position:
	lda bottom_board
	and #%00000100
	bne :+
	beq :++
:	jmp @to_nmi
:	lda bottom_board
	and #%00000010
	beq :+
	bne :++
:	jmp @to_nmi
:	lda bottom_board
	eor #%00001110
	sta bottom_board
	jsr no_place_latch
	jsr vblank_wait
	jsr PPU_off
	ldx #$0c						; Load X and Y to get rid of the
	ldy #$00						; top peg. no_peg_in_22 for $2006
	jsr no_peg_in_22				; address
	jsr routines_betwixt
	ldx #$10						; Load X and Y to get rid of the
	ldy #$00						; peg in second right slot. no_peg_in_22
	jsr no_peg_in_22				; for $2006 address
	jsr routines_betwixt
	ldx #$14						; Load X and Y to put a peg in
	ldy #$00						; third right slot. peg_in_22 for
	jsr peg_in_22					; $2006 address
	jsr stabilize
	jsr PPU_with_sprites
	jsr moves_routine
	jsr pegs_left_routine
	jsr delay_with_nmi_count2
	jmp pegs_loop
@to_nmi:
	jsr wait_for_nmi
	jmp fifth_second_peg_held
	rts
fifth_middle_peg_held:
	lda control_pad					; Up button check/routine
	eor control_old
	and control_pad
	and #up_punch
	beq :++++
	lda top_left_cursor+3
	cmp #$38
	bne :++
	beq :+
:	jsr third_left_cursor
:	cmp #$b8
	bne :++
	beq :+
:	jsr third_right_cursor

:	lda control_pad					; Down button check/routine
	eor control_old
	and control_pad
	and #down_punch
	beq :++++
	lda top_left_cursor+3
	cmp #$58
	bne :++
	beq :+
:	jsr fifth_left_cursor
:	cmp #$98
	bne :++
	beq :+
:	jsr fifth_right_cursor

:	lda control_pad					; Left button check/routine
	eor control_old
	and control_pad
	and #left_punch
	beq :++++
	lda top_left_cursor+3
	cmp #$98
	bne :++
	beq :+
:	jsr third_left_cursor
:	cmp #$b8
	bne :++
	beq :+
:	jsr fifth_left_cursor

:	lda control_pad					; Right button check/routine
	eor control_old
	and control_pad
	and #right_punch
	beq :++++
	lda top_left_cursor+3
	cmp #$58
	bne :++
	beq :+
:	jsr third_right_cursor
:	cmp #$38
	bne :++
	beq :+
:	jsr fifth_right_cursor

:	lda control_pad					; Start button check/routine
	eor control_old
	and control_pad
	and #start_punch
	beq :+
	jsr start_over
	jmp load_pegs_board

:	lda control_pad					; B button check/routine
	eor control_old
	and control_pad
	and #b_punch
	beq :+
	jsr no_place_latch
	jmp pegs_loop

:	lda control_pad					; A button check/routine
	eor control_old
	and control_pad
	and #a_punch
	beq :+
	bne :++
:	jmp @to_nmi
:	lda top_left_cursor+3
	cmp #$58
	bne :+
	beq :++
:	jmp @position2
:	lda bottom_board
	and #%10000000
	bne :+
	beq :++
:	jmp @to_nmi
:	lda top_board
	and #%00001000
	beq :+
	bne :++
:	jmp @to_nmi
:	lda top_board
	eor #%00001000
	sta top_board
	lda bottom_board
	eor #%10000100
	sta bottom_board
	jsr no_place_latch
	jsr vblank_wait
	jsr PPU_off
	ldx #$10						; Load X and Y to get rid of the
	ldy #$00						; top peg. no_peg_in_22 for $2006
	jsr no_peg_in_22				; address
	jsr routines_betwixt
	ldx #$1c						; Load X and Y to get rid of the
	ldy #$00						; peg in second left slot. no_peg_in_22
	jsr no_peg_in_22				; for $2006 address
	jsr routines_betwixt
	ldx #$0c						; Load X and Y to put a peg in
	ldy #$00						; third left slot. peg_in_21 for
	jsr peg_in_21					; $2006 address
	jsr stabilize
	jsr PPU_with_sprites
	jsr moves_routine
	jsr pegs_left_routine
	jsr delay_with_nmi_count2
	jmp pegs_loop
@position2:
	lda top_left_cursor+3
	cmp #$38
	bne :+
	beq :++
:	jmp @position3
:	lda bottom_board
	and #%00001000
	bne :+
	beq :++
:	jmp @to_nmi
:	lda bottom_board
	and #%00010000
	beq :+
	bne :++
:	jmp @to_nmi
:	lda bottom_board
	eor #%00011100
	sta bottom_board
	jsr no_place_latch
	jsr vblank_wait
	jsr PPU_off
	ldx #$10						; Load X and Y to get rid of the
	ldy #$00						; top peg. no_peg_in_22 for $2006
	jsr no_peg_in_22				; address
	jsr routines_betwixt
	ldx #$0c						; Load X and Y to get rid of the
	ldy #$00						; peg in second right slot. no_peg_in_22
	jsr no_peg_in_22				; for $2006 address
	jsr routines_betwixt
	ldx #$28						; Load X and Y to put a peg in
	ldy #$00						; third right slot. peg_in_22 for
	jsr peg_in_22					; $2006 address
	jsr stabilize
	jsr PPU_with_sprites
	jsr moves_routine
	jsr pegs_left_routine
	jsr delay_with_nmi_count2
	jmp pegs_loop
@position3:
	lda top_left_cursor+3
	cmp #$98
	bne :+
	beq :++
:	jmp @position4
:	lda bottom_board
	and #%01000000
	bne :+
	beq :++
:	jmp @to_nmi
:	lda top_board
	and #%00000010
	beq :+
	bne :++
:	jmp @to_nmi
:	lda top_board
	eor #%00000010
	sta top_board
	lda bottom_board
	eor #%01000100
	sta bottom_board
	jsr no_place_latch
	jsr vblank_wait
	jsr PPU_off
	ldx #$10						; Load X and Y to get rid of the
	ldy #$00						; top peg. no_peg_in_22 for $2006
	jsr no_peg_in_22				; address
	jsr routines_betwixt
	ldx #$20						; Load X and Y to get rid of the
	ldy #$00						; peg in second right slot. no_peg_in_22
	jsr no_peg_in_22				; for $2006 address
	jsr routines_betwixt
	ldx #$14						; Load X and Y to put a peg in
	ldy #$00						; third right slot. peg_in_21 for
	jsr peg_in_21					; $2006 address
	jsr stabilize
	jsr PPU_with_sprites
	jsr moves_routine
	jsr pegs_left_routine
	jsr delay_with_nmi_count2
	jmp pegs_loop
@position4:
	lda top_left_cursor+3
	cmp #$b8
	bne :+
	beq :++
:	jmp @to_nmi
:	lda bottom_board
	and #%00000010
	bne :+
	beq :++
:	jmp @to_nmi
:	lda bottom_board
	and #%00000001
	beq :+
	bne :++
:	jmp @to_nmi
:	lda bottom_board
	eor #%00000111
	sta bottom_board
	jsr no_place_latch
	jsr vblank_wait
	jsr PPU_off
	ldx #$10						; Load X and Y to get rid of the
	ldy #$00						; top peg. no_peg_in_22 for $2006
	jsr no_peg_in_22				; address
	jsr routines_betwixt
	ldx #$14						; Load X and Y to get rid of the
	ldy #$00						; peg in second right slot. no_peg_in_22
	jsr no_peg_in_22				; for $2006 address
	jsr routines_betwixt
	ldx #$2c						; Load X and Y to put a peg in
	ldy #$00						; third right slot. peg_in_22 for
	jsr peg_in_22					; $2006 address
	jsr stabilize
	jsr PPU_with_sprites
	jsr moves_routine
	jsr pegs_left_routine
	jsr delay_with_nmi_count2
	jmp pegs_loop
@to_nmi:
	jsr wait_for_nmi
	jmp fifth_middle_peg_held
	rts
fifth_fourth_peg_held:
	lda control_pad					; Up button check/routine
	eor control_old
	and control_pad
	and #up_punch
	beq :+
	jsr third_middle_cursor

:	lda control_pad					; Down button check/routine
	eor control_old
	and control_pad
	and #down_punch
	beq :+
	jsr fifth_second_cursor

:	lda control_pad					; Start button check/routine
	eor control_old
	and control_pad
	and #start_punch
	beq :+
	jsr start_over
	jmp load_pegs_board

:	lda control_pad					; B button check/routine
	eor control_old
	and control_pad
	and #b_punch
	beq :+
	jsr no_place_latch
	jmp pegs_loop

:	lda control_pad					; A button check/routine
	eor control_old
	and control_pad
	and #a_punch
	beq :+
	bne :++
:	jmp @to_nmi
:	lda top_left_cursor+3
	cmp #$78
	bne :+
	beq :++
:	jmp @right_position
:	lda bottom_board
	and #%01000000
	bne :+
	beq :++
:	jmp @to_nmi
:	lda top_board
	and #%00000100
	beq :+
	bne :++
:	jmp @to_nmi
:	lda top_board
	eor #%00000100
	sta top_board
	lda bottom_board
	eor #%01000010
	sta bottom_board
	jsr no_place_latch
	jsr vblank_wait
	jsr PPU_off
	ldx #$14						; Load X and Y to get rid of the
	ldy #$00						; top peg. no_peg_in_22 for $2006
	jsr no_peg_in_22				; address
	jsr routines_betwixt
	ldx #$20						; Load X and Y to get rid of the
	ldy #$00						; peg in second left slot. no_peg_in_22
	jsr no_peg_in_22				; for $2006 address
	jsr routines_betwixt
	ldx #$10						; Load X and Y to put a peg in
	ldy #$00						; third left slot. peg_in_21 for
	jsr peg_in_21					; $2006 address
	jsr stabilize
	jsr PPU_with_sprites
	jsr moves_routine
	jsr pegs_left_routine
	jsr delay_with_nmi_count2
	jmp pegs_loop
@right_position:
	lda bottom_board
	and #%00000100
	bne :+
	beq :++
:	jmp @to_nmi
:	lda bottom_board
	and #%00001000
	beq :+
	bne :++
:	jmp @to_nmi
:	lda bottom_board
	eor #%00001110
	sta bottom_board
	jsr no_place_latch
	jsr vblank_wait
	jsr PPU_off
	ldx #$14						; Load X and Y to get rid of the
	ldy #$00						; top peg. no_peg_in_22 for $2006
	jsr no_peg_in_22				; address
	jsr routines_betwixt
	ldx #$10						; Load X and Y to get rid of the
	ldy #$00						; peg in second right slot. no_peg_in_22
	jsr no_peg_in_22				; for $2006 address
	jsr routines_betwixt
	ldx #$0c						; Load X and Y to put a peg in
	ldy #$00						; third right slot. peg_in_22 for
	jsr peg_in_22					; $2006 address
	jsr stabilize
	jsr PPU_with_sprites
	jsr moves_routine
	jsr pegs_left_routine
	jsr delay_with_nmi_count2
	jmp pegs_loop
@to_nmi:
	jsr wait_for_nmi
	jmp fifth_fourth_peg_held
	rts
fifth_right_peg_held:
	lda control_pad					; Up button check/routine
	eor control_old
	and control_pad
	and #up_punch
	beq :+
	jsr third_right_cursor

:	lda control_pad					; Down button check/routine
	eor control_old
	and control_pad
	and #down_punch
	beq :+
	jsr fifth_middle_cursor

:	lda control_pad					; Start button check/routine
	eor control_old
	and control_pad
	and #start_punch
	beq :+
	jsr start_over
	jmp load_pegs_board

:	lda control_pad					; B button check/routine
	eor control_old
	and control_pad
	and #b_punch
	beq :+
	jsr no_place_latch
	jmp pegs_loop

:	lda control_pad					; A button check/routine
	eor control_old
	and control_pad
	and #a_punch
	beq :+
	bne :++
:	jmp @to_nmi
:	lda top_left_cursor+3
	cmp #$98
	bne :+
	beq :++
:	jmp @right_position
:	lda bottom_board
	and #%00100000
	bne :+
	beq :++
:	jmp @to_nmi
:	lda top_board
	and #%00000010
	beq :+
	bne :++
:	jmp @to_nmi
:	lda top_board
	eor #%00000010
	sta top_board
	lda bottom_board
	eor #%00100001
	sta bottom_board
	jsr no_place_latch
	jsr vblank_wait
	jsr PPU_off
	ldx #$2c						; Load X and Y to get rid of the
	ldy #$00						; top peg. no_peg_in_20 for $2006
	jsr no_peg_in_22				; address
	jsr routines_betwixt
	ldx #$24						; Load X and Y to get rid of the
	ldy #$00						; peg in second left slot. no_peg_in_21
	jsr no_peg_in_22				; for $2006 address
	jsr routines_betwixt
	ldx #$14						; Load X and Y to put a peg in
	ldy #$00						; third left slot. peg_in_21 for
	jsr peg_in_21					; $2006 address
	jsr stabilize
	jsr PPU_with_sprites
	jsr moves_routine
	jsr pegs_left_routine
	jsr delay_with_nmi_count2
	jmp pegs_loop
@right_position:
	lda bottom_board
	and #%00000010
	bne :+
	beq :++
:	jmp @to_nmi
:	lda bottom_board
	and #%00000100
	beq :+
	bne :++
:	jmp @to_nmi
:	lda bottom_board
	eor #%00000111
	sta bottom_board
	jsr no_place_latch
	jsr vblank_wait
	jsr PPU_off
	ldx #$2c						; Load X and Y to get rid of the
	ldy #$00						; top peg. no_peg_in_22 for $2006
	jsr no_peg_in_22				; address
	jsr routines_betwixt
	ldx #$14						; Load X and Y to get rid of the
	ldy #$00						; peg in second right slot. no_peg_in_22
	jsr no_peg_in_22				; for $2006 address
	jsr routines_betwixt
	ldx #$10						; Load X and Y to put a peg in
	ldy #$00						; third right slot. peg_in_22 for
	jsr peg_in_22					; $2006 address
	jsr stabilize
	jsr PPU_with_sprites
	jsr moves_routine
	jsr pegs_left_routine
	jsr delay_with_nmi_count2
	jmp pegs_loop
@to_nmi:
	jsr wait_for_nmi
	jmp fifth_right_peg_held
	rts

place_placeholder:					; We use this routine to put the
	lda top_left_cursor				; place holder in the place where
	sta top_left_place				; our game cursor was.
	lda top_left_cursor+3
	sta top_left_place+3
	lda top_right_cursor
	sta top_right_place
	lda top_right_cursor+3
	sta top_right_place+3
	lda bot_left_cursor
	sta bot_left_place
	lda bot_left_cursor+3
	sta bot_left_place+3
	lda bot_right_cursor
	sta bot_right_place
	lda bot_right_cursor+3
	sta bot_right_place+3
	rts
no_place_latch:						; When we are done with the place holder,
	lda #$00						; we put the sprites for it off screen and
	sta top_left_place				; save it for use.
	sta top_right_place
	sta bot_left_place
	sta bot_right_place
	lda #$ef
	sta top_left_place
	sta top_right_place
	sta bot_left_place
	sta bot_right_place
	rts

; *********************************************************
; These routines are all for the various places that the  *
; gameplay cursor can be drawn to. They all just have     *
; values that are loaded in each of the sprites X and Y   *
; positions                                               *
; *********************************************************
top_cursor:
	lda #$1f
	sta top_left_cursor
	sta top_right_cursor
	lda #$78
	sta top_left_cursor+3
	sta bot_left_cursor+3
	lda #$80
	sta top_right_cursor+3
	sta bot_right_cursor+3
	lda #$27
	sta bot_left_cursor
	sta bot_right_cursor
	rts
second_left_cursor:
	lda #$3f
	sta top_left_cursor
	sta top_right_cursor
	lda #$68
	sta top_left_cursor+3
	sta bot_left_cursor+3
	lda #$70
	sta top_right_cursor+3
	sta bot_right_cursor+3
	lda #$47
	sta bot_left_cursor
	sta bot_right_cursor
	rts
second_right_cursor:
	lda #$3f
	sta top_left_cursor
	sta top_right_cursor
	lda #$88
	sta top_left_cursor+3
	sta bot_left_cursor+3
	lda #$90
	sta top_right_cursor+3
	sta bot_right_cursor+3
	lda #$47
	sta bot_left_cursor
	sta bot_right_cursor
	rts
third_left_cursor:
	lda #$5f
	sta top_left_cursor
	sta top_right_cursor
	lda #$58
	sta top_left_cursor+3
	sta bot_left_cursor+3
	lda #$60
	sta top_right_cursor+3
	sta bot_right_cursor+3
	lda #$67
	sta bot_left_cursor
	sta bot_right_cursor
	rts
third_middle_cursor:
	lda #$5f
	sta top_left_cursor
	sta top_right_cursor
	lda #$78
	sta top_left_cursor+3
	sta bot_left_cursor+3
	lda #$80
	sta top_right_cursor+3
	sta bot_right_cursor+3
	lda #$67
	sta bot_left_cursor
	sta bot_right_cursor
	rts
third_right_cursor:
	lda #$5f
	sta top_left_cursor
	sta top_right_cursor
	lda #$98
	sta top_left_cursor+3
	sta bot_left_cursor+3
	lda #$a0
	sta top_right_cursor+3
	sta bot_right_cursor+3
	lda #$67
	sta bot_left_cursor
	sta bot_right_cursor
	rts
fourth_left_cursor:
	lda #$7f
	sta top_left_cursor
	sta top_right_cursor
	lda #$48
	sta top_left_cursor+3
	sta bot_left_cursor+3
	lda #$50
	sta top_right_cursor+3
	sta bot_right_cursor+3
	lda #$87
	sta bot_left_cursor
	sta bot_right_cursor
	rts
fourth_second_cursor:
	lda #$7f
	sta top_left_cursor
	sta top_right_cursor
	lda #$68
	sta top_left_cursor+3
	sta bot_left_cursor+3
	lda #$70
	sta top_right_cursor+3
	sta bot_right_cursor+3
	lda #$87
	sta bot_left_cursor
	sta bot_right_cursor
	rts
fourth_third_cursor:
	lda #$7f
	sta top_left_cursor
	sta top_right_cursor
	lda #$88
	sta top_left_cursor+3
	sta bot_left_cursor+3
	lda #$90
	sta top_right_cursor+3
	sta bot_right_cursor+3
	lda #$87
	sta bot_left_cursor
	sta bot_right_cursor
	rts
fourth_right_cursor:
	lda #$7f
	sta top_left_cursor
	sta top_right_cursor
	lda #$a8
	sta top_left_cursor+3
	sta bot_left_cursor+3
	lda #$b0
	sta top_right_cursor+3
	sta bot_right_cursor+3
	lda #$87
	sta bot_left_cursor
	sta bot_right_cursor
	rts
fifth_left_cursor:
	lda #$9f
	sta top_left_cursor
	sta top_right_cursor
	lda #$38
	sta top_left_cursor+3
	sta bot_left_cursor+3
	lda #$40
	sta top_right_cursor+3
	sta bot_right_cursor+3
	lda #$a7
	sta bot_left_cursor
	sta bot_right_cursor
	rts
fifth_second_cursor:
	lda #$9f
	sta top_left_cursor
	sta top_right_cursor
	lda #$58
	sta top_left_cursor+3
	sta bot_left_cursor+3
	lda #$60
	sta top_right_cursor+3
	sta bot_right_cursor+3
	lda #$a7
	sta bot_left_cursor
	sta bot_right_cursor
	rts
fifth_middle_cursor:
	lda #$9f
	sta top_left_cursor
	sta top_right_cursor
	lda #$78
	sta top_left_cursor+3
	sta bot_left_cursor+3
	lda #$80
	sta top_right_cursor+3
	sta bot_right_cursor+3
	lda #$a7
	sta bot_left_cursor
	sta bot_right_cursor
	rts
fifth_fourth_cursor:
	lda #$9f
	sta top_left_cursor
	sta top_right_cursor
	lda #$98
	sta top_left_cursor+3
	sta bot_left_cursor+3
	lda #$a0
	sta top_right_cursor+3
	sta bot_right_cursor+3
	lda #$a7
	sta bot_left_cursor
	sta bot_right_cursor
	rts
fifth_right_cursor:
	lda #$9f
	sta top_left_cursor
	sta top_right_cursor
	lda #$b8
	sta top_left_cursor+3
	sta bot_left_cursor+3
	lda #$c0
	sta top_right_cursor+3
	sta bot_right_cursor+3
	lda #$a7
	sta bot_left_cursor
	sta bot_right_cursor
	rts

; *********************************************************
; The following routines are used for placing and         *
; removing pegs from the board                            *
; *********************************************************
peg_in_20:
:	lda #$20
	sta $2006
	lda pegs_addresses,x
	sta $2006
	lda the_pegs,y
	sta $2007
	inx
	iny
	cpy #$04
	bne :-
	rts
peg_in_21:
:	lda #$21
	sta $2006
	lda pegs_addresses,x
	sta $2006
	lda the_pegs,y
	sta $2007
	inx
	iny
	cpy #$04
	bne :-
	rts
peg_in_22:
:	lda #$22
	sta $2006
	lda pegs_addresses,x
	sta $2006
	lda the_pegs,y
	sta $2007
	inx
	iny
	cpy #$04
	bne :-
	rts
no_peg_in_20:
:	lda #$20
	sta $2006
	lda pegs_addresses,x
	sta $2006
	lda no_pegs,y
	sta $2007
	inx
	iny
	cpy #$04
	bne :-
	rts
no_peg_in_21:
:	lda #$21
	sta $2006
	lda pegs_addresses,x
	sta $2006
	lda no_pegs,y
	sta $2007
	inx
	iny
	cpy #$04
	bne :-
	rts
no_peg_in_22:
:	lda #$22
	sta $2006
	lda pegs_addresses,x
	sta $2006
	lda no_pegs,y
	sta $2007
	inx
	iny
	cpy #$04
	bne :-
	rts

; *********************************************************
; This next stretch of code is used for handling how the  *
; d-pad is used during the game. There are quite a few    *
; tests to make sure the user stays within the boundaries *
; of the gameplay area. This is used at the beginning of  *
; the main loop                                           *
; *********************************************************
pegs_controls:
	lda control_pad					; Right button check/routine
	eor control_old
	and control_pad
	and #right_punch
	beq no_right_punch

	lda top_left_cursor
	cmp #$9f
	bne right_test1_done
	lda top_left_cursor+3
	cmp #$b8
	bne right_test1_done
	jmp no_right_punch
right_test1_done:
	lda top_left_cursor
	cmp #$7f
	bne right_test2_done
	lda top_left_cursor+3
	cmp #$a8
	bne right_test2_done
	jmp no_right_punch
right_test2_done:
	lda top_left_cursor
	cmp #$5f
	bne right_test3_done
	lda top_left_cursor+3
	cmp #$98
	bne right_test3_done
	jmp no_right_punch
right_test3_done:
	lda top_left_cursor
	cmp #$3f
	bne right_test4_done
	lda top_left_cursor+3
	cmp #$88
	bne right_test4_done
	jmp no_right_punch
right_test4_done:
	lda top_left_cursor
	cmp #$1f
	bne right_test5_done
	lda top_left_cursor+3
	cmp #$78
	bne right_test5_done
	jmp no_right_punch
right_test5_done:
	lda top_left_cursor+3
	clc
	adc #$20
	sta top_left_cursor+3
	lda top_right_cursor+3
	clc
	adc #$20
	sta top_right_cursor+3
	lda bot_left_cursor+3
	clc
	adc #$20
	sta bot_left_cursor+3
	lda bot_right_cursor+3
	clc
	adc #$20
	sta bot_right_cursor+3
	
no_right_punch:
	lda control_pad					; Left button check/routine
	eor control_old
	and control_pad
	and #left_punch
	beq no_left_punch

	lda top_left_cursor
	cmp #$9f
	bne left_test1_done
	lda top_left_cursor+3
	cmp #$38
	beq no_left_punch
left_test1_done:
	lda top_left_cursor
	cmp #$7f
	bne left_test2_done
	lda top_left_cursor+3
	cmp #$48
	beq no_left_punch
left_test2_done:
	lda top_left_cursor
	cmp #$5f
	bne left_test3_done
	lda top_left_cursor+3
	cmp #$58
	beq no_left_punch
left_test3_done:
	lda top_left_cursor
	cmp #$3f
	bne left_test4_done
	lda top_left_cursor+3
	cmp #$68
	beq no_left_punch
left_test4_done:
	lda top_left_cursor
	cmp #$1f
	bne left_test5_done
	lda top_left_cursor+3
	cmp #$78
	beq no_left_punch
left_test5_done:
	lda top_left_cursor+3
	sec
	sbc #$20
	sta top_left_cursor+3
	lda top_right_cursor+3
	sec
	sbc #$20
	sta top_right_cursor+3
	lda bot_left_cursor+3
	sec
	sbc #$20
	sta bot_left_cursor+3
	lda bot_right_cursor+3
	sec
	sbc #$20
	sta bot_right_cursor+3

no_left_punch:
	lda control_pad					; Down button check/routine
	eor control_old
	and control_pad
	and #down_punch
	beq no_down_punch
	lda top_left_cursor
	cmp #$9f
	beq no_down_punch
	lda top_left_cursor
	clc
	adc #$20
	sta top_left_cursor
	lda top_left_cursor+3
	sec
	sbc #$10
	sta top_left_cursor+3
	lda top_right_cursor
	clc
	adc #$20
	sta top_right_cursor
	lda top_right_cursor+3
	sec
	sbc #$10
	sta top_right_cursor+3
	lda bot_left_cursor
	clc
	adc #$20
	sta bot_left_cursor
	lda bot_left_cursor+3
	sec
	sbc #$10
	sta bot_left_cursor+3
	lda bot_right_cursor
	clc
	adc #$20
	sta bot_right_cursor
	lda bot_right_cursor+3
	sec
	sbc #$10
	sta bot_right_cursor+3
	jmp no_down_punch
fake_no_up_punch:
	jmp no_up_punch

no_down_punch:
	lda control_pad					; Up button check/routine
	eor control_old
	and control_pad
	and #up_punch
	beq fake_no_up_punch
	lda top_left_cursor
	cmp #$9f
	bne up_test1_done
	lda top_left_cursor+3
	cmp #$b8
	bne up_test1_done
	lda top_left_cursor
	sec
	sbc #$20
	sta top_left_cursor
	lda top_left_cursor+3
	sec
	sbc #$10
	sta top_left_cursor+3
	lda top_right_cursor
	sec
	sbc #$20
	sta top_right_cursor
	lda top_right_cursor+3
	sec
	sbc #$10
	sta top_right_cursor+3
	lda bot_left_cursor
	sec
	sbc #$20
	sta bot_left_cursor
	lda bot_left_cursor+3
	sec
	sbc #$10
	sta bot_left_cursor+3
	lda bot_right_cursor
	sec
	sbc #$20
	sta bot_right_cursor
	lda bot_right_cursor+3
	sec
	sbc #$10
	sta bot_right_cursor+3
	jmp no_up_punch
up_test1_done:
	lda top_left_cursor
	cmp #$7f
	bne up_test2_done
	lda top_left_cursor+3
	cmp #$a8
	bne up_test2_done
	lda top_left_cursor
	sec
	sbc #$20
	sta top_left_cursor
	lda top_left_cursor+3
	sec
	sbc #$10
	sta top_left_cursor+3
	lda top_right_cursor
	sec
	sbc #$20
	sta top_right_cursor
	lda top_right_cursor+3
	sec
	sbc #$10
	sta top_right_cursor+3
	lda bot_left_cursor
	sec
	sbc #$20
	sta bot_left_cursor
	lda bot_left_cursor+3
	sec
	sbc #$10
	sta bot_left_cursor+3
	lda bot_right_cursor
	sec
	sbc #$20
	sta bot_right_cursor
	lda bot_right_cursor+3
	sec
	sbc #$10
	sta bot_right_cursor+3
	jmp no_up_punch
up_test2_done:
	lda top_left_cursor
	cmp #$5f
	bne up_test3_done
	lda top_left_cursor+3
	cmp #$98
	bne up_test3_done
	lda top_left_cursor
	sec
	sbc #$20
	sta top_left_cursor
	lda top_left_cursor+3
	sec
	sbc #$10
	sta top_left_cursor+3
	lda top_right_cursor
	sec
	sbc #$20
	sta top_right_cursor
	lda top_right_cursor+3
	sec
	sbc #$10
	sta top_right_cursor+3
	lda bot_left_cursor
	sec
	sbc #$20
	sta bot_left_cursor
	lda bot_left_cursor+3
	sec
	sbc #$10
	sta bot_left_cursor+3
	lda bot_right_cursor
	sec
	sbc #$20
	sta bot_right_cursor
	lda bot_right_cursor+3
	sec
	sbc #$10
	sta bot_right_cursor+3
	jmp no_up_punch
up_test3_done:
	lda top_left_cursor
	cmp #$3f
	bne up_test4_done
	lda top_left_cursor+3
	cmp #$88
	bne up_test4_done
	lda top_left_cursor
	sec
	sbc #$20
	sta top_left_cursor
	lda top_left_cursor+3
	sec
	sbc #$10
	sta top_left_cursor+3
	lda top_right_cursor
	sec
	sbc #$20
	sta top_right_cursor
	lda top_right_cursor+3
	sec
	sbc #$10
	sta top_right_cursor+3
	lda bot_left_cursor
	sec
	sbc #$20
	sta bot_left_cursor
	lda bot_left_cursor+3
	sec
	sbc #$10
	sta bot_left_cursor+3
	lda bot_right_cursor
	sec
	sbc #$20
	sta bot_right_cursor
	lda bot_right_cursor+3
	sec
	sbc #$10
	sta bot_right_cursor+3
	jmp no_up_punch
up_test4_done:
	lda top_left_cursor
	cmp #$1f
	bne up_test5_done
	lda bot_right_cursor+3
	cmp #$80
	beq no_up_punch
up_test5_done:
	lda top_left_cursor
	cmp #$1f
	beq no_up_punch
	lda top_left_cursor
	sec
	sbc #$20
	sta top_left_cursor
	lda top_left_cursor+3
	clc
	adc #$10
	sta top_left_cursor+3
	lda top_right_cursor
	sec
	sbc #$20
	sta top_right_cursor
	lda top_right_cursor+3
	clc
	adc #$10
	sta top_right_cursor+3
	lda bot_left_cursor
	sec
	sbc #$20
	sta bot_left_cursor
	lda bot_left_cursor+3
	clc
	adc #$10
	sta bot_left_cursor+3
	lda bot_right_cursor
	sec
	sbc #$20
	sta bot_right_cursor
	lda bot_right_cursor+3
	clc
	adc #$10
	sta bot_right_cursor+3
no_up_punch:
	rts



end_screen_writes:
	lda #$00
	sta no_music
	jsr song_fourth
	jsr vblank_wait
	jsr PPU_off
	jsr clear_nametable  ; Go here if 10 or more
	jsr stabilize
	jsr PPU_with_sprites
	jsr delay_with_nmi_count
	jsr vblank_wait
	jsr PPU_off
	lda #$20
	sta $2006
	lda #$a6
	sta $2006
	ldx #$00
	lda you_made_word
:	sta $2007
	inx
	lda you_made_word,x
	bne :-
	jsr stabilize
	jsr PPU_with_sprites
	jsr delay_with_nmi_count
	jsr delay_with_nmi_count
	jsr vblank_wait
	jsr PPU_off
	lda #$37
	sta left_num_moves
	sta right_num_moves
	lda #$28
	sta left_num_moves+3
	lda #$30
	sta right_num_moves+3
	lda #$20
	sta $2006
	lda #$e6
	sta $2006
	ldx #$00
	lda moves_word
:	sta $2007
	inx
	lda moves_word,x
	bne :-
	jsr stabilize
	jsr PPU_with_sprites
	jsr delay_with_nmi_count
	jsr delay_with_nmi_count
	lda peg_latch
	cmp #$00
	beq :+
	bne :+++
:	jsr vblank_wait
	jsr PPU_off
	lda #$47
	sta right_num_peg
	lda #$80
	sta right_num_peg+3
	lda #$21
	sta $2006
	lda #$27
	sta $2006
	ldx #$00
	lda you_left_peg_word
:	sta $2007
	inx
	lda you_left_peg_word,x
	bne :-
	jmp @do_this_one
	jsr stabilize
	jsr PPU_with_sprites
:	lda peg_latch
	cmp #$02
	beq :+
	bne :+++
:	jsr vblank_wait
	jsr PPU_off
	lda #$47
	sta right_num_peg
	lda #$78
	sta right_num_peg+3
	lda #$21
	sta $2006
	lda #$26
	sta $2006
	ldx #$00
	lda you_left_pegs_word
:	sta $2007
	inx
	lda you_left_pegs_word,x
	bne :-
	jmp @do_this_one
	jsr stabilize
	jsr PPU_with_sprites
:	jsr vblank_wait
	jsr PPU_off
	lda #$47
	sta left_num_peg
	sta right_num_peg
	lda #$78
	sta left_num_peg+3
	lda #$80
	sta right_num_peg+3
	lda #$21
	sta $2006
	lda #$26
	sta $2006
	ldx #$00
	lda you_left_10_pegs_word
:	sta $2007
	inx
	lda you_left_10_pegs_word,x
	bne :-
@do_this_one:
	jsr stabilize
	jsr PPU_with_sprites
	jsr delay_with_nmi_count
	jsr delay_with_nmi_count
	jsr vblank_wait
	jsr PPU_off
	lda #$21
	sta $2006
	lda #$64
	sta $2006
	ldx #$00
	lda the_board_word
:	sta $2007
	inx
	lda the_board_word,x
	bne :-
	jsr stabilize
	jsr PPU_with_sprites
	jsr delay_with_nmi_count
	jsr delay_with_nmi_count
	jsr delay_with_nmi_count
	jsr delay_with_nmi_count
	rts

ranking:							; Here we do somet tests to find out what
	lda right_num_peg+1				; the player ended up ranking. The ranks are:
	cmp #$30						; 1 - Rad Dude!
	beq :+							; 2 - Smarty Pants
	bne :+++						; 3 - Meh...
:	jsr vblank_wait					; 4 - Numb Skull
	jsr PPU_off						; 5 through 9 - Blockhead
	lda #$22						; 10 - I.Q. of OSG!
	sta $2006						; 10 has a special ranking because one of
	lda #$2a						; the game testers (OSG) figured out how
	sta $2006						; to get that super low score ;)
	ldx #$00
	lda iq_word						; The tests will print the result onscreen
:	sta $2007						; with each test being performed on
	inx								; right_num_peg+1
	lda iq_word,x
	bne :-
	jsr stabilize
	jsr PPU_with_sprites
	jmp all_done
:	cmp #$31
	beq :+
	bne :+++
:	jsr vblank_wait
	jsr PPU_off
	lda #$22
	sta $2006
	lda #$2b
	sta $2006
	ldx #$00
	lda rad_word
:	sta $2007
	inx
	lda rad_word,x
	bne :-
	jsr stabilize
	jsr PPU_with_sprites
	jmp all_done
:	cmp #$32
	beq :+
	bne :+++
:	jsr vblank_wait
	jsr PPU_off
	lda #$22
	sta $2006
	lda #$29
	sta $2006
	ldx #$00
	lda smarty_word
:	sta $2007
	inx
	lda smarty_word,x
	bne :-
	jsr stabilize
	jsr PPU_with_sprites
	jmp all_done
:	cmp #$33
	beq :+
	bne :+++
:	jsr vblank_wait
	jsr PPU_off
	lda #$22
	sta $2006
	lda #$2c
	sta $2006
	ldx #$00
	lda meh_word
:	sta $2007
	inx
	lda meh_word,x
	bne :-
	jsr stabilize
	jsr PPU_with_sprites
	jmp all_done
:	cmp #$34
	beq :+
	bne :+++
:	jsr vblank_wait
	jsr PPU_off
	lda #$22
	sta $2006
	lda #$2a
	sta $2006
	ldx #$00
	lda numb_word
:	sta $2007
	inx
	lda numb_word,x
	bne :-
	jsr stabilize
	jsr PPU_with_sprites
	jmp all_done
:	jsr vblank_wait
	jsr PPU_off
	lda #$22
	sta $2006
	lda #$2b
	sta $2006
	ldx #$00
	lda block_word
:	sta $2007
	inx
	lda block_word,x
	bne :-
	jsr stabilize
	jsr PPU_with_sprites
	jmp all_done
game_done:
	lda #$f7
	sta top_left_cursor
	sta top_right_cursor
	sta bot_left_cursor
	sta bot_right_cursor
	sta left_num_moves
	sta right_num_moves
	lda #$00
	sta top_left_cursor+3
	sta top_right_cursor+3
	sta bot_left_cursor+3
	sta bot_right_cursor+3
	sta left_num_moves+3
	sta right_num_moves+3
	lda left_num_peg+1
	cmp #$30
	beq :+
	bne :+++
:	lda right_num_peg+1
	cmp #$31
	beq :+
	bne :++
:	lda #$00						; Set peg_latch to zero if 1 peg is left
	sta peg_latch					; This will mean the player gets the
	jmp good_job_screen				; "Good Job!" screen, which can only be
:	lda right_num_moves+1			; seen when there is one peg left.
	cmp #$34
	beq :+
	bne :++
:	lda #$01
	sta peg_latch
	jsr end_screen_writes
	jmp ranking

:	lda #$02
	sta peg_latch
	jsr end_screen_writes
	jmp ranking

good_job_screen:
	jsr vblank_wait
	jsr PPU_off
	lda #$20
	sta $2006
	ldx #$00
	stx $2006
	stx odd_even
compressed_screen:
	lda goodjob,x					; Grab the first byte from the
	tay								; nametable indexed by X, then
	lda odd_even					; stick that in Y. Test if odd_even
	bne tile_number					; is 0 or 1. If it's 0, keep going
									; and stick Y in 'number_of_tiles',
	sty number_of_tiles				; increment odd_even to 1 so on
	inc odd_even					; the next pass it will skip this
	inx								; portion of code, and increment
	jmp compressed_screen			; X. Jump back to compressed_screen
									; if you got to that point.
tile_number:						; If odd_even was 1, stick Y in
	sty the_tiles					; 'the_tiles'. This is the actual place
	ldy number_of_tiles				; you find the tiles in your chr. Load
:	lda the_tiles					; Y with how many tiles there are
	sta $2007						; and stick all of them in $2007
	dey								; until Y is expired. Decrement
	bne :-							; odd_even to put it back to 0,
	dec odd_even					; increment X for the overall file read
	inx								; and test for the size of the compressed file
	cpx #$7c						; if it doesn't pass the test, go back
	bne compressed_screen			; to the beginning and keep reading
	jsr vblank_wait
	jsr stabilize
	jsr PPU_with_sprites
	lda #$0f						; Jeremiah saying "Good Job!"
	sta $4010
	lda #$3f
	sta $4011
    lda #$d8
	sta $4012
	lda #$8f
	sta $4013
	lda #$0f
	sta $4015
	lda #$1f
	sta $4015
	jsr delay_with_nmi_count
	jsr delay_with_nmi_count
	jsr end_screen_writes
	jmp ranking
all_done:
	jsr delay_with_nmi_count
	jsr delay_with_nmi_count
	jsr delay_with_nmi_count
	jsr delay_with_nmi_count
	jsr vblank_wait
	jsr PPU_off
	lda #$22
	sta $2006
	lda #$c5
	sta $2006
	ldx #$00
	lda start_for_title
:	sta $2007
	inx
	lda start_for_title,x
	bne :-
	jsr stabilize
	jsr PPU_with_sprites
@loop:
	lda control_pad					; Start button check/routine
	eor control_old
	and control_pad
	and #start_punch
	beq :+
	jsr delay_with_nmi_count2
	jmp reset
:	jmp @loop

; *********************************************************
; sprite_cram is the routine to update all sprites when   *
; an NMI is triggered.                                    *
; *********************************************************
sprite_cram:
	lda #>top_left_cursor
	sta $4014
	rts

; *********************************************************
; The control routine called in NMI. This is only for     *
; strobing the controller. The actual routines of what    *
; players can do are in various parts of the main loop(s) *
; depending on what part of the program they are in.      *
; *********************************************************
controller:
	lda #$01
	sta $4016
	lda #$00
	sta $4016
	lda control_pad
	sta control_old
	ldx #$08
:	lda $4016
	lsr A
	ror control_pad
	dex
	bne :-
	rts

; *********************************************************
; The sacred NMI routine!!                                *
; *********************************************************
nmi:
	pha
	txa
	pha
	tya
	pha
	inc nmi_num						; nmi_num is incremented every NMI
	jsr sprite_cram					; Refresh the sprites
	lda no_control					; Test for control strobe to be active.
	bne :+							; if it's not, then we skip to the music.
	jsr controller					; If it is, strobe controller.
:	lda no_music					; Test if we have music. Play it if we do.
	bne :+
	jsr play
:	lda nmi_scroll_off				; Check if we want to keep the scroll at
	bne end_nmi						; zeroes. 
	lda #$00
	sta $2005
	sta $2005
end_nmi:
	pla
	tay
	pla
	tax
	pla
	rti

irq:
	rti

palette:
.incbin "g\pegs.pal"

; *********************************************************
; The .bytes below are setup for the sprites to be used   *
; .byte (Y-Pos),(Tile Number),(Attributes),(X-Pos)        *
; *********************************************************
the_sprites:
	.byte $1f,$18,$01,$78			; Top left of cursor
	.byte $1f,$19,$01,$80			; Top right of cursor
	.byte $27,$1a,$01,$78			; Bottom left of cursor
	.byte $27,$1b,$01,$80			; Bottom right of cursor
	.byte $bf,$30,$00,$98			; Left number of moves
	.byte $bf,$30,$00,$a0			; Right number of moves
	.byte $ef,$1e,$03,$00			; Top left of placeholder
	.byte $ef,$1f,$03,$00			; Top right of placeholder
	.byte $ef,$8c,$03,$00			; Bottom left of placeholder
	.byte $ef,$8d,$03,$00			; Bottom right of placeholder
	.byte $ef,$31,$00,$00			; Left number of pegs
	.byte $ef,$34,$00,$00			; Right number of pegs
	.byte $87,$1c,$00,$58			; Left side menu select
	.byte $87,$1d,$00,$60			; Right side menu select
	.byte $4d,$8e,$02,$78			; 3 top left
	.byte $4d,$8f,$02,$80			; 3 top right
	.byte $55,$90,$02,$78			; 3 bottom left
	.byte $55,$91,$02,$80			; 3 bottom right
	.byte $4d,$92,$02,$78			; 2 top left
	.byte $4d,$93,$02,$80			; 2 top right
	.byte $55,$94,$02,$78			; 2 bottom left
	.byte $55,$95,$02,$80			; 2 bottom right
	.byte $4d,$96,$02,$78			; 1 top left
	.byte $4d,$97,$02,$80			; 1 top right
	.byte $55,$98,$02,$78			; 1 bottom left
	.byte $55,$99,$02,$80			; 1 bottom right
	.byte $4d,$9a,$02,$78			; GO! top left
	.byte $4d,$9b,$02,$80			; GO! top right
	.byte $55,$9c,$02,$78			; GO! bottom left
	.byte $55,$9d,$02,$80			; GO! bottom right

; *********************************************************
; The bytes for the pegs and not having pegs              *
; *********************************************************
the_pegs:
	.byte $01,$02,$03,$04
no_pegs:
	.byte $05,$06,$07,$08
pegs_addresses:
	.byte $8f,$90,$af,$b0		; Top position
	.byte $0d,$0e,$2d,$2e		; Second row, left
	.byte $11,$12,$31,$32		; Second row, right
	.byte $8b,$8c,$ab,$ac		; Third row, left
								; and fifth row, second
	.byte $8f,$90,$af,$b0		; Third row, middle
								; and fifth row, middle
	.byte $93,$94,$b3,$b4		; Third row, right
								; and fifth row, fourth
	.byte $09,$0a,$29,$2a		; Fourth row, left
	.byte $0d,$0e,$2d,$2e		; Fourth row, second
	.byte $11,$12,$31,$32		; Fourth row, third
	.byte $15,$16,$35,$36		; Fourth row, right
	.byte $87,$88,$a7,$a8		; Fifth row, left
	.byte $97,$98,$b7,$b8		; Fifth row, right

; *********************************************************
; Sound effects down here                                 *
; *********************************************************
sound_fx:
	.byte %11001111,%10000100,%11110001,%10101001 	; Start
	.byte %01000000,%10111010,%00000000				; 3-2-1 count
	.byte %11000001,%10000010,%11110001,%10101001	; Peg lay down
	.byte %01001111,%10000100,%00001111,%10101001	; Start over
	
; *********************************************************
; Include all of the nametables for the game below        *
; *********************************************************
pegs_board:
.incbin "g\pegs_board.nam"
title:
.incbin "g\pegs_title.nam"
manual:
.incbin "g\pegs_manual.nam"
credits1:
.incbin "g\pegs_credits1.nam"
credits2:
.incbin "g\pegs_credits2.nam"
soundtest:
.incbin "g\pegs_sound.nam"
pegegg:
.incbin "g\pegs_eggy.nam"
goodjob:
.incbin "g\pegs_job.rle"

; *********************************************************
; Different words to be put on screen                     *
; *********************************************************
press_word:
	.byte "Press",0
start_word:
	.byte "Start",0
to_word:
	.byte "to",0
begin_word:
	.byte "Begin",0
again_word:
	.byte "Again",0
you_made_word:
	.byte "You made a total of",0
moves_word:
	.byte "  moves, which means",0
you_left_10_pegs_word:
	.byte "you left    pegs on",0
you_left_pegs_word:
	.byte "you left   pegs on",0
you_left_peg_word:
	.byte "you left   peg on",0
the_board_word:
	.byte "the board. Your status:",0
rad_word:
	.byte "Rad Dude!",0
smarty_word:
	.byte "Smarty Pants",0
meh_word:
	.byte "Meh...",0
numb_word:
	.byte "Numb Skull",0
block_word:
	.byte "Blockhead",0
iq_word:
	.byte "I.Q. of OSG!",0
start_for_title:
	.byte "Press Start for Title",0

; *********************************************************
; Between the samples we throw in a bit of padding. This  *
; allows us to enter a proper number to use for the       *
; address of loading each sample                          *
; *********************************************************
.segment "SAMPLES"
.incbin "s\samples\slot.dmc"
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.incbin "s\samples\go.dmc"
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.incbin "s\samples\job.dmc"


.segment "VECTORS"
	.addr nmi
	.addr reset
	.addr irq
