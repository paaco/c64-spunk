;
; trees
;
; 2522 bytes exomized

; variables
!addr {
ZP_IRQNUMBER = $10      ; current raster IRQ
ZP_MSB = $11            ; temp location for MSB calculation
ZP_SYNC = $12           ; set <>0 after top raster IRQ for sync
ZP_GAMESTATE = $13      ; current game state (0=title screen, etc. see main loop)

SPRITE_PTR = $07F8

VIC_SPR_X_MSB = $D010
VIC_SPR_ENA = $D015
VIC_SPR_DHEIGHT = $D017
VIC_SPR_BEHIND = $D01B
VIC_SPR_MC = $D01C
VIC_SPR_DWIDTH = $D01D
VIC_COL_TXT_MC1 = $D022
VIC_COL_TXT_MC2 = $D023
VIC_SPR_MC1 = $D025
VIC_SPR_MC2 = $D026
VIC_SPR_COL = $D027
}

; constants
RASTERTOP=0             ; top raster irq
TREETOPY=50             ; first y-position of trees
; animation frames
FRAME_SPUNK_WALK=SPRITE_OFFSET + (sprites_spunk-sprites)/64
FRAME_SPUNK_JUMP=FRAME_SPUNK_WALK+3

BLACK=0
WHITE=1
CYAN=3
PURPLE=4
GREEN=5
BLUE=6
YELLOW=7
ORANGE=8
BROWN=9
LIGHT_GREEN=13
LIGHT_BLUE=14

COLOR_BORDER = BLACK
COLOR_SKY = LIGHT_BLUE
COLOR_DISTANT = BLACK
COLOR_BACKGROUND = BROWN
COLOR_PLANTS = GREEN
COLOR_PLANTS_OUTLINE = BLACK
COLOR_CROWN_HIGHLIGHT = LIGHT_GREEN
COLOR_CROWN_BRANCHES = ORANGE
COLOR_TREES_LIGHT = ORANGE
COLOR_TREES_DARK = BLACK

*=$0801
!byte $0c,$08,$c0,$07,$9e,$20,$32,$30,$36,$32,$00,$00,$00   ; 1984 SYS 2062

;2062
            lda $02A6 ; $02A6/678:   Flag: TV Standard: $00 = NTSC, $01 = PAL
            bne OK_its_PAL

            ; NTSC fix X-offset correction
            lda #0
            sta NTSC_fix1+1
            sta NTSC_fix2+1

OK_its_PAL:
            ; global colors and VIC settings
            lda #COLOR_BORDER   ; NOTE D020 could already be set by decruncher
            sta $D020
            lda #COLOR_DISTANT
            sta VIC_COL_TXT_MC1
            sta ZP_SYNC ; set to 0
            lda #COLOR_PLANTS
            sta VIC_COL_TXT_MC2
            lda #%00011000 ; charset bits 3-1: $2000=$800 * %100; screen bits 7-4: $0400=$0400 * %0001
            sta $D018
            lda #%11111111
            sta VIC_SPR_ENA
            sta VIC_SPR_MC
            sta $DC00       ; disconnect keyboard
            sta ZP_GAMESTATE ; set to $FF

            ; partial cls and color setup
            ldx #0
-           lda #$20
            sta $0400,x
            lda #8+COLOR_PLANTS_OUTLINE
            sta $DB00,x
            inx
            bne -

            sei

            ; copy ROM charset
            lda #$33        ; Bank in ROM font
            sta $01
            ldx #8          ; char0 remains as it is ...
-           lda $D000,x
            sta $2000,x
            lda $D100,x
            sta $2100,x
            inx
            bne -

            lda #$35        ; Bank out KERNAL and BASIC
            sta $01

            lda #$7F        ; Disable and ACK CIA timers
            bit $DC0D
            sta $DC0D
            bit $DD0D
            sta $DD0D

            lda #<NMI
            sta $FFFA
            lda #>NMI
            sta $FFFB

            lda #$01        ; Enable raster IRQs only
            sta $D01A

            lda #$FF        ; ACK IRQs
            sta $D019

            lda InitRaster  ; raster line to trigger
            sta $D012
            lda #%00011000+3  ; set bit 9 of raster line to 0 + Y-scroll
            sta $D011

            lda InitIRQ     ; IRQ only low byte changes in raster IRQ
            sta $FFFE
            lda #>IRQ_Top
            sta $FFFF

            cli

            jmp init_state_0_title_screen


;-----------
; main loop
;-----------

            ; main loop in synced after IRQ_Top and runs in upper portion of the screen
            ; it scrolls stuff and handles joystick and gameplay
loop:       lda ZP_SYNC
            beq loop
            sta $D020               ; DEBUG

            lda ZP_GAMESTATE
            bne +

        ; game_state 0 : title screen
            ; wait for fire
            lda $DC00           ; Joystick A in control port 2 0=active: 1=up 2=down 4=left 8=right 16=fire
            and $DC01           ; Joystick B in control port 1 0=active: 1=up 2=down 4=left 8=right 16=fire
            and #%00010000      ; ignore other bits not from the joystick
            bne state_handled
            jmp init_state_1_get_ready

+           cmp #1
            bne +

        ; game_state 1 : get ready
            ; wait a few seconds
            dec getready
            bne state_handled
            jmp init_state_2_game_play

+           cmp #2
            bne state_handled

        ; game_state 2 : game play
            ; TODO handle game states:
            ; TODO 3.game over -> 4
            ; TODO scroll objects here
            jsr move_Spunk
            ; TODO DEBUG move plant movement elsewhere
            ; lda plants+1
            ; clc
            ; adc #$80
            ; sta plants+1
            ; bcc +           ; no borrow? then ok
            inc plants
            ; fall-through

state_handled:
            jsr draw_plants

            lda #0
            sta ZP_SYNC
            sta $D020               ; DEBUG
            jmp loop


init_state_0_title_screen:
            lda #%00010000  ; Text MC + 38 columns + X-scroll 0
            sta Scroll1
            sta Scroll2
            sta Scroll3
            lda #%11111111 ; sprites behind text
            sta Spr_Behind
            ; TODO setup sprites?
            jsr draw_distant_background
            jsr draw_logo
            jsr draw_introtext
next_state: inc ZP_GAMESTATE
            jmp state_handled

init_state_1_get_ready:
            ; TODO setup possible sprites and scroll values
            jsr draw_distant_background
            jsr draw_getreadytext
            lda #100
            sta getready
            jmp next_state

init_state_2_game_play:
            ; TODO
            lda #0 ; sprites before characters (obstacles)
            sta Spr_Behind
            jsr draw_notext
            jmp next_state

getready:   !byte 0


;----------------
; handle sprites
;----------------

; Sprite coordinates are 9.7 fixed point shifted 24 to account for x-expanded sprites

; high     low
; 87654321 0ddddddd
; ^ends up in MSB
; 1) subtract 24 (range -24 .. 344)
; 2) for PAL subtract 8 for values < 0: so the range is 1E0 .. 1F7 000 .. 16F
;    for NTSC don't subtract 8, so the NTSC range is    1E8 .. 1FF 000 .. 16F
; see https://bumbershootsoft.wordpress.com/2019/07/01/c64-fat-sprite-workarounds/

update_sprite_X:
; update all 8 sprites in IRQ_Top
            ldy #0
            sty ZP_MSB
-           lda Sprites_X_posH,y    ; X-pos bits 8..1
            sec
            sbc #12                 ; -24 shifted once
            ldx Sprites_X_posL,y    ; X-pos bit 0 (in bit 7)
            cpx #$80                ; copy bit 7 from X into carry
            rol                     ; 7..0, carry contains MSB bit 8
            ; C=MSB=0: pos <= 255; C=MSB=1: pos >=256 or negative if >=$E0
            bcc +
            ; C=MSB=1 here
            pha
            lda ZP_MSB
            ldx Sprites_prio,y
            ora MSB,x               ; single bit
            sta ZP_MSB
            pla
            ; subtract 8 if A < 0 (only for PAL, don't do this on NTSC, i.e. subtract 0)
            cmp #$E0
            bcc +
            ;sec carry already set
NTSC_fix1:  sbc #8
+           ldx TIMES_5,y
            ; x-position
            sta Crown_X0+1,x        ; SELF MODIFY corresponding lda# statement
            lda Sprites_prio,y
            asl
            sta Crown_XI0+1,x       ; SELF MODIFY corresponding sta $D0xx statement
            iny
            cpy #8
            bne -
            lda ZP_MSB
            sta Crown_X_MSB+1       ; SELF MODIFY corresponding lda# statement
; update the 6 trees in IRQ_Start_trees
            ldy #0
            sty ZP_MSB
-           lda Sprites_X_posH,y    ; X-pos bits 8..1
            sec
            sbc #6                  ; -12 shifted once
            ldx Sprites_X_posL,y    ; X-pos bit 0 (in bit 7)
            cpx #$80                ; copy bit 7 from X into carry
            rol                     ; 7..0, carry contains MSB bit 8
            ; C=MSB=0: pos <= 255; C=MSB=1: pos >=256 or negative if >=$E0
            bcc +
            ; C=MSB=1 here
            pha
            lda ZP_MSB
            ldx Sprites_prio,y
            ora MSB,x               ; single bit
            sta ZP_MSB
            pla
            ; subtract 8 if A < 0 (only for PAL, don't do this on NTSC, i.e. subtract 0)
            cmp #$E0
            bcc +
            ;sec carry already set
NTSC_fix2:  sbc #8
+           ldx TIMES_5,y
            ; x-position
            sta Tree_X0+1,x        ; SELF MODIFY corresponding lda# statement
            lda Sprites_prio,y
            asl
            sta Tree_XI0+1,x       ; SELF MODIFY corresponding sta $D0xx statement
            iny
            cpy #6
            bne -
            lda ZP_MSB
            sta Tree_X_MSB+1       ; SELF MODIFY corresponding lda# statement
            rts


update_sprite_data:
; update color and pointer data for all 8 sprites
            ldy #0
-           ldx TIMES_5,y
            ; color
            lda Sprites_colors,y
            sta Crown_C0+1,x        ; SELF MODIFY corresponding lda# statement
            lda Sprites_prio,y
            clc
            adc #<VIC_SPR_COL
            sta Crown_CI0+1,x       ; SELF MODIFY corresponding sta $D0xx statement
            ; pointer
            adc #<SPRITE_PTR-VIC_SPR_COL
            sta Crown_P0+1,x        ; SELF MODIFY corresponding sta $07F8 statement
            iny
            cpy #8
            bne -
; fix y-position indices for actor sprites
            lda Sprites_prio+6
            sec
            rol ; Ax2+1
            sta Crown_Y6+1          ; SELF MODIFY corresponding sta $D0xx statement
            lda Sprites_prio+7
            sec
            rol ; Ax2+1
            sta Crown_Y7+1          ; SELF MODIFY corresponding sta $D0xx statement
; set double height; disable for actor sprites
            ldx Sprites_prio+6
            lda MSB,x
            ldx Sprites_prio+7
            ora MSB,x
            eor #$FF
            sta Tree_DH+1
; update indices, color and pointer data for the 6 trees
            ldy #0
-           ldx TIMES_3,y
            lda Sprites_prio,y
            sec
            rol ; Ax2+1
            ; y-position indices
            sta Crown_Y0+1,x       ; SELF MODIFY corresponding sta $D0xx statement
            sta Tree_Y0+1,x        ; SELF MODIFY corresponding sta $D0xx statement
            sta Bump_Y0+1,x        ; SELF MODIFY corresponding sta $D0xx statement
            ; color index
            lda Sprites_prio,y
            clc
            adc #<VIC_SPR_COL
            sta Tree_CI0+1,x       ; SELF MODIFY corresponding sta $D0xx statement
            ; pointer
            adc #<SPRITE_PTR-VIC_SPR_COL
            sta Bump_P0+1,x        ; SELF MODIFY corresponding sta $07F8 statement
            iny
            cpy #6
            bne -
            rts


; move Spunk based on joystick
move_Spunk:
            lda $DC00           ; Joystick A in control port 2 0=active: 1=up 2=down 4=left 8=right 16=fire
            and $DC01           ; Joystick B in control port 1 0=active: 1=up 2=down 4=left 8=right 16=fire
            and #%00011111      ; ignore other bits not from the joystick
            tay ; backup
            and #$01
            bne +
            ; UP
            lda Spunk_Y+1
            sec
            sbc #$02
            cmp #150
            bcs .ok
            lda #150
.ok:        sta Spunk_Y+1
+           tya
            and #$02
            bne +
            ; DOWN
            lda Spunk_Y+1
            clc
            adc #$02
            cmp #220
            bcc .ok2
            lda #220
.ok2:       sta Spunk_Y+1
+           tya
            and #$10
            bne +
            ; FIRE
            inc Spunk_X ; DEBUG
+           cpy #%00011111
            bne .animate
            ; reset anim frame
            ldx #FRAME_SPUNK_WALK
            lda #0
            beq .anim_ok ; jmp always
.animate:   dec delay
            bpl .ok3
            lda #4  ; delay
            ldx Spunk_Ptr+1
            inx
            cpx #FRAME_SPUNK_JUMP
            bne .anim_ok
            ldx #FRAME_SPUNK_WALK
.anim_ok:   sta delay
            stx Spunk_Ptr+1
.ok3:       rts

delay:      !byte 0


;-------------------
; handle background
;-------------------

; draw plants row (full redraw takes as long as scrolling and updating)
draw_plants:
            lda plants
            and #$07        ; X-scroll
            eor #$07        ; reverse
            ora #%00010000  ; Text MC + 38 columns
            sta Scroll4
            lda plants
            cmp #8*11
            bcc +           ; borrow? then smaller thus ok
            sbc #8*11       ; handle modulo 11 overflow
            sta plants
+           lsr
            lsr
            lsr
            tax             ; X=initial offset (0..10)
            clc
            ldy #0
-           txa
            adc #64+32
            sta $0400+22*40,y
            adc #11
            sta $0400+23*40,y
            adc #11
            sta $0400+24*40,y
            inx
            cpx #11
            bne +
            ldx #0
+           iny
            cpy #40
            bne -
            rts


; distant trees in the background
draw_distant_background:
            ldx #0
            lda #$20
-           sta $0400+40,x
            inx
            cpx #40
            bne -
            ldx #0
-           lda distant,x
            sta $0400+40*2,x
            lda distant+$100,x
            sta $0500+40*2,x
            lda distant+520-256,x
            sta $0400+520-256+40*2,x
            lda #8 ; only the MC part matters
            sta $D800+40*2,x
            sta $D900+40*2,x
            sta $D800+520-256+40*2,x
            inx
            bne -
            rts

; logo (360 bytes) overwrites distant trees
draw_logo:
            HALFLOGO=180
            LOGOY=1
            ldx #0
-           lda logo,x
            bmi +
            sta $0400+40*LOGOY,x
            lda #PURPLE
            sta $D800+40*LOGOY,x
+           lda logo+HALFLOGO,x
            bmi +
            sta $0400+40*LOGOY+HALFLOGO,x
            lda #WHITE
            sta $D800+40*LOGOY+HALFLOGO,x
+           inx
            cpx #HALFLOGO
            bne -
            rts

; introtext
draw_introtext:
            INTROTEXTY=16
            ldx #introtext_end-introtext-1
-           lda introtext,x
            sta $0400+INTROTEXTY*40,x
            lda #CYAN
            sta $D800+INTROTEXTY*40,x
            dex
            bne -
            rts

; getreadytext
draw_getreadytext:
            ldx #getreadytext_end-getreadytext-1
-           lda getreadytext,x
            sta $0400+INTROTEXTY*40,x
            lda #CYAN
            sta $D800+INTROTEXTY*40,x
            dex
            bne -
            rts

; notext
draw_notext:
            ldx #0
            lda #$20
-           sta $0400+INTROTEXTY*40,x
            inx
            bne -
            rts


;------
; prng
;------

; RANDOM routine from https://codebase64.org/doku.php?id=base:16bit_xorshift_random_generator
rng_zp_low = $02
rng_zp_high = $03
        ; seeding
        LDA #1 ; seed, can be anything except 0
        STA rng_zp_low
        LDA #0
        STA rng_zp_high
        ; the RNG. You can get 8-bit random numbers in A or 16-bit numbers
        ; from the zero page addresses. Leaves X/Y unchanged.
random  LDA rng_zp_high
        LSR
        LDA rng_zp_low
        ROR
        EOR rng_zp_high
        STA rng_zp_high ; high part of x ^= x << 7 done
        ROR             ; A has now x >> 9 and high bit comes from low byte
        EOR rng_zp_low
        STA rng_zp_low  ; x ^= x >> 9 and the low part of x ^= x << 7 done
        EOR rng_zp_high
        STA rng_zp_high ; x ^= x << 8 done
        RTS


;----------------------------------------------------------------------------
; IRQs page aligned so only low-byte needs to change
;----------------------------------------------------------------------------
            !align 255,0,0
IRQ_PAGE:

; update sprite pointers only
IRQ_Bump_sprites:
            pha
            tya
            pha
            ldy ZP_IRQNUMBER
            lda Raster_Data1,y
Bump_Y0:    sta $D000           ; SELF-MODIFIED Y0
            sta $D000           ; SELF-MODIFIED Y1
            sta $D000           ; SELF-MODIFIED Y2
            sta $D000           ; SELF-MODIFIED Y3
            sta $D000           ; SELF-MODIFIED Y4
            sta $D000           ; SELF-MODIFIED Y5
-           cmp $D012 ; wait till after sprite fetch
            bne -
INC_SPRITE_PTRS_END_IRQ:
Bump_P0:    inc SPRITE_PTR      ; SELF-MODIFIED P0
            inc SPRITE_PTR      ; SELF-MODIFIED P1
            inc SPRITE_PTR      ; SELF-MODIFIED P2
            inc SPRITE_PTR      ; SELF-MODIFIED P3
            inc SPRITE_PTR      ; SELF-MODIFIED P4
            inc SPRITE_PTR      ; SELF-MODIFIED P5
            jmp END_IRQ


; update sprite pointers, un-X-expand sprites and fix corresponding X-offsets
IRQ_Start_trees:
            pha
            tya
            pha
            ldy ZP_IRQNUMBER
            lda Raster_Data1,y
Tree_Y0:    sta $D000           ; SELF-MODIFIED Y0
            sta $D000           ; SELF-MODIFIED Y1
            sta $D000           ; SELF-MODIFIED Y2
            sta $D000           ; SELF-MODIFIED Y3
            sta $D000           ; SELF-MODIFIED Y4
            sta $D000           ; SELF-MODIFIED Y5

            lda #%00000000
            sta VIC_SPR_DWIDTH

Tree_X0:    lda #0          ; SELF-MODIFIED
Tree_XI0:   sta $D000       ; SELF-MODIFIED X0
            lda #0          ; SELF-MODIFIED
            sta $D000       ; SELF-MODIFIED X1
            lda #0          ; SELF-MODIFIED
            sta $D000       ; SELF-MODIFIED X2
            lda #0          ; SELF-MODIFIED
            sta $D000       ; SELF-MODIFIED X3
            lda #0          ; SELF-MODIFIED
            sta $D000       ; SELF-MODIFIED X4
            lda #0          ; SELF-MODIFIED
            sta $D000       ; SELF-MODIFIED X5
Tree_X_MSB: lda #0          ; SELF-MODIFIED
            sta VIC_SPR_X_MSB
            jmp INC_SPRITE_PTRS_END_IRQ


; set a single VIC register
IRQ_Set_reg:
            ; outside of IRQ page since we have to wait anyway
            jmp Remaining_IRQ_Set_reg


; set tree colors
IRQ_Set_tree_colors:
            pha
            tya
            pha
            lda #COLOR_TREES_LIGHT
Tree_CI0:   sta $D000       ; SELF-MODIFIED C0
            sta $D000       ; SELF-MODIFIED C1
            sta $D000       ; SELF-MODIFIED C2
            sta $D000       ; SELF-MODIFIED C3
            sta $D000       ; SELF-MODIFIED C4
            sta $D000       ; SELF-MODIFIED C5
Tree_DH:    lda #0          ; SELF-MODIFIED
            sta VIC_SPR_DHEIGHT
            jmp END_IRQ


; set X-scroll
IRQ_Set_x_scroll:
            pha
            tya
            pha
            ldy ZP_IRQNUMBER
            lda Raster_Data1,y  ; data
            sta $D016
            jmp END_IRQ


; set X-scroll and sprite priority
IRQ_Set_x_scroll_2:
            pha
            tya
            pha
            ldy ZP_IRQNUMBER
            lda Raster_Data1,y  ; data
            sta $D016
            lda #$FF            ; all sprites behind characters
            sta VIC_SPR_BEHIND
            jmp END_IRQ


; setup all sprites
IRQ_Top:
            pha
            tya
            pha

            lda #COLOR_SKY
            sta $D021
            lda #COLOR_CROWN_HIGHLIGHT
            sta VIC_SPR_MC1
            lda #COLOR_CROWN_BRANCHES
            sta VIC_SPR_MC2
            lda #%00011000 ; Text MC + 40 columns + X-scroll
            sta $D016
            lda #%11111111
            sta VIC_SPR_DWIDTH  ; just set all sprites double width even though only 6 are shown
            sta VIC_SPR_DHEIGHT ; just set all sprites double width even though only 6 are shown
            sta VIC_SPR_BEHIND  ; all sprites behind characters

            txa
            pha
            jsr update_sprite_X
            jsr update_sprite_data ; TODO this is only required when Y positions/prio changes
            pla
            tax

            ; colors
Crown_C0:   lda #0          ; SELF-MODIFIED
Crown_CI0:  sta $D000       ; SELF-MODIFIED C0
            lda #0          ; SELF-MODIFIED
            sta $D000       ; SELF-MODIFIED C1
            lda #0          ; SELF-MODIFIED
            sta $D000       ; SELF-MODIFIED C2
            lda #0          ; SELF-MODIFIED
            sta $D000       ; SELF-MODIFIED C3
            lda #0          ; SELF-MODIFIED
            sta $D000       ; SELF-MODIFIED C4
            lda #0          ; SELF-MODIFIED
            sta $D000       ; SELF-MODIFIED C5
            lda #0          ; SELF-MODIFIED
            sta $D000       ; SELF-MODIFIED C6
            lda #0          ; SELF-MODIFIED
            sta $D000       ; SELF-MODIFIED C7

            ; x-positions
Crown_X0:   lda #0          ; SELF-MODIFIED
Crown_XI0:  sta $D000       ; SELF-MODIFIED X0
            lda #0          ; SELF-MODIFIED
            sta $D000       ; SELF-MODIFIED X1
            lda #0          ; SELF-MODIFIED
            sta $D000       ; SELF-MODIFIED X2
            lda #0          ; SELF-MODIFIED
            sta $D000       ; SELF-MODIFIED X3
            lda #0          ; SELF-MODIFIED
            sta $D000       ; SELF-MODIFIED X4
            lda #0          ; SELF-MODIFIED
            sta $D000       ; SELF-MODIFIED X5
            lda #0          ; SELF-MODIFIED
            sta $D000       ; SELF-MODIFIED X6
            lda #0          ; SELF-MODIFIED
            sta $D000       ; SELF-MODIFIED X7
Crown_X_MSB:lda #0
            sta VIC_SPR_X_MSB

            ; y-positions
Enemy_Y:    lda #210
Crown_Y6:   sta $D000       ; SELF-MODIFIED Y6
Spunk_Y:    lda #150
Crown_Y7:   sta $D000       ; SELF-MODIFIED Y7

            lda #TREETOPY
Crown_Y0:   sta $D000       ; SELF-MODIFIED Y0
            sta $D000       ; SELF-MODIFIED Y1
            sta $D000       ; SELF-MODIFIED Y2
            sta $D000       ; SELF-MODIFIED Y3
            sta $D000       ; SELF-MODIFIED Y4
            sta $D000       ; SELF-MODIFIED Y5

            ; pointers
            lda #SPRITE_OFFSET
Crown_P0:   sta SPRITE_PTR  ; SELF-MODIFIED P0
            lda #SPRITE_OFFSET+5
            sta SPRITE_PTR  ; SELF-MODIFIED P1
            lda #SPRITE_OFFSET+10
            sta SPRITE_PTR  ; SELF-MODIFIED P2
            lda #SPRITE_OFFSET+15
            sta SPRITE_PTR  ; SELF-MODIFIED P3
            lda #SPRITE_OFFSET+20
            sta SPRITE_PTR  ; SELF-MODIFIED P4
            lda #SPRITE_OFFSET
            sta SPRITE_PTR  ; SELF-MODIFIED P5
Enemy_Ptr:  lda #SPRITE_OFFSET+25
            sta SPRITE_PTR  ; SELF-MODIFIED P6
Spunk_Ptr:  lda #SPRITE_OFFSET+25
            sta SPRITE_PTR  ; SELF-MODIFIED P7

            lda #$FF
            sta ZP_IRQNUMBER
            sta ZP_SYNC
END_IRQ:
            inc ZP_IRQNUMBER
            ldy ZP_IRQNUMBER
            lda Raster_Line,y
            sta $D012
            lda Raster_IRQ,y
            sta $FFFE
            asl $D019       ; ACK IRQ

            pla
            tay
            pla
NMI:        rti             ; NMI ignored


; set a single VIC register
Remaining_IRQ_Set_reg:
            pha
            tya
            pha
            txa
            pha
            ldx ZP_IRQNUMBER
            ldy Raster_Data2,x  ; offset
            lda Raster_Data1,x  ; data
            ; move outside view
            nop
            nop
            nop
            nop
            nop
            nop
            sta $D000,y
            pla
            tax
            jmp END_IRQ


!if >IRQ_PAGE != >IRQ_Top {
    !error "IRQ page too long"
}


;----------------------------------------------------------------------------
; DATA raster splits
;----------------------------------------------------------------------------

; crowns are:        MC1=white(highlight), MC2=branches(orange), COL=leaves: only MC2 and COL visible
; trees/players are: MC1=black, MC2=white, COL=trees/players; MC2 not used in trees

; Raster IRQs (starts at InitRaster/InitIRQ and is the last in this list)
Raster_Line:
            !byte TREETOPY + 42*0 + 40 ; 90
            !byte TREETOPY + 42*1 + 35 ; 127 ; fix MC1
            !byte TREETOPY + 42*1 + 39 ; 131
            !byte 50 + 8*11 - 1 ; 137 (-1 to avoid flickering)
            !byte 50 + 8*11 + 2 ; 139 fix COL
            !byte 50 + 8*11 + 5 ; 143 fix MC2
            !byte 50 + 8*12 ; 146 set sprites before/behind
            !byte 50 + 8*15 ; 170 ; scroll top level
            !byte TREETOPY + 42*2 + 40 ; 174
            !byte 50 + 8*17 ; 186 ; scroll mid level
            !byte 50 + 8*19 ; 202 ; scroll bottom level
            !byte TREETOPY + 42*3 + 40 ; 216
            !byte 50 + 8*22 ; 226 ; scroll plants + all sprites behind
InitRaster: !byte RASTERTOP ; 0

Raster_IRQ:
            !byte <IRQ_Bump_sprites
            !byte <IRQ_Set_reg
            !byte <IRQ_Start_trees
            !byte <IRQ_Set_reg
            !byte <IRQ_Set_tree_colors
            !byte <IRQ_Set_reg
            !byte <IRQ_Set_reg
            !byte <IRQ_Set_x_scroll
            !byte <IRQ_Bump_sprites
            !byte <IRQ_Set_x_scroll
            !byte <IRQ_Set_x_scroll
            !byte <IRQ_Bump_sprites
            !byte <IRQ_Set_x_scroll_2
InitIRQ:    !byte <IRQ_Top

Raster_Data1:
            !byte TREETOPY + 42*1
            !byte COLOR_TREES_DARK
            !byte TREETOPY + 42*2
            !byte COLOR_BACKGROUND
            !byte 0
            !byte WHITE
Spr_Behind: !byte %00000000 ; sprites before/behind chars in playfield
Scroll1:    !byte %00010000
            !byte TREETOPY + 42*3
Scroll2:    !byte %00010000
Scroll3:    !byte %00010000
            !byte TREETOPY + 42*4
Scroll4:    !byte %00010000
            !byte 0

Raster_Data2:
            !byte 0
            !byte <VIC_SPR_MC1
            !byte 0
            !byte <$D021
            !byte 0
            !byte <VIC_SPR_MC2
            !byte <VIC_SPR_BEHIND
            !byte 0
            !byte 0
            !byte 0
            !byte 0
            !byte 0
            !byte 0
            !byte 0


;----------------------------------------------------------------------------
; DATA
;----------------------------------------------------------------------------

; Sprite X-positions 9.7 (87654321 0ddddddd) range: (left outside view) 0 .. 368 ($B8) (right outside view)
Sprites_X_posH:
            !byte $B8-50,$B8-100,30,60,90,120 ; trees
            !byte (30+24)/2     ; enemy
Spunk_X:    !byte (80+24)/2     ; Spunk
Sprites_X_posL:
            !byte 0,0,0,0,0,0 ; trees
            !byte 0     ; enemy
            !byte 0     ; Spunk

; acceptable colors are 3,5,7,10,13 CYAN GREEN YELLOW LIGHT_RED LIGHT_GREEN
Sprites_colors:
            !byte 3,5,7,10,13,5,14,PURPLE

; with Spunk placed at Y=150 his last screen line is 121 (sprite line 171)
; there are 5 trees ending at screen lines: 132, 143, 161, 172, 186

; Sprite prios (0 means $D000 sprite, 1 means $D002 sprite etc.)
Sprites_prio:
            !byte 4,3,0,6,5,2,1,7

MSB:
            !byte 1,2,4,8,16,32,64,128
TIMES_3:
            !byte 0,3,6,9,12,15 ; note only 6 entries
TIMES_5:
            !byte 0,5,10,15,20,25,30,35


; X-offset of plants 8.8 fixed point .8 is sub-pixels speed, lowest 3 bits is X-scroll, highest 5 is char scroll (mod 11)
; increase to scroll left
plants:
            !byte 0,0


;----------------------------------------------------------------------------
; DATA distant background
;----------------------------------------------------------------------------

distant:
; Sprite2asm trees-distant-ch40-mc0f.png 25 mei 2020 20:52:36
; charmap 560 bytes (40 x 14) hacked 40x13 520
!byte $42,$41,$42,$41,$42,$41,$42,$41,$40,$42,$41,$42,$41,$43,$40,$42,$41,$42,$41,$42,$41,$42,$41,$44,$40,$42,$41,$42,$41,$40,$40,$42,$41,$42,$41,$42,$41,$42,$41,$40
!byte $45,$41,$46,$41,$45,$41,$46,$41,$45,$45,$41,$46,$41,$47,$48,$45,$41,$46,$41,$45,$41,$46,$41,$43,$48,$45,$41,$46,$41,$45,$41,$45,$41,$46,$41,$45,$41,$46,$41,$43
!byte $46,$49,$45,$45,$46,$49,$45,$45,$41,$46,$49,$45,$45,$4a,$4b,$46,$49,$45,$45,$46,$49,$45,$45,$4a,$4b,$46,$49,$45,$45,$41,$41,$46,$49,$45,$45,$46,$49,$45,$45,$47
;!byte $45,$45,$45,$45,$45,$45,$45,$45,$49,$45,$45,$45,$45,$4a,$4b,$45,$45,$45,$45,$45,$45,$45,$45,$4a,$4b,$45,$45,$45,$45,$49,$46,$45,$45,$45,$45,$45,$45,$45,$45,$4a
!byte $45,$46,$49,$46,$45,$46,$49,$46,$4c,$45,$46,$49,$46,$4a,$4b,$45,$46,$49,$46,$45,$46,$49,$46,$4a,$4b,$45,$46,$49,$46,$4c,$45,$45,$46,$49,$46,$45,$46,$49,$46,$4a
!byte $46,$45,$46,$42,$46,$45,$46,$42,$45,$46,$45,$46,$42,$43,$48,$46,$45,$46,$42,$46,$45,$46,$42,$4d,$48,$46,$45,$46,$42,$45,$45,$46,$45,$46,$42,$46,$45,$46,$42,$43
!byte $4e,$45,$45,$4f,$4e,$45,$45,$4f,$46,$4e,$45,$45,$4f,$40,$40,$4e,$45,$45,$4f,$4e,$45,$45,$4f,$50,$40,$4e,$45,$45,$4f,$46,$49,$4e,$45,$45,$4f,$4e,$45,$45,$4f,$40
!byte $51,$45,$52,$40,$51,$45,$52,$40,$45,$52,$45,$52,$40,$40,$40,$51,$45,$52,$40,$51,$45,$52,$40,$40,$40,$51,$45,$52,$51,$45,$52,$51,$45,$52,$40,$51,$45,$52,$40,$40
!byte $40,$53,$54,$40,$40,$53,$54,$40,$53,$54,$55,$54,$40,$40,$40,$40,$53,$54,$40,$40,$53,$54,$40,$40,$40,$40,$53,$54,$40,$53,$54,$40,$53,$54,$40,$40,$53,$54,$40,$40
!byte $40,$53,$56,$40,$40,$55,$54,$40,$55,$54,$53,$54,$40,$40,$40,$40,$55,$54,$40,$40,$53,$56,$40,$40,$40,$40,$55,$54,$40,$53,$56,$40,$55,$54,$40,$40,$55,$54,$40,$40
!byte $40,$55,$54,$40,$40,$53,$54,$40,$53,$56,$53,$54,$40,$40,$57,$58,$53,$54,$40,$40,$53,$54,$40,$40,$57,$58,$53,$56,$40,$55,$54,$40,$53,$54,$40,$40,$55,$56,$40,$40
!byte $40,$53,$54,$57,$58,$59,$56,$57,$59,$54,$59,$56,$57,$58,$5a,$5b,$53,$54,$57,$58,$59,$54,$57,$58,$5a,$5b,$59,$54,$40,$53,$54,$40,$59,$56,$57,$58,$53,$54,$40,$40
!byte $58,$5c,$5d,$5a,$5b,$5c,$5d,$5a,$5c,$5d,$5c,$5d,$5a,$5b,$57,$58,$5c,$5d,$5a,$5b,$5c,$5d,$5a,$5b,$57,$58,$5c,$5d,$40,$5c,$5d,$40,$5c,$5d,$5a,$5b,$5c,$5d,$57,$58
!byte $5b,$5e,$5f,$40,$40,$5e,$5f,$40,$5a,$5b,$5e,$5f,$40,$40,$5a,$5b,$5e,$5f,$40,$40,$5e,$5f,$40,$40,$5a,$5b,$5e,$5f,$40,$5e,$5f,$40,$5e,$5f,$40,$40,$5e,$5f,$5a,$5b


;----------------------------------------------------------------------------
; DATA logo
;----------------------------------------------------------------------------

logo:
; Sprite2asm trees-logo-ch00-mcff.png 25 mei 2020 21:14:57 EDITED
; charmap 360 bytes (40 x 9)
!byte $80,$80,$80,$80,$00,$00,$00,$00,$00,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
!byte $80,$80,$80,$00,$00,$80,$80,$00,$00,$80,$00,$00,$00,$00,$00,$80,$80,$00,$00,$80,$80,$00,$00,$80,$00,$00,$80,$80,$00,$00,$80,$00,$00,$80,$80,$00,$00,$80,$80,$80
!byte $80,$80,$80,$00,$00,$00,$80,$80,$80,$80,$00,$00,$80,$80,$00,$00,$80,$00,$00,$80,$80,$00,$00,$80,$00,$00,$00,$80,$00,$00,$80,$00,$00,$80,$00,$00,$80,$80,$80,$80
!byte $80,$80,$80,$80,$00,$00,$00,$00,$80,$80,$00,$00,$80,$80,$00,$00,$80,$00,$00,$80,$80,$00,$00,$80,$00,$00,$00,$00,$00,$00,$80,$00,$00,$00,$00,$80,$80,$80,$80,$80
!byte $80,$80,$80,$80,$80,$80,$00,$00,$00,$80,$00,$00,$00,$00,$00,$80,$80,$00,$00,$80,$80,$00,$00,$80,$00,$00,$80,$00,$00,$00,$80,$00,$00,$80,$00,$00,$80,$80,$80,$80
!byte $80,$80,$80,$00,$00,$80,$80,$00,$00,$80,$00,$00,$80,$80,$80,$80,$80,$00,$00,$80,$80,$00,$00,$80,$00,$00,$80,$80,$00,$00,$80,$00,$00,$80,$80,$00,$00,$80,$80,$80
!byte $80,$80,$80,$00,$00,$00,$00,$00,$80,$80,$00,$00,$80,$80,$80,$80,$80,$80,$00,$00,$00,$00,$00,$80,$00,$00,$80,$80,$00,$00,$80,$00,$00,$80,$80,$00,$00,$80,$80,$80
!byte $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
;the purple skunk v.s. the rest
!byte $80,$80,$80,$80,$80, 20,  8,  5,$80, 16, 21, 18, 16, 12,  5,$80, 19, 11, 21, 14, 11,$80, 22, 46, 19, 46,$80, 20,  8,  5,$80, 18,  5, 19, 20,$80,$80,$80,$80,$80

introtext:
;      12345678901234567890123456789012345678
!scr "          press fire to start!          "
!scr "                                        "
!scr "                                        "
!scr "    written by alex ",34,"paaco",34," paalvast    "
!scr "                                        "
!scr "      (c) 2020 twa",129,"n pa",129,"n software     "
introtext_end:

getreadytext:
;      12345678901234567890123456789012345678
!scr " will spunk beat his friends again      "
!scr " and grab the most apples?              "
!scr "                                        "
!scr "                                        "
!scr "         get ready for spunk!           "
!scr "                                        "
getreadytext_end:


;----------------------------------------------------------------------------
; CHARSET
;----------------------------------------------------------------------------
            * = $2000

            !src "chars.inc"


;----------------------------------------------------------------------------
; SPRITES
;----------------------------------------------------------------------------
            !align 63,0

SPRITE_OFFSET = (sprites and $3FFF) >> 6
sprites:
            !src "sprites.inc"
