;
; trees
;
; 2365 bytes exomized

; variables
!addr {
ZP_IRQNUMBER = $10      ; current raster IRQ
ZP_MSB = $11            ; temp location for MSB calculation

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
RASTERTOP=30            ; top raster irq TODO why not just 0
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
            lda #COLOR_PLANTS
            sta VIC_COL_TXT_MC2
            lda #%00011000 ; charset bits 3-1: $2000=$800 * %100; screen bits 7-4: $0400=$0400 * %0001
            sta $D018
            lda #%11111111
            sta VIC_SPR_ENA
            sta VIC_SPR_MC
            sta $DC00       ; disconnect keyboard

            ; cls TODO can we shorten this? most is overwritten below anyway
            ldx #0
-           lda #$20
            sta $0400,x
            sta $0500,x
            sta $0600,x
            sta $0700,x
            lda #8+COLOR_PLANTS_OUTLINE
            sta $D800,x
            sta $D900,x
            sta $DA00,x
            sta $DB00,x
            inx
            bne -

            ; distant background
            ldx #0
-           lda distant,x
            sta $0400+40*2,x
            lda distant+$100,x
            sta $0500+40*2,x
            lda distant+520-256,x
            sta $0400+520-256+40*2,x
            inx
            bne -

            ; logo (360 bytes)
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

            ; introtext
            INTROTEXTY=16
            ldx #introtext_end-introtext-1
-           lda introtext,x
            sta $0400+INTROTEXTY*40,x
            lda #CYAN
            sta $D800+INTROTEXTY*40,x
            dex
            bne -

            jsr draw_plants

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

            ; TODO sync the main loop to the raster IRQ
loop:       inc $0404
            jsr draw_plants
            jmp loop


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

; TODO update $D000 register indices based on prio
update_sprite_data:
; update all 8 sprites in IRQ_Top
            ldy #0
            sty ZP_MSB
-           lda Sprites_X_posH,y     ; X-pos bits 8..1
            sec
            sbc #12                 ; -24 shifted once
            ldx Sprites_X_posL,y   ; X-pos bit 0 (in bit 7)
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
            sta Crown_X0+1,x        ; SELF MODIFY corresponding lda# statement
            iny
            cpy #8
            bne -
            lda ZP_MSB
            sta Crown_X_MSB+1       ; SELF MODIFY corresponding lda# statement
            ; fall-through

; update the 6 trees in IRQ_Start_trees
            ldy #0
            sty ZP_MSB
-           lda Sprites_X_posH,y     ; X-pos bits 8..1
            sec
            sbc #6                     ; -24 shifted once
            ldx Sprites_X_posL,y     ; X-pos bit 0 (in bit 7)
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
            sta Tree_X0+1,x        ; SELF MODIFY corresponding lda# statement
            iny
            cpy #6
            bne -
            lda ZP_MSB
            sta Tree_X_MSB+1       ; SELF MODIFY corresponding lda# statement
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
            ; TODO DEBUG move plant movement elsewhere
            lda plants+1
            clc
            adc #$16
            sta plants+1
            bcc +           ; no borrow? then ok
            inc plants
+
            lda plants
            and #$07        ; X-scroll
            eor #$07        ; reverse
            ora #%00010000  ; Text MC + 38 columns
            sta Plants_X

            lda plants
            cmp #8*11
            bcc +           ; borrow? then smaller thus ok
            sbc #8*11       ; handle modulo 11 overflow
            sta plants
+           lsr
            lsr
            lsr
            tax             ; initial offset (0..10)

            ; X=initial offset (0..10)
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
            sta $D001
            sta $D003
            sta $D005
            sta $D007
            sta $D009
            sta $D00B
-           cmp $D012 ; wait till after sprite fetch
            bne -
INC_SPRITE_PTRS_END_IRQ:
            inc SPRITE_PTR+0
            inc SPRITE_PTR+1
            inc SPRITE_PTR+2
            inc SPRITE_PTR+3
            inc SPRITE_PTR+4
            inc SPRITE_PTR+5
            jmp END_IRQ


; update sprite pointers, un-X-expand sprites and fix corresponding X-offsets
IRQ_Start_trees:
            pha
            tya
            pha
            ldy ZP_IRQNUMBER
            lda Raster_Data1,y
            sta $D001
            sta $D003
            sta $D005
            sta $D007
            sta $D009
            sta $D00B

            lda #%00000000
            sta VIC_SPR_DWIDTH

Tree_X0:    lda #0          ; SELF-MODIFIED
Tree_P0:    sta $D000       ; SELF-MODIFIED X0
            lda #0          ; SELF-MODIFIED
            sta $D002       ; SELF-MODIFIED X1
            lda #0          ; SELF-MODIFIED
            sta $D004       ; SELF-MODIFIED X2
            lda #0          ; SELF-MODIFIED
            sta $D006       ; SELF-MODIFIED X3
            lda #0          ; SELF-MODIFIED
            sta $D008       ; SELF-MODIFIED X4
            lda #0          ; SELF-MODIFIED
            sta $D00A       ; SELF-MODIFIED X5
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
            sta VIC_SPR_COL+0
            sta VIC_SPR_COL+1
            sta VIC_SPR_COL+2
            sta VIC_SPR_COL+3
            sta VIC_SPR_COL+4
            sta VIC_SPR_COL+5
            lda #%00111111
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
            lda #$FF
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

            ; colors
            ldx #5
            stx VIC_SPR_COL+0
            ;dex
            stx VIC_SPR_COL+1
            ldx #2
            stx VIC_SPR_COL+2
            ldx #7
            stx VIC_SPR_COL+3
            ldx #13
            stx VIC_SPR_COL+4
            ;dex
            stx VIC_SPR_COL+5
            ldx #PURPLE
            stx VIC_SPR_COL+7 ; SPUNK

            jsr update_sprite_data ; TODO is this a good location?

Crown_X0:   lda #0          ; SELF-MODIFIED
Crown_P0:   sta $D000       ; SELF-MODIFIED X0
            lda #0          ; SELF-MODIFIED
            sta $D002       ; SELF-MODIFIED X1
            lda #0          ; SELF-MODIFIED
            sta $D004       ; SELF-MODIFIED X2
            lda #0          ; SELF-MODIFIED
            sta $D006       ; SELF-MODIFIED X3
            lda #0          ; SELF-MODIFIED
            sta $D008       ; SELF-MODIFIED X4
            lda #0          ; SELF-MODIFIED
            sta $D00A       ; SELF-MODIFIED X5
            lda #0          ; SELF-MODIFIED
            sta $D00C       ; SELF-MODIFIED X6 enemy
            lda #0          ; SELF-MODIFIED
            sta $D00E       ; SELF-MODIFIED X7 Spunk
Crown_X_MSB:lda #0
            sta VIC_SPR_X_MSB

            lda #210
            sta $D00D       ; Y6
Spunk_Y:    lda #150
            sta $D00F       ; Y7

            lda #TREETOPY
            sta $D001
            sta $D003
            sta $D005
            sta $D007
            sta $D009
            sta $D00B
            lda #SPRITE_OFFSET
            sta SPRITE_PTR+0
            lda #SPRITE_OFFSET+5
            sta SPRITE_PTR+1
            lda #SPRITE_OFFSET+10
            sta SPRITE_PTR+2
            lda #SPRITE_OFFSET+15
            sta SPRITE_PTR+3
            lda #SPRITE_OFFSET+20
            sta SPRITE_PTR+4
            lda #SPRITE_OFFSET
            sta SPRITE_PTR+5
            lda #SPRITE_OFFSET+25
            sta SPRITE_PTR+6
Spunk_Ptr:  lda #SPRITE_OFFSET+25
            sta SPRITE_PTR+7

            jsr move_Spunk

            lda #$FF
            sta ZP_IRQNUMBER
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
            !byte 50 + 8*11 + 7 ; 146 set sprites background/front
            !byte 50 + 8*15 ; 170 ; scroll top level
            !byte TREETOPY + 42*2 + 40 ; 174
            !byte 50 + 8*17 ; 186 ; scroll mid level
            !byte 50 + 8*19 ; 202 ; scroll bottom level
            !byte TREETOPY + 42*3 + 40 ; 216
            !byte 50 + 8*22 ; 226 ; scroll plants + all sprites background
InitRaster: !byte RASTERTOP ; 30

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
Spr_Behind: !byte $FF;%00000000 ; no sprites behind characters
            !byte %00010000+7
            !byte TREETOPY + 42*3
            !byte %00010000+6
            !byte %00010000+5
            !byte TREETOPY + 42*4
Plants_X:   !byte %00010000+4
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
            !byte $B8-50,$B8-100,0,0,0,0 ; trees
            !byte (30+24)/2     ; enemy
            !byte (80+24)/2     ; Spunk
Sprites_X_posL:
            !byte 0,0,0,0,0,0 ; trees
            !byte 0     ; enemy
            !byte 0     ; Spunk

; Sprite prio (index into MSB table) NOTE spaced 2 bytes apart
Sprites_prio:
            !byte 0,1,2,3,4,5,6,7

; MSBs
MSB:
            !byte 1,2,4,8,16,32,64,128

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
;     12345678901234567890123456789012345678
!scr "     will spunk outrun his forest       "
!scr "  friends and grab the most apples?     "
!scr "                                        "
!scr "          press fire to start           "
!scr "                                        "
;!scr"      (c)2020 by paaco/twa",129,"n pa",129,"n"
!scr "  (c)2020 alexander ",34,"paaco",34," paalvast"
introtext_end:

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
