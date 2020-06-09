;
; trees
;
; 1804 bytes exomized

; variables
!addr {
ZP_IRQNUMBER = $10      ; current raster IRQ

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
RASTERTOP=40            ; top raster irq
TREETOPY=50             ; first y-position of trees

BLACK=0
WHITE=1
CYAN=3
PURPLE=4
GREEN=5
BLUE=6
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

            ; cls
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

            ; draw plants row
            clc
            ldy #0
            ldx #0 ; TODO initial offset (0..10)
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

            ; enable sprites
            lda #%11111111
            sta VIC_SPR_ENA
            lda #%00111111
            sta VIC_SPR_DHEIGHT
            sta VIC_SPR_DWIDTH
            sta VIC_SPR_BEHIND ; TODO this is a HACK for now do properly
            lda #%11111111
            sta VIC_SPR_MC

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

            ; run everything from IRQ
loop:       inc $0404
            jmp loop


; place trees
update_trees:
            lda trees
            sec
            sbc #12     ; -24 shifted once
            ldx trees+1
            jsr calc_X
            ; A contains X-position, $FF contains MSB as 0 or 1
            sta Crown_X0+1
            lda $FF ; MSB
            sta Crown_X_MSB+1
            lda trees
            sec
            sbc #6      ; -12 shifted once to center stem below crown
            ldx trees+1
            jsr calc_X
            sta Tree_X0+1
            lda $FF ; MSB
            sta Tree_X_MSB+1
            rts

; Convert sprite X-offsets from 9.7 value (range 0 .. 368) to MSB + X-pos in A
; calculate X position A=bits 8..1, X=00/80
;
; high     low
; 87654321 0ddddddd
; ^ends up in MSB
; 1) subtract 24 (range -24 .. 344)
; 2) for PAL subtract 8 for values < 0: so the range is 1E0 .. 1F7 000 .. 16F
;    for NTSC don't subtract 8, so the NTSC range is    1E8 .. 1FF 000 .. 16F
; see https://bumbershootsoft.wordpress.com/2019/07/01/c64-fat-sprite-workarounds/
calc_X:
            cpx #$80    ; copy bit 7 from X into carry so bit 0 from X-coordinate ends in carry
            rol         ; 7..0, carry contains MSB bit 8
            ; C=0: pos <= 255; C=1: pos >=256 or negative if >=$E0
            bcs .check
            ; C=MSB=0 here
            rol $FF     ; store MSB somewhere
            rts
.check:     ; C=MSB=1 here
            rol $FF     ; store MSB somewhere
            ; subtract 8 if A < 0 (only for PAL, don't do this on NTSC, i.e. subtract 0)
            cmp #$E0
            bcc +
            ;sec carry already set
NTSC_fix1:  sbc #8
+           rts         ; A = sprite X-position


;----------------------------------------------------------------------------
; IRQs page aligned so only low-byte needs to change
;----------------------------------------------------------------------------
            !align 255,0,0
IRQ_PAGE:

; TODO: update sprites before/behind characters
; TODO: make sure tree-leaves lowest row is same as top row from tree-stem

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

Tree_X0:    lda #<(50+12)
Tree_P0:    sta $D000       ; X0
            lda #<(100+12)
            sta $D002       ; X1
            lda #<(150+12)
            sta $D004       ; X2
            lda #<(200+12)
            sta $D006       ; X3
            lda #<(250+12)
            sta $D008       ; X4
            lda #<(300+12)
            sta $D00A       ; X5
Tree_X_MSB: lda #$30
            sta VIC_SPR_X_MSB
            jmp INC_SPRITE_PTRS_END_IRQ


; set a single VIC register
IRQ_Set_reg:
            ; outside of IRQ page since we have to wait anyway
            jmp Remaining_IRQ_Set_reg


; set colors MC2 and trees
IRQ_Set_colors:
            pha
            tya
            pha
            lda #WHITE
            sta VIC_SPR_MC2
            lda #COLOR_TREES_LIGHT
            sta VIC_SPR_COL+0
            sta VIC_SPR_COL+1
            sta VIC_SPR_COL+2
            sta VIC_SPR_COL+3
            sta VIC_SPR_COL+4
            sta VIC_SPR_COL+5
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


; setup all sprites
IRQ_Top:
            pha
            tya
            pha

            lda #COLOR_CROWN_HIGHLIGHT
            sta VIC_SPR_MC1
            lda #COLOR_CROWN_BRANCHES
            sta VIC_SPR_MC2
            lda #%00011000 ; Text MC + 40 columns + X-scroll
            sta $D016
            lda #%00111111
            sta VIC_SPR_DWIDTH

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

            jsr update_trees
            lda trees+1 ; low
            sec
            sbc #$40    ; half pixel
            sta trees+1 ; update low
            bcs +       ; no borrow? then ok
            dec trees
+
Crown_X0:   lda #50
Crown_P0:   sta $D000       ; X0
            lda #100
            sta $D002       ; X1
            lda #150
            sta $D004       ; X2
            lda #200
            sta $D006       ; X3
            lda #250
            sta $D008       ; X4
            lda #<300
            sta $D00A       ; X5
            ; enemy
            lda #30
            sta $D00C       ; X6
            lda #210
            sta $D00D       ; Y6
            ; Purple Spunk
            lda #30
            sta $D00E       ; X7
            lda #150
            sta $D00F       ; Y7
Crown_X_MSB:lda #$30
            sta VIC_SPR_X_MSB

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
            lda #SPRITE_OFFSET
            sta SPRITE_PTR+3
            lda #SPRITE_OFFSET+5
            sta SPRITE_PTR+4
            lda #SPRITE_OFFSET+10
            sta SPRITE_PTR+5
            lda #SPRITE_OFFSET+15
            sta SPRITE_PTR+6
            lda #SPRITE_OFFSET+15
            sta SPRITE_PTR+7

            lda #COLOR_SKY
            sta $D021

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
            !byte 50 + 8*11 + 2 ; 139
            !byte 50 + 8*15 ; 170
            !byte TREETOPY + 42*2 + 40 ; 174
            !byte 50 + 8*17 ; 186
            !byte 50 + 8*19 ; 202
            !byte TREETOPY + 42*3 + 40 ; 216
            !byte 50 + 8*22 ; 226
InitRaster: !byte RASTERTOP ; 40

Raster_IRQ:
            !byte <IRQ_Bump_sprites
            !byte <IRQ_Set_reg
            !byte <IRQ_Start_trees
            !byte <IRQ_Set_reg
            !byte <IRQ_Set_colors
            !byte <IRQ_Set_x_scroll
            !byte <IRQ_Bump_sprites
            !byte <IRQ_Set_x_scroll
            !byte <IRQ_Set_x_scroll
            !byte <IRQ_Bump_sprites
            !byte <IRQ_Set_x_scroll
InitIRQ:    !byte <IRQ_Top

Raster_Data1:
            !byte TREETOPY + 42*1
            !byte COLOR_TREES_DARK
            !byte TREETOPY + 42*2
            !byte COLOR_BACKGROUND
            !byte 0
            !byte %00010000+7
            !byte TREETOPY + 42*3
            !byte %00010000+6
            !byte %00010000+5
            !byte TREETOPY + 42*4
            !byte %00010000+4
            !byte 0

Raster_Data2:
            !byte 0
            !byte <VIC_SPR_MC1
            !byte 0
            !byte <$D021
            !byte 0
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

; X-positions of trees 9.7 (87654321 0ddddddd) range: (left outside view) 0 .. 368 ($B8) (right outside view)
trees:
            !byte $B8,0
            !byte 0,0
            !byte 0,0
            !byte 0,0
            !byte 0,0
            !byte 0,0

; MSBs
msb:
            !byte 1,2,4,8,16,32,64,128


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
