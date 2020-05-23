;
; trees
;
; 1530 bytes exomized

; variables
!addr {
ZP_TREEX = $10          ; 6 tree x-offsets in ZP for speed
ZP_IRQNUMBER = $16

SPRITE_PTR = $07F8

VIC_SPR_ENA = $D015
VIC_SPR_DHEIGHT = $D017
VIC_SPR_BEHIND = $D01B
VIC_SPR_MC = $D01C
VIC_SPR_DWIDTH = $D01D
VIC_SPR_MC1 = $D025
VIC_SPR_MC2 = $D026
VIC_SPR_COL = $D027
}

; constants
RASTERTOP=40            ; top raster irq
TREETOPY=50             ; first y-position of trees

BLACK=0
GREEN=5
BLUE=6
BROWN=8
ORANGE=9
LIGHT_BLUE=14

COLOR_SKY = LIGHT_BLUE
COLOR_DISTANT = BLACK
COLOR_BACKGROUND = ORANGE
COLOR_PLANTS = GREEN
COLOR_PLANTS_OUTLINE = BLACK

*=$0801
!byte $0c,$08,$b5,$07,$9e,$20,$32,$30,$36,$32,$00,$00,$00   ; sys 2062

;2062
;            $02A6/678:   Flag: TV Standard: $00 = NTSC, $01 = PAL
;            lda $02A6 ; needed for X-offset correction

            lda #0    ; NOTE D020 could already be set by decruncher
            sta $D020
            lda #COLOR_DISTANT
            sta $D022 ; Text MC1
            lda #COLOR_PLANTS
            sta $D023 ; Text MC2

            ldx #0
-           lda #0
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

            ; test
            lda #16
            sta $0400+15*40+2
            sta $D800+15*40+2
            sta $0400+16*40+2
            sta $D800+16*40+2
            sta $0400+17*40+2
            sta $D800+17*40+2
            sta $0400+18*40+2
            sta $D800+18*40+2
            sta $0400+19*40+2
            sta $D800+19*40+2
            sta $0400+20*40+2
            sta $D800+20*40+2
            sta $0400+21*40+2
            sta $D800+21*40+2
            sta $0400+15*40+34
            sta $D800+15*40+34
            sta $0400+16*40+34
            sta $D800+16*40+34
            sta $0400+17*40+34
            sta $D800+17*40+34
            sta $0400+18*40+34
            sta $D800+18*40+34
            sta $0400+19*40+34
            sta $D800+19*40+34
            sta $0400+20*40+34
            sta $D800+20*40+34
            sta $0400+21*40+34
            sta $D800+21*40+34

            lda #%00011000 ; charset bits 3-1: $2000=$800 * %100; screen bits 7-4: $0400=$0400 * %0001
            sta $D018
            lda #%00010000 ; Text MC + 38 columns + X-scroll
            sta $D016

            ; fill plants row
            clc
            ldx #0
-           txa
            adc #64+32
            sta $0400+22*40,x
            sta $0400+22*40+11,x
            sta $0400+22*40+22,x
            adc #11
            sta $0428+22*40,X
            sta $0428+22*40+11,X
            sta $0428+22*40+22,X
            adc #11
            sta $0450+22*40,X
            sta $0450+22*40+11,X
            sta $0450+22*40+22,X
            inx
            cpx #11
            bne -

            ; distant background
            ldx #0
-           lda distant,x
            sta $0400+40,x
            lda distant+$100,x
            sta $0500+40,x
            lda distant+560-256,x
            sta $0400+560-256+40,x
            inx
            bne -

            lda #0
            sta VIC_SPR_MC1
            lda #8
            sta VIC_SPR_MC2
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
            ldx #4
            stx VIC_SPR_COL+7 ; SPUNK

            ; enable sprites
            lda #%11111111
            sta VIC_SPR_ENA
            lda #%00111111
            sta VIC_SPR_DHEIGHT
            sta VIC_SPR_DWIDTH
            sta VIC_SPR_BEHIND ; TODO this is a HACK for now do properly
            lda #%11111111
            sta VIC_SPR_MC

            ; init trees
            ldx #0
-           lda TreeXL,x
            sta ZP_TREEX,x
            inx
            cpx #6
            bne -

            sei

            lda #$35        ; Bank out KERNAL and BASIC
            sta $01

            ;lda #%00010100 + MEM_VIC_DD00 ; select VIC bank
            ;sta $DD00

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


;----------------------------------------------------------------------------
; DATA
;----------------------------------------------------------------------------

; TODO: x-offsets should be 2 bytes: highest 8 bits in one (looks like x-value/2)
; TODO: and lowest bit + 7 bits behind comma in another
TreeXL:     !byte <50,<100,<150,<200,<250,<300       ; 8-bit fixed point
TreeXH:     !byte >50,>100,>150,>200,>250,>300       ; only 1-bit used (bit-9)
SprXMSB:    !byte $20                                ; precalculated from TreeXH
; TODO: you need two versions of SprXMSB, one for the double width treetops and one for the trees

; Raster IRQs (starts at InitRaster/InitIRQ and is the last in this list)
Raster_Line:
            !byte TREETOPY + 42*0 + 40
            !byte 50 + 8*6
            !byte TREETOPY + 42*1 + 39
            !byte 50 + 8*15
            !byte TREETOPY + 42*2 + 40 ; 174
            !byte 50 + 8*17
            !byte 50 + 8*19
            !byte TREETOPY + 42*3 + 40 ; 216
            !byte 50 + 8*22
InitRaster: !byte RASTERTOP

Raster_IRQ:
            !byte <IRQ_Bump_sprites
            !byte <IRQ_Set_reg
            !byte <IRQ_Start_trees
            !byte <IRQ_Set_x_scroll
            !byte <IRQ_Bump_sprites
            !byte <IRQ_Set_x_scroll
            !byte <IRQ_Set_x_scroll
            !byte <IRQ_Bump_sprites
            !byte <IRQ_Set_x_scroll
InitIRQ:    !byte <IRQ_Top

Raster_Data1:
            !byte TREETOPY + 42*1
            !byte COLOR_BACKGROUND
            !byte TREETOPY + 42*2
            !byte %00010000+1
            !byte TREETOPY + 42*3
            !byte %00010000+2
            !byte %00010000+3
            !byte TREETOPY + 42*4
            !byte %00010000+7
            !byte 0

Raster_Data2:
            !byte 0
            !byte $21
            !byte 0
            !byte $21
            !byte 0
            !byte $21
            !byte $21
            !byte 0
            !byte $21
            !byte 0


;----------------------------------------------------------------------------
; IRQs page aligned so only low-byte needs to change
;----------------------------------------------------------------------------
            !align 255,0,0
IRQ_PAGE:

; TODO: update SPR_MC1 to black
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
            ;inc $D020 ; DEBUG
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

            nop ; make sure changing width happens outside the screen TODO this is in the middle of the screen
            nop

            lda #%00000000
            sta VIC_SPR_DWIDTH

            lda #<(50+12)
            sta $D000       ; X0
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
            lda #$30
            sta $D010       ; X-MSB

            lda #0
            sta VIC_SPR_MC1

            jmp INC_SPRITE_PTRS_END_IRQ


; set a single VIC register
IRQ_Set_reg:
            ; outside of IRQ page since we have to wait anyway
            jmp Remaining_IRQ_Set_reg


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

            ; lda #$D
            ; sta VIC_SPR_MC1
            lda #%00111111
            sta VIC_SPR_DWIDTH

            lda ZP_TREEX+0
            sta $D000       ; X0
            lda ZP_TREEX+1
            sta $D002       ; X1
            lda ZP_TREEX+2
            sta $D004       ; X2
            lda ZP_TREEX+3
            sta $D006       ; X3
            lda ZP_TREEX+4
            sta $D008       ; X4
            lda ZP_TREEX+5
            sta $D00A       ; X5
            ; enemy
            lda #60
            sta $D00C       ; X6
            lda #210
            sta $D00D       ; Y6
            ; Purple Spunk
            lda #60
            sta $D00E       ; X7
            lda #170
            sta $D00F       ; Y7
            lda SprXMSB
            sta $D010       ; X-MSB

            lda #TREETOPY
            sta $D001
            sta $D003
            sta $D005
            sta $D007
            sta $D009
            sta $D00B
            lda #SPRITE_OFFSET
            sta SPRITE_PTR+0
            lda #SPRITE_OFFSET
            sta SPRITE_PTR+1
            lda #SPRITE_OFFSET
            sta SPRITE_PTR+2
            lda #SPRITE_OFFSET
            sta SPRITE_PTR+3
            lda #SPRITE_OFFSET
            sta SPRITE_PTR+4
            lda #SPRITE_OFFSET
            sta SPRITE_PTR+5
            lda #SPRITE_OFFSET+5
            sta SPRITE_PTR+6
            lda #SPRITE_OFFSET+5
            sta SPRITE_PTR+7

            lda #COLOR_SKY
            ;sta $D020 ; DEBUG
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
; DATA distant background
;----------------------------------------------------------------------------

distant:
; Sprite2asm trees-distant-ch40-mc0f.png 15 mei 2020 16:07:40
; charmap 560 bytes (40 x 14)
!byte $42,$41,$42,$41,$42,$41,$42,$41,$40,$42,$41,$42,$41,$43,$40,$42,$41,$42,$41,$42,$41,$42,$41,$44,$40,$42,$41,$42,$41,$40,$40,$42,$41,$42,$41,$42,$41,$42,$41,$40
!byte $45,$41,$46,$41,$45,$41,$46,$41,$45,$45,$41,$46,$41,$47,$48,$45,$41,$46,$41,$45,$41,$46,$41,$43,$48,$45,$41,$46,$41,$45,$41,$45,$41,$46,$41,$45,$41,$46,$41,$43
!byte $46,$49,$45,$45,$46,$49,$45,$45,$41,$46,$49,$45,$45,$4a,$4b,$46,$49,$45,$45,$46,$49,$45,$45,$4a,$4b,$46,$49,$45,$45,$41,$41,$46,$49,$45,$45,$46,$49,$45,$45,$47
!byte $45,$45,$45,$45,$45,$45,$45,$45,$49,$45,$45,$45,$45,$4a,$4b,$45,$45,$45,$45,$45,$45,$45,$45,$4a,$4b,$45,$45,$45,$45,$49,$46,$45,$45,$45,$45,$45,$45,$45,$45,$4a
!byte $45,$46,$49,$46,$45,$46,$49,$46,$4c,$45,$46,$49,$46,$4a,$4b,$45,$46,$49,$46,$45,$46,$49,$46,$4a,$4b,$45,$46,$49,$46,$4c,$45,$45,$46,$49,$46,$45,$46,$49,$46,$4a
!byte $46,$45,$46,$42,$46,$45,$46,$42,$45,$46,$45,$46,$42,$43,$48,$46,$45,$46,$42,$46,$45,$46,$42,$4d,$48,$46,$45,$46,$42,$45,$45,$46,$45,$46,$42,$46,$45,$46,$42,$43
!byte $4e,$45,$45,$4f,$4e,$45,$45,$4f,$46,$4e,$45,$45,$4f,$40,$40,$4e,$45,$45,$4f,$4e,$45,$45,$4f,$50,$40,$4e,$45,$45,$4f,$46,$49,$4e,$45,$45,$4f,$4e,$45,$45,$4f,$40
!byte $51,$45,$52,$40,$51,$45,$52,$40,$45,$52,$45,$52,$40,$40,$40,$51,$45,$52,$40,$51,$45,$52,$40,$40,$40,$51,$45,$52,$51,$45,$52,$51,$45,$52,$40,$51,$45,$52,$40,$40
!byte $40,$53,$54,$40,$40,$53,$54,$40,$53,$54,$53,$54,$40,$40,$40,$40,$53,$54,$40,$40,$53,$54,$40,$40,$40,$40,$53,$54,$40,$53,$54,$40,$53,$54,$40,$40,$53,$54,$40,$40
!byte $40,$53,$55,$40,$40,$56,$54,$40,$56,$54,$56,$54,$40,$40,$40,$40,$56,$54,$40,$40,$53,$55,$40,$40,$40,$40,$56,$54,$40,$56,$54,$40,$56,$54,$40,$40,$56,$54,$40,$40
!byte $40,$56,$54,$40,$40,$53,$54,$40,$53,$54,$53,$54,$40,$40,$57,$58,$53,$54,$40,$40,$53,$54,$40,$40,$57,$58,$53,$55,$40,$53,$54,$40,$53,$54,$40,$40,$53,$54,$40,$40
!byte $40,$59,$54,$57,$58,$59,$55,$57,$59,$55,$59,$55,$57,$58,$5a,$5b,$59,$54,$57,$58,$59,$54,$57,$58,$5a,$5b,$59,$54,$40,$59,$55,$40,$59,$55,$57,$58,$59,$55,$40,$40
!byte $58,$5c,$5d,$5a,$5b,$5c,$5d,$5a,$5c,$5d,$5c,$5d,$5a,$5b,$57,$58,$5c,$5d,$5a,$5b,$5c,$5d,$5a,$5b,$57,$58,$5c,$5d,$40,$5c,$5d,$40,$5c,$5d,$5a,$5b,$5c,$5d,$57,$58
!byte $5b,$5e,$5f,$40,$40,$5e,$5f,$40,$5a,$5b,$5e,$5f,$40,$40,$5a,$5b,$5e,$5f,$40,$40,$5e,$5f,$40,$40,$5a,$5b,$5e,$5f,$40,$5e,$5f,$40,$5e,$5f,$40,$40,$5e,$5f,$5a,$5b


;----------------------------------------------------------------------------
; SPRITES
;----------------------------------------------------------------------------
            !align 63,0

SPRITE_OFFSET = (sprites and $3FFF) >> 6
sprites:
            !src "sprites.inc"

; https://bumbershootsoft.wordpress.com/2019/07/01/c64-fat-sprite-workarounds/
; If you want to scroll a “fat” (X- or horizontally-expanded) sprite off the left edge of the screen on the C64,
; the first 24 columns are covered with X locations 0-23, but for the last 24 you have to know if you’re an NTSC or PAL system.
; If you are on an NTSC system, old or new, the values from 489 to 511 (high bit set, low byte 233-255) cover columns 25-48,
; and on PAL you subtract eight, for 481-503 (low byte 225-247).

; SO, for PAL, use values: (-24) 1E1-1F7 (-1) followed by 0-319
; and for NTSC, use:       (-24) 1E9-1FF (-1) followed by 0-319


;----------------------------------------------------------------------------
; CHARSET
;----------------------------------------------------------------------------
            * = $2000

            !src "chars.inc"
            !byte 0,0,0,0,0,0,0,0
