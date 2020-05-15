;
; trees
;

; variables
ZP_TREEX = $10          ; 6 tree x-offsets in ZP for speed
SPRITE_PTR = $07F8

VIC_SPR_ENA = $D015
VIC_SPR_DHEIGHT = $D017
VIC_SPR_BEHIND = $D01B
VIC_SPR_MC = $D01C
VIC_SPR_DWIDTH = $D01D
VIC_SPR_MC1 = $D025
VIC_SPR_MC2 = $D026
VIC_SPR_COL = $D027

RASTERTOP=40            ; top raster irq
TREETOPY=50             ; first y-position of trees

*=$0801
!byte $0c,$08,$b5,$07,$9e,$20,$32,$30,$36,$32,$00,$00,$00   ; sys 2062

;2062
;            $02A6/678:   Flag: TV Standard: $00 = NTSC, $01 = PAL
;            lda $02A6 ; needed for X-offset correction
;            sta $0428

            lda #0
            sta $D020
            lda #8
            sta $D021
            lda #0    ; distant background color
            sta $D022 ; Text MC1
            lda #5
            sta $D023 ; Text MC2

            ldx #0
-           lda #0
            sta $0400,x
            sta $0500,x
            sta $0600,x
            sta $0700,x
            lda #0+8
            sta $D800,x
            sta $D900,x
            sta $DA00,x
            sta $DB00,x
            inx
            bne -

            lda #%00011000 ; charset bits 3-1: $2000=$800 * %100; screen bits 7-4: $0400=$0400 * %0001
            sta $D018
            lda #%00011000 ; Text MC + 38/40 columns + X-scroll
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
            sta $0400+120,x
            lda distant+$100,x
            sta $0500+120,x
            lda distant+560-256,x
            sta $0400+560-256+120,x
            inx
            bne -

            lda #0
            sta VIC_SPR_MC1
            lda #9
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

            ; enable sprites
            lda #%00111111
            sta VIC_SPR_ENA
            sta VIC_SPR_DHEIGHT
            sta VIC_SPR_DWIDTH
            sta VIC_SPR_MC
            sta VIC_SPR_BEHIND ; TODO this is a HACK for now do properly

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

            lda #$01        ; Enable RASTER IRQs only
            sta $D01A

            lda #$FF        ; ACK IRQs
            sta $D019

            lda #RASTERTOP  ; rasterline to trigger
            sta $D012
            lda $D011
            and #$7F        ; set bit 9 of rasterline to 0
            sta $D011

            lda #<IRQ_Top
            sta $FFFE
            lda #>IRQ_Top
            sta $FFFF

            cli

            ; run everything from IRQ
loop:       inc $0400
            jmp loop


;----------------------------------------------------------------------------
; DATA
;----------------------------------------------------------------------------

; TODO: x-offsets should be 2 bytes: highest 8 bits in one (looks like x-value/2)
; TODO: and lowest bit + 7 bits behind comma in another
TreeXL:     !byte <10,<100,<150,<200,<250,<300       ; 8-bit fixed point
TreeXH:     !byte >10,>100,>150,>200,>250,>300       ; only 1-bit used (bit-9)
SprXMSB:    !byte $20                                ; precalculated from TreeXH
; TODO: you need two versions of SprXMSB, one for the double width treetops and one for the trees


;----------------------------------------------------------------------------
; IRQs page align so only low-byte needs to change
;----------------------------------------------------------------------------
            !align 255,0,0

; TODO: update SPR_MC1 to black
; TODO: update scroll position
; TODO: update sprites behind characters
; TODO: reusable IRQ to update sprite pointers and Y-locations (3x)
; TODO: reusable IRQ to set single VIC register (3x)
; TODO: "copper bar" with: rasterline-to-trigger, IRQ-low-byte, value to set
; TODO: make sure tree-leaves lowest row is same as top row from tree-stem

; update sprite pointers only
IRQ_Two:
            sta IRQ_A
            sty IRQ_Y

            lda #TREETOPY+42
            sta $D001
            sta $D003
            sta $D005
            sta $D007
            sta $D009
            sta $D00B
-           cmp $D012 ; wait till end of raster line
            bne -
            inc SPRITE_PTR+0
            inc SPRITE_PTR+1
            inc SPRITE_PTR+2
            inc SPRITE_PTR+3
            inc SPRITE_PTR+4
            inc SPRITE_PTR+5

            ldy #TREETOPY+39+42
            lda #<IRQ_Two_Copy
            jmp END_IRQ


; update sprite pointers, normal width sprites and fix X-offsets
IRQ_Two_Copy:
            sta IRQ_A
            sty IRQ_Y

            lda #TREETOPY+42+42
            sta $D001
            sta $D003
            sta $D005
            sta $D007
            sta $D009
            sta $D00B

            nop ; make sure changing width happens outside the screen
            nop

            lda #%00000000
            sta VIC_SPR_DWIDTH

            lda #<(10+12)
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

            inc SPRITE_PTR+0
            inc SPRITE_PTR+1
            inc SPRITE_PTR+2
            inc SPRITE_PTR+3
            inc SPRITE_PTR+4
            inc SPRITE_PTR+5

            ldy #TREETOPY+42+42+40
            lda #<IRQ_Three
            jmp END_IRQ

IRQ_Three:
            sta IRQ_A
            sty IRQ_Y

            lda #TREETOPY+42+42+42+20 ; TODO: fix this HACK to go into plants (no 5th sprite yet)
            sta $D001
            sta $D003
            sta $D005
            sta $D007
            sta $D009
            sta $D00B
-           cmp $D012 ; wait till end of raster line
            bne -
            inc SPRITE_PTR+0
            inc SPRITE_PTR+1
            inc SPRITE_PTR+2
            inc SPRITE_PTR+3
            inc SPRITE_PTR+4
            inc SPRITE_PTR+5

            ldy #RASTERTOP
            lda #<IRQ_Top
            jmp END_IRQ

; setup all sprites
IRQ_Top:
            sta IRQ_A
            sty IRQ_Y

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
            lda ZP_TREEX+0
            sta $D00C       ; X6
            lda ZP_TREEX+0
            sta $D00E       ; X7
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

            ldy #TREETOPY+40
            lda #<IRQ_Two
END_IRQ:
            sty $D012
            sta $FFFE
            asl $D019       ; ACK IRQ

            IRQ_A = *+1
            lda #0          ; SELF-MODIFIED
            IRQ_Y = *+1
            ldy #0          ; SELF-MODIFIED
NMI:        rti             ; NMI ignored


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
