;
; trees
;

; variables
ZP_TREEX = $10          ; 6 tree x-offsets in ZP for speed
SPRITE_PTR = $07F8

RASTERTOP=40            ; top rasterirq
TREETOPY=50             ; first y-position of trees

*=$0801
!byte $0c,$08,$b5,$07,$9e,$20,$32,$30,$36,$32,$00,$00,$00   ; sys 2062

;2062
            jsr $E544       ; cls

;            $02A6/678:   Flag: TV Standard: $00 = NTSC, $01 = PAL
;            lda $02A6 ; needed for X-offset correction
;            sta $0428

            lda #0
            sta $D020
            lda #8
            sta $D021

            lda #0
            sta $D025       ; MC1
            lda #9
            sta $D026       ; MC2
            ldx #5
            stx $D027       ; C0
            dex
            stx $D028       ; C1
            dex
            stx $D029       ; C2
            dex
            stx $D02A       ; C3
            dex
            stx $D02B       ; C4
            dex
            stx $D02C       ; C5

            ; enable sprites
            lda #%00111111
            sta $D015       ; enable
            sta $D017       ; Y-expand
            sta $D01D       ; X-expand
            sta $D01C       ; MC

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

;             lda #TOPY+42+42
; -           cmp $D012
;             bne -

;             lda #TOPY+42+42
;             sta $D001       ; Y0
;             lda #SPRITE_OFFSET+0
;             sta $07F8       ; PTR0

;             lda #$0
;             sta $D025       ; MC1

; ;             lda #TOPY+42+42+1 ; delay to next line before disabling x-expand
; ; -           cmp $D012
; ;             bne -

;             lda #%00000000
;             sta $D01D       ; X-expand
;             dec $d021

;             lda TreeX
;             clc
;             adc #12
;             sta $D000       ; X0

;             lda #TOPY+42+42+42
; -           cmp $D012
;             bne -

;             lda #TOPY+42+42+42
;             sta $D001       ; Y0
;             lda #SPRITE_OFFSET+1
;             sta $07F8       ; PTR0

;             lda #TOPY+42+42+42+42
; -           cmp $D012
;             bne -

;             lda #TOPY+42+42+42+42
;             sta $D001       ; Y0
;             lda #SPRITE_OFFSET+2
;             sta $07F8       ; PTR0

;----------------------------------------------------------------------------
; DATA
;----------------------------------------------------------------------------

TreeXL:     !byte <50,<100,<150,<200,<250,<300       ; 8-bit fixed point
TreeXH:     !byte >50,>100,>150,>200,>250,>300       ; only 1-bit used (bit-9)
SprXMSB:    !byte $20                                ; precalculated from TreeXH

;----------------------------------------------------------------------------
; IRQs page align so only low-byte needs to change
;----------------------------------------------------------------------------
            !align 255,0,0

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
            lda #SPRITE_OFFSET+4
            sta SPRITE_PTR+0
            lda #SPRITE_OFFSET+4
            sta SPRITE_PTR+1
            lda #SPRITE_OFFSET+4
            sta SPRITE_PTR+2
            lda #SPRITE_OFFSET+4
            sta SPRITE_PTR+3
            lda #SPRITE_OFFSET+4
            sta SPRITE_PTR+4
            lda #SPRITE_OFFSET+4
            sta SPRITE_PTR+5

            ldy #RASTERTOP
            lda #<IRQ_Top
            jmp END_IRQ

; setup all sprites
IRQ_Top:
            sta IRQ_A
            sty IRQ_Y

;            inc $D020               ; DEBUG

            lda #$D
            sta $D025       ; MC1
            lda #%00111111
            sta $D01D       ; X-expand

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
            sta $D001       ; Y0
            sta $D003       ; Y1
            sta $D005       ; Y2
            sta $D007       ; Y3
            sta $D009       ; Y4
            sta $D00B       ; Y5
            lda #SPRITE_OFFSET+3
            sta SPRITE_PTR+0
            lda #SPRITE_OFFSET+3
            sta SPRITE_PTR+1
            lda #SPRITE_OFFSET+3
            sta SPRITE_PTR+2
            lda #SPRITE_OFFSET+3
            sta SPRITE_PTR+3
            lda #SPRITE_OFFSET+3
            sta SPRITE_PTR+4
            lda #SPRITE_OFFSET+3
            sta SPRITE_PTR+5

;            dec $D020               ; DEDEBUG

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
;            IRQ_X = *+1
;            ldx #0          ; SELF-MODIFIED
NMI:        rti             ; NMI ignored


;----------------------------------------------------------------------------
; CHARSET
;----------------------------------------------------------------------------

            !src "chars.inc"

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
