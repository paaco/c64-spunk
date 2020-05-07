;
; trees
;

*=$0801
!byte $0c,$08,$b5,$07,$9e,$20,$32,$30,$36,$32,$00,$00,$00   ; sys 2062

;2062
            jsr $E544       ; cls

;            $02A6/678:   Flag: TV Standard: $00 = NTSC, $01 = PAL
            lda $02A6
            sta $0428

            lda #0
            sta $D020
            lda #8
            sta $D021

            lda #0
            sta $D025       ; MC1
            lda #9
            sta $D026       ; MC2

            ; enable sprites
            lda #%00000001
            sta $D015       ; enable
            sta $D017       ; Y-expand
            sta $D01D       ; X-expand
            sta $D01C       ; MC

            lda #255
            sta $FC

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

            lda #100        ; rasterline to trigger
            sta $D012

            lda $d011
            and #$7F        ; set bit 9 of rasterline to 0
            sta $d011

            lda #$01        ; Enable RASTER IRQs only
            sta $D01A

            lda #$FF        ; ACK IRQs
            sta $D019

            lda #<IRQ_Top
            sta $FFFE
            lda #>IRQ_Top
            sta $FFFF

            ;cli

loop:
            lda #240
-           cmp $D012
            bne -

            TOPY=100

            lda #TOPY
-           cmp $D012
            bne -

            lda $FC
            sta $D000       ; X0
            lda #$01
            sta $D010       ; X-MSB

            lda #TOPY
            sta $D001       ; Y0
            lda #SPRITE_OFFSET+0
            sta $07F8       ; PTR0

            lda #TOPY+42
-           cmp $D012
            bne -

            lda #TOPY+42
            sta $D001       ; Y0
            lda #SPRITE_OFFSET+0
            sta $07F8       ; PTR0

            lda #TOPY+42+42
-           cmp $D012
            bne -

            lda #TOPY+42+42
            sta $D001       ; Y0
            lda #SPRITE_OFFSET+1
            sta $07F8       ; PTR0

            lda #TOPY+42+42+42
-           cmp $D012
            bne -

            lda #TOPY+42+42+42
            sta $D001       ; Y0
            lda #SPRITE_OFFSET+2
            sta $07F8       ; PTR0

            dec $FC
            jmp loop

;----------------------------------------------------------------------------

IRQ_Top:
            sta IRQ1_A+1
            stx IRQ1_X+1
            sty IRQ1_Y+1

            ;inc $D021

            asl $D019 ; ACK IRQ

IRQ1_A      lda #0          ; SELF-MODIFIED
IRQ1_X      ldx #0          ; SELF-MODIFIED
IRQ1_Y      ldy #0          ; SELF-MODIFIED
NMI:        rti             ; NMI ignored


;----------------------------------------------------------------------------

        *= $0A00

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
