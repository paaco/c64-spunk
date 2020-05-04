;
; trees
;

*=$0801
!byte $0c,$08,$b5,$07,$9e,$20,$32,$30,$36,$32,$00,$00,$00   ; sys 2062

;2062
        jsr $E544

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
        sta $D01C       ; MC

        lda #255
        sta $FC

        sei

loop:
        lda #240
-       cmp $D012
        bne -

        TOPY=100

        lda #TOPY
-       cmp $D012
        bne -

        lda $FC
        sta $D000

        lda #TOPY
        sta $D001       ; Y0
        lda #SPRITE_OFFSET+0
        sta $07F8       ; PTR0

        lda #TOPY+42
-       cmp $D012
        bne -

        lda #TOPY+42
        sta $D001       ; Y0
        lda #SPRITE_OFFSET+0
        sta $07F8       ; PTR0

        lda #TOPY+42+42
-       cmp $D012
        bne -

        lda #TOPY+42+42
        sta $D001       ; Y0
        lda #SPRITE_OFFSET+1
        sta $07F8       ; PTR0

        lda #TOPY+42+42+42
-       cmp $D012
        bne -

        lda #TOPY+42+42+42
        sta $D001       ; Y0
        lda #SPRITE_OFFSET+2
        sta $07F8       ; PTR0

        dec $FC
        jmp loop

    *= $0A00

SPRITE_OFFSET = (sprites and $3FFF) >> 6
sprites:
    !src "sprites.asm"