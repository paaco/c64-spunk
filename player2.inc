; Cybernoid II drum kit based on siddump

;--------
; player
;--------

!addr SID = $D400
FL = 0
FH = 1
PL = 2
PH = 3
WF = 4
AD = 5
SR = 6

    *= $1800

music_init:
            jmp .music_init

music_play:
            lda row_i
            bne .wave_table
.initial_note:
            ldx #0
-           lda instr,x
            sta SID,x
            inx
            cpx #7
            bne -
            beq .end ; always
.wave_table:
            asl
            tax
            lda instr+8,x
            cmp #$FF
            beq .done
            sta SID+WF
            lda instr+7 ; no index
            sta SID+FL
            lda instr+9,x
            sta SID+FH
.end:       inc row_i
.done:      rts


.music_init:
            ldx #$18
-           lda music_SID_init,x
            sta SID,x
            dex
            bpl -
            ; reset song pointers
            lda #0
            sta row_i
            rts


; $18 bytes SID init
music_SID_init:
        !byte 0,0,0,0,0,0,0
        !byte $9C,$1A,0,0,0,0,0 ; set ring-mod freq for instr3 (actually modulated by a chord arp)
        !byte 0,0,0,0,0,0,0
        FILTER=0
        !byte (FILTER & $F)     ; filter cutoff bits 3-0
        !byte (FILTER >> 4)     ; filter cutoff bits 11-4
        !byte $00               ;        reso | ext v3 v2 v1
        !byte $0F               ; V3 HP BP LP | VOL


; timer
row_i:  !byte 0


; initial row: 8 bytes FL FH PL PH | WF AD SR FL2
; next rows:   2 bytes WF FH
instr:
instr1: ; drum1
        !word $070C,$800 ; G#2
        !byte $11,$00,$A8,$00
        !byte $81,$34
        !byte $41,$0A
        !byte $40,$08
        !byte $40,$06
        !byte $40,$04
        !byte $40,$05
        !byte $40,$06
        !byte $40,$05
        !byte $40,$04
        !byte $40,$05
        !byte $FF ; stop

instr2: ; drum2
        !word $0E18,$800 ; G#3
        !byte $11,$00,$A8,$00
        !byte $81,$34
        !byte $41,$0A
        !byte $40,$08
        !byte $40,$06
        !byte $40,$04
        !byte $40,$05
        !byte $40,$06
        !byte $40,$05
        !byte $40,$04
        !byte $40,$05
        !byte $FF ; stop

instr3: ; ring-mod-sync blip
        !word $3863,$000
        !byte $17,$00,$88,$63
        !byte $81,$FA
        !byte $17,$38
        !byte $16,$38
        !byte $FF

instr4: ; snare
        !word $0E18,$800
        !byte $11,$00,$A9,$00
        !byte $81,$3C
        !byte $41,$0E
        !byte $80,$00
        !byte $40,$0C
        !byte $80,$38
        !byte $80,$3C
        !byte $80,$38
        !byte $80,$3C
        !byte $10,$0D
        !byte $FF
