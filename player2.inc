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

; ZP
row_i = $40             ; wave table row
instr_off = $41         ; instrument offset
note_length = $42       ; note length remaining
pat_offset = $43        ; offset in pattern
song_offset = $44       ; offset in song


    *= $1c00

; music_init:
;             jmp .music_init

music_play:
            dec note_length
            bpl .just_play
            ; next note
            ldx pat_offset
.fetch_note:
.play_p1:   lda pattern0,x
            bne .note_ok
            ; 0=next pattern
.next_pat:  ldx song_offset
            lda song,x
            bne +
            ; loop song
            lda #song_loop-song
            sta song_offset
            bne .next_pat ; jmp always
+           sta .play_p1+1
            sta .play_p2+1
            inc song_offset
            ldx #0
            beq .fetch_note ; jmp always
.note_ok:   sta note_length
            inx
.play_p2:   lda pattern0,x
            sta instr_off
            inx
            stx pat_offset
            lda #0
            sta row_i
            ; fall-through
.just_play:
            lda row_i
            bne .wave_table
.initial_note:
            ldy instr_off
            ldx #0
-           lda instruments,y
            sta SID,x
            iny
            inx
            cpx #7
            bne -
            beq .end ; always
.wave_table:
            asl
            clc
            adc instr_off
            tax
            lda instruments+8,x
            beq .done
            sta SID+WF
            lda instruments+7 ; FL from instrument, not wave table
            sta SID+FL
            lda instruments+9,x
            sta SID+FH
.end:       inc row_i
.done:      rts


music_init:
            ldx #$18
-           lda music_SID_init,x
            sta SID,x
            dex
            bpl -
            ; reset song pointers
            lda #0
            sta pat_offset
            sta note_length
            sta song_offset
            lda #<pattern_e
            sta .play_p1+1
            sta .play_p2+1
            rts


; $18 bytes SID init
music_SID_init:
        !byte 0,0,0,0,0,0,0
        !byte 0,0,0,0,0,0,0
        !byte 0,0,0,0,0,0,0
        FILTER=100
        !byte (FILTER & $F)     ; filter cutoff bits 3-0
        !byte (FILTER >> 4)     ; filter cutoff bits 11-4
        !byte $F1               ;        reso | ext v3 v2 v1
        !byte $5F               ; V3 HP BP LP | VOL


song:
        !byte <pattern0
        !byte <pattern0
song_loop:
        !byte <pattern1
        !byte <pattern1
        !byte <pattern1
        !byte <pattern1_brk
        !byte <pattern2
        !byte <pattern2
        !byte <pattern2
        !byte <pattern2_brk
        !byte <pattern2
        !byte <pattern2
        !byte <pattern2
        !byte <pattern2_brk
        !byte <pattern2
        !byte <pattern2
        !byte <pattern2_brk
        !byte <pattern1_brk
        !byte 0 ; end


;page align so everything is addressable by single byte pointers
        !align 255,0,0


; initial row: 8 bytes FL FH PL PH | WF AD SR FL2
; next rows:   2 bytes WF FH
instruments:
instr1: ; drum1
DRUM1 = instr1-instruments
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
        !byte 0 ; stop

instr2: ; drum2
DRUM2 = instr2-instruments
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
        !byte 0 ; stop

; instr3: ; ring-mod-sync blip
; BLIP = instr3-instruments
;         !word $3863,$000
;         !byte $17,$00,$88,$63
;         !byte $81,$FA
;         !byte $17,$38
;         !byte $16,$38
;         !byte $FF

instr4: ; snare
SNARE = instr4-instruments
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
        !byte 0


;page align so everything is addressable by single byte pointers
        !align 255,0,0

L8=8*6
L6=6*6
L4=4*6
L3=3*6
L2=2*6
L1=1*6
        ; length (raster lines),instr, pattern ends with just 0 length
; empty pattern first!
pattern_e:
        !byte 0

pattern0: ; 32
        !byte L4,<DRUM1,L4,<DRUM1,L8,<DRUM1
        !byte L4,<DRUM1,L4,<DRUM1,L8,<DRUM1
        !byte 0
pattern1: ; 32
        !byte L3,<DRUM1,L1,<DRUM2,L2,<DRUM2,L2,<DRUM2,L8,<DRUM1
        !byte L3,<DRUM1,L1,<DRUM2,L2,<DRUM2,L2,<DRUM2,L4,<DRUM1,L4,<DRUM2
        !byte 0
pattern1_brk: ; 16
        !byte L3,<DRUM1,L1,<DRUM2,L2,<DRUM2,L2,<DRUM2,L2,<DRUM1,L2,<SNARE,L2,<SNARE,L2,<SNARE
        !byte 0
pattern2: ; 32
        !byte L3,<DRUM1,L1,<DRUM2,L2,<SNARE,L2,<DRUM2,L6,<DRUM1,L2,<DRUM1
        !byte L3,<DRUM1,L1,<DRUM2,L2,<SNARE,L2,<DRUM2,L4,<DRUM1,L4,<DRUM2
        !byte 0
pattern2_brk: ; 16
        !byte L3,<DRUM1,L1,<DRUM2,L2,<SNARE,L2,<DRUM2,L3,<DRUM1,L1,<SNARE,L2,<DRUM2,L2,<SNARE
        !byte 0
