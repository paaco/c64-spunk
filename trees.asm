;
; trees
;
; 4089 bytes exomized -M256 -Di_perf=-1

; variables
!addr {
ZP_IRQNUMBER = $10      ; current raster IRQ
ZP_MSB = $11            ; temp location for MSB calculation
ZP_SYNC = $12           ; set <>0 after top raster IRQ for sync
ZP_GAMESTATE = $13      ; current game state (0=title screen, etc. see main loop)
ZP_DIDSWAP = $14        ; indicates swap during bubble sort
ZP_RNG_LOW = $15
ZP_RNG_HIGH = $16
ZP_TEMP = $17           ; temp buffer
ZP_IS_SPUNK = $17       ; also ZP_TEMP just a different name
ZP_SCRPTR = $18         ; screen pointer for character collision
ZP_HIGHSCORE = $1A      ; <>0 in case of new high score
ZP_SPEED = $1B          ; bitmask for calculated X speed: %11=forward, %01=slow, %00=blocked
ZP_SPRITES_IDX = $20    ; 8 sprite indices for sorting Y desc (depth sorting)
Sprites_prio = $28      ; 8 sprite prios (0 means $D000 sprite, 1 means $D002 sprite etc.)
delay = $30
tree_delay = $31
object_delay = $32
getready = $33
enemy_y_delta = $34     ; $01=down $FF=up
enemy_timer = $35       ; random delay to change direction
energy = $36            ; energy burst
sleep_init = $38        ; reinit sleep counter
sleep_timer = $37       ; sleep countdown

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
INTROTEXTY=16           ; y-position in characters for text
GAMEOVERY=9             ; y-position in characters for game over text
MINY=150                ; min Y for actors (incl)
MAXY=220                ; max Y for actors (incl)
SPUNK_PY=9              ; sprite hit box Y offset
SPUNK_PX=16             ; sprite hit box X offset
ENEMY_WANTS_X = 120     ; max x-position/2 enemy AI wants to move to
SPUNK_WANTS_X = 100     ; max x-position/2 Spunk can move to
MAX_ENERGY = 200        ; energy length
MIN_SLEEP = 10          ; minimal sleep (increases per try) it's game over at approx 150
SLEEP_INC = 1           ; sleep increase after burst

; animation frames
FRAME_SPUNK_WALK=SPRITE_OFFSET + (sprites_spunk-sprites)/64
FRAME_SPUNK_JUMP=FRAME_SPUNK_WALK+3
; each enemy has 2 frames
FRAME_ENEMY0_WALK=FRAME_SPUNK_WALK+4

; tree/lane speeds in 8.8 pixels
SPEED1_TOP=$080
SPEED2_MID=$100
SPEED3_BOT=$180
SPEED4_BOT=$1FF
SPEED5_PLA=$200

BLACK=0 : WHITE=1 : RED=2 : CYAN=3
PURPLE=4 : GREEN=5 : BLUE=6 : YELLOW=7
ORANGE=8 : BROWN=9 : LIGHT_RED=10 : DARK_GREY=11
GREY=12 : LIGHT_GREEN=13 : LIGHT_BLUE=14 : LIGHT_GREY=15

COLOR_BORDER = LIGHT_BLUE
COLOR_SKY = BLUE
COLOR_DISTANT = BLACK
COLOR_BACKGROUND = BROWN
COLOR_PLANTS = GREEN
COLOR_PLANTS_OUTLINE = BLACK
COLOR_CROWN_HIGHLIGHT = LIGHT_GREEN
COLOR_CROWN_BRANCHES = ORANGE
COLOR_TREES_LIGHT = ORANGE
COLOR_TREES_DARK = BLACK
COLOR_OBJECTS = DARK_GREY

DEBUG_40COL=0;%1000 ; set to %1000 to show 40 columns

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
            ;lda #COLOR_BORDER   ; NOTE D020 already be set by decruncher
            ;sta $D020
            lda #COLOR_PLANTS_OUTLINE
            sta VIC_COL_TXT_MC2
            lda #%00011000 ; charset bits 3-1: $2000=$800 * %100; screen bits 7-4: $0400=$0400 * %0001
            sta $D018
            lda #%11111111
            sta VIC_SPR_ENA
            sta VIC_SPR_MC
            sta $DC00       ; disconnect keyboard

            ; partial cls and color setup
            ldx #0
            stx ZP_SYNC ; set to 0
            stx ZP_RNG_HIGH ; set to 0
-           lda toptext,x
            sta $0400,x
            lda toptext_color,x
            sta $D800,x
            lda #8+COLOR_PLANTS
            sta $DB00,x
            inx
            bne -

            ; init ZP_SPRITES_IDX with 0 to 7
            ldx #7
-           txa
            sta ZP_SPRITES_IDX,x
            dex
            bpl -

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

            lda $D012
            ora #$01
            sta ZP_RNG_LOW ; seed, can be anything except 0
            jsr random

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

            ; main loop is synced after IRQ_Top and runs in upper portion of the screen
            ; it scrolls stuff and handles joystick and gameplay
loop:       lda ZP_SYNC
            beq loop
            ;sta $D020               ; DEBUG

            lda ZP_GAMESTATE
            bne +

        ; game_state 0 : title screen
            ; flicker PRESS FIRE TO START
            PRESSFIREY=16
            dec delay
            bpl ++
            lda #5
            sta delay
            ldx #0
-           lda $D800+PRESSFIREY*40,x
            eor #$01
            sta $D800+PRESSFIREY*40,x
            inx
            cpx #40
            bne -
++
            ; wait for fire
            lda $DC00           ; Joystick A in control port 2 0=active: 1=up 2=down 4=left 8=right 16=fire
            and $DC01           ; Joystick B in control port 1 0=active: 1=up 2=down 4=left 8=right 16=fire
            and #%00010000      ; ignore other bits not from the joystick
            bne state_handled
            jmp init_state_1_get_ready

+           cmp #1
            bne +

        ; game_state 1 : get ready
            jsr animate_sprites
            ; wait a few seconds
            dec getready
            bne state_handled
            jmp init_state_2_game_play

+           cmp #2
            bne +

        ; game_state 2 : game play
            jsr move_objects
            jsr scroll_sprites_X
            jsr move_actors
            inc plants
            inc plants

            ; if either Spunk or enemy fall off screen, it's game over
            lda Sprites_X_posH+6
            cmp #$B8
            beq .game_over
            lda Sprites_X_posH+7
            cmp #$B8
            bne state_handled
.game_over: jmp init_state_3_game_over

+           cmp #3
            bne state_handled

        ; game_state 3 : game over
            ; debounce fire button (wait until <>0)
            lda $DC00           ; Joystick A in control port 2 0=active: 1=up 2=down 4=left 8=right 16=fire
            and $DC01           ; Joystick B in control port 1 0=active: 1=up 2=down 4=left 8=right 16=fire
            and #%00010000      ; ignore other bits not from the joystick
            beq state_handled ;pressed
            dec getready
            bne state_handled
            jmp init_state_0_title_screen

state_handled:
            jsr draw_plants

            lda #0
            sta ZP_SYNC
            ;sta $D020               ; DEBUG
            jmp loop


init_state_0_title_screen:
            lda #%00010000  ; Text MC + 38 columns + X-scroll 0
            sta Scroll1
            sta Scroll2
            sta Scroll3
            lda #%11111111 ; sprites behind text
            sta ZP_GAMESTATE ; set to $FF
            sta Spr_Behind
            ldx #0
            stx delay
            stx tree_delay
            stx object_delay
-           lda INIT_SPRITES_DATA,x
            sta SPRITES_DATA,x
            inx
            cpx #END_SPRITES_DATA-INIT_SPRITES_DATA
            bne -
            ; pick and place random enemy and place Spunk
            jsr random
            and #7-1 ; select 0,2,4 or 6
            clc
            adc #FRAME_ENEMY0_WALK
            sta Enemy_Ptr
            jsr random
            and #7
            tax
            lda ENEMY_COLORS,x
            sta Enemy_Color
            lda #174
            sta Enemy_Y
            lda #190
            sta Spunk_Y
            jsr music_init
            jsr reset_Spunk
            jsr draw_distant_background
            jsr draw_logo
            jsr draw_notext
            ; draw_introtext
            ldx #introtext_end-introtext-1
-           lda introtext,x
            sta $0400+INTROTEXTY*40,x
            lda #CYAN
            sta $D800+INTROTEXTY*40,x
            dex
            bne -
next_state: inc ZP_GAMESTATE
            jmp state_handled

init_state_1_get_ready:
            jsr draw_distant_background
            jsr draw_getreadytext
            lda #100
            sta getready
            lda #MAX_ENERGY
            sta energy
            lda #MIN_SLEEP
            sta sleep_init
            ; reset score
            ldx #DIGITS-1
            lda #$30
-           sta SCORELOC,x
            dex
            bpl -
            jmp next_state

init_state_2_game_play:
            lda #0 ; sprites before characters (obstacles)
            sta Spr_Behind
            sta ZP_HIGHSCORE ; no new high score
            sta enemy_timer
            ; erase all objects
            ldx #ALL_OBJECTS-1
-           sta Objects_X,x
            dex
            bpl -
            jsr draw_notext
            jmp next_state

init_state_3_game_over:
            jsr draw_gameovertext
            lda #100
            sta getready
            jmp next_state

;getready:   !byte 0 ; moved to ZP


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
            ; keep 2 bits from actor sprites in ZP_MSB calculated before
            ldx Sprites_prio+6
            lda MSB,x
            ldx Sprites_prio+7
            ora MSB,x
            and ZP_MSB
            sta ZP_MSB
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
            lda Sprites_ptrs,y
            sta Crown_F0+1,x
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

; apply speed to sprites
scroll_sprites_X:
            ldx #0
-           lda Sprites_X_posL,x
            sec
            sbc Sprites_speed,x
            sta Sprites_X_posL,x
            bcs +
            dec Sprites_X_posH,x
            bne +
            ; free sprite on underflow
            lda #0
            sta Sprites_speed,x
            sta Sprites_X_posL,x
            lda #$B8
            sta Sprites_X_posH,x
+           inx
            cpx #8 ; also includes actors
            bne -
            ; if the time comes, fetch a new tree
            lda tree_delay
            beq add_tree
            dec tree_delay
            rts
add_tree:
            ; find an empty spot
            ldx #0
-           lda Sprites_speed,x
            beq +
            inx
            cpx #6
            bne -
            rts
            ; found one, fetch new tree in empty
+           jsr random
            and #$07
            tay
            lda TEMPLATES_PTR,y
            sta Sprites_ptrs,x
            lda TEMPLATES_Y,y
            sta Sprites_Y_pos,x
            tay
            lda Y_TO_SPEED,y
            sta Sprites_speed,x
            jsr random
            and #$07
            tay
            lda TREE_COLORS,y
            sta Sprites_colors,x
            lda #50 ; take at least a small break
            sta tree_delay
            rts

;tree_delay: !byte 0 ; moved to ZP


; Bubble sort Y to calculate Sprites_prio from Sprites_Y_pos (highest Y gets lowest prio 0..7)
sort_sprite_Y:
--          ldy #0
            ; sty ZP_DIDSWAP
-           ldx ZP_SPRITES_IDX,y
            lda Sprites_Y_pos,x
            iny
            ldx ZP_SPRITES_IDX,y
            cmp Sprites_Y_pos,x  ; ==> compared number is C=0 larger | C=1 smaller | C=Z=1 equal than A
            beq .no_swap
            bcs .no_swap
            ; swap indices of Y-1 and Y(currently in X)
            lda ZP_SPRITES_IDX-1,y
            pha
            stx ZP_SPRITES_IDX-1,y
            pla
            sta ZP_SPRITES_IDX,y
            ; inc ZP_DIDSWAP
.no_swap:   cpy #8-1
            bne -
            ; lda ZP_DIDSWAP ---- only do single pass otherwise trees will flicker (top IRQ takes too long)
            ; bne --
            ; now fill Sprites_prio based on ZP_SPRITES_IDX
            ldy #0
-           ldx ZP_SPRITES_IDX,y
            tya
            sta Sprites_prio,x
            iny
            cpy #8
            bne -
            rts


; move actors: enemy AI and Spunk based on joystick
move_actors:
.enemy_ai:
            lda #1 ; NOT SPUNK so apples don't score
            sta ZP_IS_SPUNK
            ldx Enemy_X
            lda Enemy_Y
            jsr handle_hitbox ; handles apple and determines speed modifier
            ldy Enemy_Y
            lda Y_TO_SPEED,y
            ; A=full speed
            ldx ZP_SPEED ; >1 OK, 1=SLOW, 0=BLOCKED
            beq .ai_blocked
.ai_move_X: ldy Enemy_X
            cpy #ENEMY_WANTS_X ; C=Z=1 equal, C=0 Y is smaller, C=1 Y is larger
            bcs .ai_blocked ; don't move
            ; enemy advances to position
            cpx #1 ; SLOW
            beq .ai_slow
            ; normal move
            lsr ; half speed; leaves C=0 because Y_TO_SPEED is either 40,80 or C0
            adc Enemy_XL
            sta Enemy_XL
            bcc +
            inc Enemy_X
+           lda #0 ; no scroll speed
.ai_slow:   lsr
.ai_blocked:sta Enemy_speed
.ai_move_Y: ; move up or down
            dec enemy_timer
            bpl .ai_do_Y
            ; new direction
            jsr random
            and #$1f
            sta enemy_timer
            and #$07
            tax
            lda SPEED_AI_Y,x
            sta enemy_y_delta
.ai_do_Y:   lda Enemy_Y
            clc
            adc enemy_y_delta
            cmp #MINY
            bcs +
            lda #MINY
+           cmp #MAXY
            bcc +
            lda #MAXY
+           sta Enemy_Y
            ; fall-through

.move_Spunk:
            dec ZP_IS_SPUNK
            ldx Spunk_X
            lda Spunk_Y
            jsr handle_hitbox ; handles apple and determines speed modifier
            ldy Spunk_Y
            lda Y_TO_SPEED,y
            sta Spunk_speed
            ; read joystick
            lda $DC00           ; Joystick A in control port 2 0=active: 1=up 2=down 4=left 8=right 16=fire
            and $DC01           ; Joystick B in control port 1 0=active: 1=up 2=down 4=left 8=right 16=fire
            and #%00011111      ; ignore other bits not from the joystick
            tay ; backup
            ldx energy
            bne .energy
            and #$10            ; don't touch joystick when out of energy!
            beq .no_energy
            dec sleep_timer
            bne .no_energy
            lda #MAX_ENERGY     ; restore energy
            sta energy
            tya
.energy:    and #$10
            bne .spunk_no_FIRE
            ; only when FIRE is pressed can you move UP or DOWN
            tya
            and #$01
            bne .spunk_no_UP
            ; UP
            lda Spunk_Y
            sec
            sbc #$02
            cmp #MINY
            bcs +
            lda #MINY
+           sta Spunk_Y
.spunk_no_UP:
            tya
            and #$02
            bne .spunk_no_DOWN
            ; DOWN
            lda Spunk_Y
            clc
            adc #$02
            cmp #MAXY
            bcc +
            lda #MAXY
+           sta Spunk_Y
.spunk_no_DOWN:
            ; decrease energy only if used
            dec energy
            bne +
            lda sleep_init
            sta sleep_timer
            clc
            adc #SLEEP_INC
            sta sleep_init
            ; handle forward motion
+           ldy Spunk_Y
            lda Y_TO_SPEED,y
            ; A=full speed
            ldx ZP_SPEED ; >1 OK, 1=SLOW, 0=BLOCKED
            beq .sp_blocked
            ldy Spunk_X
            cpy #SPUNK_WANTS_X ; C=Z=1 equal, C=0 Y is smaller, C=1 Y is larger
            bcs .sp_blocked ; don't move
            ; spunk advances to position
            cpx #1 ; SLOW
            beq .sp_slow
            ; normal move
            lsr ; half speed; leaves C=0 because Y_TO_SPEED is either 40,80 or C0
            adc Spunk_XL
            sta Spunk_XL
            bcc +
            inc Spunk_X
+           lda #0 ; no scroll speed
            beq .sp_blocked ; always
.sp_slow:   lsr
            ldx #$CC
            stx $D401+7
            ldx #$81 ; gate-on
            stx $D404+7
.sp_blocked:sta Spunk_speed
.no_energy:
.spunk_no_FIRE:
            ; fall-through

animate_sprites:
            dec delay
            bpl .no_anim
            ; animate enemy always
            lda Enemy_Ptr
            eor #1
            sta Enemy_Ptr
            lda energy
            beq reset_Spunk
            lda sleep_timer
            bne +
            inc sleep_timer
            ldx #FRAME_SPUNK_JUMP
            bne ++
+           ldx Spunk_Ptr
            inx
            cpx #FRAME_SPUNK_JUMP
            bcc +
reset_Spunk:ldx #FRAME_SPUNK_WALK
++          lda #$10 ; gate-off
            sta $D404+7
            sta $D401+7
+           lda #4  ; delay
            sta delay
            stx Spunk_Ptr
.no_anim:   rts

; X=x location (bits 8..1) A=Y location (150..220)
; returns calculated speed in ZP_SPEED: >1=OK, 1=SLOW, 0=BLOCK
handle_hitbox:
            ldy #%11
            sty ZP_SPEED
            sec
            sbc #TREETOPY + (SCROLL1Y-2)*8 - SPUNK_PY
            lsr
            lsr
            lsr
            tay
            lda SCREENROWL,y
            sta ZP_SCRPTR
            lda SCREENROWH,y
            sta ZP_SCRPTR+1
            txa
            lsr
            lsr
            sec
            sbc #24/4-SPUNK_PX/8
            tay
            jsr handle_collision
            ; check second row
            tya
            clc
            adc #40
            tay
            ; fall-through

handle_collision:
            lda (ZP_SCRPTR),y
            cmp #GROUNDOBJ_MAXPROP ; C=Z=1 equal, C=0 A is smaller, C=1 A is larger
            beq .apple
            bcs .not_interesting
            sbc #GROUNDOBJ_MINPROP-1 ; -1 to account for C=0
            bcc .not_interesting
            tax
            lda ZP_SPEED
            and groundobj_prop,x ; DEBUG comment this line to disable collisions
            sta ZP_SPEED
.not_interesting:
            rts
.apple:     lda ZP_IS_SPUNK ; 0=Spunk 1=enemy
            bne destroy_apple
            jsr score_inc ; destroys A and X
            ldx #$21 ; gate-on
            stx $D401+7
            stx $D404+7
            ldx #$10 ; gate-off
            stx $D404+7
            ; fall-through

; destroy apple on screen and in object array; destroys A and X; returns <>0 in X
destroy_apple:
            lda #$20 ; erase directly
            sta (ZP_SCRPTR),y
            lda ZP_SCRPTR ; low
            ldx #1 ; move off-screen
            cmp #<(MIN_SCREENLOC+40*2)
            beq +
            cmp #<(MIN_SCREENLOC+40*3)
            bne ++
+           stx Objects1_X
            rts
++          cmp #<(MIN_SCREENLOC+40*4)
            beq +
            cmp #<(MIN_SCREENLOC+40*5)
            bne ++
+           stx Objects2_X
            rts
++          stx Objects3_X
            rts

;delay:      !byte 0 ; moved to ZP


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
-           lda distant,x
            sta $0400+40*1,x
            lda distant+$100,x
            sta $0500+40*1,x
            lda distant+560-256,x
            sta $0400+560-256+40*1,x
            lda #8 ; only the MC part matters
            sta $D800+40*2,x
            sta $D900+40*2,x
            sta $D800+560-256+40*1,x
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

draw_getreadytext:
            ldx #getreadytext_end-getreadytext-1
-           lda getreadytext,x
            sta $0400+INTROTEXTY*40,x
            lda #CYAN
            sta $D800+INTROTEXTY*40,x
            dex
            bne -
            rts

draw_notext:
            ldx #0
            lda #$20
-           sta $0400+INTROTEXTY*40-40,x
            sta $0400+INTROTEXTY*40,x
            inx
            bne -
            rts

draw_gameovertext:
            ldx #gameovertext_end-gameovertext-1
-           lda gameovertext,x
            sta $0400+GAMEOVERY*40+15,x
            lda #WHITE
            sta $D800+GAMEOVERY*40+15,x
            dex
            bpl -
            ; also draw new high score
            lda ZP_HIGHSCORE
            beq +
            ldx #6+DIGITS
-           lda HIGHLOC-6,x
            sta $0400+GAMEOVERY*40+80+15,x
            lda #WHITE
            sta $D800+GAMEOVERY*40+80+15,x
            dex
            bpl -
+           rts

; objects come in 3 lanes with 3 different speed synced with the trees
move_objects:
.move_objects1:
            lda objects1+1 ; low
            sec
            sbc #<SPEED1_TOP
            sta objects1+1
            lda objects1 ; hi
            sbc #>SPEED1_TOP
            and #$07        ; X-scroll
            sta objects1
            bcs ++
            ; shift and redraw still visible objects
            ldy #MAX_OBJECTS-1
-           lda Objects1_X,y
            beq + ; empty
            tax
            dex ; shift
            txa
            sta Objects1_X,y
            sty ZP_TEMP ; backup
            lda Objects1_ptrs,y
            tay
            jsr draw_object
            ldy ZP_TEMP ; restore
+           dey
            bpl -
++          lda objects1
            ora #%00010000+DEBUG_40COL ; Text MC + 38 columns
            sta Scroll1
            ; fall-through
.move_objects2:
            lda objects2+1 ; low
            sec
            sbc #<SPEED2_MID
            sta objects2+1
            lda objects2 ; hi
            sbc #>SPEED2_MID
            and #$07        ; X-scroll
            sta objects2
            bcs ++
            ; shift and redraw still visible objects
            ldy #MAX_OBJECTS-1
-           lda Objects2_X,y
            beq + ; empty
            tax
            dex ; shift
            txa
            cmp #80
            bne +++
            lda #0 ; force empty
+++         sta Objects2_X,y
            sty ZP_TEMP ; backup
            lda Objects2_ptrs,y
            tay
            jsr draw_object
            ldy ZP_TEMP ; restore
+           dey
            bpl -
++          lda objects2
            ora #%00010000+DEBUG_40COL  ; Text MC + 38 columns
            sta Scroll2
            ; fall-through
.move_objects3:
            lda objects3+1 ; low
            sec
            sbc #<SPEED3_BOT
            sta objects3+1
            lda objects3 ; hi
            sbc #>SPEED3_BOT
            and #$07        ; X-scroll
            sta objects3
            bcs ++
            ; shift and redraw still visible objects
            ldy #MAX_OBJECTS-1
-           lda Objects3_X,y
            beq + ; empty
            tax
            dex ; shift
            txa
            cmp #160
            bne +++
            lda #0 ; force empty
+++         sta Objects3_X,y
            sty ZP_TEMP ; backup
            lda Objects3_ptrs,y
            tay
            jsr draw_object
            ldy ZP_TEMP ; restore
+           dey
            bpl -
++          lda objects3
            ora #%00010000+DEBUG_40COL  ; Text MC + 38 columns
            sta Scroll3
            ; fall-through

            ; if the time comes, fetch new objects
            lda object_delay
            beq add_object
            dec object_delay
            rts

add_object:
            ; pick random X < MAX_OBJECTS
            jsr random
            and #$07 ; make sure < MAX_OBJECTS
            tax ; x=object
.add_object1:
            lda Objects1_X,x
            bne ++ ; used
            ; check for room (bottom part of screen row)
            ldy #2
-           lda $0400 + SCROLL1Y*40 + 40 + 37,y
            cmp #$20
            bne ++ ; no room
            dey
            bpl -
            lda #GROUNDOBJ_APPLE
            cpx #0
            beq +
            jsr random
            and #$07
            tay
            lda RANDOM_OBJECTS,y
+           sta Objects1_ptrs,x
            lda #MAX_OBJ_WIDTH+40 ; top lane init
            sta Objects1_X,x
++
.add_object2:
            jsr random
            and #$07 ; make sure < MAX_OBJECTS
            tax ; x=object
            lda Objects2_X,x
            bne ++ ; used
            ; check for room (bottom part of screen row)
            ldy #2
-           lda $0400 + SCROLL1Y*40 + 40 + 37 + 80,y
            cmp #$20
            bne ++ ; no room
            dey
            bpl -
            lda #GROUNDOBJ_APPLE
            cpx #0
            beq +
            jsr random
            and #$07
            tay
            lda RANDOM_OBJECTS,y
+           sta Objects2_ptrs,x
            lda #MAX_OBJ_WIDTH+40+80 ; middle lane init
            sta Objects2_X,x
++
.add_object3:
            jsr random
            and #$07 ; make sure < MAX_OBJECTS
            tax ; x=object
            lda Objects3_X,x
            bne ++ ; used
            ; check for room (bottom part of screen row)
            ldy #2
-           lda $0400 + SCROLL1Y*40 + 40 + 37 + 160,y
            cmp #$20
            bne ++ ; no room
            dey
            bpl -
            lda #GROUNDOBJ_APPLE
            cpx #0
            beq +
            jsr random
            and #$07
            tay
            lda RANDOM_OBJECTS,y
+           sta Objects3_ptrs,x
            lda #MAX_OBJ_WIDTH+40+160 ; bottom lane init
            sta Objects3_X,x
++
            lda #40
            sta object_delay
            rts

;object_delay: !byte 0  ; moved to ZP


SCROLL1Y=15
MAX_OBJ_WIDTH=10
; x=screen offset + MAX_OBJ_WIDTH
;   screen offset should be in range [0..39], [80..119] or [160..199] otherwise the object is partially clipped
; y=groundobj ptr 0,4,9, etc.
draw_object:
            lda CLIPPED,x ; 0=clipped,1=draw
            beq .clip
            ; only first char needs to be colored
            lda groundobj,y ; color
            sta $D800 + SCROLL1Y*40-MAX_OBJ_WIDTH,x
            sta $D800 + SCROLL1Y*40-MAX_OBJ_WIDTH + 40,x
            ; draw object
-           lda CLIPPED,x ; 0=clipped,1=draw
            beq .clip
            lda groundobj+1,y ; data upper row
            sta $0400 + SCROLL1Y*40-MAX_OBJ_WIDTH,x
            lda groundobj+1+GROUNDOBJ_STRIDE,y ; data lower row
            sta $0400 + SCROLL1Y*40-MAX_OBJ_WIDTH + 40,x
.clip:      lda groundobj+1+GROUNDOBJ_STRIDE,y ; data lower row
            inx
            iny
            cmp #$20
            bne -
            rts


;------
; prng
;------

; RANDOM routine from https://codebase64.org/doku.php?id=base:16bit_xorshift_random_generator
; the RNG. You can get 8-bit random numbers in A or 16-bit numbers
; from the zero page addresses. Leaves X/Y unchanged.
random:
        LDA ZP_RNG_HIGH
        LSR
        LDA ZP_RNG_LOW
        ROR
        EOR ZP_RNG_HIGH
        STA ZP_RNG_HIGH ; high part of x ^= x << 7 done
        ROR             ; A has now x >> 9 and high bit comes from low byte
        EOR ZP_RNG_LOW
        STA ZP_RNG_LOW  ; x ^= x >> 9 and the low part of x ^= x << 7 done
        EOR ZP_RNG_HIGH
        STA ZP_RNG_HIGH ; x ^= x << 8 done
        RTS


;---------
; scoring
;---------

!addr SCORELOC=$0400+12
!addr HIGHLOC=$0400+31
DIGITS=3
; score is 3 digits located inside "toptext"

; increments score counter by 1 with wrap around
score_inc:
            ldx #DIGITS-1
-           lda SCORELOC,x
            clc
            adc #1 ; extra points
            cmp #$30+10 ; C=Z=1 equal, C=0 A is smaller, C=1 A is larger
            bcc +
            sbc #10
            sta SCORELOC,x
            dex
            bpl -
+           sta SCORELOC,x
            ; fall-through

; update high score if current is higher
score_update_high:
            ldx #0
-           lda SCORELOC,x
            cmp HIGHLOC,x  ; C=Z=1 equal, C=0 A is smaller, C=1 A is larger
            bcc +
            bne .copy_score
            inx
            cpx #DIGITS
            bne -
+           rts
            ; copy score into high
--          lda SCORELOC,x
.copy_score:
            sta HIGHLOC,x
            inx
            cpx #DIGITS
            bne --
            stx ZP_HIGHSCORE
            rts


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


; set X-scroll and fix char MC1 (playfield)
IRQ_Set_x_scroll_1:
            pha
            tya
            pha
            ldy ZP_IRQNUMBER
            lda Raster_Data1,y  ; data
            sta $D016
            lda #COLOR_OBJECTS
            sta VIC_COL_TXT_MC1
            jmp END_IRQ


; set X-scroll and sprite priority (plants)
IRQ_Set_x_scroll_2:
            pha
            tya
            pha
            ldy ZP_IRQNUMBER
            lda Raster_Data1,y  ; data
            sta $D016
            lda #$FF            ; all sprites behind characters
            sta VIC_SPR_BEHIND
            txa
            pha
            jsr music_play
            pla
            tax
            jmp END_IRQ


; setup all sprites
IRQ_Top:
            pha
            tya
            pha

            lda #COLOR_SKY
            sta $D021
            lda #COLOR_DISTANT
            sta VIC_COL_TXT_MC1
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
            jsr sort_sprite_Y
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
            lda Enemy_Y
Crown_Y6:   sta $D000       ; SELF-MODIFIED Y6
            lda Spunk_Y
Crown_Y7:   sta $D000       ; SELF-MODIFIED Y7

            lda #TREETOPY
Crown_Y0:   sta $D000       ; SELF-MODIFIED Y0
            sta $D000       ; SELF-MODIFIED Y1
            sta $D000       ; SELF-MODIFIED Y2
            sta $D000       ; SELF-MODIFIED Y3
            sta $D000       ; SELF-MODIFIED Y4
            sta $D000       ; SELF-MODIFIED Y5

            ; pointers
Crown_F0:   lda #0          ; SELF-MODIFIED
Crown_P0:   sta SPRITE_PTR  ; SELF-MODIFIED P0
            lda #0          ; SELF-MODIFIED
            sta SPRITE_PTR  ; SELF-MODIFIED P1
            lda #0          ; SELF-MODIFIED
            sta SPRITE_PTR  ; SELF-MODIFIED P2
            lda #0          ; SELF-MODIFIED
            sta SPRITE_PTR  ; SELF-MODIFIED P3
            lda #0          ; SELF-MODIFIED
            sta SPRITE_PTR  ; SELF-MODIFIED P4
            lda #0          ; SELF-MODIFIED
            sta SPRITE_PTR  ; SELF-MODIFIED P5
            lda #0          ; SELF-MODIFIED
            sta SPRITE_PTR  ; SELF-MODIFIED P6
            lda #0          ; SELF-MODIFIED
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
            !byte 50 + 8*11 + 2 ; 139 fix tree COL and y-expansion
            !byte 50 + 8*11 + 5 ; 143 fix MC2
            !byte 50 + 8*12 ; 146 set sprites before/behind
            !byte 50 + 8*12 + 6; 152 fix ground color
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
            !byte <IRQ_Set_tree_colors
            !byte <IRQ_Set_reg
            !byte <IRQ_Set_reg
            !byte <IRQ_Set_reg
            !byte <IRQ_Set_x_scroll_1
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
            !byte 0
            !byte WHITE
Spr_Behind: !byte %00000000 ; sprites before/behind chars in playfield
            !byte COLOR_BACKGROUND
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
            !byte 0
            !byte <VIC_SPR_MC2
            !byte <VIC_SPR_BEHIND
            !byte <$D021
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

SPRITES_DATA:
; Sprite X-positions 9.7 (87654321 0ddddddd) range: (left outside view) 0 .. 368 ($B8) (right outside view)
Sprites_X_posH:
            !byte 0,0,0,0,0,0 ; trees
Enemy_X:    !byte 0 ; enemy
Spunk_X:    !byte 0 ; Spunk
Sprites_X_posL:
            !byte 0,0,0,0,0,0 ; trees
Enemy_XL:   !byte 0 ; enemy
Spunk_XL:   !byte 0 ; Spunk

Sprites_Y_pos:
            !byte 0,0,0,0,0,0 ; trees (only used for depth sorting; Y-positions indicate the lowest visible line of a tree)
Enemy_Y:    !byte 0 ; enemy
Spunk_Y:    !byte 0 ; Spunk

Sprites_colors:
            !byte 0,0,0,0,0,0 ; trees
Enemy_Color:!byte 0 ; enemy
            !byte 0 ; Spunk

Sprites_ptrs:
            !byte 0,0,0,0,0,0
Enemy_Ptr:  !byte 0
Spunk_Ptr:  !byte 0

Sprites_speed:
            !byte 0,0,0,0,0,0 ; trees
Enemy_speed:!byte 0 ; enemy
Spunk_speed:!byte 0 ; Spunk


; X-offset of plants 8.8 fixed point .8 is sub-pixels speed, lowest 3 bits is X-scroll, highest 5 is char scroll (mod 11)
; increase to scroll left
plants:
            !byte 0,0

; X-offset of objects 8.8 fixed point .8 is sub-pixels speed, lowest 3 bits is X-scroll, highest 5 is char scroll
; scroll speeds are the same as the trees: $80*2=$100, $A0*2=$140, $C0*2=$180
objects1:   !byte 0,0
objects2:   !byte 0,0
objects3:   !byte 0,0

; screen location of object (0=empty)
MAX_OBJECTS=8 ; max objects per lane
Objects_X:
Objects1_X: ; top lane
            !fill MAX_OBJECTS,0
Objects2_X: ; middle lane
            !fill MAX_OBJECTS,0
Objects3_X: ; bottom lane
            !fill MAX_OBJECTS,0
Objects_end:
ALL_OBJECTS = Objects_end - Objects_X

; object image
Objects_ptrs:
            !fill ALL_OBJECTS,0
Objects1_ptrs=Objects_ptrs
Objects2_ptrs=Objects_ptrs+MAX_OBJECTS
Objects3_ptrs=Objects_ptrs+MAX_OBJECTS*2


;----------------------------------------------------------------------------
; CONSTANTS
;----------------------------------------------------------------------------

MSB:
            !byte 1,2,4,8,16,32,64,128
TIMES_3:
            !byte 0,3,6,9,12,15 ; note only 6 entries
TIMES_5:
            !byte 0,5,10,15,20,25,30,35
TREE_COLORS: ; 8 different colors to choose from
            !byte CYAN,GREEN,YELLOW,LIGHT_GREEN,LIGHT_RED,GREEN,GREEN,GREEN
ENEMY_COLORS: ; 8 different colors to choose from
            !byte CYAN,YELLOW,ORANGE,LIGHT_RED,GREY,ORANGE,LIGHT_GREEN,LIGHT_BLUE

; overwrites Sprites_X_posH, Sprites_X_posL, Sprites_Y_pos, Sprites_colors, Sprites_ptrs
INIT_SPRITES_DATA:
            !byte $9F,$90,0,0,0,0, 35,32 ; 8x X_posH
            !fill 8,0 ; 8x X_posL
            !byte 215,161,0,0,0,0,0,0 ; 8x Y_pos
            !byte GREEN,LIGHT_RED,0,0,0,0, 0,PURPLE ; 8x colors
            !byte SPRITE_OFFSET,SPRITE_OFFSET+20,0,0,0,0, 0,0 ; 8x pointers
            !byte SPEED4_BOT>>1,SPEED1_TOP>>1,0,0,0,0,0,0 ; 8x speeds
END_SPRITES_DATA:

; 8 tree templates consisting of ptr, max-Y and speed (corresponding with levels 1,2,3 or 4)
; To make randomized choosing easier, there are some duplicates
; Tree sprites are stored from longest tree to smallest
TEMPLATES_PTR:
            !byte SPRITE_OFFSET, SPRITE_OFFSET+5, SPRITE_OFFSET+10, SPRITE_OFFSET+15, SPRITE_OFFSET+20
            !byte SPRITE_OFFSET+5, SPRITE_OFFSET+10, SPRITE_OFFSET+15

; tree ending Y-positions used for depth sorting
TEMPLATES_Y:
            !byte 215, 201, 191, 173, 161
            !byte 201, 191, 173

; speeds from 0 to MAXY(220) 70 bytes
Y_TO_SPEED:
            !fill 150,SPEED1_TOP>>1
            !fill 20,SPEED1_TOP>>1 ; MINY
            !fill 16,SPEED2_MID>>1
            !fill 35,SPEED3_BOT>>1

; 8 random speeds to choose from
SPEED_AI_Y: !byte $FE,$FF,$FF,0,0,1,1,2


;----------------------------------------------------------------------------
; DATA distant background
;----------------------------------------------------------------------------

distant:
!fill 40,$20 ; space row
; Sprite2asm trees-distant-ch40-mc0f.png 24 jun. 2020 14:28:36
; charmap 560 bytes (40 x 14) hacked 40x12 520
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
!byte $40,$55,$54,$40,$40,$53,$54,$40,$53,$56,$53,$54,$40,$40,$40,$40,$53,$54,$40,$40,$53,$54,$40,$40,$40,$40,$53,$56,$40,$55,$54,$40,$53,$54,$40,$40,$55,$56,$40,$40
!byte $40,$53,$54,$57,$58,$59,$56,$57,$59,$54,$59,$56,$57,$58,$40,$40,$53,$54,$57,$58,$59,$54,$57,$58,$40,$40,$59,$54,$40,$53,$54,$40,$59,$56,$57,$58,$53,$54,$40,$40
!byte $58,$5a,$5b,$5c,$5d,$5a,$5b,$5c,$5a,$5b,$5a,$5b,$5c,$5d,$57,$58,$5a,$5b,$5c,$5d,$5a,$5b,$5c,$5d,$57,$58,$5a,$5b,$40,$5a,$5b,$40,$5a,$5b,$5c,$5d,$5a,$5b,$57,$58
!byte $5d,$5e,$5f,$40,$40,$5e,$5f,$40,$5c,$5d,$5e,$5f,$40,$40,$5c,$5d,$5e,$5f,$40,$40,$5e,$5f,$40,$40,$5c,$5d,$5e,$5f,$40,$5e,$5f,$40,$5e,$5f,$40,$40,$5e,$5f,$5c,$5d


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
!scr "      by alexander ",34,"paaco",34," paalvast     "
!scr "                                        "
!scr "        (c) 2020 twa",129,"n pa",129,"n games       "
introtext_end:

getreadytext:
;      12345678901234567890123456789012345678
!scr "                                        "
!scr "                                        "
!scr "         get ready for spunk!           "
!scr "                                        "
!scr "                                        "
!scr "    how many apples can you grab?       "
getreadytext_end:

toptext: ; 147=apple
;     1234567890123456789012345678901234567890
!scr "     score ",147,"000          high ",147,"000      "
toptext_color:
!scr "aaaaaaaaaaagaaaaaaaaaaaaaaaaaagaaaaaaaaa"

gameovertext:
!scr "game over!"
gameovertext_end:


;----------------------------------------------------------------------------
; DATA ground objects
;----------------------------------------------------------------------------

GROUNDOBJ_APPLE=19
GROUNDOBJ_EMPTY=20

; ground objects start with a color, followed by 2 columns of data, ending with a $20 character in the lower column
GROUNDOBJ_STRIDE=31
groundobj:
;     tree              rock                    plants                                      apple          grass         weed          bump
;     0     1   2   3   4       5   6   7   8   9       10  11  12  13  14  15  16  17  18  19     20  21  22    23  24  25    26  27  28    29  30
!byte RED+8,$82,$83,$20,WHITE+8,$84,$85,$86,$20,GREEN+8,$87,$88,$89,$8a,$87,$88,$89,$8a,$20,YELLOW,$20,$20,GREEN,$20,$20,BLACK,$20,$20,BLACK,$20,$20
!byte     0,$8b,$8c,$20,      0,$8d,$8e,$8f,$20,    $20,$90,$91,$92,$91,$92,$91,$92,$90,$20,   $20,147,$20,  $20,148,$20,  $20,149,$20,  $20,150,$20

; 8 random groundobjs (excludes apple)
RANDOM_OBJECTS:
            !byte 0,4,4,9,9,22,25,28

;256 bytes table to determine if object character should be clipped. 0 means clipped
CLIPPED:
            !fill MAX_OBJ_WIDTH,0
            !fill 40,1
            !fill 40,0
            !fill 40,1
            !fill 40,0
            !fill 40,1
            !fill 40,0
            !fill 6,0

; properties for interesting objects
GROUNDOBJ_MINPROP=$82 ; inclusive
GROUNDOBJ_MAXPROP=$93 ; exclusive $93=147=APPLE
OBJ_BLOCK=0
OBJ_SLOW=1
groundobj_prop:
            ;     82 83 84 85 86 87 88 89 8A 8B 8C 8D 8E 8F 90 91 92; 93
            !byte  0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1;, 3

; pointers to screen for character collision
MIN_SCREENLOC=$0400 + (SCROLL1Y-2)*40
SCREENROWL:
            !byte <(MIN_SCREENLOC+40*0)
            !byte <(MIN_SCREENLOC+40*1)
            !byte <(MIN_SCREENLOC+40*2)
            !byte <(MIN_SCREENLOC+40*3)
            !byte <(MIN_SCREENLOC+40*4)
            !byte <(MIN_SCREENLOC+40*5)
            !byte <(MIN_SCREENLOC+40*6)
            !byte <(MIN_SCREENLOC+40*7)
            !byte <(MIN_SCREENLOC+40*8)
            !byte <(MIN_SCREENLOC+40*9)
SCREENROWH:
            !byte >(MIN_SCREENLOC+40*0)
            !byte >(MIN_SCREENLOC+40*1)
            !byte >(MIN_SCREENLOC+40*2)
            !byte >(MIN_SCREENLOC+40*3)
            !byte >(MIN_SCREENLOC+40*4)
            !byte >(MIN_SCREENLOC+40*5)
            !byte >(MIN_SCREENLOC+40*6)
            !byte >(MIN_SCREENLOC+40*7)
            !byte >(MIN_SCREENLOC+40*8)
            !byte >(MIN_SCREENLOC+40*9)


;----------------------------------------------------------------------------
; MUSIC
;----------------------------------------------------------------------------
            * = $1c00

            !src "player2.inc"


;----------------------------------------------------------------------------
; CHARSET
;----------------------------------------------------------------------------
            * = $2000

            !src "chars.inc"


;----------------------------------------------------------------------------
; SPRITES
;----------------------------------------------------------------------------
            !align 63,0,0

SPRITE_OFFSET = (sprites and $3FFF) >> 6
sprites:
            !src "sprites.inc"
