acme -f cbm -o .cache\trees1.prg trees.asm
exomizer sfx systrim -s "lda #0 sta $d020 sta $d011" -n .cache\trees1.prg -o .cache\trees2.prg
dir .cache\trees?.prg