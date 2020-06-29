acme -f cbm -o .cache\trees1.prg trees.asm
exomizer sfx systrim -s "lda #14 sta $d020 sta $d011" -M256 -Di_perf=-1 -n .cache\trees1.prg -o .cache\spunk.prg
dir .cache\trees?.prg