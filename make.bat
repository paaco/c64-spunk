acme -f cbm -o .cache\trees1.prg trees.asm
exomizer sfx systrim -s "lda #5 sta $d020 sta $d011" -M256 -n .cache\trees1.prg -o .cache\treesp.prg
dir .cache\trees?.prg