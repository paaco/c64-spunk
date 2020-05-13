# c64-trees

### Trees

There are a maximum of 6 trees on screen, consisting of 5 sprites each.
With y-expanding, they fill the entire screen.
The upper 2 sprites are x-expanded making them even larger.

If the sprites are consecutive in memory, you only need 1 byte of PTR and inc them each IRQ.

Note that there are 2 player sprites that are weaved in between the 6 tree sprites because of priority.

There are a number of IRQs:

1. Set up all 8 sprites X, Y, ptrs and X- and Y- expansion (all but the player sprites) outside of the visible screen.

2. Updates tree sprite ptrs and their Y offset.

3. Updates tree sprite ptrs and their Y offset. Switch off x-expansion for the trees and fix their X-positions.

4. TODO


### Assets

Assets are drawn in Aseprite and saved as .png file. Custom converter Sprite2asm converts the png file based on instructions in the filename:

    -bgX specifies background color (defaults to transparent color index) and forces hires
    -mcXY specifies multi color 1 (X) and 2 (Y) and forces multi color
    -chXX specifies to create charset and charmap (bytes start at XX) instead of sprites

Note that multicolor bits are interpreted differently for sprites and characters:

        bits    00        01         10         11
    sprites     BG:$D021  MC1:$D025  $D027+     MC2:$D026
    chars       BG:$D021  MC1:$D022  MC2:$D023  $D800+

Sprites will always cover character/bitmap bits 00 and 01 (bit 0 for hires). However, sprites with 'lower' priority (their bit is set to 1 in $D01B) will be covered by bits 10 and 11 (bit 1 for hires).
