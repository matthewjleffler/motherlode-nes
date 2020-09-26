; tables.asm
;   Nametables, palettes, LUTs, math

  .org $E000                  ; Align background so the lower address is $00

; Gameplay
gameplayBackground:           ; Background is 32x30
  .db $28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28 ; att 1
  .db $28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28
  .db $28,$0B,$28,$0A,$28,$11,$12,$1D,$1C,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$1C,$0C,$18,$1B,$0E,$28
  .db $28,$63,$28,$63,$28,$61,$61,$61,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$00,$00,$00,$00,$00,$00,$00,$00,$28 ; Status bar

  .db $35,$36,$37,$38,$35,$36,$37,$38,$37,$38,$37,$38,$37,$38,$37,$38,$37,$38,$37,$38,$37,$38,$37,$38,$37,$38,$37,$38,$37,$38,$35,$36 ; att 2
  .db $35,$36,$39,$3A,$35,$36,$39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A,$35,$36
  .db $35,$36,$2F,$30,$35,$36,$2F,$30,$2F,$30,$3B,$3C,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$35,$36
  .db $35,$36,$33,$34,$35,$36,$31,$32,$31,$32,$3D,$3E,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$33,$34,$35,$36
  .db $35,$36,$37,$38,$37,$38,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$3F,$40,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$37,$38,$35,$36 ; att 3
  .db $35,$36,$39,$3A,$39,$3A,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$41,$42,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$39,$3A,$35,$36
  .db $35,$36,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$35,$36
  .db $35,$36,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$35,$36
  .db $35,$36,$2C,$2D,$3F,$40,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$35,$36 ; att 4
  .db $35,$36,$29,$2E,$41,$42,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$33,$34,$29,$2E,$29,$2E,$29,$2E,$35,$36
  .db $35,$36,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$37,$38,$2F,$30,$2F,$30,$2F,$30,$35,$36
  .db $35,$36,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$39,$3A,$31,$32,$31,$32,$31,$32,$35,$36
  .db $35,$36,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$35,$36 ; att 5
  .db $35,$36,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$35,$36
  .db $35,$36,$2F,$30,$3B,$3C,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$3B,$3C,$2F,$30,$35,$36
  .db $35,$36,$31,$32,$3D,$3E,$31,$32,$31,$32,$31,$32,$33,$34,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$3D,$3E,$31,$32,$35,$36
  .db $35,$36,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$37,$38,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$35,$36 ; att 6
  .db $35,$36,$29,$2E,$33,$34,$33,$34,$33,$34,$29,$2E,$39,$3A,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$33,$34,$35,$36
  .db $35,$36,$2F,$30,$37,$38,$37,$38,$35,$36,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$37,$38,$35,$36
  .db $35,$36,$31,$32,$39,$3A,$39,$3A,$35,$36,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$33,$34,$31,$32,$39,$3A,$35,$36
  .db $35,$36,$2C,$2D,$2C,$2D,$2C,$2D,$37,$38,$2C,$2D,$2C,$2D,$2C,$2D,$3F,$40,$2C,$2D,$2C,$2D,$2C,$2D,$37,$38,$2C,$2D,$2C,$2D,$35,$36 ; att 7
  .db $35,$36,$29,$2E,$29,$2E,$29,$2E,$39,$3A,$29,$2E,$29,$2E,$29,$2E,$41,$42,$29,$2E,$29,$2E,$29,$2E,$39,$3A,$29,$2E,$29,$2E,$35,$36
  .db $35,$36,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$35,$36
  .db $35,$36,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$35,$36
  .db $35,$36,$33,$34,$33,$34,$33,$34,$33,$34,$33,$34,$33,$34,$33,$34,$33,$34,$33,$34,$33,$34,$33,$34,$33,$34,$33,$34,$33,$34,$35,$36 ; att 8
  .db $35,$36,$33,$34,$33,$34,$33,$34,$33,$34,$33,$34,$33,$34,$33,$34,$33,$34,$33,$34,$33,$34,$33,$34,$33,$34,$33,$34,$33,$34,$35,$36

  ; 8 x 8 = 64 bytes
  ;   %BRBLTRTL
gameplayAttributes:
  .db %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111  ; Row  1/ 2/ 3/ 4 ; Status bar
  .db %00000100, %00000100, %00000101, %00000101, %00000101, %00000101, %00000101, %00000101  ; Row  5/ 6/ 7/ 8
  .db %00000100, %00000001, %00000000, %00000000, %00000000, %00000000, %00000000, %00000101  ; Row  9/10/11/12
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %01000000, %00000000, %00000000  ; Row 13/14/15/16
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000  ; Row 17/18/19/20
  .db %00000000, %01010000, %00000000, %00000001, %00000000, %00000000, %00000000, %00010000  ; Row 21/22/23/24
  .db %00000000, %00000000, %00000001, %00000000, %00000000, %00000000, %00000001, %00000000  ; Row 25/26/27/28
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000  ; Row 29/30

; Title
titleBackground:              ; Background is 32x30
  .db $28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28 ; att 1
  .db $28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28
  .db $28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28
  .db $28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28
  .db $28,$28,$28,$33,$28,$28,$28,$33,$28,$33,$34,$33,$28,$33,$34,$33,$28,$33,$28,$33,$28,$33,$34,$33,$28,$33,$34,$33,$28,$28,$28,$28 ; att 2
  .db $28,$28,$28,$35,$33,$28,$34,$35,$28,$35,$39,$35,$28,$39,$35,$39,$28,$35,$28,$35,$28,$35,$39,$3A,$28,$35,$39,$35,$28,$28,$28,$28
  .db $28,$28,$28,$35,$39,$33,$39,$35,$28,$35,$28,$35,$28,$28,$35,$28,$28,$35,$34,$35,$28,$35,$34,$28,$28,$35,$34,$39,$28,$28,$28,$28
  .db $28,$28,$28,$35,$28,$39,$28,$35,$28,$35,$28,$35,$28,$28,$35,$28,$28,$35,$39,$35,$28,$35,$39,$28,$28,$35,$39,$33,$28,$28,$28,$28
  .db $28,$28,$28,$35,$28,$28,$28,$35,$28,$35,$33,$35,$28,$28,$35,$28,$28,$35,$28,$35,$28,$35,$34,$33,$28,$35,$28,$35,$28,$28,$28,$28 ; att 3
  .db $28,$28,$28,$39,$28,$28,$28,$39,$28,$39,$3A,$39,$28,$28,$39,$28,$28,$39,$28,$39,$28,$39,$3A,$39,$28,$39,$28,$39,$28,$28,$28,$28
  .db $28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28
  .db $28,$28,$28,$28,$28,$28,$28,$28,$28,$33,$28,$28,$28,$33,$34,$33,$28,$33,$34,$28,$28,$33,$34,$33,$28,$28,$28,$28,$28,$28,$28,$28
  .db $28,$28,$28,$28,$28,$28,$28,$28,$28,$35,$28,$28,$28,$35,$39,$35,$28,$35,$39,$33,$28,$35,$39,$3A,$28,$28,$28,$28,$28,$28,$28,$28 ; att 4
  .db $28,$28,$28,$28,$28,$28,$28,$28,$28,$35,$28,$28,$28,$35,$28,$35,$28,$35,$28,$35,$28,$35,$34,$28,$28,$28,$28,$28,$28,$28,$28,$28
  .db $28,$28,$28,$28,$28,$28,$28,$28,$28,$35,$28,$28,$28,$35,$28,$35,$28,$35,$28,$35,$28,$35,$39,$28,$28,$28,$28,$28,$28,$28,$28,$28
  .db $28,$28,$28,$28,$28,$28,$28,$28,$28,$35,$33,$34,$28,$35,$34,$35,$28,$35,$34,$39,$28,$35,$33,$34,$28,$28,$28,$28,$28,$28,$28,$28
  .db $28,$28,$28,$28,$28,$28,$28,$28,$28,$39,$3A,$39,$28,$39,$3A,$39,$28,$39,$3A,$28,$28,$39,$3A,$39,$28,$28,$28,$28,$28,$28,$28,$28 ; att 5
  .db $28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28
  .db $28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28
  .db $28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28
  .db $28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$19,$1B,$0E,$1C,$1C,$28,$1C,$1D,$0A,$1B,$1D,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28 ; att 6
  .db $28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28
  .db $28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28
  .db $28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28
  .db $28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28 ; att 7
  .db $28,$28,$28,$24,$28,$02,$00,$02,$00,$28,$16,$0A,$1D,$1D,$28,$15,$0E,$0F,$0F,$15,$0E,$1B,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28
  .db $28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28
  .db $28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28
  .db $28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28 ; att 8
  .db $28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28

titleAttributes:
  .db %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101  ; Row  1/ 2/ 3/ 4 ; Status bar
  .db %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101  ; Row  5/ 6/ 7/ 8
  .db %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101  ; Row  9/10/11/12
  .db %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101  ; Row 13/14/15/16
  .db %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101  ; Row 17/18/19/20
  .db %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111  ; Row 21/22/23/24
  .db %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111  ; Row 25/26/27/28
  .db %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111  ; Row 29/30

; COLLISION

; $E400
collision:
  .db %00000000,%00000000,%00000000,%00000000
  .db %00000000,%00000000,%00000000,%00000000
  .db %00000000,%00000000,%00000000,%00000000
  .db %00000000,%00000000,%00000000,%00000000 ; status

  .db %11111111,%11111111,%11111111,%11111111
  .db %11001100,%00000000,%00000000,%00000011 ; top of wall
  .db %11001100,%00000000,%00000000,%00000011
  .db %11111100,%00000000,%00000000,%00001111
  .db %11111100,%00000000,%00000000,%00001111
  .db %11000000,%00000000,%00000000,%00000011
  .db %11000000,%00000000,%00000000,%00000011
  .db %11000000,%00000000,%00000000,%00000011
  .db %11000000,%00000000,%00000000,%00000011
  .db %11000000,%00000000,%00000011,%00000011
  .db %11000000,%00000000,%00000011,%00000011
  .db %11000000,%00000000,%00000000,%00000011
  .db %11000000,%00000000,%00000000,%00000011
  .db %11000000,%00000000,%00000000,%00000011
  .db %11000000,%00000000,%00000000,%00000011
  .db %11000000,%00001100,%00000000,%00000011
  .db %11000000,%00001100,%00000000,%00000011
  .db %11001111,%11000000,%00000000,%00001111
  .db %11001111,%11000000,%00000000,%00001111
  .db %11000000,%11000000,%00000000,%11000011
  .db %11000000,%11000000,%00000000,%11000011
  .db %11000000,%00000000,%00000000,%00000011
  .db %11000000,%00000000,%00000000,%00000011
  .db %11000000,%00000000,%00000000,%00000011
  .db %11111111,%11111111,%11111111,%11111111 ; Bottom edge of screen

  .db %00000000,%00000000,%00000000,%00000000 ; Black bar

collisionLookupY:
  .db 000, 004, 008, 012, 016, 020, 024, 028, 032, 036
  .db 040, 044, 048, 052, 056, 060, 064, 068, 072, 076
  .db 080, 084, 088, 092, 096, 100, 104, 108, 112, 116

collisionLookupX:
  .db 00, 08, 016, 024

; PALETTES

defaultPalette:
  ;   Map 1             Map 2                               Hud
  .db $00,$22,$01,$23,  $00,$13,$01,$23,  $00,$30,$21,$0F,  $00,$20,$16,$04
  ;   Player            Player Bullets    Enemey            Enemy Bullets
  .db $3E,$06,$30,$08,  $00,$24,$23,$34,  $00,$20,$28,$1D,  $00,$2A,$3A,$3C

; LOOKUP TABLES

; Gets position offset by index
positionOffset:
  .db 0,  2,  4,  6,  8, 10, 12, 14

; Gets byte offset for 4x4 sprites by index
spriteOffset:
  .db 0, 16, 32, 48, 64, 80

; Gets byte offset for 1x1 sprites by index
spriteOffset1:
  .db 0,  4,  8, 12, 16, 20, 24, 28

; Player input patterns, matching up to move velocities below
playerInput:
      ; R        ; DR       ; D         ; DL
  .db %00000001, %00000101, %00000100, %00000110
      ; L        ; UL       ; U         ; UR
  .db %00000010, %00001010, %00001000, %00001001

; Enemy spawn point - tile * tile width = pixel location
enemySpawnX:
  .db 11 * TILE_WIDTH,  5 * TILE_WIDTH, 27 * TILE_WIDTH, 17 * TILE_WIDTH,  5 * TILE_WIDTH, 17 * TILE_WIDTH

enemySpawnY:
  .db  7 * TILE_WIDTH, 19 * TILE_WIDTH, 19 * TILE_WIDTH,  9 * TILE_WIDTH, 13 * TILE_WIDTH, 25 * TILE_WIDTH

; VELOCITY TABLES

; Player movement velocity table (8)
playerMoveX:
  .db $80,$0F,$00,$0F,$80,$0F,$00,$0F
  .db $01,$01,$00,$81,$81,$81,$00,$01
playerMoveY:
  .db $00,$0F,$80,$0F,$00,$0F,$80,$0F
  .db $00,$01,$01,$01,$00,$81,$81,$81

; Player bullet movement velocity table (32)
playerBulletMoveX:
  .db $BF,$A8,$63,$F2,$5C,$A3,$D1,$ED,$00,$ED,$D1,$A3,$5C,$F2,$63,$A8,$BF,$A8,$63,$F2,$5C,$A3,$D1,$ED,$00,$ED,$D1,$A3,$5C,$F2,$63,$A8
  .db $04,$04,$04,$03,$03,$02,$01,$00,$00,$80,$81,$82,$83,$83,$84,$84,$84,$84,$84,$83,$83,$82,$81,$80,$00,$00,$01,$02,$03,$03,$04,$04
playerBulletMoveY:
  .db $00,$ED,$D1,$A3,$5C,$F2,$63,$A8,$BF,$A8,$63,$F2,$5C,$A3,$D1,$ED,$00,$ED,$D1,$A3,$5C,$F2,$63,$A8,$BF,$A8,$63,$F2,$5C,$A3,$D1,$ED
  .db $00,$00,$01,$02,$03,$03,$04,$04,$04,$04,$04,$03,$03,$02,$01,$00,$00,$80,$81,$82,$83,$83,$84,$84,$84,$84,$84,$83,$83,$82,$81,$80

; Enemy bullet movement velocity table (32)
enemyBulletMoveX:
  .db $00,$F5,$D9,$A8,$69,$1C,$C4,$63,$00,$63,$C4,$1C,$69,$A8,$D9,$F5,$00,$F5,$D9,$A8,$69,$1C,$C4,$63,$00,$63,$C4,$1C,$69,$A8,$D9,$F5
  .db $02,$01,$01,$01,$01,$01,$00,$00,$00,$80,$80,$81,$81,$81,$81,$81,$82,$81,$81,$81,$81,$81,$80,$80,$00,$00,$00,$01,$01,$01,$01,$01
enemyBulletMoveY:
  .db $00,$63,$C4,$1C,$69,$A8,$D9,$F5,$00,$F5,$D9,$A8,$69,$1C,$C4,$63,$00,$63,$C4,$1C,$69,$A8,$D9,$F5,$00,$F5,$D9,$A8,$69,$1C,$C4,$63
  .db $00,$00,$00,$01,$01,$01,$01,$01,$02,$01,$01,$01,$01,$01,$00,$00,$00,$80,$80,$81,$81,$81,$81,$81,$82,$81,$81,$81,$81,$81,$80,$80

; Enemy slow movement velocity table (16)
enemySlowMoveX:
  .db $00,$EB,$B5,$61,$00,$61,$B5,$EB,$00,$EB,$B5,$61,$00,$61,$B5,$EB
  .db $01,$00,$00,$00,$00,$80,$80,$80,$81,$80,$80,$80,$00,$00,$00,$00
enemySlowMoveY:
  .db $00,$61,$B5,$EB,$00,$EB,$B5,$61,$00,$61,$B5,$EB,$00,$EB,$B5,$61
  .db $00,$00,$00,$00,$01,$00,$00,$00,$00,$80,$80,$80,$81,$80,$80,$80

; MATH TABLES

octant_adjust:
  .db %00111111               ; x+,y+,|x|>|y|
  .db %00000000               ; x+,y+,|x|<|y|
  .db %11000000               ; x+,y-,|x|>|y|
  .db %11111111               ; x+,y-,|x|<|y|
  .db %01000000               ; x-,y+,|x|>|y|
  .db %01111111               ; x-,y+,|x|<|y|
  .db %10111111               ; x-,y-,|x|>|y|
  .db %10000000               ; x-,y-,|x|<|y|

atan_tab:                     ; atan(2^(x/32))*128/pi
  .db $00,$00,$00,$00,$00,$00,$00,$00
  .db $00,$00,$00,$00,$00,$00,$00,$00
  .db $00,$00,$00,$00,$00,$00,$00,$00
  .db $00,$00,$00,$00,$00,$00,$00,$00
  .db $00,$00,$00,$00,$00,$00,$00,$00
  .db $00,$00,$00,$00,$00,$00,$00,$00
  .db $00,$00,$00,$00,$00,$00,$00,$00
  .db $00,$00,$00,$00,$00,$00,$00,$00
  .db $00,$00,$00,$00,$00,$00,$00,$00
  .db $00,$00,$00,$00,$00,$00,$00,$00
  .db $00,$00,$00,$00,$00,$01,$01,$01
  .db $01,$01,$01,$01,$01,$01,$01,$01
  .db $01,$01,$01,$01,$01,$01,$01,$01
  .db $01,$01,$01,$01,$01,$01,$01,$01
  .db $01,$01,$01,$01,$01,$02,$02,$02
  .db $02,$02,$02,$02,$02,$02,$02,$02
  .db $02,$02,$02,$02,$02,$02,$02,$02
  .db $03,$03,$03,$03,$03,$03,$03,$03
  .db $03,$03,$03,$03,$03,$04,$04,$04
  .db $04,$04,$04,$04,$04,$04,$04,$04
  .db $05,$05,$05,$05,$05,$05,$05,$05
  .db $06,$06,$06,$06,$06,$06,$06,$06
  .db $07,$07,$07,$07,$07,$07,$08,$08
  .db $08,$08,$08,$08,$09,$09,$09,$09
  .db $09,$0a,$0a,$0a,$0a,$0b,$0b,$0b
  .db $0b,$0c,$0c,$0c,$0c,$0d,$0d,$0d
  .db $0d,$0e,$0e,$0e,$0e,$0f,$0f,$0f
  .db $10,$10,$10,$11,$11,$11,$12,$12
  .db $12,$13,$13,$13,$14,$14,$15,$15
  .db $15,$16,$16,$17,$17,$17,$18,$18
  .db $19,$19,$19,$1a,$1a,$1b,$1b,$1c
  .db $1c,$1c,$1d,$1d,$1e,$1e,$1f,$1f

log2_tab:                     ; log2(x)*32
  .db $00,$00,$20,$32,$40,$4a,$52,$59
  .db $60,$65,$6a,$6e,$72,$76,$79,$7d
  .db $80,$82,$85,$87,$8a,$8c,$8e,$90
  .db $92,$94,$96,$98,$99,$9b,$9d,$9e
  .db $a0,$a1,$a2,$a4,$a5,$a6,$a7,$a9
  .db $aa,$ab,$ac,$ad,$ae,$af,$b0,$b1
  .db $b2,$b3,$b4,$b5,$b6,$b7,$b8,$b9
  .db $b9,$ba,$bb,$bc,$bd,$bd,$be,$bf
  .db $c0,$c0,$c1,$c2,$c2,$c3,$c4,$c4
  .db $c5,$c6,$c6,$c7,$c7,$c8,$c9,$c9
  .db $ca,$ca,$cb,$cc,$cc,$cd,$cd,$ce
  .db $ce,$cf,$cf,$d0,$d0,$d1,$d1,$d2
  .db $d2,$d3,$d3,$d4,$d4,$d5,$d5,$d5
  .db $d6,$d6,$d7,$d7,$d8,$d8,$d9,$d9
  .db $d9,$da,$da,$db,$db,$db,$dc,$dc
  .db $dd,$dd,$dd,$de,$de,$de,$df,$df
  .db $df,$e0,$e0,$e1,$e1,$e1,$e2,$e2
  .db $e2,$e3,$e3,$e3,$e4,$e4,$e4,$e5
  .db $e5,$e5,$e6,$e6,$e6,$e7,$e7,$e7
  .db $e7,$e8,$e8,$e8,$e9,$e9,$e9,$ea
  .db $ea,$ea,$ea,$eb,$eb,$eb,$ec,$ec
  .db $ec,$ec,$ed,$ed,$ed,$ed,$ee,$ee
  .db $ee,$ee,$ef,$ef,$ef,$ef,$f0,$f0
  .db $f0,$f1,$f1,$f1,$f1,$f1,$f2,$f2
  .db $f2,$f2,$f3,$f3,$f3,$f3,$f4,$f4
  .db $f4,$f4,$f5,$f5,$f5,$f5,$f5,$f6
  .db $f6,$f6,$f6,$f7,$f7,$f7,$f7,$f7
  .db $f8,$f8,$f8,$f8,$f9,$f9,$f9,$f9
  .db $f9,$fa,$fa,$fa,$fa,$fa,$fb,$fb
  .db $fb,$fb,$fb,$fc,$fc,$fc,$fc,$fc
  .db $fd,$fd,$fd,$fd,$fd,$fd,$fe,$fe
  .db $fe,$fe,$fe,$ff,$ff,$ff,$ff,$ff
