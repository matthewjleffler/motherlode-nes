; Nametable maps, attributes, palettes, sprite definitions

  .org $E000                  ; Align background so the lower address is $00

background:
  ; Background is 32x30
  ; Row 1
  .db $04,$05,$04,$05,$04,$05,$04,$05,$04,$05,$04,$05,$04,$05,$04,$05
  .db $04,$05,$04,$05,$04,$05,$04,$05,$04,$05,$04,$05,$04,$05,$04,$05
  ; Row 2
  .db $01,$06,$01,$06,$01,$06,$01,$06,$01,$06,$01,$06,$01,$06,$01,$06
  .db $01,$06,$01,$06,$01,$06,$01,$06,$01,$06,$01,$06,$01,$06,$01,$06
  ; Row 3
  .db $07,$08,$07,$08,$07,$08,$07,$08,$07,$08,$07,$08,$07,$08,$07,$08
  .db $07,$08,$07,$08,$07,$08,$07,$08,$07,$08,$07,$08,$07,$08,$07,$08
  ; Row 4
  .db $09,$0A,$09,$0A,$09,$0A,$09,$0A,$09,$0A,$09,$0A,$09,$0A,$09,$0A
  .db $09,$0A,$09,$0A,$09,$0A,$09,$0A,$09,$0A,$09,$0A,$09,$0A,$09,$0A
  ; Row 5
  .db $04,$05,$04,$05,$04,$05,$04,$05,$04,$05,$04,$05,$04,$05,$04,$05
  .db $04,$05,$04,$05,$04,$05,$04,$05,$04,$05,$04,$05,$04,$05,$04,$05
  ; Row 6
  .db $01,$06,$0B,$0C,$01,$06,$01,$06,$01,$06,$01,$06,$01,$06,$01,$06
  .db $01,$06,$01,$06,$01,$06,$01,$06,$01,$06,$01,$06,$01,$06,$01,$06
  ; Row 7
  .db $07,$08,$0D,$0E,$07,$08,$07,$08,$07,$08,$07,$08,$07,$08,$07,$08
  .db $07,$08,$07,$08,$07,$08,$07,$08,$07,$08,$07,$08,$07,$08,$07,$08
  ; Row 8
  .db $09,$0A,$0D,$0E,$0B,$0C,$09,$0A,$09,$0A,$09,$0A,$09,$0A,$09,$0A
  .db $09,$0A,$09,$0A,$09,$0A,$09,$0A,$09,$0A,$09,$0A,$09,$0A,$09,$0A
  ; Row 9
  .db $04,$05,$0F,$10,$0F,$10,$04,$05,$04,$05,$04,$05,$04,$05,$04,$05
  .db $04,$05,$04,$05,$04,$05,$04,$05,$04,$05,$04,$05,$04,$05,$04,$05
  ; Row 10
  .db $01,$06,$11,$12,$11,$12,$01,$06,$01,$06,$01,$06,$01,$06,$01,$06
  .db $01,$06,$01,$06,$01,$06,$01,$06,$01,$06,$01,$06,$01,$06,$01,$06
  ; Row 11
  .db $07,$08,$07,$08,$07,$08,$07,$08,$07,$08,$07,$08,$07,$08,$07,$08
  .db $07,$08,$07,$08,$07,$08,$07,$08,$07,$08,$07,$08,$07,$08,$07,$08
  ; Row 12
  .db $09,$0A,$09,$0A,$09,$0A,$09,$0A,$09,$0A,$09,$0A,$09,$0A,$09,$0A
  .db $09,$0A,$09,$0A,$09,$0A,$09,$0A,$09,$0A,$09,$0A,$09,$0A,$09,$0A
  ; Row 13
  .db $04,$05,$04,$05,$04,$05,$04,$05,$04,$05,$04,$05,$04,$05,$04,$05
  .db $04,$05,$04,$05,$04,$05,$04,$05,$04,$05,$04,$05,$04,$05,$04,$05
  ; Row 14
  .db $01,$06,$01,$06,$01,$06,$01,$06,$01,$06,$01,$06,$01,$06,$01,$06
  .db $01,$06,$01,$06,$01,$06,$01,$06,$01,$06,$01,$06,$01,$06,$01,$06
  ; Row 15
  .db $07,$08,$07,$08,$07,$08,$07,$08,$07,$08,$07,$08,$07,$08,$07,$08
  .db $07,$08,$07,$08,$07,$08,$07,$08,$07,$08,$07,$08,$07,$08,$07,$08
  ; Row 16
  .db $09,$0A,$09,$0A,$09,$0A,$09,$0A,$09,$0A,$09,$0A,$09,$0A,$09,$0A
  .db $09,$0A,$09,$0A,$09,$0A,$09,$0A,$09,$0A,$09,$0A,$09,$0A,$09,$0A
  ; Row 17
  .db $04,$05,$04,$05,$04,$05,$04,$05,$04,$05,$04,$05,$04,$05,$04,$05
  .db $04,$05,$04,$05,$04,$05,$04,$05,$04,$05,$04,$05,$04,$05,$04,$05
  ; Row 18
  .db $01,$06,$01,$06,$01,$06,$01,$06,$01,$06,$01,$06,$01,$06,$01,$06
  .db $01,$06,$01,$06,$01,$06,$01,$06,$01,$06,$01,$06,$01,$06,$01,$06
  ; Row 19
  .db $07,$08,$07,$08,$07,$08,$07,$08,$07,$08,$07,$08,$07,$08,$07,$08
  .db $07,$08,$07,$08,$07,$08,$07,$08,$07,$08,$07,$08,$07,$08,$07,$08
  ; Row 20
  .db $09,$0A,$09,$0A,$09,$0A,$09,$0A,$09,$0A,$09,$0A,$09,$0A,$09,$0A
  .db $09,$0A,$09,$0A,$09,$0A,$09,$0A,$09,$0A,$09,$0A,$09,$0A,$09,$0A
  ; Row 21
  .db $04,$05,$04,$05,$04,$05,$04,$05,$04,$05,$04,$05,$04,$05,$04,$05
  .db $04,$05,$04,$05,$04,$05,$04,$05,$04,$05,$04,$05,$04,$05,$04,$05
  ; Row 22
  .db $01,$06,$01,$06,$01,$06,$01,$06,$01,$06,$01,$06,$01,$06,$01,$06
  .db $01,$06,$01,$06,$01,$06,$01,$06,$01,$06,$01,$06,$01,$06,$01,$06
  ; Row 23
  .db $07,$08,$07,$08,$07,$08,$07,$08,$07,$08,$07,$08,$07,$08,$07,$08
  .db $07,$08,$07,$08,$07,$08,$07,$08,$07,$08,$07,$08,$07,$08,$07,$08
  ; Row 24
  .db $09,$0A,$09,$0A,$09,$0A,$09,$0A,$09,$0A,$09,$0A,$09,$0A,$09,$0A
  .db $09,$0A,$09,$0A,$09,$0A,$09,$0A,$09,$0A,$09,$0A,$09,$0A,$09,$0A
  ; Row 25
  .db $04,$05,$04,$05,$04,$05,$04,$05,$04,$05,$04,$05,$04,$05,$04,$05
  .db $04,$05,$04,$05,$04,$05,$04,$05,$04,$05,$04,$05,$04,$05,$04,$05
  ; Row 26
  .db $01,$06,$01,$06,$01,$06,$01,$06,$01,$06,$01,$06,$01,$06,$01,$06
  .db $01,$06,$01,$06,$01,$06,$01,$06,$01,$06,$01,$06,$01,$06,$01,$06
  ; Row 27
  .db $07,$08,$07,$08,$07,$08,$07,$08,$07,$08,$07,$08,$07,$08,$07,$08
  .db $07,$08,$07,$08,$07,$08,$07,$08,$07,$08,$07,$08,$07,$08,$07,$08
  ; Row 28
  .db $09,$0A,$09,$0A,$09,$0A,$09,$0A,$09,$0A,$09,$0A,$09,$0A,$09,$0A
  .db $09,$0A,$09,$0A,$09,$0A,$09,$0A,$09,$0A,$09,$0A,$09,$0A,$09,$0A
  ; Row 29
  .db $04,$05,$04,$05,$04,$05,$04,$05,$04,$05,$04,$05,$04,$05,$04,$05
  .db $04,$05,$04,$05,$04,$05,$04,$05,$04,$05,$04,$05,$04,$05,$04,$05
  ; Row 30
  .db $01,$06,$01,$06,$01,$06,$01,$06,$01,$06,$01,$06,$01,$06,$01,$06
  .db $01,$06,$01,$06,$01,$06,$01,$06,$01,$06,$01,$06,$01,$06,$01,$06

  ; 8 x 8 = 64 bytes
  ;   %BRBLTRTL
attributes:
  ; Row  1/ 2/ 3/ 4
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  ; Row  5/ 6/ 7/ 8
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  ; Row  9/10/11/12
  .db %00000100, %00000001, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  ; Row 13/14/15/16
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  ; Row 17/18/19/20
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  ; Row 21/22/23/24
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  ; Row 25/26/27/28
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  ; Row 29/30/31/32
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000

palette:
  ; Background palette
  .db $00,$22,$01,$23,  $00,$13,$01,$23,  $00,$30,$21,$0F,  $00,$27,$17,$0F
  ; Sprite palette
  .db $22,$06,$30,$08,  $00,$24,$23,$34,  $00,$1C,$15,$14,  $00,$02,$38,$3C

sprites:
  ;   Y,    tile, attr,       X
  ;               76543210
  ;               |||   ||
  ;               |||   ++- Palette
  ;               |||
  ;               ||+------ Priority (0: in front of background; 1: behind)
  ;               |+------- Flip sprite horizontally
  ;               +-------- Flip sprite vertically
  ; PLAYER
  .db $80,  $00,  %00000000,  $80
  .db $80,  $01,  %00000000,  $88
  .db $88,  $02,  %00000000,  $80
  .db $88,  $03,  %00000000,  $88
  .db $90,  $04,  %00000000,  $80
  .db $90,  $05,  %00000000,  $88
  ; BULLET0
  .db $80,  $01,  %00000001,  $C8
  .db $80,  $01,  %00000001,  $D0
  .db $88,  $01,  %00000001,  $C8
  .db $88,  $01,  %00000001,  $D0
  ; BULLET1
  .db $40,  $01,  %00000001,  $30
  .db $40,  $01,  %00000001,  $30
  .db $40,  $01,  %00000001,  $30
  .db $40,  $01,  %00000001,  $30
  ; BULLET2
  .db $20,  $01,  %00000001,  $60
  .db $40,  $01,  %00000001,  $30
  .db $40,  $01,  %00000001,  $30
  .db $40,  $01,  %00000001,  $30
  ; BULLET3
  .db $C8,  $01,  %00000001,  $80
  .db $40,  $01,  %00000001,  $30
  .db $40,  $01,  %00000001,  $30
  .db $40,  $01,  %00000001,  $30
