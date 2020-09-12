; Nametable maps, attributes, palettes, sprite definitions

  .org $E000                  ; Align background so the lower address is $00

background:
  ; Background is 32x30
  ; Row 1
  .db $2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D
  ; Row 2
  .db $29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E
  ; Row 3
  .db $2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30
  ; Row 4
  .db $31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32
  ; Row 5
  .db $2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D
  ; Row 6
  .db $29,$2E,$33,$34,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E
  ; Row 7
  .db $2F,$30,$35,$36,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30
  ; Row 8
  .db $31,$32,$35,$36,$33,$34,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32
  ; Row 9
  .db $2C,$2D,$37,$38,$37,$38,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D
  ; Row 10
  .db $29,$2E,$39,$3A,$39,$3A,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E
  ; Row 11
  .db $2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30
  ; Row 12
  .db $31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32
  ; Row 13
  .db $2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D
  ; Row 14
  .db $29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E
  ; Row 15
  .db $2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30
  ; Row 16
  .db $31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32
  ; Row 17
  .db $2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D
  ; Row 18
  .db $29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E
  ; Row 19
  .db $2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30
  ; Row 20
  .db $31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32
  ; Row 21
  .db $2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D
  ; Row 22
  .db $29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E
  ; Row 23
  .db $2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30
  ; Row 24
  .db $31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32
  ; Row 25
  .db $2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D
  ; Row 26
  .db $29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E
  ; Row 27
  .db $2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30,$2F,$30
  ; Row 28
  .db $31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32,$31,$32
  ; Row 29
  .db $2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D,$2C,$2D
  ; Row 30
  .db $29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E,$29,$2E

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
  ;   Background palette
  .db $00,$22,$01,$23,  $00,$13,$01,$23,  $00,$30,$21,$0F,  $00,$27,$17,$0F
  ;   Player            Player Bullets    Enemey            Enemy Bullets
  .db $3E,$06,$30,$08,  $00,$24,$23,$34,  $00,$20,$28,$14,  $00,$02,$38,$3C

playersprites:
  .db $80,  $00,  %00000000,  $80
  .db $80,  $01,  %00000000,  $88
  .db $88,  $02,  %00000000,  $80
  .db $88,  $03,  %00000000,  $88
  .db $90,  $04,  %00000000,  $80
  .db $90,  $05,  %00000000,  $88

skelsprites:
  .db $44,  $0A,  %00000010,  $84
  .db $44,  $0A,  %01000010,  $8C
  .db $4C,  $0B,  %00000010,  $84
  .db $4C,  $0B,  %01000010,  $8C

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Velocity tables

; Player velocity is based on 1.5 pixels per second
playerMoveX:
  .db $75,$75,$75,$75,$75,$75,$75,$75 ; subpixels/frame
  .db $01,$01,$01,$01,$01,$01,$01,$01 ; pixels/frame (high bit is sign)

playerMoveY:
  .db $00,$00,$00,$00,$00,$00,$00,$00 ; subpixels/frame
  .db $00,$00,$00,$00,$00,$00,$00,$00 ; pixels/frame (high bit is sign)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Math tables

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
