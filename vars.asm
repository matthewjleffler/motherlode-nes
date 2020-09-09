;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Variables

  .rsset $0000                ; put pointers in zero page

pointerLo               .rs 1 ; pointer variables are declared in RAM
pointerHi               .rs 1 ; low byte first, high byte immediately after
temp                    .rs 1 ; temp reusable byte
buttons1                .rs 1 ; controller 1 buttons
buttons2                .rs 1 ; controller 2 buttons
buttons1fresh           .rs 1 ; controller 1 buttons fresh
buttons2fresh           .rs 1 ; controller 2 buttons fresh
animTick                .rs 1 ; Slows down animation counting
bulletAnim              .rs 1 ; Bullet anim state
spriteLayoutOriginY     .rs 1 ; Y of sprite origin
spriteLayoutOriginX     .rs 1 ; X of sprite origin
bulletFrame             .rs 4 ; The frames to apply to the current bullet
bulletAttr              .rs 4 ; The attributes to apply to the current bullet
bulletCount             .rs 1 ; Total number of bullets to render
playerBulletStates      .rs 1 ; On/off states for 4 player bullets
                              ; 00 - off
                              ; 01 - on
                              ; 10 - explosion?
                              ; 11 - unused ??
                              ; 44332211
; multFactor              .rs 1 ; Multiplication factor
; multRes1                .rs 1 ; Multiplication result 1
; multRes2                .rs 1 ; Multiplication result 2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Constants

; Rendering
SPRITEHI    = $02             ; High byte of sprite address is always the same
TILEW       = $08
SPRITETIL   = $01
SPRITEATT   = $02
SPRITEX     = $03

; Controllers
CONTROLHI   = $40
CONTROLLO   = $16
BUTTONA     = %10000000
BUTTONB     = %01000000
BUTTONSEL   = %00100000
BUTTONSTA   = %00010000
BUTTONU     = %00001000
BUTTOND     = %00000100
BUTTONL     = %00000010
BUTTONR     = %00000001

; Gameplay
STATEMASK   = %00000011       ; Mask for lower two bits
HICLEAR     = %00111111       ; Mask to clear high bits
BULLETCOUNT = $04             ; Number of bullets to render

; Move Speed
SPDSLOW     = $01             ; 1 pixel per frame
SPDFAST     = $03             ; 3 pixels per frame
SPDBULLET   = $05

; Sprite lo addresses
EBULLET0    = $00             ; Size: 4*8=32
PBULLET0    = $20             ; Size: 4*4*4=64
PLAYER      = $60             ; Size: 4*6=24
ITEM        = $78             ; Size: 4*4=8
ENEMY0      = $80             ; Size: 4*4*?

PLAYERSIZE  = $18             ; player byte size

; Animation
BULLETNOFL  = %00000001
BULLETFLX   = %01000001
BULLETFLY   = %10000001
BULLETFLXY  = %11000001
BULLFRAME0  = $06
BULLFRAME1  = $07
BULLFRAME2  = $08
BULLFRAME3  = $09

; Bullet States
BULL_OFF    = $00
BULL_MOV    = $01
BULL_EXP    = $02
