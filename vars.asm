;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Variables
  .rsset $0000                ; put pointers in zero page

pointerLo               .rs 1 ; pointer variables are declared in RAM
pointerHi               .rs 1 ; low byte first, high byte immediately after
buttons                 .rs 1 ; buttons
animTick                .rs 1 ; Slows down animation counting
bulletAnim              .rs 1 ; Bullet anim state
spriteLayoutOriginY     .rs 1 ; Y of sprite origin
spriteLayoutOriginX     .rs 1 ; X of sprite origin
bulletFrame             .rs 4 ; The frames to apply to the current bullet
bulletAttr              .rs 4 ; The attributes to apply to the current bullet
bulletCount             .rs 1 ; Total number of bullets to render
; multFactor              .rs 1 ; Multiplication factor
; multRes1                .rs 1 ; Multiplication result 1
; multRes2                .rs 1 ; Multiplication result 2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Constants

; Render Constants
SPRITEHI    = $02             ; High byte of sprite address is always the same
TILEW       = $08
SPRITETIL   = $01
SPRITEATT   = $02
SPRITEX     = $03
SPRITE2X    = $07             ; In a 2 tile wide sprite, the right hand tile

; Controllers
CONTROLLER1 = $4016
CONTROLLER2 = $4017
BUTTONA     = %10000000
BUTTONB     = %01000000
BUTTONSEL   = %00100000
BUTTONSTA   = %00010000
BUTTONU     = %00001000
BUTTOND     = %00000100
BUTTONL     = %00000010
BUTTONR     = %00000001

; Move Speed
SPDSLOW     = $01             ; 1 pixel per frame
SPDFAST     = $03             ; 3 pixels per frame
SPDBULLET   = $01

; Sprites, low addresses relative to $0200
PLAYER      = $00
BULLET0     = $18

; Animation
BULLETNOFL  = %00000001
BULLETFLX   = %01000001
BULLETFLY   = %10000001
BULLETFLXY  = %11000001
BULLSTATE   = %00000011
BULLFRAME0  = $06
BULLFRAME1  = $07
BULLFRAME2  = $08
BULLFRAME3  = $09
