;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Variables
  .rsset $0000                ; put pointers in zero page

pointerLo   .rs 1             ; pointer variables are declared in RAM
pointerHi   .rs 1             ; low byte first, high byte immediately after
buttons     .rs 1             ; buttons
animTick    .rs 1             ; Slows down animation counting
bulletAnim0 .rs 1             ; Bullet 0 anim state

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Constants

; Render Constants
TILEW       = $08
SPRITETIL   = $01
SPRITEATT   = $02
SPRITEX     = $03

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

; Sprites
PLAYER0     = $0200
PLAYER1     = $0204
PLAYER2     = $0208
PLAYER3     = $020C
PLAYER4     = $0210
PLAYER5     = $0214
BULLET00    = $0218
BULLET01    = $021C
BULLET02    = $0220
BULLET03    = $0224

; ANIMATION
BULLETNOFL  = %00000001
BULLETFLX   = %01000001
BULLETFLY   = %10000001
BULLETFLXY  = %11000001
BULLSTATE   = %00000011
BULLFRAME0  = $06
BULLFRAME1  = $07
BULLFRAME2  = $08
BULLFRAME3  = $09
