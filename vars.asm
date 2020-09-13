;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Variables

  .rsset $0000                ; put pointers in zero page

arg0                    .rs 1 ; Reusable subroutine arg0
arg1                    .rs 1 ; " "
arg2                    .rs 1 ; " "
arg3                    .rs 1 ; " "
arg4                    .rs 1 ; " "
arg5                    .rs 1 ; " "
arg6                    .rs 1 ; " "
arg7                    .rs 1 ; " "
return                  .rs 1 ; Reusable return arg
pointerLo               .rs 1 ; pointer variables are declared in RAM
pointerHi               .rs 1 ; low byte first, high byte immediately after
pointerSub              .rs 1 ; pointer to subpixel
pointerSubHi            .rs 1 ; pointer to high subpixel (0?)
pointerColLo            .rs 1 ; pointer to lo collision map
pointerColHi            .rs 1 ; pointer to hi collision map
temp                    .rs 1 ; temp reusable byte
buttons1                .rs 1 ; controller 1 buttons
buttons2                .rs 1 ; controller 2 buttons
buttons1fresh           .rs 1 ; controller 1 buttons fresh
buttons2fresh           .rs 1 ; controller 2 buttons fresh
animTick                .rs 1 ; Slows down animation counting
bulletAnim              .rs 1 ; Bullet anim state
spriteLayoutOriginY     .rs 1 ; Y of sprite origin
spriteLayoutOriginX     .rs 1 ; X of sprite origin
spriteLastPosY          .rs 1 ; Y of sprite last frame
spriteLastPosX          .rs 1 ; X of sprite last frame
bulletFrame             .rs 4 ; The frames to apply to the current bullet
bulletAttr              .rs 4 ; The attributes to apply to the current bullet
bulletCount             .rs 1 ; Total number of bullets to render
playerBulletStates      .rs 1 ; On/off states for 4 player bullets
                              ; 00 - off
                              ; 01 - on
                              ; 10 - explosion?
                              ; 11 - unused ??
                              ; 44332211
enemyBulletStates       .rs 2 ; On/off states for 8 enemy bullets
                              ; 00 - off
                              ; 01 - on
                              ; 10 - ??
                              ; 11 - ??
                              ; 88776655 44332211
playerSub               .rs 2 ; Player subpixel, y+0, x+1
playerBulletSub         .rs 8 ; 4 * 2
playerBulletVel         .rs 4 ; 4 (indexes)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Constants

; Rendering
SPRITEHI          = $02       ; High byte of sprite address is always the same
TILE_HALF         = 4
TILE_WIDTH        = 8
SPRITETIL         = $01
SPRITEATT         = $02
SPRITEX           = $03

; Controllers
CONTROLHI         = $40
CONTROLLO         = $16
BUTTONA           = %10000000
BUTTONB           = %01000000
BUTTONSEL         = %00100000
BUTTONSTA         = %00010000
BUTTONU           = %00001000
BUTTOND           = %00000100
BUTTONL           = %00000010
BUTTONR           = %00000001

; Gameplay
COLLISIONMASK     = %10000000 ; Mask for collision bit
STATEMASK         = %00000011 ; Mask for lower two bits
BULLETSHOOTMASK   = %00000111 ; Mask for shooting bullet tick
HICLEAR           = %00111111 ; Mask to clear high bits
BULLETCOUNT       = 4         ; Number of bullets to render
BULLETEDGE        = 6         ; 1 pixel wider than movement speed
BULLETEDGEW       = $FF - BULLETEDGE - $10
                              ; Right hand, bottom, includes bullet width
PL_EDGE_LEFT      = 10        ; Player bounds
PL_EDGE_TOP       = 15
PL_EDGE_RIGHT     = $FF - PL_EDGE_LEFT - TILE_WIDTH * 2
PL_EDGE_BOTTOM    = $FF - PL_EDGE_TOP - TILE_WIDTH * 6

; Move Speed
NEG_SIGN          = %10000000 ; Indicates negative movement
MOV_MASK          = %01111111 ; Non-sign movement

; Sprite lo addresses         ;                         n * s = t
EBULLET0          = $00       ; Size: 4 * 8     =  32   8 * 1 = 8
PBULLET0          = $20       ; Size: 4 * 4 * 4 =  64   4 * 4 = 16
PLAYER            = $60       ; Size: 4 * 6     =  24   6 * 1 = 6
ITEM              = $78       ; Size: 4 * 4     =   8   1 * 4 = 4
ENEMY0            = $80       ; Size: 4 * 4 * 6 =  96   4 * 6 = 24
                              ;                 = 232         = 58 / 64

PLAYERSIZE        = 24        ; player byte size
ENEMYSIZE         = 16        ; enemy byte size

; Animation
BULLETNOFL        = %00000001 ; Bullet attributes with flipping and color
BULLETFLX         = %01000001
BULLETFLY         = %10000001
BULLETFLXY        = %11000001
BULLFRAME0        = $06
BULLFRAME1        = $07
BULLFRAME2        = $08
BULLFRAME3        = $09

; Bullet States
BULL_OFF          = $00
BULL_MOV          = $01
BULL_EXP          = $02
