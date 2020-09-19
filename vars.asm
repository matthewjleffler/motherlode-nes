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
arg8                    .rs 1 ; " "
arg9                    .rs 1 ; " "
pointerLo               .rs 1 ; pointer variables are declared in RAM
pointerHi               .rs 1 ; low byte first, high byte immediately after
pointerSubLo            .rs 1 ; pointer to subpixel
pointerSubHi            .rs 1 ; pointer to high subpixel (0?)
pointerColLo            .rs 1 ; pointer to lo collision map
pointerColHi            .rs 1 ; pointer to hi collision map
state                   .rs 1 ; state, for bullets or enemies
buttons1                .rs 1 ; controller 1 buttons
buttons2                .rs 1 ; controller 2 buttons
buttons1fresh           .rs 1 ; controller 1 buttons fresh
buttons2fresh           .rs 1 ; controller 2 buttons fresh
animTick                .rs 1 ; Slows down animation counting
seed                    .rs 2 ; Stores prng seed
bulletAnim              .rs 1 ; Bullet anim state
spriteLayoutOriginY     .rs 1 ; Y of sprite origin
spriteLayoutOriginX     .rs 1 ; X of sprite origin
spriteLastPosY          .rs 1 ; Y of sprite last frame
spriteLastPosX          .rs 1 ; X of sprite last frame
bulletCount             .rs 1 ; Total number of bullets to render
enemyCount              .rs 1 ; Total number of enemies alive now
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
playerDodge             .rs 1 ; hi bit is on/off, lo is cooldown

; TODO move arrays to another page?
backgroundBuffer  = $0100

  .rsset $0400                ; Arrays in upper register here

playerPosX              .rs 2 ; Lo sub, hi pixel
playerPosY              .rs 2 ; Lo sub, hi pixel
bulletFrame             .rs 4 ; The frames to apply to the current bullet
bulletAttr              .rs 4 ; The attributes to apply to the current bullet
playerBulletVel         .rs 4 ; 4 (indexes)
playerBulletPosX        .rs 2 * 4 ; Lo sub, hi pixel * 4
playerBulletPosY        .rs 2 * 4 ; Lo sub, hi pixel * 4
enemyPosX               .rs 2 * 6 ; Lo sub, hi pixel * 6
enemyPosY               .rs 2 * 6 ; Lo sub, hi pixel * 6

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Constants

; Rendering
SPRITEHI          = $02       ; High byte of sprite address is always the same
TILE_HALF         = 4
TILE_WIDTH        = 8
SPRITETIL         = $01
SPRITEATT         = $02
SPRITEX           = $03
TILES_PX_3        = 3 * TILE_WIDTH ; 3 tiles high in pixels
TILES_PX_2        = 2 * TILE_WIDTH ; 2 tiles high in pixels
BG_HI             = $20       ; hi pointer to background table addresses
STATUS_LO         = $60       ; lo pointer index of status bar bg row

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
MOVE_INPUT        = %00001111
MASK_UD           = %00001100
MASK_LR           = %00000011
REMOVE_UD         = %11110011
REMOVE_LR         = %11111100

; Gameplay
COLLISIONMASK     = %10000000 ; Mask for collision bit
STATEMASK         = %00000011 ; Mask for lower two bits
BULLETSHOOTMASK   = %00000111 ; Mask for shooting bullet tick
HICLEAR           = %00111111 ; Mask to clear high bits
BULLETCOUNT       = 4         ; Number of bullets to render
ENEMYCOUNT        = 6         ; Max number of enemies
PLAYER_BULLET_RAD = 10        ; Distance player bullets can hit at
PLAYER_SPAWN_X    = $80
PLAYER_SPAWN_Y    = $80

; Move Speed
NEG_SIGN          = %10000000 ; Indicates negative movement
MOV_MASK          = %01111111 ; Non-sign movement
DODGE_ON          = %10000000 ; Whether or not the dodge bit is set
DODGE_TIME_MASK   = %01111111 ; The time bits of dodge
DODGE_TIME        = 7         ;  7/60 of a second
DODGE_COOLDOWN    = 25        ; 25/60  of a second

; Sprite lo addresses         ;                         n * s = t
EBULLET0          = $00       ; Size: 4 * 8     =  32   8 * 1 = 8
PBULLET0          = $20       ; Size: 4 * 4 * 4 =  64   4 * 4 = 16
PLAYER            = $60       ; Size: 4 * 6     =  24   6 * 1 = 6
ITEM              = $78       ; Size: 4 * 4     =   8   1 * 4 = 4
ENEMY0            = $80       ; Size: 4 * 4 * 6 =  96   4 * 6 = 24
                              ;                 = 232         = 58 / 64

PLAYERSIZE        = 6 * 4     ; player byte size
ENEMYSIZE         = 4 * 4     ; enemy byte size

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Argument constants

; Positional
posX              = arg0
posY              = arg1
posX2             = arg2
posY2             = arg3

; Atan2 / Math
octant            = arg4
angle             = arg4 ; Reuse arg4 after finishing with octant
sum               = arg4
distance          = arg9

; Tile sizes
tilesW            = arg2
tilesH            = arg3
tilesX            = arg4
tilesWOriginal    = arg5

; Player movement
playerMoveDir     = arg9

; Subpixel movement
velLo             = arg0
velHi             = arg1
velSign           = arg2

; Background buffer upadte
len               = arg0
startX            = arg1
