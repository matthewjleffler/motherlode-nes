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
debug                   .rs 1 ; for debug rendering - when set, draws in score
scoreChanged            .rs 1 ; Whether or not score changed this frame
bufferUpdateIndex       .rs 1 ; the current buffer offset for this update
buttons1                .rs 1 ; controller 1 buttons
buttons2                .rs 1 ; controller 2 buttons
buttons1fresh           .rs 1 ; controller 1 buttons fresh
buttons2fresh           .rs 1 ; controller 2 buttons fresh
animTick                .rs 1 ; Slows down animation counting
seed                    .rs 2 ; Stores prng seed
bulletAnim              .rs 1 ; Bullet anim state
enemyAnim               .rs 1 ; Enemy anim state
spriteLastPosY          .rs 1 ; Y of sprite last frame
spriteLastPosX          .rs 1 ; X of sprite last frame
bulletCount             .rs 1 ; Total number of bullets to render
enemyCount              .rs 1 ; Total number of enemies to update
enemySpawnTimer         .rs 1 ; How many spawn ticks remain before trying to spawn
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

  .rsset $0100                ; Background update buffer here
backgroundBuffer        .rs 160

  .rsset $0400                ; Arrays in upper register here
score                   .rs 8 ; 8 places for score counting
playerPosX              .rs 2 ; Lo sub, hi pixel
playerPosY              .rs 2 ; Lo sub, hi pixel
spriteFrame             .rs 6 ; The frames to apply to the current sprite
spriteAttr              .rs 6 ; The attributes to apply to the current bullet
playerBulletVel         .rs 4 ; 4 (indexes)
playerBulletPosX        .rs 2 * 4 ; Lo sub, hi pixel * 4
playerBulletPosY        .rs 2 * 4 ; Lo sub, hi pixel * 4
enemyState              .rs 6 ; 6, one byte per enemy
enemyTick               .rs 6 ; 6, one byte per enemy
enemyHealth             .rs 6 ; 6, one byte per enemy
enemyPosX               .rs 2 * 6 ; Lo sub, hi pixel * 6
enemyPosY               .rs 2 * 6 ; Lo sub, hi pixel * 6
enemyVel                .rs 6 ; 6 (indexes)

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
DEBUG_TILE        = 19
SCORE_TILE        = 23
LOW_MASK          = %00001111

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
ENEMYANIMMASK     = %00001111 ; Mask for animating enemies
HICLEAR           = %00111111 ; Mask to clear high bits
BULLETCOUNT       = 4         ; Number of bullets to render
ENEMYCOUNT        = 6         ; Max number of enemies
SCOREPLACES       = 8         ; Score places
PLAYER_BULLET_RAD = 12        ; Distance player bullets can hit at
PLAYER_SPAWN_X    = $80
PLAYER_SPAWN_Y    = $80

; Enemies
ENEMY_ALIVE_STATE = 10        ; Min enemy state alive
SPAWN_MIN_TICKS   = 15        ; Min spawn ticks between spawns
SPAWN_BLOCK_TICKS = 3         ; Blocked spawn retry ticks
EN_STATE_OFF      = 0         ; Off
EN_STATE_SPAWN1   = 1         ; Spawn1
EN_STATE_SPAWN2   = 2         ; Spawn2
EN_STATE_DIE1     = 3         ; Die1
EN_STATE_SKEL     = 10        ; Skeleton

EN_TIME_ANIM      = 1         ; Default anim time
EN_TIME_SPAWN1    = 12        ; Ticks to stay in spawn1
EN_TIME_SPAWN2    = 2         ; " " spawn2
EN_TIME_DIE1      = 6         ; " " die1
EN_TIME_PATH      = 20        ; How many ticks before we run atan to pathfind

EN_SKEL_HEALTH    = 10

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
ANIM_MASK         = %00000001 ; Low bit to check animation
BULLETNOFL        = %00000001 ; Bullet attributes with flipping and color
BULLETFLX         = %01000001
BULLETFLY         = %10000001
BULLETFLXY        = %11000001
BULLFRAME0        = $06
BULLFRAME1        = $07
BULLFRAME2        = $08
BULLFRAME3        = $09
ENEMYATT_L        = %00000010
ENEMYATT_R        = %01000010
ENEMY_SPAWN10     = $0A
ENEMY_SPAWN11     = $0B
ENEMY_SPAWN20     = $0C
ENEMY_SPAWN21     = $0D
ENEMY_DIE10       = $0E
ENEMY_DIE11       = $0F
ENEMY_SKEL10      = $10
ENEMY_SKEL11      = $11
ENEMY_SKEL20      = $12
ENEMY_SKEL21      = $13

; Tiles
STATUS_BUTT_OFF   = $62
STATUS_BUTT_ON    = $63
STATUS_HEART_OFF  = $60
STATUS_HEART_ON   = $61

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
angle             = arg4      ; Reuse arg4 after finishing with octant
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
oldX              = arg2

; Sprite assignment
spriteT           = arg0
spriteB           = arg1
