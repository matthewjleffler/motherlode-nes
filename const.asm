; Const.asm
;   global constants

; CONSTANTS

; Rendering
SPRITEHI          = $02       ; High byte of sprite address is always the same
TILE_HALF         = 4
TILE_WIDTH        = 8
TILES_PX_3        = 3 * TILE_WIDTH ; 3 tiles high in pixels
TILES_PX_2        = 2 * TILE_WIDTH ; 2 tiles high in pixels

; Input
BUTTONA           = %10000000
BUTTONB           = %01000000
BUTTONSEL         = %00100000
BUTTONSTA         = %00010000
BUTTONU           = %00001000
BUTTOND           = %00000100
BUTTONL           = %00000010
BUTTONR           = %00000001

; Gameplay
STATEMASK         = %00000011 ; Mask for lower two bits
ENEMYCOUNT        = 6         ; Max number of enemies
SPAWN_MIN_TICKS   = 15        ; Min spawn ticks between spawns

; Sprite addresses
SPRITETIL         = $01       ; Sprite tile byte offset
SPRITEATT         = $02       ; Sprite attribute byte offset
SPRITEX           = $03       ; Sprite X byte offset

; Sprite lo addresses         ;                         n * s = t
EBULLET0          = $00       ; Size: 4 * 8     =  32   8 * 1 = 8
PBULLET0          = $20       ; Size: 4 * 4 * 4 =  64   4 * 4 = 16
PLAYER            = $60       ; Size: 4 * 6     =  24   6 * 1 = 6
ITEM              = $78       ; Size: 4 * 4     =   8   1 * 4 = 4
ENEMY0            = $80       ; Size: 4 * 4 * 6 =  96   4 * 6 = 24
                              ;                 = 232         = 58 / 64

; Game state
GAME_TITLE        = 0
GAME_RUN          = 1
GAME_KILL         = 2
GAME_PAUSE        = 3

; ARGUMENTS

; Positional
posX              = arg0
posY              = arg1
posX2             = arg2
posY2             = arg3

; Tile sizes
tilesW            = arg2
tilesH            = arg3
tilesX            = arg4
tilesWOriginal    = arg5

; Subpixel movement
velLo             = arg0
velHi             = arg1
velSign           = arg2

; Background buffer upadte
len               = arg0
startX            = arg1
startY            = arg2
oldX              = arg3
bgLo              = arg4
bgHi              = arg5

; Sprite assignment
spriteT           = arg0
spriteB           = arg1
