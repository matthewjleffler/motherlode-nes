; Const.asm
;   global constants

; CONSTANTS

; Rendering
SPRITEHI          = $02       ; High byte of sprite address is always the same
TILE_HALF         = 4
TILE_WIDTH        = 8
TILES_PX_3        = 3 * TILE_WIDTH ; 3 tiles high in pixels
TILES_PX_2        = 2 * TILE_WIDTH ; 2 tiles high in pixels
BG_HI             = $20       ; hi pointer to background table addresses

; Gameplay
STATEMASK         = %00000011 ; Mask for lower two bits
ENEMYCOUNT        = 6         ; Max number of enemies
SPAWN_MIN_TICKS   = 15        ; Min spawn ticks between spawns

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
oldX              = arg2

; Sprite assignment
spriteT           = arg0
spriteB           = arg1
