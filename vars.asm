; vars.asm
;   variable definitions / reservations

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
nametable               .rs 1 ; Which table we're on
state                   .rs 1 ; state, for bullets or enemies
gamestate               .rs 1 ; gamestate, for controlling what screen we're on
debug                   .rs 1 ; for debug rendering
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
                              ; 10 - unused ??
                              ; 11 - unused ??
                              ; 44332211
enemyBulletStates       .rs 2 ; On/off states for 8 enemy bullets
                              ; 00 - off
                              ; 01 - on
                              ; 10 - ??
                              ; 11 - ??
                              ; 88776655 44332211
playerDodge             .rs 1 ; hi bit is on/off, lo is cooldown
playerAbilityTick       .rs 1 ; Tick for using player ability
playerAbility           .rs 1 ; Timer for using player ability
playerHealth            .rs 1 ; Player health
playerDamageCooldown    .rs 1 ; Ticks for player damage cooldown
playerDamageFlash       .rs 1 ; Timer for player damage flash
fadeTime                .rs 1 ; Timer for use during fades
fadeCount               .rs 1 ; Counter for use during fades
monochromeTime          .rs 1 ; Counter for flashing monochrome, used to block bullets
monochrome              .rs 1 ; Flag for monochrome

  .rsset $0100                ; Background update buffer here
backgroundBuffer        .rs 160

  .rsset $0400                ; Arrays in upper register here
palette                 .rs 32 ; 32 palette bytes
playerPalette           .rs 3 ; 3 player palette bytes
score                   .rs 8 ; 8 places for score counting
playerPosX              .rs 2 ; Lo sub, hi pixel
playerPosY              .rs 2 ; Lo sub, hi pixel
spriteFrame             .rs 6 ; The frames to apply to the current sprite
spriteAttr              .rs 6 ; The attributes to apply to the current bullet
playerBulletPosX        .rs 2 * 4 ; Lo sub, hi pixel * 4
playerBulletPosY        .rs 2 * 4 ; Lo sub, hi pixel * 4
playerBulletVel         .rs 4 ; 4 (indexes)
enemyState              .rs 6 ; 6, one byte per enemy
enemyTick               .rs 6 ; 6, one byte per enemy
enemyHealth             .rs 6 ; 6, one byte per enemy
enemyPosX               .rs 2 * 6 ; Lo sub, hi pixel * 6
enemyPosY               .rs 2 * 6 ; Lo sub, hi pixel * 6
enemyVel                .rs 6 ; 6 (indexes)
enemyBulletPosX         .rs 2 * 8; Lo sub, hi pixel * 8
enemyBulletPosY         .rs 2 * 8; Lo sub, hi pixel * 8
enemyBulletVel          .rs 8 ; 8 (indexes)
