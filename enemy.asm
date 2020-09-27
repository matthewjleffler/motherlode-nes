; enemy.asm
;   enemy logic

; CONSTANTS

; Masks
TICKMASK          = %00000111 ; Mask for updating enemy logic
ENEMYANIMMASK     = %00001111 ; Mask for updating animation

; Gameplay
SKEL_DAMAGE_RAD   = 8         ; 8 pixel rad for skeleton damage

; Timing
SPAWN_BLOCK_TICKS = 3         ; Blocked spawn retry ticks
EN_TIME_SPAWN1    = 12        ; Ticks to stay in spawn1
EN_TIME_SPAWN2    = 2         ; " " spawn2
EN_TIME_DIE1      = 6         ; " " die1
EN_TIME_PATH      = 20        ; How many ticks before we run atan to pathfind
EN_TIME_HEAD      = 20         ; How many ticks in between shooting?

; States
EN_STATE_OFF      = 0         ; Off
EN_STATE_SPAWN1   = 1         ; Spawn1
EN_STATE_SPAWN2   = 2         ; Spawn2
EN_STATE_DIE1     = 3         ; Die1
EN_STATE_SKEL     = 10        ; Skeleton
EN_STATE_HEAD     = 11        ; Head

; Health values
EN_SKEL_HEALTH    = 10
EN_HEAD_HEALTH    = 25

; Attributes
ENEMYATT_L        = %00000010
ENEMYATT_R        = %01000010

; Tiles
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
ENEMY_HEAD10      = $14
ENEMY_HEAD11      = $15
ENEMY_HEAD20      = $16
ENEMY_HEAD21      = $17

; SUBROUTINES

; A will be state, 0 is alive, 1 is dead
TestEnemyHealth:
  LDX enemyCount
  LDA enemyHealth, X
  BEQ .die
  LDA #0
  RTS
.die:
  LDA #EN_STATE_DIE1
  JSR SetEnemyState
  LDA #1
  RTS

TestSpawnEnemies:
  LDA animTick
  AND #TICKMASK               ; Count every 8 ticks
  BEQ .countTick              ; Are we on 0?
  RTS                         ; No
.countTick:
  DEC enemySpawnTimer
  LDA enemySpawnTimer
  BEQ .spawnEnemy             ; Timer ran out, spawn enemy
  RTS
.spawnEnemy:
  JSR RNG                     ; Randomly pick a new spawn time
  LSR A                       ; Divide rng by 16
  LSR A
  LSR A
  LSR A
  CMP #SPAWN_MIN_TICKS        ; Are we at least min time?
  BCC .setMin
  JMP .storeSpawnTime
.setMin:
  LDA #SPAWN_MIN_TICKS
.storeSpawnTime:
  STA enemySpawnTimer         ; Store spawn time
  JSR RNG                     ; Rng to find a spawn slot
  LSR A                       ; LSR 5 times to get 1-8
  LSR A
  LSR A
  LSR A
  LSR A
  CMP #ENEMYCOUNT             ; Are we within the enemy count?
  BCC .checkEnemyState        ; Yes - check whether this spot is free
  LDA #SPAWN_BLOCK_TICKS      ; No, set ticks to blocked amount and try again
  STA enemySpawnTimer
  RTS
.checkEnemyState:
  TAX                         ; Put index in X
  LDA enemyState, X           ; Load state for that enemy
  BEQ .doSpawnEnemy           ; Is it ready to spawn? (in off state)
  LDA #SPAWN_BLOCK_TICKS      ; No - blocked spawn at this slot
  STA enemySpawnTimer
  RTS
.doSpawnEnemy:
  STX enemyCount              ; Store this index in enemycount for later calls
  LDA #0                      ; Clear subpixels
  LDY positionOffset, X
  STA enemyPosX, Y
  STA enemyPosY, Y
  LDA enemySpawnX, X          ; Set position X to spawn X
  STA enemyPosX+1, Y
  LDA enemySpawnY, X          ; Set position Y to spawn Y
  STA enemyPosY+1, Y
  LDA #EN_STATE_SPAWN1        ; Flag enemy as spawning
  JSR SetEnemyState
  RTS

; A should be state to assign ; TODO state variable instead?
SetEnemyState:
  LDX enemyCount              ; Put index in X
  STA enemyState, X           ; Store state
  CMP #EN_STATE_OFF           ; Are we turned off?
  BEQ .stateTurnOff
  CMP #EN_STATE_SPAWN1        ; Are we spawning?
  BEQ .stateSpawn1
  CMP #EN_STATE_SPAWN2
  BEQ .stateSpawn2
  CMP #EN_STATE_DIE1
  BEQ .stateDie1
  CMP #EN_STATE_SKEL
  BEQ .stateSkel
  CMP #EN_STATE_HEAD
  BEQ .stateHead
  RTS                         ; No state matched
.stateTurnOff:
  JMP SetEnemyStateOff
.stateSpawn1:
  JMP SetEnemyStateSpawn1
.stateSpawn2:
  JMP SetEnemyStateSpawn2
.stateDie1:
  JMP SetEnemyStateDie1
.stateSkel:
  JMP SetEnemyStateSkel
.stateHead:
  JMP SetEnemyStateHead

SetEnemyStateOff:
  JSR SetPointerForEnemy
  LDA #2
  STA tilesH
  JSR HideSpriteLayout
  RTS

; Expects mirrored L/R
;
; Args:
;  spriteT                    - Top sprite
;  spriteB                    - Bottom sprite
SetEnemySprites:
  LDA spriteT
  STA spriteFrame+0           ; TL/TR mirrored
  STA spriteFrame+1
  LDA spriteB
  STA spriteFrame+2
  STA spriteFrame+3
  LDA #ENEMYATT_L             ; Enemy, No mirror
  STA spriteAttr+0
  STA spriteAttr+2
  LDA #ENEMYATT_R             ; Enemy, mirror
  STA spriteAttr+1
  STA spriteAttr+3
  JSR SetPointerForEnemy
  JSR ApplySpriteSettings
  RTS

SetEnemyStateSpawn1:
  LDA #EN_TIME_SPAWN1
  STA enemyTick, X
  LDA #ENEMY_SPAWN10
  STA spriteT
  LDA #ENEMY_SPAWN11
  STA spriteB
  JSR SetEnemySprites
  RTS

SetEnemyStateSpawn2:
  LDA #EN_TIME_SPAWN2
  STA enemyTick, X
  LDA #ENEMY_SPAWN20
  STA spriteT
  LDA #ENEMY_SPAWN21
  STA spriteB
  JSR SetEnemySprites
  RTS

SetEnemyStateDie1:
  LDA #EN_TIME_DIE1
  STA enemyTick, X
  LDA #ENEMY_DIE10
  STA spriteT
  LDA #ENEMY_DIE11
  STA spriteB
  JSR SetEnemySprites
  RTS

SetEnemyStateSkel:
  LDA #1
  STA enemyTick, X            ; Set up path search tick to run first frame
  LDA #EN_SKEL_HEALTH
  STA enemyHealth, X
  JSR DrawSkeletonFrame1
  RTS

SetEnemyStateHead:
  LDA #$FF
  STA enemyVel, X
  LDA #EN_TIME_HEAD
  STA enemyTick, X
  LDA #EN_HEAD_HEALTH
  STA enemyHealth, X
  JSR DrawHeadFrame1
  RTS

DrawSkeletonFrame1:
  LDA #ENEMY_SKEL10
  STA spriteT
  LDA #ENEMY_SKEL11
  STA spriteB
  JSR SetEnemySprites
  RTS

DrawSkeletonFrame2:
  LDA #ENEMY_SKEL20
  STA spriteT
  LDA #ENEMY_SKEL21
  STA spriteB
  JSR SetEnemySprites
  RTS

DrawHeadFrame1:
  LDA #ENEMY_HEAD10
  STA spriteT
  LDA #ENEMY_HEAD11
  STA spriteB
  JSR SetEnemySprites
  RTS

DrawHeadFrame2:
  LDA #ENEMY_HEAD20
  STA spriteT
  LDA #ENEMY_HEAD21
  STA spriteB
  JSR SetEnemySprites
  RTS

MoveEnemy:
  LDX enemyCount              ; Get enemy index
  LDY positionOffset, X       ; Offset position
  LDA enemyPosX+1, Y          ; Store last positions
  STA spriteLastPosX
  LDA enemyPosY+1, Y
  STA spriteLastPosY
; Start Y movement
  LDY enemyVel, X             ; Get enemy velocity index
  CPY #$FF                    ; Do we have the non-moving sentinel set?
  BNE .applyY                 ; No, move
  RTS                         ; Yes, don't move
.applyY:
  LDA enemySlowMoveY, Y       ; Get actual velocity for index
  STA velLo                   ; Velocity Lo
  LDA enemySlowMoveY+16, Y    ; 16 directions
  STA velHi                   ; Velocity Hi
  JSR StoreVelocitySign       ; Store the sign, for subpixel move and dodging
; Do Y movement
  LDA #HIGH(enemyPosY)        ; Set pointerSub for player Y
  STA pointerSubHi
  LDA #LOW(enemyPosY)
  CLC
  ADC positionOffset, X       ; Offset to correct enemy
  STA pointerSubLo
  JSR SubPixelMove            ; Do movement
; Do Y collision
  LDY positionOffset, X
  LDA enemyPosY+1, Y          ; Load new enemy Y pos
  SEC
  SBC #TILE_WIDTH             ; Offset up to top edge
  STA posY                    ; Store the value in the Y arg
  LDA velSign                 ; Check sign from earlier
  BNE .runCollisionY          ; Moving in negative, check top edge
  LDA posY                    ; Collision test on bottom, move Y to test down
  CLC                         ;   by 3 tiles, the height of the enemy
  ADC #TILES_PX_2
  STA posY                    ; Store the change
.runCollisionY:
  LDA enemyPosX+1, Y          ; Load enemy X pos
  SEC
  SBC #TILE_WIDTH             ; Offset to left edge
  CLC
  ADC #TILE_HALF              ; Offset it slightly to squeeze through gaps
  STA posX                    ; Store collision X
  LDA #1
  STA tilesW                  ; Testing 1 tile width, left and right edge
  LDA #0
  STA tilesH                  ; Testing only a single row H
  JSR TestWorldCollision
  BEQ .applyX                 ; No collision, we're good
  LDA spriteLastPosY          ; We got a collsiion, reset Y
  LDX enemyCount
  LDY positionOffset, X
  STA enemyPosY+1, Y
.applyX:
  LDX enemyCount              ; Make sure enemy index is in X
  LDY enemyVel, X             ; Get enemy velocity index
  LDA enemySlowMoveX, Y       ; Subpixel
  STA velLo                   ; Velocity Lo
  LDA enemySlowMoveX+16, Y    ; 16 directions
  STA velHi                   ; Velocity Hi
  JSR StoreVelocitySign       ; Store the sign, for subpixel move and dodging
; Do X movement
  LDA #HIGH(enemyPosX)        ; Set pointerSub for player subpixel X
  STA pointerSubHi
  LDA #LOW(enemyPosX)
  CLC
  ADC positionOffset, X
  STA pointerSubLo
  JSR SubPixelMove            ; Do move
; Do X collision
  LDY positionOffset, X       ; Load up position we moved to
  LDA enemyPosX+1, Y
  SEC
  SBC #TILE_WIDTH             ; Offset back to left edge
  STA posX                    ; Store the value in the X arg
  LDA velSign
  BNE .runCollisionX          ; Moving in negative, check left edge
  LDA posX
  CLC                         ; Collision test on right, move X to test right by
  ADC #TILES_PX_2             ; 2 tiles, the width of the enemy
  STA posX
.runCollisionX:
  LDA enemyPosY+1, Y          ; Load enemy Y position
  SEC
  SBC #TILE_WIDTH             ; Offset to top edge
  STA posY                    ; Store y position
  LDA #0
  STA tilesW                  ; Single line X
  LDA #2
  STA tilesH                  ; 2 tiles high
  JSR TestWorldCollision
  BEQ .done                   ; No collision, we're good
  LDX enemyCount
  LDY positionOffset, X
  LDA spriteLastPosX          ; We got a collision, set X back
  STA enemyPosX+1, Y
.done:
  RTS

UpdateEnemies:
  LDA #SPRITEHI               ; Set hi sprite pointer for later
  STA pointerHi
  LDX #0                      ; Get X ready
  STX enemyCount
  LDA animTick                ; Check global anim tick
  AND #ENEMYANIMMASK          ; Is it the right tick?
  BNE UpdateEnemyLoop         ; No, just update
  INC enemyAnim               ; Yes, update anim
UpdateEnemyLoop:
  LDX enemyCount              ; Get count from enemyCount
  LDA animTick                ; Check tick again
  AND #STATEMASK
  BEQ .checkState             ; Not the right frame, skip count
  DEC enemyTick, X            ; Decrement this enemy's tick
.checkState:
  LDA enemyState, X           ; Check state
  CMP #EN_STATE_OFF
  BEQ .checkStateOff
  CMP #EN_STATE_SPAWN1
  BEQ .checkStateSpawn1
  CMP #EN_STATE_SPAWN2
  BEQ .checkStateSpawn2
  CMP #EN_STATE_DIE1
  BEQ .checkStateDie1
  CMP #EN_STATE_SKEL
  BEQ .checkStateSkeleton
  CMP #EN_STATE_HEAD
  BEQ .checkStateHead
.checkStateOff:
  JMP IncrementEnemyCount     ; Not on, skip enemy
.checkStateSpawn1:
  JMP .stateSpawn1
.checkStateSpawn2:
  JMP .stateSpawn2
.checkStateDie1:
  JMP .stateDie1
.checkStateSkeleton:
  JMP .stateSkeleton
.checkStateHead:
  JMP .stateHead

; Actual state implementation
.stateSpawn1:
  LDA enemyTick, X            ; Did our timer run out?
  BEQ .stateSpawn1Done        ; Yes
  JMP .updateLayout           ; No, just draw
.stateSpawn1Done:
  LDA #EN_STATE_SPAWN2
  JSR SetEnemyState
  JMP .updateLayout

.stateSpawn2:
  LDA enemyTick, X            ; Did our timer run out?
  BEQ .stateSpawn2Done        ; Yes
  JMP .updateLayout           ; No, just draw
.stateSpawn2Done:
  LDA #EN_STATE_SKEL          ; TODO pick enemy randomly
  JSR SetEnemyState
  JMP .updateLayout

.stateDie1:
  LDA enemyTick, X            ; Did our timer run out?
  BEQ .stateDie1Done          ; Yes
  JMP .updateLayout           ; No, just draw
.stateDie1Done:
  LDA #EN_STATE_OFF
  JSR SetEnemyState
  JMP IncrementEnemyCount

.stateSkeleton:
  LDA enemyAnim               ; Load enemy anim
  CLC
  ADC enemyCount              ; Offset by anim
  LSR A                       ; Divide by 2
  AND #FRAME_MASK
  BEQ .skelFrame1
  JSR DrawSkeletonFrame2
  JMP .skelUpdate
.skelFrame1:
  JSR DrawSkeletonFrame1
.skelUpdate:
  JSR TestEnemyHealth
  BEQ .skelSetupPos           ; Still alive
  LDX #1                      ; Dead, add 10 points
  JSR AddScore
  JMP .updateLayout
.skelSetupPos:
  LDY positionOffset, X
  LDA enemyPosX+1, Y          ; Store X pos for atan2
  STA posX
  LDA enemyPosY+1, Y          ; Store Y pos for atan2
  STA posY
  JSR StorePlayerPosForSearch
  JSR ManhattanDistance       ; Test if we've hit player
  CMP #SKEL_DAMAGE_RAD
  BCC .hitPlayer
; Didn't hit player, check for velocity change
  LDA enemyTick, X            ; Did our pathfind tick run out?
  BNE .skelMove               ; No, move with our last velocity
  LDA #EN_TIME_PATH
  STA enemyTick, X            ; Reset pathfind tick
  JSR Atan216                 ; Atan2 16 degrees
  LDX enemyCount              ; Reset X for enemy
  STA enemyVel, X             ; Store velocity
.skelMove:
  JSR MoveEnemy
  JMP .updateLayout
.hitPlayer:
  JSR PlayerTakeDamage        ; Deal damage to player
  LDX enemyCount              ; Kill this skeleton
  LDA #0
  STA enemyHealth, X
  JSR TestEnemyHealth         ; Don't add score
  JMP .updateLayout

.stateHead:
  LDA enemyAnim               ; Load enemy anim
  CLC
  ADC enemyCount              ; Offset by anim
  LSR A                       ; Divide by 2
  AND #FRAME_MASK
  BEQ .headFrame1
  JSR DrawHeadFrame2
  JMP .headUpdate
.headFrame1:
  JSR DrawHeadFrame1
.headUpdate:
  JSR TestEnemyHealth
  BEQ .headShoot
  LDX #2
  JSR AddScore
  JMP .updateLayout
.headShoot:
  LDA enemyTick, X            ; Did our shooting tick run out?
  BNE .headMove               ; No - just move
  LDA #EN_TIME_HEAD           ; Yes, reset the timer
  STA enemyTick, X
  LDY positionOffset, X
  LDA enemyPosX+1, Y          ; Set up position X for spawning bullet
  STA posX
  LDA enemyPosY+1, Y          ; Set up position Y for spawning bullet
  STA posY
  JSR SpawnEnemyBullet        ; Try to spawn bullet
  LDX enemyCount              ; Return enemyCount to X
  JSR RNG
  JSR Divide4
  BNE .still
  JSR RNG                     ; Pick a random move direction
  JSR Divide16
  STA enemyVel, X             ; Set the velocity
  JMP .headMove
.still:
  LDA #$FF                    ; Not moving
  STA enemyVel, X
.headMove:
  JSR MoveEnemy
  JMP .updateLayout

.updateLayout:
  JSR SetPointerForEnemy
  LDY positionOffset, X       ; Get position index offset for current enemy
  LDA enemyPosX+1, Y          ; Load enemy X position
  SEC
  SBC #TILE_WIDTH             ; Left edge is 1 tile left
  STA posX                    ; Store X position
  LDA enemyPosY+1, Y          ; Load enemy Y position
  SEC
  SBC #TILE_WIDTH             ; Top edge is 1 tile up
  STA posY                    ; Store Y position
  LDA #2                      ; Enemies are 2 tiles high
  STA tilesH
  JSR UpdateSpriteLayout
IncrementEnemyCount:
  INC enemyCount
  LDA enemyCount
  CMP #ENEMYCOUNT
  BNE .continue
  RTS
.continue:
  JMP UpdateEnemyLoop
