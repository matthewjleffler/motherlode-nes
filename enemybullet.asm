; enemybullet.asm
;   Enemy bullet logic

; CONSTANTS

STATEMASK         = %00000011 ; Low bits for the state of a bullet
HICLEAR           = %00111111 ; Clear the high state off
EBULLETCOUNT      = 8         ; Number of bullets to render
EBULLET_ATTRIB    = %00000011 ; Bullet attribute

; States
BULL_OFF          = 0
BULL_MOV          = 1

; Tiles
ENEMY_BULLET0     = $18

; SUBROUTINES

; Gets the current bullet state, stores it in state
; And reassigns it to the leftmost bullet state
GetEnemyBulletState:
  LDA enemyBulletStates
  AND #STATEMASK
  STA state                   ; Store low bits of state
  LSR enemyBulletStates+1     ; Shift high state, low goes to carry
  ROR enemyBulletStates       ; Shift low right, carry into high
  LSR enemyBulletStates+1     ; Shift high state, low goes to carry
  ROR enemyBulletStates       ; Shift low right, carry into high
  JSR SetEnemyBulletState
  LDA state
  RTS

; Expects the new state to be stored in state, sets it to the
; high bits of enemyBulletStates. Can be called multiple times,
; preserves state
SetEnemyBulletState:
  LDA enemyBulletStates+1     ; Load high state
  AND #HICLEAR                ; Clear off high bits
  STA enemyBulletStates+1     ; Save cleared
  LDA state
  CLC
  ROR A                       ; %000000AB -> B
  ROR A                       ; %B000000A -> A
  ROR A                       ; %AB000000 -> 0
  ORA enemyBulletStates+1     ; Combine with high state
  STA enemyBulletStates+1     ; Save result
  RTS

; Spawns an enemy bullet
;
; Args:
; posX
; posY
;
; Trashes X and Y
SpawnEnemyBullet:
  ; Find a free bullet
  LDX #0                      ; Clear X loop
  STX bulletCount
  LDY #0                      ; Clear Y, it indicates we've shot
.loop:
  JSR GetEnemyBulletState     ; Get enemy state
  CMP #BULL_OFF               ; No - Are we off?
  BNE .incrementBullet        ; No - continue on to next bullet
  CPY #0                      ; Have we already shot?
  BNE .incrementBullet        ; Yes - just loop through bullets to put state back
; Spawn bullet
  LDY positionOffset, X       ; Get the position offset for this bullet
  LDA #0                      ; Clear subpixels
  STA enemyBulletPosX, Y
  STA enemyBulletPosY, Y
  LDA posX
  STA enemyBulletPosX+1, Y    ; Store position X assigned
  LDA posY
  STA enemyBulletPosY+1, Y    ; Store position Y assigned
  LDA #BULL_MOV
  STA state
  JSR SetEnemyBulletState     ; Set the current bullet state to MOV
  JSR StorePlayerPosForSearch ; Store player pos for atan2, posX and posY are set up
  JSR Atan232                 ; Get Atan2 in 32 degrees, store in velocity index
  LDX bulletCount
  STA enemyBulletVel, X       ; Store resulting velocity index for the current bullet
  LDY #1                      ; Mark that we've shot
.incrementBullet:
  INC bulletCount
  LDX bulletCount
  CPX #EBULLETCOUNT           ; Did we loop through all bullets?
  BNE .loop                   ; No - loop
  RTS                         ; Finished

UpdateEnemyBullets:
  LDX #0
  STX bulletCount             ; Clear X and bullet loop
.loop:
  JSR GetEnemyBulletState     ; Get bullet state
  CMP #BULL_OFF               ; Are we off? skip
  BEQ .incrementBullet
  CMP #BULL_MOV
  BEQ .doMoveBullet
  JMP .incrementBullet        ; Some unhandled state? Increment
.doMoveBullet:
  JSR .moveBullet
.incrementBullet:
  INC bulletCount
  LDX bulletCount
  CPX #EBULLETCOUNT           ; Did we loop through all bullets
  BNE .loop                   ; No - loop
  RTS                         ; Finished

.moveBullet:
  ; TODO actually move bullet
  ; TODO animate bullet
  LDX bulletCount             ; Make sure bullet index is in X
  LDA #SPRITEHI
  STA pointerHi
  LDA #EBULLET0               ; Get first enemy bullet pointer
  CLC
  ADC spriteOffset1, X        ; Offset to this sprite pointer
  STA pointerLo               ; Store the sprite address in pointerLo
  LDA positionOffset, X       ; Store position offset for index in X
  TAX
  LDA enemyBulletPosY+1, X    ; Load bullet pos Y
  SEC
  SBC #TILE_HALF              ; Offset by half tile, bullet pos is center
  LDY #0                      ; Clear Y, for sprite Y
  STA [pointerLo], Y          ; Store the position
  LDA enemyBulletPosX+1, X    ; Load bullet pos X
  SEC
  SBC #TILE_HALF              ; Offset by half tile, bullet pos is center
  LDY #SPRITEX
  STA [pointerLo], Y          ; Store the position
  LDA #ENEMY_BULLET0          ; TODO animate, TODO set sprite
  LDY #SPRITETIL
  STA [pointerLo], Y          ; Save the sprite position
  LDA #EBULLET_ATTRIB
  LDY #SPRITEATT
  STA [pointerLo], Y
  RTS
