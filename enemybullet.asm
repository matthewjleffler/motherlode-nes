; enemybullet.asm
;   Enemy bullet logic

; CONSTANTS

STATEMASK         = %00000011 ; Low bits for the state of a bullet
HICLEAR           = %00111111 ; Clear the high state off
EBULLETCOUNT      = 8         ; Number of bullets to render
EBULLET_ATTRIB    = %00000011 ; Bullet attribute
EBULLET_RAD       = 6         ; Distance player can be hit at

; States
BULL_OFF          = 0
BULL_MOV          = 1

; Tiles
ENEMY_BULLET0     = $18
ENEMY_BULLET1     = $19

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

; Bullet is dead, hide it, then set state to off
HideEnemyBullet:
  LDX bulletCount
  LDA #BULL_OFF
  STA state
  JSR SetEnemyBulletState
  LDA #SPRITEHI
  STA pointerHi
  LDA #EBULLET0
  CLC
  ADC spriteOffset1, X
  STA pointerLo
  LDY #0
  LDA #$FF                    ; Store offscreen in sprite Y
  STA [pointerLo], Y
  RTS

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
  ; Move pixel Y
  LDX bulletCount             ; Store current bullet in X
  LDY enemyBulletVel, X       ; Store enemy velocity index in Y
  LDA enemyBulletMoveY, Y     ; Velocity lo
  STA velLo
  LDA enemyBulletMoveY+32, Y  ; Velocity hi
  STA velHi
  JSR StoreVelocitySign       ; Set up pointer for bullet Y pos
  LDA #HIGH(enemyBulletPosY)
  STA pointerSubHi
  LDA #LOW(enemyBulletPosY)
  CLC
  ADC positionOffset, X       ; Move the pointer forward  to the right index
  STA pointerSubLo
  JSR SubPixelMove            ; Move the Y position
  ; Move pixel X
  LDY enemyBulletVel, X       ; Load bullet velocity index in Y
  LDA enemyBulletMoveX, Y     ; Velocity Lo
  STA velLo
  LDA enemyBulletMoveX+32, Y  ; Velocity hi
  STA velHi
  JSR StoreVelocitySign
  LDA #HIGH(enemyBulletPosX)  ; Set up pointer for bullet x pos
  STA pointerSubHi
  LDA #LOW(enemyBulletPosX)
  CLC
  ADC positionOffset, X       ; Move the pointer forward to the right spot
  STA pointerSubLo
  JSR SubPixelMove            ; Move the X position
  LDX bulletCount
  LDY positionOffset, X
  LDA enemyBulletPosX+1, Y
  STA posX
  LDA enemyBulletPosY+1, Y
  STA posY
  JSR StorePlayerPosForSearch ; Store player pos for atan2, posX and posY are set up
  JSR ManhattanDistance
  CMP #EBULLET_RAD
  BCC .hitPlayer
; World Collision
  LDA #0
  STA tilesW
  STA tilesH
  JSR TestWorldCollision
  CMP #1                      ; We've collided
  BEQ .collision
  JMP .animateBullet
.hitPlayer:
  JSR PlayerTakeDamage
  LDA playerHealth
  BEQ .animateBullet          ; Don't delete bullet if it killed player
.collision:
  JSR HideEnemyBullet
  RTS
.animateBullet:
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
  LDA #EBULLET_ATTRIB
  LDY #SPRITEATT
  STA [pointerLo], Y
; Animate Bullet
  LDA enemyAnim               ; Load enemy anim
  CLC
  ADC bulletCount             ; Offset by anim
  AND #FRAME_MASK
  BEQ .frame2
  LDA #ENEMY_BULLET0
  JMP .drawFrame
.frame2:
  LDA #ENEMY_BULLET1
.drawFrame:
  LDY #SPRITETIL
  STA [pointerLo], Y          ; Save the sprite position
  RTS
