; playerbullet.asm
;   handles player bullet code

; CONSTANTS

HICLEAR           = %00111111 ; Mask to clear high bits
PBULLETCOUNT      = 4         ; Number of bullets to render
PLAYER_BULLET_RAD = 12        ; Distance player bullets can hit at

; Attributes
BULLETNOFL        = %00000001 ; Bullet attributes with flipping and color
BULLETFLX         = %01000001
BULLETFLY         = %10000001
BULLETFLXY        = %11000001
BULLFRAME0        = $06
BULLFRAME1        = $07
BULLFRAME2        = $08
BULLFRAME3        = $09

; States
BULL_OFF          = $00
BULL_MOV          = $01

; SUBROUTINES

; Gets current bullet state, and shifts bullet info over
; Stores resulting state in state
GetPlayerBulletState:
  LDA playerBulletStates      ; Get current state in A
  AND #STATEMASK
  STA state                   ; Store result in temp
  LSR playerBulletStates      ; Move state off playerBulletStates
  LSR playerBulletStates      ; Last two bits are 0
  JSR SetPlayerBulletState    ; Store current state back, in case of no change
  LDA state
  RTS

; Sets current bullet state in high bits of playerBulletStates
; Expects the new state to already be in the low bits of state
; Can be assigned multiple times, state is preserved between calls
SetPlayerBulletState:
  LDA playerBulletStates      ; Make sure high bits are clear
  AND #HICLEAR
  STA playerBulletStates
  LDA state
  CLC
  ROR A                       ; 000000AB -> B
  ROR A                       ; B000000A -> A
  ROR A                       ; AB000000 -> 0
  ORA playerBulletStates      ; Inclusive OR with playerBulletStates,
                              ; high bits should be empty and ready for this
  STA playerBulletStates      ; Store result
  RTS

TestPlayerShootBullet:
  LDA #BULLETSHOOTMASK
  AND animTick                ; Every 8 frames
  BEQ .testEnemyNear
  RTS                         ; Not the right tick
.testEnemyNear:
  LDA playerPosY+1            ; Load player Y
  STA posY                    ; Store player Y for atan2 and find enemy
  LDA playerPosX+1            ; Load player X
  STA posX                    ; Store player X for atan2 and find enemy
  JSR FindClosestEnemyIndex
  LDX #0
  STX bulletCount             ; In case we loop, start bullet count here
  LDY #0                      ; Set Y to 0 (haven't shot)
  CMP #$FF                    ; No enemy to shoot at? Don't shoot
  BNE .findFreeBullet
  RTS
.findFreeBullet:
  JSR GetPlayerBulletState    ; Need to loop through all bullets when checking state
  CMP #BULL_OFF               ; If the bullet is off? It's a candidate
  BNE .nextBullet             ; Not off, skip
  CPY #0                      ; Have we already shot a bullet?
  BNE .nextBullet             ; Aleady shot, skip
; Found free bullet
  LDA #BULL_MOV               ; Set the current bullet state to moving
  STA state
  JSR SetPlayerBulletState
  LDY positionOffset, X       ; Get position byte offset for bullet index
  LDA #0                      ; Clear bullet subpixels
  STA playerBulletPosX, Y
  STA playerBulletPosY, Y
  LDA playerPosX+1            ; Load position X from player
  STA playerBulletPosX+1, Y   ; Set bullet pixel X
  STA posX                    ; Set atan2 x1 to spawn position
  LDA playerPosY+1            ; Load position Y from player
  STA playerBulletPosY+1, Y   ; Set bullet pixel Y
  STA posY                    ; Set atan2 y1 to spawn position
  LDX enemyCount              ; Get enemy index from earlier
  LDY positionOffset, X       ; Get position offset for enemy index
  LDA enemyPosX+1, Y          ; Load enemy X
  STA posX2                   ; Store enemy X
  LDA enemyPosY+1, Y          ; Load enemy Y
  STA posY2                   ; Store enemy Y
  JSR Atan2Deg32              ; Get degrees between player and enemy
  LDX bulletCount             ; Put bulletCount back in X
  STA playerBulletVel, X      ; Store the velocity index for the current bullet
  LDY #1                      ; Mark that we've already shot
.nextBullet:
  INC bulletCount             ; Increment counter
  LDX bulletCount
  CPX #PBULLETCOUNT
  BNE .findFreeBullet         ; If not 0, check next bullet, or cycle the state
  RTS

; Bullet is dead, hide it, then set state to off
; Expects pointerLo to be pointing at sprite0 y of current bullet
HidePlayerBullet:
  LDA #BULL_OFF               ; Set state to off
  STA state
  JSR SetPlayerBulletState
  JSR SetPointerForBullet
  LDA #2
  STA tilesH
  JSR HideSpriteLayout        ; Hide sprite, pointer is advanced
  RTS

; bulletCount counts the current bullet index through this update
UpdatePlayerBullets:
  LDA #SPRITEHI               ; Set up high sprite pointer for later
  STA pointerHi
  LDA #0                      ; Zero out bullet count
  STA bulletCount             ; Store bullet count
  LDA #STATEMASK              ; Are we on the anim tick? Increment the
  AND animTick                ;   bullet animation counter if so
  BEQ .countAnim
  JMP .updateLoop
.countAnim:
  INC bulletAnim              ; Increment bullet anim counter
.updateLoop:
  JSR GetPlayerBulletState    ; state now stores bullet state
  CMP #BULL_MOV               ; Are we moving?
  BEQ .bulletMove
  JMP .incrementLoop          ; Other bullet states do nothing
.bulletMove:
  JSR DoPlayerBulletMove
.incrementLoop:               ; Bullet update done
  INC bulletCount
  LDA bulletCount
  CMP #PBULLETCOUNT           ; Are we done with the loop?
  BNE .updateLoop
  RTS

DoPlayerBulletMove:
  ; Move pixel Y
  LDX bulletCount
  LDY playerBulletVel, X      ; Load bullet velocity index in Y
  LDA playerBulletMoveY, Y    ; Velocity Lo
  STA velLo
  LDA playerBulletMoveY+32, Y ; Velocity Hi
  STA velHi
  JSR StoreVelocitySign
  LDA #HIGH(playerBulletPosY) ; Set up pointer for bullet y pos
  STA pointerSubHi
  LDA #LOW(playerBulletPosY)
  CLC
  ADC positionOffset, X       ; Move the pointer forward to the right index
  STA pointerSubLo
  JSR SubPixelMove            ; Move the Y position
  ; Move pixel X
  LDY playerBulletVel, X      ; Load bullet velocity index in Y
  LDA playerBulletMoveX, Y    ; Velocity Lo
  STA velLo
  LDA playerBulletMoveX+32, Y ; Velocity Hi
  STA velHi
  JSR StoreVelocitySign
  LDA #HIGH(playerBulletPosX) ; Set up pointer for bullet x pos
  STA pointerSubHi
  LDA #LOW(playerBulletPosX)
  CLC
  ADC positionOffset, X       ; Move the pointer forward to the right index
  STA pointerSubLo
  JSR SubPixelMove            ; Move the X position
  ; Save the final movement positions for searches
  LDX bulletCount
  LDY positionOffset, X
  LDA playerBulletPosX+1, Y
  STA posX                    ; Store bullet X position
  LDA playerBulletPosY+1, Y
  STA posY                    ; Store bullet Y position
  ; Test collsiion with enemies
  JSR FindClosestEnemyIndex
  CMP #$FF                    ; Sentinel index, Nothing nearby
  BEQ .worldCollision
  LDA distance                ; The distance stored by the search
  CLC
  CMP #PLAYER_BULLET_RAD      ; Is it less than the player hit distance?
  BCC .hitEnemy               ; Go to hit enemy routine
.worldCollision:
  LDA #0
  STA tilesW                  ; Store zero in tilesW and tilesH, only test
  STA tilesH                  ;   a single point
  JSR TestWorldCollision
  CMP #1                      ; We've collided
  BEQ .collision
  JMP .updateLayout           ; No collision
.hitEnemy:
  LDA #SFX_BULLET_HIT
  JSR SoundLoad
  LDX enemyCount
  DEC enemyHealth, X
  JSR HidePlayerBullet
  RTS
.collision:
  LDA #SFX_BULLET_MISS
  JSR SoundLoad
  JSR HidePlayerBullet        ; We left the screen, bullet is dead
  RTS
.updateLayout:
  JSR SetPointerForBullet     ; Also sets X to sprite index
  LDY positionOffset, X       ; Get index of position variable for bullet
  LDA playerBulletPosY+1, Y   ; Load the Y position
  SEC
  SBC #TILE_WIDTH             ; Pos is center of bullet, offset by 1 tile
  STA posY                    ; Save Y position
  LDA playerBulletPosX+1, Y   ; Load the X position
  SEC
  SBC #TILE_WIDTH             ; Pos is center of bullet, offset by 1 tile
  STA posX                    ; Save X position
  LDA #2                      ; Bullet is 2 tiles tall
  STA tilesH
  JSR UpdateSpriteLayout      ; Update sprite layout
  JSR SetPointerForBullet     ; Set pointer back to update frames
  LDA bulletAnim              ; Load the animation state
  CLC
  ADC bulletCount             ; Offset by bullet count
  AND #STATEMASK              ; Check the bullet frame
  CMP #3                      ; Frame 3
  BEQ .frame3
  CMP #2                      ; Frame 2
  BEQ .frame2
  CMP #1                      ; Frame 1
  BEQ .frame1
;frame0
  JMP AssignPlayerBulletAnim0
.frame1:
  JMP AssignPlayerBulletAnim1
.frame2:
  JMP AssignPlayerBulletAnim2
.frame3:
  JMP AssignPlayerBulletAnim3

; ANIMATION FRAME

AssignPlayerBulletAnim0:
  LDA #BULLFRAME0             ; Assign frame 0 to all tiles
  STA spriteFrame+0
  STA spriteFrame+1
  STA spriteFrame+2
  STA spriteFrame+3
  LDA #BULLETNOFL             ; TL no flip
  STA spriteAttr+0
  LDA #BULLETFLX              ; TR flip x
  STA spriteAttr+1
  LDA #BULLETFLY              ; BL flip y
  STA spriteAttr+2
  LDA #BULLETFLXY             ; BR flip xy
  STA spriteAttr+3
  JSR ApplySpriteSettings
  RTS

AssignPlayerBulletAnim1:
  LDA #BULLFRAME1             ; Assign frame 1 to TL and BR tiles
  STA spriteFrame+0
  STA spriteFrame+3
  LDA #BULLFRAME2             ; Assign frame 2 to TR and BL tiles
  STA spriteFrame+1
  STA spriteFrame+2
  LDA #BULLETNOFL             ; TL and TR no flip
  STA spriteAttr+0
  STA spriteAttr+1
  LDA #BULLETFLXY             ; BL and BR flip xy
  STA spriteAttr+2
  STA spriteAttr+3
  JSR ApplySpriteSettings
  RTS

AssignPlayerBulletAnim2:
  LDA #BULLFRAME3             ; Assign frame 3 to all tiles
  STA spriteFrame+0
  STA spriteFrame+1
  STA spriteFrame+2
  STA spriteFrame+3
  LDA #BULLETNOFL             ; TL no flip
  STA spriteAttr+0
  LDA #BULLETFLX              ; TR flip x
  STA spriteAttr+1
  LDA #BULLETFLY              ; BL flip y
  STA spriteAttr+2
  LDA #BULLETFLXY             ; BR flip xy
  STA spriteAttr+3
  JSR ApplySpriteSettings
  RTS

AssignPlayerBulletAnim3:
  LDA #BULLFRAME2             ; Assign frame 2 to TL and BR tiles
  STA spriteFrame+0
  STA spriteFrame+3
  LDA #BULLFRAME1             ; Assign frame 1 to TR and BL tiles
  STA spriteFrame+1
  STA spriteFrame+2
  LDA #BULLETFLX              ; TL and TR flip x
  STA spriteAttr+0
  STA spriteAttr+1
  LDA #BULLETFLY              ; BL and BR flip y
  STA spriteAttr+2
  STA spriteAttr+3
  JSR ApplySpriteSettings
  RTS
