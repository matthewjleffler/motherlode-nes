; game.asm contains game lifecycle code
;     RESET and init
;     NMI interrupt
;     Game loop
; Note: variables and constants defined in vars.asm
;       tables and sprites are in tables.asm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; RESET and init

vblankwait:                   ; VBLANK reset wait
  BIT $2002
  BPL vblankwait
  RTS

reenableppu:
  LDA #%10010000              ; enable NMI, sprites from Pattern Table 0,
                              ; background from Pattern Table 1
  STA $2000
  LDA #%00011110              ; enable sprites, enable background, no clipping
                              ; on left side
  STA $2001
  LDA #$00                    ; tell the ppu there is no background scrolling
  STA $2005
  STA $2005
  RTS

RESET:
  SEI                         ; disable IRQs
  CLD                         ; disable decimal mode
  LDX #$40
  STX $4017                   ; disable APU frame IRQ
  LDX #$FF
  TXS                         ; Set up stack
  INX                         ; now X = 0
  STX $2000                   ; disable NMI
  STX $2001                   ; disable rendering
  STX $4010                   ; disable DMC IRQs
  JSR vblankwait              ; First VBLANK

clrmem:
  LDA #$00
  STA $0000, x
  STA $0100, x
  STA $0300, x
  STA $0400, x
  STA $0500, x
  STA $0600, x
  STA $0700, x
  LDA #$FE
  STA $0200, x
  INX
  BNE clrmem
  JSR vblankwait              ; Second VBLANK, PPU is now ready

LoadPalettes:
  LDA $2002                   ; read PPU status to reset the high/low latch
  LDA #$3F
  STA $2006                   ; write the high byte of $3F00 address
  LDA #$00
  STA $2006                   ; write the low byte of $3F00 address
  LDX #$00                    ; start out at 0
.loop:
  LDA palette, x              ; load data from address (palette + x)
  STA $2007                   ; write to PPU
  INX
  CPX #$20                    ; Size of all pallete bytes
  BNE .loop                   ; Branch to LoadPalettesLoop if loop not done

LoadSprites:
  LDX #$00                    ; start at 0
  LDA #$FF                    ; fill with FF so sprites are hidden
.loop:
  STA $0200, X
  INX
  BNE .loop                   ; Add until we loop back to 0

; TODO move this to gameplay code
AssignPlayerSprites:
  LDA #SPRITEHI               ; setup player sprite pointer
  STA pointerHi
  LDA #PLAYER
  STA pointerLo
  LDY #$00
.loop:
  LDA playersprites, Y
  STA [pointerLo], Y
  INY
  CPY #PLAYERSIZE             ; Loop until we have finished all the player bytes
  BNE .loop

LoadBackground:
  LDA $2002                   ; Read PPU status to reset the high/low latch
  LDA #BG_HI
  STA $2006                   ; write the high byte of $2000 address
  LDA #$00
  STA $2006                   ; write the low byte of $2000 address
  LDA #$00
  STA pointerLo               ; put the low byte of the address of background
                              ; into pointer
  LDA #HIGH(background)
  STA pointerHi               ; put the high byte of the address into pointer
  LDX #$00                    ; start at pointer + 0
  LDY #$00
.loopX:
.loopY:
  LDA [pointerLo], y          ; copy one background byte from address in
                              ; pointer plus Y
  STA $2007                   ; this runs 256 * 4 times
  INY                         ; inside loop counter
  CPY #$00
  BNE .loopY                  ; run the inside loop 256 times before continuing
  ; End inner loop
  INC pointerHi               ; low byte went 0 to 256, so high byte needs to be
                              ; changed now
  INX
  CPX #$04
  BNE .loopX                  ; run the outside loop 256 times before continuing
  ; End outer loop

InitializeVariables:
  LDA #PLAYER_SPAWN_X         ; Set up player spawn position
  STA playerPosX+1
  LDA #PLAYER_SPAWN_Y
  STA playerPosY+1
  LDA #10
  STA seed                    ; TODO init seed with player input
  LDA #SPAWN_MIN_TICKS
  STA enemySpawnTimer

  JSR reenableppu             ; Finish setting up palettes, reenable NMI

Forever:
  JMP Forever                 ; Infinite loop until next NMI

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; NMI frame interrupt

NMI:
  JSR UpdateBackground
  ; Copy sprites through DMA
  LDA #$00
  STA $2003                   ; set the low byte (00) of the RAM address
  LDA #$02
  STA $4014                   ; set the high byte (02) of the RAM address,
                              ; start the transfer
  JSR reenableppu
  JSR GameLoop
  RTI                         ; Return from interrupt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Background buffer

UpdateBackground:
  LDY #0                      ; Count through the buffer
  LDA backgroundBuffer
  BNE .loopUpdate
  RTS
.loopUpdate:
  LDX backgroundBuffer, Y     ; Length we'll count through the buffer
  CPX #0                      ; Are we at the end of the buffer?
  BNE .startDraw              ; No, draw this buffer
  RTS                         ; Yes, Done
.startDraw:
  INY                         ; Increment buffer
  LDA $2002                   ; Read PPU status to reset the high/low latch
  LDA #BG_HI
  STA $2006                   ; Store hi byte of bg index
  LDA backgroundBuffer, Y     ; X index of the status bar to draw at
  CLC
  ADC #STATUS_LO              ; Put it within the status bar row
  STA $2006                   ; Store lo byte of status index row
  INY                         ; Increment buffer
.loopDraw:
  LDA backgroundBuffer, Y     ; Load tile to draw from buffer
  STA $2007                   ; Write tile to PPU
  INY                         ; Increment buffer
  DEX                         ; Decrement length
  CPX #0                      ; Done with this buffer?
  BEQ .loopUpdate             ; Yes, draw next buffer
  JMP .loopDraw               ; No, continue this draw

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Game Loop

GameLoop:
  ; Clear and modify values
  LDA #0                      ; Clear background update buffer, and count
  STA backgroundBuffer
  STA bufferUpdateIndex
  STA scoreChanged
  INC animTick                ; Increment animation tick
  ; Game loop
  JSR ReadControllers
  JSR TestPlayerMove
  JSR UpdatePlayerSprites
  JSR TestPlayerSpecial
  JSR TestShootBullet
  JSR UpdateBullets
  JSR TestSpawnEnemies
  JSR UpdateEnemies
  JSR DrawScoreUpdate
  ; Add a trailing 0 to the end of the background update buffer
  LDA #0
  LDX bufferUpdateIndex
  STA backgroundBuffer, X
  RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Controllers and Input

ReadControllers:
  LDA #CONTROLHI              ; Setup pointers for controller 1
  STA pointerHi
  LDA #CONTROLLO
  STA pointerLo
  LDY #$00
  LDA #$01                    ; Latch both controller buttons
  STA [pointerLo], Y
  LDA #$00
  STA [pointerLo], Y
.controller:
  LDX #$08                    ; Setup counter
.button:
  LDA [pointerLo], Y
  LSR A                       ; bit0 -> carry
  ROL arg0                    ; bit0 <- carry
  DEX                         ; see if this loop is done
  BNE .button                 ; continue loop
  LDA arg0                    ; Load this frame's state to calculate freshness
  EOR buttons1, y             ; EOR to get changes
  AND arg0                    ; AND to only keep newly on bits
  STA buttons1fresh, y        ; Store the freshness var
  LDA arg0                    ; Now store actual held state back
  STA buttons1, y
  INY
  CPY #$01                    ; Are we on controller 2?
  BEQ .controller
  RTS

TestPlayerMove:
  ; Check Dodge first
  LDA playerDodge             ; Count dodge timers
  BEQ .testDodge              ; No value, check if we're dodging now
  DEC playerDodge             ; A value, decrement it
  LDA playerDodge
  AND #DODGE_TIME_MASK        ; Check the time bits - did time run out?
  BNE .beginMove              ; No - keep doing whatever we were doing
  LDA playerDodge             ; Yes - check if we're dodging or on cooldown
  AND #DODGE_ON
  CMP #DODGE_ON               ; Dodging, start the cooldown
  BEQ .startDodgeCooldown
  LDA #0                      ; Cooldown ran out
  STA playerDodge             ; Clear dodge
  LDA #1                      ; Update the status bar to show dodge is ready
  STA len                     ; 1 tile to update
  STA startX                  ; X index is 1
  JSR StartBackgroundUpdate
  LDA #STATUS_BUTT_ON         ; "on" tile
  JSR AddBackgroundByte
  JMP .beginMove
.startDodgeCooldown:
  LDA #DODGE_COOLDOWN
  STA playerDodge
  JMP .beginMove
.testDodge:
  LDA buttons1fresh
  AND #BUTTONB                ; Are we pressing B?
  BEQ .beginMove              ; No - just move
  ; Start dodge
  LDA #1                      ; Update the status bar to show dodge is not ready
  STA len                     ; 1 tile to update
  STA startX                  ; X index is 1
  JSR StartBackgroundUpdate
  LDA #STATUS_BUTT_OFF        ; "off" tile
  JSR AddBackgroundByte
  LDA #DODGE_ON               ; Yes - set dodging bit
  CLC
  ADC #DODGE_TIME             ; Add the dodge timer
  STA playerDodge
.beginMove:
  LDA buttons1                ; Get controller input
  AND #MOVE_INPUT             ; Mask out just the movement bits
  STA playerMoveDir           ; Store raw movement input
;testUD
  LDA buttons1                ; Cancel out opposite direction movement
  AND #MASK_UD
  CMP #MASK_UD                ; Are we pressing UD at once?
  BEQ .removeUD
  JMP .testRL
.removeUD:
  LDA playerMoveDir           ; Load the pressed values
  AND #REMOVE_UD              ; Mask out the remove bits
  STA playerMoveDir           ; Store change
.testRL:
  LDA buttons1                ; Load the pressed values
  AND #MASK_LR                ; Are we pressing LR at once?
  CMP #MASK_LR
  BEQ .removeLR
  JMP .testNoMove
.removeLR:
  LDA playerMoveDir           ; Load the pressed values
  AND #REMOVE_LR              ; Mask out the remove bits
  STA playerMoveDir           ; Store change
.testNoMove:
  LDA playerMoveDir
  BNE .doPlayerMove           ; Something is pressed, do the move
  RTS                         ; Done, don't apply movement
.doPlayerMove:
  LDX #0                      ; Find the direction index that matches
.loop:                        ;   our input pattern
  CMP playerInput, X
  BEQ .applyY
  INX
  JMP .loop
.applyY:
  TXA
  STA playerMoveDir           ; Store X index for later
  ; Store last position so we can revert move if there's a collision
  LDA playerPosX+1            ; Pixel in player X pos
  STA spriteLastPosX          ; Store last X
  LDA playerPosY+1            ; Pixel in player Y pos
  STA spriteLastPosY          ; Store last Y
  ; Set up velocity args for Y
  LDA playerMoveY, X
  STA velLo                   ; Velocity Lo
  LDA playerMoveY+8, X        ; 8 directions
  STA velHi                   ; Velocity Hi
  JSR StoreVeloctySign        ; Store the sign, for subpixel move and dodging
  LDA playerDodge
  AND #DODGE_ON               ; Check dodging
  BEQ .moveY                  ; Not dodging if 0
  JSR QuadrupleVelocity       ; Dodging - 4x velocity
.moveY:
  LDA #HIGH(playerPosY)       ; Set pointerSub for player Y
  STA pointerSubHi
  LDA #LOW(playerPosY)
  STA pointerSubLo
  JSR SubPixelMove            ; Do movement
  LDA playerPosY+1            ; Load new player Y pos
  SEC
  SBC #TILE_WIDTH             ; Offset up to top edge
  STA posY                    ; Store the value in the Y arg
  ; Test Y collision
  LDA velSign                 ; Check sign from earlier
  BNE .runCollisionY          ; Moving in negative, check top edge
  LDA posY                    ; Collision test on bottom, move Y to test down
  CLC                         ;   by 3 tiles, the height of the player
  ADC #TILES_PX_3
  STA posY                    ; Store the change
.runCollisionY:
  LDA playerPosX+1            ; Load player X pos
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
  STA playerPosY+1
.applyX:
  ; Set up velocity args for X
  LDA playerMoveDir           ; Get X we stored earlier back into X
  TAX
  LDA playerMoveX, X
  STA velLo                   ; Velocity Lo
  LDA playerMoveX+8, X        ; 8 directions
  STA velHi                   ; Velocity Hi
  JSR StoreVeloctySign        ; Store the sign, for subpixel move and dodging
  ; Check dodging
  LDA playerDodge
  AND #DODGE_ON
  BEQ .moveX                  ; Not dodging
  JSR QuadrupleVelocity       ; Dodging - 4x velocity
.moveX:
  LDA #HIGH(playerPosX)       ; Set pointerSub for player subpixel X
  STA pointerSubHi
  LDA #LOW(playerPosX)
  STA pointerSubLo
  JSR SubPixelMove            ; Do move
  LDA playerPosX+1
  SEC
  SBC #TILE_WIDTH             ; Offset back to left edge
  STA posX                    ; Store the value in the X arg
  ; Test X collision
  LDA velSign
  BNE .runCollisionX          ; Moving in negative, check left edge
  LDA posX
  CLC                         ; Collision test on right, move X to test right by
  ADC #TILES_PX_2             ; 2 tiles, the width of the player
  STA posX
.runCollisionX:
  LDA playerPosY+1            ; Load player Y position
  SEC
  SBC #TILE_WIDTH             ; Offset to top edge
  STA posY                    ; Store y position
  LDA #0
  STA tilesW                  ; Single line X
  LDA #3
  STA tilesH                  ; 3 tiles high
  JSR TestWorldCollision
  BEQ .done                   ; No collision, we're good
  LDA spriteLastPosX          ; We got a collision, set X back
  STA playerPosX+1
.done:
  RTS

TestShootBullet:
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
  JSR GetBulletState          ; Need to loop through all bullets when checking state
  CMP #BULL_OFF               ; If the bullet is off? It's a candidate
  BNE .nextBullet             ; Not off, skip
  CPY #0                      ; Have we already shot a bullet?
  BNE .nextBullet             ; Aleady shot, skip
; Found free bullet
  LDA #BULL_MOV               ; Set the current bullet state to moving
  STA state
  JSR SetBulletState
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
  JSR Atan232                 ; Get degrees between player and enemy
  LDX bulletCount             ; Put bulletCount back in X
  STA playerBulletVel, X      ; Store the velocity index for the current bullet
  LDY #1                      ; Mark that we've already shot
.nextBullet:
  INC bulletCount             ; Increment counter
  LDX bulletCount
  CPX #BULLETCOUNT
  BNE .findFreeBullet         ; If not 0, check next bullet, or cycle the state
  RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Sprite Updates

UpdatePlayerSprites:
  LDA #SPRITEHI               ; Sprite hi bites
  STA pointerHi
  LDA #PLAYER                 ; Player low bytes
  STA pointerLo
  LDA playerPosX+1
  SEC
  SBC #TILE_WIDTH             ; Offset X left by one tile to left edge
  STA posX                    ; Store X position
  LDA playerPosY+1
  SEC
  SBC #TILE_WIDTH             ; Offset Y up by one tile to top edge
  STA posY                    ; Store Y position
  LDA #03                     ; Player is 3 tiles high
  STA tilesH                  ; Store in sprite height
  JSR UpdateSpriteLayout      ; Update sprites now
  RTS

TestPlayerSpecial:
  LDA buttons1fresh
  AND #BUTTONA
  CMP #BUTTONA
  BEQ .doPlayerSpecial
  RTS
.doPlayerSpecial:
  ; INC debug
  ; LDA debug
  ; JSR DrawDebug
  ; LDX #6
  ; JSR AddScore
  RTS

; Set pointer for bullet sprite
SetPointerForBullet:
  LDX bulletCount
  LDA spriteOffset, X         ; Get sprite offset for index
  CLC
  ADC #PBULLET0
  STA pointerLo               ; Store sprite at index in pointerLo
  RTS

; Gets current bullet state, and shifts bullet info over
; Stores resulting state in state
GetBulletState:
  LDA playerBulletStates      ; Get current state in A
  AND #STATEMASK
  STA state                   ; Store result in temp
  LSR playerBulletStates      ; Move state off playerBulletStates
  LSR playerBulletStates      ; Last two bits are 0
  JSR SetBulletState          ; Store current state back, in case of no change
  LDA state
  RTS

; Sets current bullet state in high bits of playerBulletStates
; Expects the new state to already be in the low bits of state
; Can be assigned multiple times, state is preserved between calls
SetBulletState:
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

; Bullet is dead, hide it, then set state to off
; Expects pointerLo to be pointing at sprite0 y of current bullet
HideBullet:
  LDA #BULL_OFF               ; Set state to off
  STA state
  JSR SetBulletState
  JSR SetPointerForBullet
  LDA #2
  STA tilesH
  JSR HideSpriteLayout        ; Hide sprite, pointer is advanced
  JMP IncrementBulletLoop     ; Continue loop

; bulletCount counts the current bullet index through this update
UpdateBullets:
  LDA #SPRITEHI               ; Set up high sprite pointer for later
  STA pointerHi
  LDA #0                      ; Zero out bullet count
  STA bulletCount             ; Store bullet count
  LDA #STATEMASK              ; Are we on the anim tick? Increment the
  AND animTick                ;   bullet animation counter if so
  BEQ .countAnim
  JMP UpdateBulletLoop
.countAnim:
  INC bulletAnim              ; Increment bullet anim counter
UpdateBulletLoop:
  JSR GetBulletState          ; state now stores bullet state
  CMP #BULL_EXP               ; Are we exploding?
  BEQ .bulletExplode
  CMP #BULL_MOV               ; Are we moving?
  BEQ .bulletMove
  JMP IncrementBulletLoop     ; Other bullet states do nothing
.bulletExplode:
  JMP DoBulletExplode
.bulletMove:
  JMP DoBulletMove
IncrementBulletLoop:          ; Bullet update done
  INC bulletCount
  LDA bulletCount
  CMP #BULLETCOUNT            ; Are we done with the loop?
  BNE UpdateBulletLoop
  RTS

; TODO implement
DoBulletExplode:
  JMP IncrementBulletLoop

DoBulletMove:
  ; Move pixel Y
  LDX bulletCount
  LDY playerBulletVel, X      ; Load bullet velocity index in Y
  LDA playerBulletMoveY, Y    ; Velocity Lo
  STA velLo
  LDA playerBulletMoveY+32, Y ; Velocity Hi
  STA velHi
  JSR StoreVeloctySign
  LDA #HIGH(playerBulletPosY) ; Set up pointer for bullet y pos
  STA pointerSubHi
  LDA #LOW(playerBulletPosY)
  CLC
  ADC positionOffset, X       ; Move the pointer forward to the right index
  STA pointerSubLo
  JSR SubPixelMove            ; Move the Y position
  ; STA posY                    ; Store new position for collision
  ; Move pixel X
  LDY playerBulletVel, X      ; Load bullet velocity index in Y
  LDA playerBulletMoveX, Y    ; Velocity Lo
  STA velLo
  LDA playerBulletMoveX+32, Y ; Velocity Hi
  STA velHi
  JSR StoreVeloctySign
  LDA #HIGH(playerBulletPosX) ; Set up pointer for bullet x pos
  STA pointerSubHi
  LDA #LOW(playerBulletPosX)
  CLC
  ADC positionOffset, X       ; Move the pointer forward to the right index
  STA pointerSubLo
  JSR SubPixelMove            ; Move the X position
  ; STA posX                    ; Store new position for collision
  ; Save the final movement positions for searches
  ; TODO why doesn't SubPixelMove do this with A?
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
  LDX enemyCount
  DEC enemyHealth, X
.collision:
  JMP HideBullet              ; We left the screen, bullet is dead

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
  JMP AssignBulletAnim0
.frame1:
  JMP AssignBulletAnim1
.frame2:
  JMP AssignBulletAnim2
.frame3:
  JMP AssignBulletAnim3

AssignBulletAnim0:
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
  JMP IncrementBulletLoop

AssignBulletAnim1:
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
  JMP IncrementBulletLoop

AssignBulletAnim2:
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
  JMP IncrementBulletLoop

AssignBulletAnim3:
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
  JMP IncrementBulletLoop

; Takes pre-filled frames and attributes and applies them to the
; current sprite pointer
ApplySpriteSettings:
  LDX #0                      ; Starts our loop at 0
.loop:
  LDA spriteFrame, X
  LDY #SPRITETIL              ; Assign tile
  STA [pointerLo], Y
  LDY #SPRITEATT              ; Assign attributes
  LDA spriteAttr, X
  STA [pointerLo], Y
  LDA pointerLo               ; Increment pointer by 4 bytes to next sprite
  CLC
  ADC #$04
  STA pointerLo
  INX
  CPX #$04                    ; Check whether we're done with the loop
  BNE .loop
  RTS

StorePlayerPosForSearch:
  LDA playerPosX+1
  STA posX2
  LDA playerPosY+1
  STA posY2
  RTS

; Set pointer for enemy sprite
SetPointerForEnemy:
  LDA #SPRITEHI
  STA pointerHi
  LDX enemyCount
  LDA spriteOffset, X         ; Get sprite offset for index
  CLC
  ADC #ENEMY0
  STA pointerLo               ; Store sprite at index in pointerLo
  RTS

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
  AND #BULLETSHOOTMASK        ; Count every 8 ticks
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
  JSR StoreVeloctySign        ; Store the sign, for subpixel move and dodging
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
  JSR StoreVeloctySign        ; Store the sign, for subpixel move and dodging
; Do X movement
  LDA #HIGH(enemyPosX)       ; Set pointerSub for player subpixel X
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
  LDA #EN_STATE_HEAD          ; TODO pick enemy randomly
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
  AND #ANIM_MASK
  BEQ .skelFrame1
  JSR DrawSkeletonFrame2
  JMP .skelUpdate
.skelFrame1:
  JSR DrawSkeletonFrame1
.skelUpdate:
  JSR TestEnemyHealth
  BEQ .skelCheckVel           ; Still alive
  LDX #1                      ; Dead, add 10 points
  JSR AddScore
  JMP .updateLayout
.skelCheckVel:
  LDA enemyTick, X            ; Did our pathfind tick run out?
  BNE .skelMove               ; No, move with our last velocity
  LDA #EN_TIME_PATH
  STA enemyTick, X            ; Reset pathfind tick
  LDY positionOffset, X
  LDA enemyPosX+1, Y          ; Store X pos for atan2
  STA posX
  LDA enemyPosY+1, Y          ; Store Y pos for atan2
  STA posY
  JSR StorePlayerPosForSearch
  JSR Atan216                 ; Atan2 16 degrees
  LDX enemyCount              ; Reset X for enemy
  STA enemyVel, X             ; Store velocity
.skelMove:
  JSR MoveEnemy
  JMP .updateLayout

.stateHead:
  LDA enemyAnim               ; Load enemy anim
  CLC
  ADC enemyCount              ; Offset by anim
  LSR A                       ; Divide by 2
  AND #ANIM_MASK
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
  LDA #EN_TIME_HEAD
  STA enemyTick, X
  ; TODO shoot a bullet
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Scoring

; Requires X to be set up for the place to increment
AddScore:
  LDA #1
  STA scoreChanged            ; Mark that we've changed score this frame
  CPX #SCOREPLACES            ; Did we overflow the total loop? We're done
  BNE .doAdd                  ; No, continue
.fillScore:                   ; Yes, mark score full
  DEX
  LDA #9
  STA score, X                ; Put 9 in this place
  CPX #0                      ; Did we finish the last place?
  BNE .fillScore              ; No, continue filling
  RTS                         ; Yes, stop updating
.doAdd:
  INC score, X
  LDA score, X
  CMP #$A                     ; Do we have 10 in this place?
  BEQ .addPlace               ; Yes, increment place
  RTS                         ; No, our add is done
.addPlace:                    ; We overflowed, carry 1 up a place
  LDA #0                      ; Set zero in this place
  STA score, X
  INX                         ; Increment place
  JMP AddScore                ; Add one to new place

DrawScoreUpdate:
  LDA scoreChanged            ; Check if score changed
  BNE .doUpdate               ; Yes, draw change
  RTS                         ; No, nothing to draw
.doUpdate:
  LDX #SCOREPLACES            ; We're drawing all score places
  STX len
  LDA #SCORE_TILE             ; We start drawing at score place
  STA startX
  JSR StartBackgroundUpdate
.drawScoreLoop:
  DEX                         ; Decrement X
  LDA score, X                ; Draw score place into buffer, reversing order
  JSR AddBackgroundByte
  CPX #0                      ; Did we draw the last place?
  BNE .drawScoreLoop          ; No, do next place
  RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Utils

; For sprite updates, move pointerLo down by two sprites
MovePointerOneRow:
  LDA pointerLo
  CLC
  ADC #$08                    ; 8 bytes for 2 4 byte sprites
  STA pointerLo
  RTS

; Sets all sprites to hidden for a given layout
; Expects all sprites to be 2 tiles wide
; Expects pointerLo to be set for the top left sprite address
;
; Args:
;   tilesH                    - Height of sprite in tiles
HideSpriteLayout:
  LDA #$FF                    ; Store "offscreen" FF in sprite y
  LDY #0                      ; Store sprite y origin offset
  STA [pointerLo], Y          ; Set Y's
  LDY #4                      ; Next sprite's Y
  STA [pointerLo], Y
  JSR MovePointerOneRow       ; Increment pointer for next loop
  DEC tilesH
  LDA tilesH
  BNE HideSpriteLayout        ; Continue loop for next row
  RTS

; Update sprite layout for a group of sprites
; Expects pointerLo to be set to the top left sprite address, and the sprite
; to be placed where it should be in the world already
; Expects all sprites to be 2 tiles wide
;
; Args:
;   posX                      - TL sprite position X
;   posY                      - TL sprite position Y
;   tilesH                    - Number of tiles high the sprite is
;
; Trashes Y
UpdateSpriteLayout:
  LDA posY                    ; Set sprite Y's
  LDY #0                      ; Row y0
  STA [pointerLo], Y
  LDY #4                      ; Row y1
  STA [pointerLo], Y
  LDA posX                    ; Load X
  LDY #SPRITEX                ; Set sprite X's
  STA [pointerLo], Y
  CLC
  ADC #TILE_WIDTH
  LDY #SPRITEX+4              ; Second sprite's X
  STA [pointerLo], Y
  JSR MovePointerOneRow       ; Increment Y for next loop
  LDA posY                    ; Increment row Y
  CLC
  ADC #TILE_WIDTH
  STA posY
  DEC tilesH
  LDA tilesH
  BNE UpdateSpriteLayout
  RTS

; In preparation of SubPixelMove, find sign on velocity
;
; Args:
;   velHi                     - hi velocity
; Sets velSign to hi velocity sign
StoreVeloctySign:
  LDA velHi                   ; Check hi velocity for sign
  AND #NEG_SIGN
  STA velSign                 ; Store sign
  LDA velHi                   ; Clear sign off of velocity
  AND #MOV_MASK
  STA velHi
  LDA velSign                 ; Load sign back up
  CMP #NEG_SIGN               ; Are we negative?
  BEQ .negative
  LDA #0                      ; Positive
  STA velSign                 ; Store result
  RTS
.negative:
  LDA #1                      ; Negative
  STA velSign                 ; Store result
  RTS

; Multiply the stored velocity by 4
; velHi     - hi velocity
QuadrupleVelocity:
  LDA velHi                   ; Load current velocity hi
  ASL A                       ; Multiply current by 4
  ASL A
  STA velHi                   ; Save scaled velocity
  RTS

; Move subpixel based on velocity
; pointerSubLo should be set up to subpixel
;
; Args:
;   velLo                     - lo velocity
;   velHi                     - hi velocity
;   velSign                   - Sign of velocity
;
; A is new pixel position at the end ; TODO it's not - why?
SubPixelMove:
  LDY #0                      ; Set Y to 0
  LDA velSign                 ; Check sign
  BEQ .add                    ; Positive Movement
  JMP .sub                    ; Negative Movement
.add:
  LDA [pointerSubLo], Y       ; Load subpixel
  CLC
  ADC velLo                   ; Add lo velocity
  STA [pointerSubLo], Y       ; Store subpixel
  LDY #1
  LDA [pointerSubLo], Y       ; Load pixel
  ADC velHi                   ; Add hi velocity with carry
  STA [pointerSubLo], Y       ; Store result
  RTS
.sub:
  LDA [pointerSubLo], Y       ; Load subpixel
  SEC
  SBC velLo                   ; Subtract lo speed
  STA [pointerSubLo], Y       ; Store subpixel
  LDY #1
  LDA [pointerSubLo], Y       ; Load pixel
  SBC velHi                   ; Subtract hi velocity with carry
  STA [pointerSubLo], Y       ; Store pixel
  RTS

Divide32:
  LSR A
  LSR A
  LSR A
  RTS

Divide16:
  LSR A
  LSR A
  LSR A
  LSR A
  RTS

Divide8:
  LSR A
  LSR A
  LSR A
  LSR A
  LSR A
  RTS

Divide4:
  LSR A
  LSR A
  LSR A
  LSR A
  LSR A
  LSR A
  RTS

; Atan2 divided down to 32 degrees
Atan232:
  JSR Atan2
  JSR Divide32
  RTS

; Atan2 divided down to 16 degrees
Atan216:
  JSR Atan2
  JSR Divide16
  RTS

; from https://codebase64.org/doku.php?id=base:8bit_atan2_8-bit_angle
;
; Args:
;   posX                      - x1
;   posY                      - y1
;   posX2                     - x2
;   posY2                     - y2
;
; Local:
;   octant
; A will be the 256 degree angle, also in angle
; X and Y will be trashed
Atan2:
  LDA #0
  STA octant
  LDA posX
  SEC
  SBC posX2
  BCS .o1
  EOR #$ff
.o1:
  TAX
  ROL octant
  LDA posY
  SEC
  SBC posY2
  BCS .o2
  EOR #$ff
.o2:
  TAY
  ROL octant
  LDA log2_tab,x
  SEC
  SBC log2_tab,y
  BCC .o3
  EOR #$ff
.o3:
  TAX
  LDA octant
  ROL A
  AND #%111
  TAY
  LDA atan_tab,x
  EOR octant_adjust,y
  STA angle
  RTS

; Based on the source position, find the closest enemy
;
; Args:
;   posX                      - source X
;   posY                      - source Y
;
; Stores index in A, and enemyCount
; Stores distance in distance
FindClosestEnemyIndex:
  LDX #0                      ; Start count at 0
  LDA #$FF                    ; Set max distance in distance, to check against
  STA distance
  STA enemyCount              ; Set a sentinel value of FF in result index
.loop:
  LDA enemyState, X          ; Check enemy states to ensure it's a valid target
  CMP #ENEMY_ALIVE_STATE      ; Is it alive?
  BCC .count                  ; No - go to next enemy
  LDY positionOffset, X       ; Get position offset for enemy index
  LDA enemyPosX+1, Y          ; Load enemy X position
  STA posX2                   ; Store in x2 for manhattan distance
  LDA enemyPosY+1, Y          ; Load enemy Y position
  STA posY2                   ; Store in y2 for manhattan distance
  JSR ManhattanDistance       ; Get distance in A
  CMP distance                ; Compare to current lowest distance
  BCC .smallest
  JMP .count
.smallest:
  STA distance                ; What we have now is now smallest distance
  STX enemyCount              ; Store the new lowest index in enemyCount
.count:
  INX                         ; Increment X
  CPX #ENEMYCOUNT
  BNE .loop
  LDA enemyCount              ; Record the lowest index we got
  RTS

; Calclulate distance between two points
;
; Args:
;   posX                      - x1
;   posY                      - y1
;   posX2                     - x2
;   posY2                     - y2
; Local:
;   sum
ManhattanDistance:
;findX
  LDA posX                    ; Load x1
  CMP posX2                   ; Compare x2
  BCC .x1less
;x2less
  SEC                         ; x1 still in A
  SBC posX2                   ; subtract x2
  STA sum                     ; store result
  JMP .findY
.x1less:
  LDA posX2                   ; Load x2
  SEC
  SBC posX                    ; subtract x1
  STA sum
.findY:
  LDA posY                    ; Load y1
  CMP posY2                   ; Compare y2
  BCC .y1less
;y2less
  SEC                         ; y1 still in A
  SBC posY2                   ; subtract y2
  JMP .sum
.y1less:
  LDA posY2                   ; Load y2
  SEC
  SBC posY                    ; subtract y1
.sum:
  CLC                         ; y is in A
  ADC sum                     ; add with x
  BVC .finish                 ; if we didn't overflow, we're done
  LDA #$FF                    ; Just set result to full if we overflowed
.finish:
  STA sum                     ; Store final sum
  CLC
  RTS

; Tests world collision at point
;
; Args:
;   posX                      - origin x
;   posY                      - origin y
;   tilesW                    - tiles w
;   tilesH                    - tiles h
;
; Local:
;   tilesX                    - store x tile after calculating
;   tilesWOriginal            - store original w
;
; A will be 0 if not colliding, 1 if colliding
; X and Y will be trashed
TestWorldCollision:
  LDA #HIGH(collision)        ; Setup pointer for collison table
  STA pointerColHi
  LDA tilesW                  ; Load tile width
  STA tilesWOriginal          ; Cache tile width in arg5
.loopY:
  LDA #LOW(collision)
  STA pointerColLo
  LDA posY                    ; Y pixel
  LSR A                       ; LSR 3 times to be y/8 to get tile
  LSR A
  LSR A                       ; Now we have the tile
  CLC
  ADC tilesH                  ; Add our current tile Y offset
  TAY                         ; Now we have a Y
  LDA pointerColLo            ; Look up the pointer offset for our Y
  CLC
  ADC collisionLookupY, Y
  STA pointerColLo
.loopX:
  LDA posX                    ; X pixel
  LSR A                       ; LSR 3 times to be x/8 to get tile
  LSR A
  LSR A                       ; Now we have the tile pos
  CLC
  ADC tilesW                  ; Add current tile W
  STA tilesX                  ; Store our tile back in arg4, we're going to find
  LSR A                       ;   the collision byte with another 3 LSRs to
  LSR A                       ;   be tileX/8
  LSR A
  TAY                         ; Now we have an offset for the actual collision
  LDA tilesX                  ; Load the tile back into A
  SEC
  SBC collisionLookupX, Y     ; Subtract tiles to be in the right quadrant
  TAX                         ; Number of tiles into this quadrant to count
  LDA [pointerColLo], Y       ; Load the collision data
.shiftBits:
  CPX #0                      ; See if the current left bit is the one to test
  BEQ .testBit
  ASL A                       ; Shift bits left
  DEX
  JMP .shiftBits
.testBit:
  AND #COLLISIONMASK          ; Mask collision at this tile
  CMP #COLLISIONMASK          ; Do we equal the collision mask? We're colliding
  BEQ .collision
; Are we done looping?
  DEC tilesW                  ; Decrease W
  LDA tilesW                  ; Test to see if we finished
  CMP #$FF                    ; Looped, done
  BEQ .testFinishY            ; This row is done, test if Y is done
  JMP .loopX                  ; Check next w
.testFinishY:
  DEC tilesH                  ; Decrease H
  LDA tilesH                  ; Test to see if we finished
  CMP #$FF                    ; Looped, done
  BEQ .finish
  LDA tilesWOriginal          ; Reset W
  STA tilesW
  JMP .loopY
.finish:
  LDA #0
  RTS
.collision:
  LDA #1
  RTS

; Returns a random 8-bit number in A (0-255), clobbers arg0
RNG:
  LDA seed+1
  STA arg0 ; store copy of high byte
  ; compute seed+1 ($39>>1 = %11100)
  LSR A ; shift to consume zeroes on left...
  LSR A
  LSR A
  STA seed+1 ; now recreate the remaining bits in reverse order... %111
  LSR A
  EOR seed+1
  LSR A
  EOR seed+1
  EOR seed+0 ; recombine with original low byte
  STA seed+1
  ; compute seed+0 ($39 = %111001)
  LDA arg0 ; original high byte
  STA seed+0
  ASL A
  EOR seed+0
  ASL A
  EOR seed+0
  ASL A
  ASL A
  ASL A
  EOR seed+0
  STA seed+0
  RTS

; Sets up a background update into the buffer
; Assumes we're drawing into the status bar
;
; Args:
;  len                        - How many bytes to copy
;  startX                     - The X tile to start the draw into
;  oldX                       - Preserve X loop when adding to buffer
StartBackgroundUpdate:
  LDA len
  JSR AddBackgroundByte
  LDA startX
  JSR AddBackgroundByte
  RTS

; Adds a byte to the background update, make sure len
; has been set in StartBackgroundUpdate first
AddBackgroundByte:
  STX oldX                    ; Store X in oldX
  LDX bufferUpdateIndex
  STA backgroundBuffer, X
  INC bufferUpdateIndex
  LDX oldX                    ; Restore old X
  RTS

; Sets whatever is in A to the debug value, and draws it into the score
;
; Args:
;  A                          -> debug
DrawDebug:
  STA debug                   ; Store debug value - stop score from rendering
  LDA #2
  STA len                     ; 2 characters going to end of score
  LDA #DEBUG_TILE
  STA startX                  ; Tile X beginning of score
  JSR StartBackgroundUpdate
  LDA debug                   ; Load debug
  LSR A                       ; Shift bottom 4 bits off
  LSR A
  LSR A
  LSR A
  JSR AddBackgroundByte       ; Put high half of debug in first slot
  LDA debug
  AND #LOW_MASK
  JSR AddBackgroundByte       ; Put low half of debug in second slot
  RTS
