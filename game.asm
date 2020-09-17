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

; TODO move this to gameplay code
AssignEnemySprites:
  LDA #SPRITEHI               ; setup enemy skeleton sprite
  STA pointerHi
  LDA #ENEMY0
  STA pointerLo
  LDX #6                      ; 6 enemy spawns
.loopX:
  LDY #0
.loopY
  LDA skelsprites, Y
  STA [pointerLo], Y
  INY
  CPY #ENEMYSIZE
  BNE .loopY                  ; Increment Y loop
  LDA pointerLo
  CLC
  ADC #ENEMYSIZE
  STA pointerLo
  DEX
  CPX #0
  BNE .loopX

AssignEnemyPositions:
  LDA #ENEMY0
  STA pointerLo
  LDX #0
.loop:
  LDY #0
  LDA enemySpawnY, X
  STA [pointerLo], Y
  LDY #SPRITEX
  LDA enemySpawnX, X
  STA [pointerLo], Y
  LDA pointerLo
  CLC
  ADC #ENEMYSIZE
  STA pointerLo
  INX
  CPX #ENEMYCOUNT
  BNE .loop

LoadBackground:
  LDA $2002                   ; read PPU status to reset the high/low latch
  LDA #$20
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

; Initialize Variables
  LDA #PLAYER_SPAWN_X         ; Set up player spawn position
  STA playerPosX+1
  LDA #PLAYER_SPAWN_Y
  STA playerPosY+1
  ; TODO set up game state - title etc

  JSR reenableppu             ; Finish setting up palettes, reenable NMI

Forever:
  JMP Forever                 ; Infinite loop until next NMI

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; NMI frame interrupt

NMI:
  LDA #$00
  STA $2003                   ; set the low byte (00) of the RAM address
  LDA #$02
  STA $4014                   ; set the high byte (02) of the RAM address,
                              ; start the transfer
  JSR reenableppu
  JSR GameLoop
  RTI                         ; Return from interrupt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Game Loop

GameLoop:
  INC animTick                ; Increment animation tick
  JSR ReadControllers
  JSR TestPlayerMove
  JSR UpdatePlayerSprites
  ; JSR TestShootBullet
  ; JSR UpdateBullets
  ; JSR UpdateEnemies
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
  JMP .beginMove
.startDodgeCooldown:
  LDA #DODGE_COOLDOWN
  STA playerDodge
  JMP .beginMove
.testDodge:
  LDA buttons1
  AND #BUTTONB                ; Are we pressing B?
  BEQ .beginMove              ; No - just move
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
  LDA #playerPosY             ; Set pointerSub for player Y
  STA pointerSub
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
  LDA #playerPosX             ; Set pointerSub for player subpixel X
  STA pointerSub
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
  AND animTick                ; Every 7 frames?
  BEQ .shootBullet
  RTS
.shootBullet:
  LDA #SPRITEHI               ; Setup pointers for player
  STA pointerHi
  LDA #PLAYER
  STA pointerLo
  LDY #0
  LDA [pointerLo], Y          ; Load player Y
  STA spriteLayoutOriginY     ; Store player Y
  STA spriteLastPosY
  CLC
  ADC #TILE_WIDTH             ; Center of bullet
  STA arg2                    ; Store center in arg2 for atan2
  LDY #SPRITEX                ; Load player X
  LDA [pointerLo], Y
  STA spriteLayoutOriginX     ; Store player X
  STA spriteLastPosX
  CLC
  ADC #TILE_WIDTH             ; Center of bullet
  STA arg0                    ; Store center in arg0 for atan2
  JSR FindClosestEnemyIndex
  TAX                         ; Got index, put in X
  CMP #$FF                    ; No enemy to shoot at? Don't shoot
  BNE .shootAtEnemy
  RTS
.shootAtEnemy:
  LDA #ENEMY0                 ; Setup pointers for enemy
  STA pointerLo
  LDA enemyOffset, X          ; Get offset from LUT
  CLC
  ADC pointerLo               ; Offset pointer to the correct enemy
  STA pointerLo
  LDY #0
  LDA [pointerLo], Y          ; Load enemy Y
  CLC
  ADC #TILE_WIDTH             ; Add half tile for enemy center
  STA arg3                    ; Store enemy center Y in arg3 (y2)
  LDY #SPRITEX
  LDA [pointerLo], Y          ; Load enemy X
  CLC
  ADC #TILE_WIDTH             ; Add half tile for enemy center
  STA arg1                    ; Store enemy X in arg1 (x2)
  JSR Atan2                   ; Get degrees between player and enemy
  LSR A                       ; LSR 4 times to get 32 degrees
  LSR A
  LSR A
  STA arg1                    ; Store index in arg1, don't need enemy pos
  ; Now loop to find a free bullet to shoot
  LDX #0                      ; Bullet count
  LDY #0                      ; Flag for whether or not we shot, and pointer
  STY bulletCount             ; Count pointers for current bullet
.findFreeBullet:
  JSR GetBulletState
  CMP #BULL_OFF               ; If the bullet is off? Turn it on
  BNE .nextBullet
  ; Found a free bullet
  CPY #$00
  BNE .nextBullet             ; Have we already shot a bullet?
  LDA #BULL_MOV               ; Set the current bullet state to moving
  STA temp
  JSR SetBulletState
  ; Set bullet position
  LDA #PBULLET0               ; Point at selected bullet
  CLC
  ADC bulletCount
  STA pointerLo
  LDY #$00
  LDA spriteLastPosY          ; Player Y stored earlier
  STA [pointerLo], Y          ; Apply player Y
  LDY #SPRITEX
  LDA spriteLastPosX          ; Player X stored earlier
  STA [pointerLo], Y          ; Apply player X
  ; Set bullet velocity
  LDA arg1                    ; Previously saved velocity
  STA playerBulletVel, X
  ; Clear bullet subpixel
  LDA #0                      ; TODO player subpixel?
  STA playerBulletSub, X
  STA playerBulletSub+1, X
  LDY #$01                    ; Mark that we've already shot
.nextBullet:
  LDA bulletCount
  CLC
  ADC #$10                    ; Move to next bullet pointer
  STA bulletCount
  INX                         ; Increment counter
  CPX #BULLETCOUNT
  BNE .findFreeBullet         ; If not 0, check next bullet, or cycle the state
  RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Sprite Updates

MovePointerOneRow:
  LDA pointerLo
  CLC
  ADC #$08                    ; 8 bytes for 2 4 byte sprites
  STA pointerLo
  RTS

MovePointerTwoRows:
  LDA pointerLo
  CLC
  ADC #$10                    ; 16 bytes for 4 4 byte sprites
  STA pointerLo
  RTS

UpdatePlayerSprites:
  LDX #$03                    ; Player is 3 tiles high
  LDA #SPRITEHI               ; Sprite hi bites
  STA pointerHi
  LDA #PLAYER                 ; Player low bytes
  STA pointerLo
  ; Apply player pos to player sprite 0, offset by player w/h
  LDY #0
  LDA playerPosY+1
  SEC
  SBC #TILE_WIDTH             ; Offset up by one tile
  STA [pointerLo], Y
  LDY #SPRITEX
  LDA playerPosX+1
  SEC
  SBC #TILE_WIDTH             ; Offset left by one tile
  STA [pointerLo], Y
  JSR UpdateSpriteLayout      ; Update rest of sprites now
  RTS

; Gets current bullet state, and shifts bullet info over
; Stores resulting state in temp
GetBulletState:
  LDA playerBulletStates      ; Get current state in A
  AND #STATEMASK
  LSR playerBulletStates      ; Move state off playerBulletStates
  LSR playerBulletStates      ; Last two bits are 0
  STA temp                    ; Store result in temp
  JSR SetBulletState          ; Store current state back, in case of no change
  LDA temp
  RTS

; Sets current bullet state in high bits of playerBulletStates
; Expects the new state to already be in the low bits of temp
; Can be assigned multiple times, temp is preserved between calls
SetBulletState:
  LDA playerBulletStates      ; Make sure high bits are clear
  AND #HICLEAR
  STA playerBulletStates
  LDA temp
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
  STA temp
  JSR SetBulletState
  LDX #02                     ; Sprite is 2 tiles high
  JSR HideSpriteLayout        ; Hide sprite, pointer is advanced
  JMP IncrementBulletLoop     ; Continue loop, pointer updated by hide

UpdateBullets:
  LDA #SPRITEHI               ; point the hi pointer at SPRITEHI for bullets
  STA pointerHi
  LDA #PBULLET0               ; point the lo pointer at PBULLET0
  STA pointerLo
  LDX #$00                    ; Zero out bullet count
  STX bulletCount
  LDA #STATEMASK              ; Are we on the anim tick? Increment the
  AND animTick                ; bullet animation counter if so
  BEQ .countAnim
  JMP UpdateBulletLoop
.countAnim:
  INC bulletAnim              ; Increment bullet anim counter
UpdateBulletLoop:
  JSR GetBulletState          ; Temp now stores bullet state
  CMP #BULL_EXP               ; Are we exploding?
  BEQ .bulletExplode
  CMP #BULL_MOV               ; Are we moving?
  BEQ .bulletMove
  JMP .incrementPointer       ; Other bullet states do nothing
.bulletExplode:
  JMP DoBulletExplode
.bulletMove:
  JMP DoBulletMove
.incrementPointer:            ; We did nothing to the bullet, move the pointer
  JSR MovePointerTwoRows      ; To the next bullet
IncrementBulletLoop:          ; Bullet update done
  LDX bulletCount
  INX
  STX bulletCount
  CPX #BULLETCOUNT            ; Are we done with the loop?
  BNE UpdateBulletLoop
  RTS

; TODO implement
DoBulletExplode:
  JSR MovePointerTwoRows
  JMP IncrementBulletLoop

DoBulletMove:
  LDA pointerLo               ; Store the current bullet pointer in bulletFrame
  STA bulletFrame             ; since it's not used until later when we anim.
  LDX bulletCount             ; Get the velocity index of this bullet
  LDA playerBulletVel, X
  TAX
  ; Set up velocity args for Y
  LDA playerBulletMoveY, X    ; Velocity Lo
  STA arg0
  LDA playerBulletMoveY+32, X ; Velocity Hi
  STA arg1
  LDY #0                      ; Set Y register for sprite Y
  LDA #playerBulletSub
  CLC
  ADC bulletCount
  STA pointerSub
  JSR SubPixelMove
  LDA [pointerLo], Y
  STA spriteLayoutOriginY
  ; Set up velocity args for X
  LDA playerBulletMoveX, X    ; Velocity Lo
  STA arg0
  LDA playerBulletMoveX+32, X ; Velocity Hi
  STA arg1
  LDY #SPRITEX
  LDA #playerBulletSub
  CLC
  ADC bulletCount
  STA pointerSub
  JSR SubPixelMove
  JSR StoreSpritePosition
  LDA spriteLayoutOriginX
  CLC
  ADC #TILE_WIDTH
  STA arg0                    ; Collision detection checking middle of bullet
  LDA spriteLayoutOriginY
  CLC
  ADC #TILE_WIDTH
  STA arg1                    ; Collision detection checking middle of bullet
  ; Test collsiion with enemies
  JSR FindClosestEnemyIndex
  CMP #$FF                    ; Sentinel index, Nothing nearby
  BEQ .worldCollision
  ; TODO store index to apply damage
  LDA arg7                    ; The distance stored by the search
  CMP #PLAYER_BULLET_RAD      ; Is it less than the player hit distance?
  BCC .hitEnemy
.worldCollision:
  ; x and y are still set up from enemy check, but set the pointers back
  LDA bulletFrame
  STA pointerLo
  JSR StoreSpritePosition
  LDA spriteLayoutOriginX
  CLC
  ADC #TILE_WIDTH             ; Center of bullet x
  STA arg0
  LDA spriteLayoutOriginY
  CLC
  ADC #TILE_WIDTH             ; Center of bullet y
  STA arg1
  LDA #0
  STA arg2                    ; 0 width and height for collision, so we only
  STA arg3                    ; Check a point
  JSR TestWorldCollision
  CMP #1                      ; We've collided
  BEQ .collision
  JMP .updateLayout           ; No collision
.hitEnemy:
  LDA bulletFrame
  STA pointerLo
  ; TODO apply damage
.collision:
  JMP HideBullet              ; We left the screen, bullet is dead
.updateLayout:
  LDX #$02                    ; Bullet is 2 tiles tall
  JSR UpdateSpriteLayout      ; Update sprite layout
  LDA #STATEMASK              ; Check again if we're in an anim tick, if so
  AND animTick                ; update the anim, otherwise we're done
  BEQ .updateAnim
  JMP IncrementBulletLoop
.updateAnim:
  LDA bulletFrame             ; Move pointer back to where we stored it
  STA pointerLo               ; So we can update attributes
  LDA bulletAnim              ; Load the animation state
  CLC
  ADC bulletCount             ; Add an offset of the current bullet count
  AND #STATEMASK              ; Check the bullet frame
  CMP #$03                    ; Frame 3
  BEQ .frame3
  CMP #$02                    ; Frame 2
  BEQ .frame2
  CMP #$01                    ; Frame 1
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
  STA bulletFrame+0
  STA bulletFrame+1
  STA bulletFrame+2
  STA bulletFrame+3
  LDA #BULLETNOFL             ; TL no flip
  STA bulletAttr+0
  LDA #BULLETFLX              ; TR flip x
  STA bulletAttr+1
  LDA #BULLETFLY              ; BL flip y
  STA bulletAttr+2
  LDA #BULLETFLXY             ; BR flip xy
  STA bulletAttr+3
  JMP ApplyBulletSettings

AssignBulletAnim1:
  LDA #BULLFRAME1             ; Assign frame 1 to TL and BR tiles
  STA bulletFrame+0
  STA bulletFrame+3
  LDA #BULLFRAME2             ; Assign frame 2 to TR and BL tiles
  STA bulletFrame+1
  STA bulletFrame+2
  LDA #BULLETNOFL             ; TL and TR no flip
  STA bulletAttr+0
  STA bulletAttr+1
  LDA #BULLETFLXY             ; BL and BR flip xy
  STA bulletAttr+2
  STA bulletAttr+3
  JMP ApplyBulletSettings

AssignBulletAnim2:
  LDA #BULLFRAME3             ; Assign frame 3 to all tiles
  STA bulletFrame+0
  STA bulletFrame+1
  STA bulletFrame+2
  STA bulletFrame+3
  LDA #BULLETNOFL             ; TL no flip
  STA bulletAttr+0
  LDA #BULLETFLX              ; TR flip x
  STA bulletAttr+1
  LDA #BULLETFLY              ; BL flip y
  STA bulletAttr+2
  LDA #BULLETFLXY             ; BR flip xy
  STA bulletAttr+3
  JMP ApplyBulletSettings

AssignBulletAnim3:
  LDA #BULLFRAME2             ; Assign frame 2 to TL and BR tiles
  STA bulletFrame+0
  STA bulletFrame+3
  LDA #BULLFRAME1             ; Assign frame 1 to TR and BL tiles
  STA bulletFrame+1
  STA bulletFrame+2
  LDA #BULLETFLX              ; TL and TR flip x
  STA bulletAttr+0
  STA bulletAttr+1
  LDA #BULLETFLY              ; BL and BR flip y
  STA bulletAttr+2
  STA bulletAttr+3
  JMP ApplyBulletSettings

; Takes pre-filled bullet frames and attributes and applies them to the
; current bullet pointer
ApplyBulletSettings:
  LDX #$00                    ; Starts our loop at 0
.loop:
  LDA bulletFrame, X
  LDY #SPRITETIL              ; Assign tile
  STA [pointerLo], Y
  LDY #SPRITEATT              ; Assign attributes
  LDA bulletAttr, X
  STA [pointerLo], Y
  LDA pointerLo               ; Increment pointer by 4 bytes to next sprite
  CLC
  ADC #$04
  STA pointerLo
  INX
  CPX #$04                    ; Check whether we're done with the loop
  BNE .loop
  JMP IncrementBulletLoop

UpdateEnemies:
  LDA #SPRITEHI
  STA pointerHi
  LDA #ENEMY0
  STA pointerLo
  LDA #0
  STA arg0
.loop:
  LDX #2
  JSR UpdateSpriteLayout
  INC arg0
  LDA arg0
  CMP #ENEMYCOUNT
  BNE .loop
  RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Utils

; Set up pointerHi and lo for the sprite to cache,
; puts its current pixel values in:
; spriteLayoutOriginY
; spriteLayoutOriginX
StoreSpritePosition:
  LDY #0                      ; pixel Y
  LDA [pointerLo], Y
  STA spriteLayoutOriginY
  LDY #SPRITEX                ; pixel X
  LDA [pointerLo], y
  STA spriteLayoutOriginX
  RTS

; Sets all sprites to hidden for a given layout
; Expects pointerLo to be set for the top left sprite address
; Expects X to be set to sprite height in tiles
; Expects all sprites to be 2 tiles wide
HideSpriteLayout:
  LDA #$FF                    ; Store "offscreen" FF in sprite y
  LDY #$00                    ; Store sprite y origin offset
  STA [pointerLo], Y          ; Set Y's
  LDY #$04                    ; Next sprite's Y
  STA [pointerLo], Y
  JSR MovePointerOneRow       ; Increment pointer for next loop
  DEX
  BNE HideSpriteLayout        ; Continue loop for next row
  RTS

; Update sprite layout for a group of sprites
; Expects pointerLo to be set to the top left sprite address
; Expects X to be set to sprite height in tiles
; Expects all sprites to be 2 tiles wide
UpdateSpriteLayout:
  LDY #$00                    ; Store sprite y origin
  LDA [pointerLo], Y
  STA spriteLayoutOriginY
  LDY #SPRITEX                ; Store sprite x origin
  LDA [pointerLo], Y
  STA spriteLayoutOriginX
.loop:
  LDA spriteLayoutOriginY     ; Set sprite Y's
  LDY #$00                    ; Row y0
  STA [pointerLo], Y
  LDY #$04                    ; Row y1
  STA [pointerLo], Y
  LDA spriteLayoutOriginX     ; Set X's
  LDY #SPRITEX                ; Set sprite X's
  STA [pointerLo], Y
  CLC
  ADC #TILE_WIDTH
  LDY #SPRITEX+4              ; Second sprite's X
  STA [pointerLo], Y
  JSR MovePointerOneRow       ; Increment Y for next loop
  LDA spriteLayoutOriginY     ; Increment row Y
  CLC
  ADC #TILE_WIDTH
  STA spriteLayoutOriginY
  DEX
  BNE .loop
  RTS

; In preparation of SubPixelMove, find sign on velocity
; velHi     - hi velocity
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
; pointerSub should be set up to subpixel
; velLo     - lo velocity
; velHi     - hi velocity
; velSign   - Sign of velocity
SubPixelMove:
  LDY #0                      ; Set Y to 0
  LDA velSign                 ; Check sign
  BEQ .add                    ; Positive Movement
  JMP .sub                    ; Negative Movement
.add:
  LDA [pointerSub], Y         ; Load subpixel
  CLC
  ADC velLo                   ; Add lo velocity
  STA [pointerSub], Y         ; Store subpixel
  LDY #1
  LDA [pointerSub], Y         ; Load pixel
  ADC velHi                   ; Add hi velocity with carry
  STA [pointerSub], Y         ; Store result
  RTS
.sub:
  LDA [pointerSub], Y         ; Load subpixel
  SEC
  SBC velLo                   ; Subtract lo speed
  STA [pointerSub], Y         ; Store subpixel
  LDY #1
  LDA [pointerSub], Y         ; Load pixel
  SBC velHi                   ; Subtract hi velocity with carry
  STA [pointerSub], Y         ; Store pixel
  RTS

; Test whether the coordinates for Atan2 are equal, uses the same
; args as Atan2, but does not modify them.
; A will be 0 if true, 1 if false
CoordsEqual:
  LDA arg0                    ; x1
  CMP arg1                    ; x2
  BNE .notEqual
  LDA arg2                    ; y1
  CMP arg3                    ; y2
  BNE .notEqual
  LDA #0                      ; Both are equal
  RTS
.notEqual:
  LDA #1                      ; Not equal
  RTS

; from https://codebase64.org/doku.php?id=base:8bit_atan2_8-bit_angle
; arg0 - x1
; arg1 - x2
; arg2 - y1
; arg3 - y2
; uses arg4 for octant
; A will be the 256 degree angle
Atan2:
  LDA #0
  STA arg4
  LDA arg0
  SEC
  SBC arg1
  BCS .o1
  EOR #$ff
.o1:
  TAX
  ROL arg4
  LDA arg2
  SEC
  SBC arg3
  BCS .o2
  EOR #$ff
.o2:
  TAY
  ROL arg4
  LDA log2_tab,x
  SEC
  SBC log2_tab,y
  BCC .o3
  EOR #$ff
.o3:
  TAX
  LDA arg4
  ROL a
  AND #%111
  TAY
  LDA atan_tab,x
  EOR octant_adjust,y
  RTS

; Based on the source position, find the closest enemy
; arg0 - source X
; arg1 - source Y
; Stores index in A
; Stores distance in arg7
FindClosestEnemyIndex:
  LDX #0                      ; Start count at 0
  LDA #$FF                    ; Set max distance in arg7, to check against
  STA arg7
  STA arg6                    ; Set a sentinel value of FF in result index
  LDA #ENEMY0
  STA pointerLo               ; Setup pointer for enemy
.loop:
  JSR StoreSpritePosition
  LDA spriteLayoutOriginX
  CLC
  ADC #TILE_WIDTH
  STA arg1                    ; Put enemy center X in arg1
  LDA spriteLayoutOriginY
  CLC
  ADC #TILE_WIDTH
  STA arg3                    ; Put enemy center Y in arg3
  JSR ManhattanDistance       ; Get distance in A
  CLC
  CMP arg7                    ; Compare to current lowest distance
  BCC .smallest
  JMP .count
.smallest:
  STA arg7                    ; What we have now is now smallest distance
  TXA
  STA arg6                    ; Store the new lowest index in arg6
.count:
  INX                         ; Increment X
  JSR MovePointerTwoRows      ; Move pointer to next enemy
  CPX #ENEMYCOUNT
  BNE .loop
  LDA arg6                    ; Record the lowest index we got
  RTS

; Calclulate distance between two points
; arg0 - x1
; arg1 - x2
; arg2 - y1
; arg3 - y2
; uses arg4 sum
ManhattanDistance:
;findX
  LDA arg0                    ; Load x1
  CMP arg1                    ; Compare x2
  BCC .x1less
;x2less
  SEC                         ; x1 still in A
  SBC arg1                    ; subtract x2
  STA arg4                    ; store result
  JMP .findY
.x1less:
  LDA arg1                    ; Load x2
  SEC
  SBC arg0                    ; subtract x1
  STA arg4
.findY:
  LDA arg2                    ; Load y1
  CMP arg3                    ; Compare y2
  BCC .y1less
;y2less
  SEC                         ; y1 still in A
  SBC arg3                    ; subtract y2
  JMP .sum
.y1less:
  LDA arg3                    ; Load y2
  SEC
  SBC arg2                    ; subtract y1
.sum:
  CLC                         ; y is in A
  ADC arg4                    ; add with x
  BVC .finish                 ; if we didn't overflow, we're done
  LDA #$FF                    ; Just set result to full if we overflowed
.finish:
  RTS

; Tests world collision at point
; posX            - origin x
; posY            - origin y
; tilesW          - tiles w
; tilesH          - tiles h
;
; Local:
; tilesX          - store x tile after calculating
; tilesWOriginal  - store original w
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
