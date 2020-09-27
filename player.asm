; player.asm
;   player logic

; CONSTANTS

; Input
MOVE_INPUT        = %00001111
MASK_UD           = %00001100
MASK_LR           = %00000011
REMOVE_UD         = %11110011
REMOVE_LR         = %11111100

; Gameplay
BULLETSHOOTMASK   = %00000111 ; Mask for shooting bullet tick
DODGE_ON          = %10000000 ; Whether or not the dodge bit is set
DODGE_TIME_MASK   = %01111111 ; The time bits of dodge
DODGE_TIME        = 7         ;  7/60 of a second
DODGE_COOLDOWN    = 25        ; 25/60 of a second
DAMAGE_COOLDOWN   = 20        ; 20/60 of a second
FLASH_DAMAGE      = 12
FLASH_DEATH       = 48

; Status Tiles
STATUS_BUTT_OFF   = $62
STATUS_BUTT_ON    = $63

; Arguments
playerMoveDir     = arg9

; Frames
PL_FRAME0         = $00
PL_FRAME1         = $01
PL_FRAME2         = $02
PL_FRAME3         = $03
PL_FRAME4         = $04
PL_FRAME5         = $05

; Attributes
PL_ATTRIB         = %00000000

; SUBROUTINES

CountPlayerTime:
  LDA playerDamageCooldown    ; Check if we need to count the cooldown
  BEQ .testFlash              ; No - it's 0
  DEC playerDamageCooldown    ; Yes, count down
.testFlash:
  LDA playerDamageFlash       ; Is damage flashing?
  BEQ .done                   ; No - done
  DEC playerDamageFlash
  LDA playerDamageFlash
  BEQ .clearFlash             ; Flashing is done, clear any palette
; Actively flashing
  LDA animTick
  LSR A
  AND #FRAME_MASK
  BEQ .flashOn
  LDA #$0E
  STA PALETTE_BG
  JMP .done
.flashOn:
  LDA #$15
  STA PALETTE_BG
  JMP .done
.clearFlash:
  LDA #$0E
  STA PALETTE_BG              ; Set bg back to black
.done:
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
  LDA #STATUS_Y
  STA startY
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
  LDA #STATUS_Y
  STA startY
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
  JSR StoreVelocitySign       ; Store the sign, for subpixel move and dodging
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
  JSR StoreVelocitySign       ; Store the sign, for subpixel move and dodging
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

TestPlayerSpecial:
  LDA buttons1fresh
  AND #BUTTONA
  CMP #BUTTONA
  BEQ .doPlayerSpecial
  RTS
.doPlayerSpecial:
  LDA #0
  STA playerHealth
  ; LDA #GAME_KILL
  ; STA gamestate
  ; JSR SetGameState
  ; INC debug
  ; LDA debug
  ; JSR DrawDebug
  ; LDX #6
  ; JSR AddScore
  RTS

SetPointerForPlayer:
  LDA #SPRITEHI
  STA pointerHi
  LDA #PLAYER
  STA pointerLo
  RTS

UpdatePlayerSprites:
  JSR SetPointerForPlayer
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
  JSR SetPointerForPlayer
  JSR AssignPlayerFrame1
  RTS

AssignPlayerFrame1:
  LDX #0
.loop:
  TXA
  STA spriteFrame, X
  LDA #PL_ATTRIB
  STA spriteAttr, X
  INX
  CPX #06                     ; Finished writing player
  BNE .loop
  JSR ApplyPlayerSpriteSettings
  RTS

; Takes pre-filled frames and attributes and applies them to the
; current sprite pointer
ApplyPlayerSpriteSettings:
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
  CPX #$06                    ; Check whether we're done with the loop
  BNE .loop
  RTS

PlayerTakeDamage:
  LDA playerDodge
  AND #DODGE_ON
  BNE .done                   ; If the value isn't 0, dodging is on ignore damage
  LDA playerDamageCooldown
  BNE .done                   ; If the player damage cooldown isn't 0, ignore
  LDA #DAMAGE_COOLDOWN
  STA playerDamageCooldown    ; Set the damage cooldown
  DEC playerHealth            ; Take damage
  LDA playerHealth
  BEQ .longFlash
  LDA #FLASH_DAMAGE           ; Short flash
  JMP .doFlash
.longFlash:
  LDA #FLASH_DEATH
.doFlash:
  STA playerDamageFlash
  JSR DrawPlayerHealth
.done:
  RTS
