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
LoadPalettesLoop:
  LDA palette, x              ; load data from address (palette + x)
  STA $2007                   ; write to PPU
  INX
  CPX #$20                    ; Size of all pallete bytes
  BNE LoadPalettesLoop        ; Branch to LoadPalettesLoop if loop not done

LoadSprites:
  LDX #$00                    ; start at 0
  LDA #$FF                    ; fill with FF so sprites are hidden
LoadSpriteLoop:
  STA $0200, X
  INX
  BNE LoadSpriteLoop          ; Add until we loop back to 0

; TODO move this to gameplay code
AssignPlayerSprites:
  LDA #SPRITEHI               ; setup player sprite pointer
  STA pointerHi
  LDA #PLAYER
  STA pointerLo
  LDY #$00
AssignPlayerSpriteLoop:
  LDA playersprites, Y
  STA [pointerLo], Y
  INY
  CPY #PLAYERSIZE             ; Loop until we have finished all the player bytes
  BNE AssignPlayerSpriteLoop

; TODO move this to gameplay code
AssignEnemySprites:
  LDA #SPRITEHI               ; setup enemy skeleton sprite
  STA pointerHi
  LDA #ENEMY0
  STA pointerLo
  LDY #$00
AssignEnemySpriteLoop:
  LDA skelsprites, Y
  STA [pointerLo], Y
  INY
  CPY #ENEMYSIZE
  BNE AssignEnemySpriteLoop

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
LoadBackgroundX:
LoadBackgroundY:
  LDA [pointerLo], y          ; copy one background byte from address in
                              ; pointer plus Y
  STA $2007                   ; this runs 256 * 4 times
  INY                         ; inside loop counter
  CPY #$00
  BNE LoadBackgroundY         ; run the inside loop 256 times before continuing
  ; End inner loop
  INC pointerHi               ; low byte went 0 to 256, so high byte needs to be
                              ; changed now
  INX
  CPX #$04
  BNE LoadBackgroundX         ; run the outside loop 256 times before continuing
  ; End outer loop

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
  JSR TestShootBullet
  JSR UpdatePlayerSprites
  JSR UpdateBullets
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
ReadControllerLoop:
  LDX #$08                    ; Setup counter
ReadControllerButton:
  LDA [pointerLo], Y
  LSR A                       ; bit0 -> carry
  ROL arg0                    ; bit0 <- carry
  DEX                         ; see if this loop is done
  BNE ReadControllerButton    ; continue loop
  LDA arg0                    ; Load this frame's state to calculate freshness
  EOR buttons1, y             ; EOR to get changes
  AND arg0                    ; AND to only keep newly on bits
  STA buttons1fresh, y        ; Store the freshness var
  LDA arg0                    ; Now store actual held state back
  STA buttons1, y
  INY
  CPY #$01                    ; Are we on controller 2?
  BEQ ReadControllerLoop
  RTS

TestPlayerMove:
  LDA #SPRITEHI               ; Setup pointers for player
  STA pointerHi
  LDA #PLAYER
  STA pointerLo
  ; Mask inputs to get player move direction
  LDA buttons1                ; Test up, bit in right spot
  AND #BUTTONU
  STA arg0
  LDA buttons1                ; Test right, bit in right spot
  AND #BUTTONR
  ORA arg0
  STA arg0
  LDA buttons1                ; Test down, bit needs to shift R
  AND #BUTTOND
  LSR A
  ORA arg0
  STA arg0
  LDA buttons1
  AND #BUTTONL                ; Test left, bit needs to shift L
  ASL A
  ORA arg0
  CMP #0                      ; Nothing pressed
  BEQ NoPlayerMove
  JMP DoPlayerMove
NoPlayerMove:
  RTS                         ; Done, don't apply movement
DoPlayerMove:
  LDX #0
LoopFindPlayerMoveIndex:
  CMP playerInput, X
  BEQ ApplyPlayerMove
  INX
  JMP LoopFindPlayerMoveIndex
ApplyPlayerMove:
  ; Set up velocity args for Y
  LDA playerMoveY, X
  STA arg0                    ; Velocity Lo
  LDA playerMoveY+8, X        ; 8 directions
  STA arg1                    ; Velocity Hi
  LDY #0                      ; Set Y register for sprite Y
  LDA #playerSub              ; Set pointerSub for player subpixel Y
  STA pointerSub
  JSR SubPixelMove
  ; Set up velocity args for X
  LDA playerMoveX, X
  STA arg0                    ; Velocity Lo
  LDA playerMoveX+8, X        ; 8 directions
  STA arg1                    ; Velocity Hi
  LDY #SPRITEX                ; Set Y register for sprite X
  LDA #playerSub+1            ; Set pointerSub for player subpixel X
  STA pointerSub
  JSR SubPixelMove
  RTS

TestShootBullet:
  LDA buttons1fresh
  AND #BUTTONA
  BNE ShootBullet
  RTS
ShootBullet:
  LDA #SPRITEHI               ; Setup pointers for player
  STA pointerHi
  LDA #PLAYER
  STA pointerLo
  LDY #0
  LDA [pointerLo], Y          ; Load player Y
  STA spriteLayoutOriginY     ; Store player Y
  CLC
  ADC #TILE_WIDTH             ; Center of bullet
  STA arg2                    ; Store center in arg1 for atan2
  LDY #SPRITEX                ; Load player X
  LDA [pointerLo], Y
  STA spriteLayoutOriginX     ; Store player X
  CLC
  ADC #TILE_WIDTH             ; Center of bullet
  STA arg0                    ; Store center in arg0 for atan2
  LDA #ENEMY0                 ; Setup pointers for enemy
  STA pointerLo
  LDY #0
  LDA [pointerLo], Y          ; Load enemy Y
  STA arg3                    ; Store enemy Y in arg3 (y2)
  LDY #SPRITEX
  LDA [pointerLo], Y          ; Load enemy X
  CLC
  ADC #TILE_WIDTH             ; Add half tile for enemy center
  STA arg1                    ; Store enemy X in arg1 (x2)
  ; TODO equals
  JSR Atan2                   ; Get degrees between player and enemy
  LSR A                       ; LSR 4 times to get 32 degrees
  LSR A
  LSR A
  STA arg1                    ; Store index in arg1, don't need enemy pos
  ; Now loop to find a free bullet to shoot
  LDX #0                      ; Bullet count
  LDY #0                      ; Flag for whether or not we shot, and pointer
  STY bulletCount             ; Count pointers for current bullet
FindFreeBullet:
  JSR GetBulletState
  CMP #BULL_OFF               ; If the bullet is off? Turn it on
  BNE NextBullet
  ; Found a free bullet
  CPY #$00
  BNE NextBullet              ; Have we already shot a bullet?
  LDA #BULL_MOV               ; Set the current bullet state to moving
  STA temp
  JSR SetBulletState
  ; Set bullet position
  LDA #PBULLET0               ; Point at selected bullet
  CLC
  ADC bulletCount
  STA pointerLo
  LDY #$00
  LDA spriteLayoutOriginY     ; Player Y stored earlier
  STA [pointerLo], Y          ; Apply player Y
  LDY #SPRITEX
  LDA spriteLayoutOriginX     ; Player X stored earlier
  STA [pointerLo], Y          ; Apply player X
  ; Set bullet velocity
  LDA arg1                    ; Previously saved velocity
  STA playerBulletVel, X
  ; Clear bullet subpixel
  LDA #0                      ; TODO player subpixel?
  STA playerBulletSub, X
  STA playerBulletSub+1, X
  LDY #$01                    ; Mark that we've already shot
NextBullet:
  LDA bulletCount
  CLC
  ADC #$10                    ; Move to next bullet pointer
  STA bulletCount
  INX                         ; Increment counter
  CPX #BULLETCOUNT
  BNE FindFreeBullet          ; If not 0, check next bullet, or cycle the state
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
  JSR UpdateSpriteLayout
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
  BEQ CountBulletAnim
  JMP UpdateBulletLoop
CountBulletAnim:
  INC bulletAnim              ; Increment bullet anim counter
UpdateBulletLoop:
  JSR GetBulletState          ; Temp now stores bullet state
  CMP #BULL_EXP               ; Are we exploding?
  BEQ UpdateBulletExplode
  CMP #BULL_MOV               ; Are we moving?
  BEQ UpdateBulletMove
  ; Other bullet states do nothing
  JMP IncrementBulletPointer
UpdateBulletExplode:
  JMP DoBulletExplode
UpdateBulletMove:
  JMP DoBulletMove
IncrementBulletPointer:       ; We did nothing to the bullet, move the pointer
  JSR MovePointerTwoRows      ; To the next bullet
IncrementBulletLoop:
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
  LDA [pointerLo], Y
  STA spriteLayoutOriginX
  ; TODO collision
  ; Test Bounds
  LDA spriteLayoutOriginX     ; Are we within the bullet edge X?
  CMP #BULLETEDGE
  BCC BulletLeftScreen
  CMP #BULLETEDGEW
  BCS BulletLeftScreen
  LDA spriteLayoutOriginY     ; Are we within the bullet edge Y?
  CMP #BULLETEDGE
  BCC BulletLeftScreen
  CMP #BULLETEDGEW
  BCS BulletLeftScreen
  JMP UpdateBulletLayout      ; Still on screen, normal sprite update
BulletLeftScreen:
  JMP HideBullet              ; We left the screen, bullet is dead
UpdateBulletLayout:
  LDX #$02                    ; Bullet is 2 tiles tall
  JSR UpdateSpriteLayout      ; Update sprite layout
  LDA #STATEMASK              ; Check again if we're in an anim tick, if so
  AND animTick                ; update the anim, otherwise we're done
  BEQ UpdateBulletAnim
  JMP IncrementBulletLoop
UpdateBulletAnim:
  LDA bulletFrame             ; Move pointer back to where we stored it
  STA pointerLo               ; So we can update attributes
  LDA bulletAnim              ; Load the animation state
  CLC
  ADC bulletCount             ; Add an offset of the current bullet count
  AND #STATEMASK              ; Check the bullet frame
  CMP #$03                    ; Frame 3
  BEQ BulletAnim3
  CMP #$02                    ; Frame 2
  BEQ BulletAnim2
  CMP #$01                    ; Frame 1
  BEQ BulletAnim1
;BulletAnim0
  JMP AssignBulletAnim0
BulletAnim1:
  JMP AssignBulletAnim1
BulletAnim2:
  JMP AssignBulletAnim2
BulletAnim3:
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
ApplyBulletSettingsLoop:
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
  BNE ApplyBulletSettingsLoop
  JMP IncrementBulletLoop

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
UpdateSpriteLoop:
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
  BNE UpdateSpriteLoop
  RTS

; Move subpixel based on velocity
; pointerSub should be set up to subpixel
; arg0 - lo velocity
; arg1 - hi velocity. hi bit is sign
; Local:
; arg2 - Y register for pointerLo
; arg3 - Sign
SubPixelMove:
  STY arg2                    ; Store Y register for pointerLo
  LDA arg1                    ; Load hi velocity to check sign
  AND #NEG_SIGN
  CLC
  ROL A                       ; Move it to low bit
  ROL A
  STA arg3                    ; Store sign for later
  LDA arg1                    ; Clear sign off high velocity
  AND #MOV_MASK
  STA arg1
  LDY #0                      ; Set Y to 0
  LDA arg3                    ; Now check sign
  CMP #0
  BEQ SubPixelAdd             ; Positive Movement
  JMP SubPixelSubtract        ; Negative Movement

SubPixelAdd:
  LDA [pointerSub], Y         ; Load subpixel
  CLC
  ADC arg0                    ; Add lo velocity
  STA [pointerSub], Y         ; Store subpixel
  LDY arg2                    ; Restore Y offset
  LDA [pointerLo], Y          ; Load pixel
  ADC arg1                    ; Add hi velocity with carry
  STA [pointerLo], Y          ; Store result
  RTS

SubPixelSubtract:
  LDA [pointerSub], Y         ; Load subpixel
  SEC
  SBC arg0                    ; Subtract lo speed
  STA [pointerSub], Y         ; Store subpixel
  LDY arg2                    ; Restore Y offset
  LDA [pointerLo], Y          ; Load pixel
  SBC arg1                    ; Subtract hi velocity with carry
  STA [pointerLo], Y          ; Store pixel
  RTS

; Test whether the coordinates for Atan2 are equal, uses the same
; args as Atan2, but does not modify them.
; A will be 0 if true, 1 if false
CoordsEqual:
  LDA arg0                    ; x1
  CMP arg1                    ; x2
  BNE CoordsNotEqual
  LDA arg2                    ; y1
  CMP arg3                    ; y2
  BNE CoordsNotEqual
  LDA #0                      ; Both are equal
  RTS
CoordsNotEqual:
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
  BCS *+4
  EOR #$ff
  TAX
  ROL arg4
  LDA arg2
  SEC
  SBC arg3
  BCS *+4
  EOR #$ff
  TAY
  ROL arg4
  LDA log2_tab,x
  SEC
  SBC log2_tab,y
  BCC *+4
  EOR #$ff
  TAX
  LDA arg4
  ROL a
  AND #%111
  TAY
  LDA atan_tab,x
  EOR octant_adjust,y
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

