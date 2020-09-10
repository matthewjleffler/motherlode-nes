; main.asm contains game lifecycle code
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
  LDA #$FF                    ; fill with empty bytes
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
  JSR CountAnimation
  JSR ReadControllers
  JSR TestPlayerMove
  JSR TestShootBullet
  JSR UpdatePlayerSprites
  JSR UpdateBullets
  RTS

CountAnimation:
  LDX animTick
  INX
  STX animTick
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
  ROL temp                    ; bit0 <- carry
  DEX                         ; see if this loop is done
  BNE ReadControllerButton    ; continue loop
  LDA temp                    ; Load this frame's state to calculate freshness
  EOR buttons1, y             ; EOR to get changes
  AND temp                    ; AND to only keep newly on bits
  STA buttons1fresh, y        ; Store the freshness var
  LDA temp                    ; Now store actual held state back
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
  LDY #SPRITEX                ; First check x, Left/right
  JSR TestMoveLeft
  JSR TestMoveRight
  LDY #0                      ; Then check y, Up/Down
  JSR TestMoveUp
  JSR TestMoveDown
  RTS

TestMoveLeft:
  LDA buttons1
  AND #BUTTONL
  BNE MoveLeft
  RTS
MoveLeft:
  LDA [pointerLo], Y          ; Sprite 0 x position
  JSR SubSpeed
  STA [pointerLo], Y
  RTS

TestMoveRight:
  LDA buttons1
  AND #BUTTONR
  BNE MoveRight
  RTS
MoveRight:
  LDA [pointerLo], Y
  JSR AddSpeed
  STA [pointerLo], Y
  RTS

TestMoveUp:
  LDA buttons1
  AND #BUTTONU
  BNE MoveUp
  RTS
MoveUp:
  LDA [pointerLo], Y
  JSR SubSpeed
  STA [pointerLo], Y
  RTS

TestMoveDown:
  LDA buttons1
  AND #BUTTOND
  BNE MoveDown
  RTS
MoveDown:
  LDA [pointerLo], Y
  JSR AddSpeed
  STA [pointerLo], Y
  RTS

AddSpeed:
  TAX                         ; Put sprite location A in x
  LDA buttons1
  AND #BUTTONB                ; Are we holding B?
  CLC
  BNE AddSpeedFast
; AddSpeedSlow
  TXA                         ; Move sprite location back to A
  ADC #SPDSLOW                ; Add slow
  RTS
AddSpeedFast:
  TXA                         ; Move sprite location back to A
  ADC #SPDFAST                ; Add fast
  RTS

SubSpeed:
  TAX                         ; Put sprite location A in x
  LDA buttons1
  AND #BUTTONB                ; Are we holding B?
  SEC
  BNE SubSpeedFast
; SubSpeedSlow
  TXA                         ; Move sprite location back to A
  SBC #SPDSLOW                ; Sub slow
  RTS
SubSpeedFast:
  TXA                         ; Move sprite location back to A
  SBC #SPDFAST                ; Sub fast
  RTS

TestShootBullet:
  LDA buttons1fresh
  AND #BUTTONA
  BNE ShootBullet
  RTS
ShootBullet:
  ; TODO set pos, etc
  ; TODO button fresh
  LDX #BULLETCOUNT
  LDY #$00                    ; Flag for whether or not we shot, and pointer
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
  LDA #SPRITEHI               ; Setup pointers for player
  STA pointerHi
  LDA #PLAYER
  STA pointerLo
  LDA [pointerLo], Y          ; Store player Y
  STA spriteLayoutOriginY
  LDY #SPRITEX
  LDA [pointerLo], Y          ; Store player X
  STA spriteLayoutOriginX
  LDA #PBULLET0               ; Point at selected bullet
  CLC
  ADC bulletCount
  STA pointerLo
  LDY #$00
  LDA spriteLayoutOriginY
  STA [pointerLo], Y          ; Apply player Y
  LDY #SPRITEX
  LDA spriteLayoutOriginX
  STA [pointerLo], Y          ; Apply player X
  LDY #$01                    ; Mark that we've already shot
NextBullet:
  LDA bulletCount
  CLC
  ADC #$10                    ; Move to next bullet pointer
  STA bulletCount
  DEX                         ; Decrement counter
  CPX #0
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

UpdateBullets:
  LDA #SPRITEHI               ; point the high pointer at SPRITEHI for bullets
  STA pointerHi
  LDA #STATEMASK
  AND animTick
  BEQ UpdateBulletAnim        ; Animate every 4 ticks, if the AND is 0
  JSR UpdateBulletPos
  RTS

UpdateBulletAnim:
  LDX bulletAnim              ; Increment bullet anim
  INX
  STX bulletAnim
  LDX #0                      ; Start the bullet count
  STX bulletCount
  LDA #PBULLET0               ; point the low pointer at the first bullet
  STA pointerLo
UpdateBulletAnimLoop:
  JSR UpdateSingleBullet
  LDX bulletCount             ; Count that we've updated a bullet
  INX
  STX bulletCount
  CPX #BULLETCOUNT
  BNE UpdateBulletAnimLoop    ; Update next bullet, pointerLo has been updated
                              ; to the correct spot by applying bullet attr
  RTS

UpdateSingleBullet:
  JSR GetBulletState          ; Get bullet state
  CMP #BULL_EXP               ; Exploding
  BEQ BulletStateExploding
  CMP #BULL_MOV               ; Animating and moving
  BEQ BulletTravel

; BulletStateOff              ; Bullet is already off, and hidden
  JSR MovePointerTwoRows      ; Move pointer to next bullet
  RTS

; Bullet is dead, hide it, then set state to off
HideBullet:
  LDA #BULL_OFF               ; Set state to off
  STA temp
  JSR SetBulletState
  LDX #02                     ; Sprite is 2 tiles high
  JSR HideSpriteLayout        ; Hide sprite, pointer is advanced
  RTS

; Bullet is exploding, animate the explosion and then set state to off
BulletStateExploding:
  ; TODO - for now just move the pointer ahead for the next bullet
  ; TODO - animate exploding
  JSR HideBullet
  RTS

BulletTravel:
  LDA bulletAnim              ; Test animation state
  CLC
  ADC bulletCount             ; Add an offset of the current bullet count
  AND #STATEMASK              ; Check the bullet state
  CMP #$03                    ; State3
  BEQ BulletAnim3
  CMP #$02                    ; State2
  BEQ BulletAnim2
  CMP #$01                    ; State1
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
  JSR ApplyBulletSettings
  RTS

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
  JSR ApplyBulletSettings
  RTS

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
  JSR ApplyBulletSettings
  RTS

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
  JSR ApplyBulletSettings
  RTS

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
  RTS

UpdateBulletPos:
  LDA #SPRITEHI               ; Set pointer to sprite hi
  STA pointerHi
  LDA #PBULLET0               ; Set pointerLo to first bullet
  STA pointerLo
  LDX #0                      ; Set up counter
  STX bulletCount
UpdateBulletPosLoop:
  JSR GetBulletState          ; Get bullet state
  CMP #BULL_MOV
  BEQ MoveBullet
; No Change
  JSR MovePointerTwoRows      ; Move pointer to next bullet
  JMP IncrementBulletPosLoop

MoveBullet:
  ; Move bullet
  ; TODO move direction
  LDY #$00
  LDA [pointerLo], Y
  CLC
  ADC #SPDBULLET
  STA [pointerLo], Y
  STA spriteLayoutOriginY
  LDY #SPRITEX
  LDA [pointerLo], Y
  SEC
  SBC #SPDBULLET
  STA [pointerLo], Y
  STA spriteLayoutOriginX
  ; TODO test collision
  ; Test Bounds
  LDA spriteLayoutOriginX     ; Are we within the bullet edge x?
  CMP #BULLETEDGE
  BCC BulletLeftEdge
  LDA spriteLayoutOriginY     ; Are we within the bullet edge y?
  CMP #BULLETEDGE
  BCC BulletLeftEdge
  JMP UpdateBulletSprites     ; Normal sprite update
BulletLeftEdge:
  JSR HideBullet
  JMP IncrementBulletPosLoop
  ; TODO why do bullet bottoms stick around? They should be hidden
UpdateBulletSprites:
  LDX #$02                    ; Sprite is 2 tiles high
  JSR UpdateSpriteLayout      ; Pointer is in correct spot already, and will
                              ; be incremented to next bullet by the layout
IncrementBulletPosLoop:
  LDX bulletCount
  INX
  STX bulletCount
  CPX #BULLETCOUNT
  BNE UpdateBulletPosLoop
  RTS

; Sets all sprites to hidden for a given layout
; Expects pointerLo to be set for the top left sprite address
; Expects X to be set to sprite height in tiles
; Expects all sprites to be 2 tiles wide
HideSpriteLayout:
  LDA #$FE                    ; Store "offscreen" FF in sprite y
HideSpriteLoop:
  LDY #$00                    ; Store sprite y origin offset
  STA [pointerLo], Y          ; Set Y's
  LDY #SPRITEX
  STA [pointerLo], Y
  LDY #$04                    ; Next sprite's Y
  STA [pointerLo], Y
  LDY #SPRITEX+4
  STA [pointerLo], Y
  JSR MovePointerOneRow       ; Increment pointer for next loop
  DEX
  BNE HideSpriteLoop          ; Continue loop for next row
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
  ; Set Y's
  LDA spriteLayoutOriginY     ; Set sprite Y's
  LDY #$00                    ; Row y0
  STA [pointerLo], Y
  LDY #$04                    ; Row y1
  STA [pointerLo], Y
  ; Set X's
  LDA spriteLayoutOriginX
  LDY #SPRITEX                ; Set sprite X's
  STA [pointerLo], Y
  CLC
  ADC #TILEW
  LDY #SPRITEX+4              ; Second sprite's X
  STA [pointerLo], Y
  ; Increment Y for next loop
  JSR MovePointerOneRow
  LDA spriteLayoutOriginY     ; Increment row Y
  CLC
  ADC #TILEW
  STA spriteLayoutOriginY
  DEX
  BNE UpdateSpriteLoop
  RTS

; 8-bit multiply
; by Bregalad
; Enter with A,Y, numbers to multiply
; Output with YA = 16-bit result (X is unchanged)
; Multiply:
;   STY multFactor              ; Store input factor
;   LDY #$00
;   STY multRes1                ; Clear result
;   STY multRes2
;   LDY #$08                    ; Number of shifts needed
; MultNeg:
;   LSR A                       ; Shift right input number
;   BCC MultPos                 ; Check if bit is set
;   PHA
;   LDA multRes2
;   CLC
;   ADC multFactor
;   STA multRes2                ; If so add number to result
;   PLA
; MultPos:
;   LSR multRes2                ; Shift result right
;   ROR multRes1
;   DEY
;   BNE MultNeg
;   LDA multRes1
;   LDY multRes2
;   RTS
