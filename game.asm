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
LoadSpritesLoop:
  LDA sprites, x              ; load data from address (sprites +  x)
  STA $0200, x                ; store into RAM address ($0200 + x)
  INX
  CPX #$58                    ; Size of all sprites
  BNE LoadSpritesLoop         ; Branch to LoadSpritesLoop if loop not done

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
  LDA temp                    ; get ready to store the temp buttons
  CPY #$01                    ; Second controller?
  BEQ StoreController2
  STA buttons1
  INY                         ; Increment Y for next controller
  JMP ReadControllerLoop
StoreController2:
  STA buttons2
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Sprite Updates

UpdatePlayerSprites:
  LDX #$03                    ; Player is 3 tiles high
  LDA #SPRITEHI               ; Sprite hi bites
  STA pointerHi
  LDA #PLAYER                 ; Player low bytes
  STA pointerLo
  JSR UpdateSpriteLayout
  RTS

UpdateBullets:
  LDA #SPRITEHI               ; point the high pointer at SPRITEHI for bullets
  STA pointerHi
  LDA #%00000011
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
  LDA #BULLET0                ; point the low pointer at the first bullet
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
  LDA bulletAnim              ; Test animation state
  CLC
  ADC bulletCount             ; Add an offset of the current bullet count
  AND #BULLSTATE              ; Check the bullet state
  CMP #$03                    ; State3
  BEQ BulletState3
  CMP #$02                    ; State2
  BEQ BulletState2
  CMP #$01                    ; State1
  BEQ BulletState1
BulletState0:
  JMP AssignBulletState0
BulletState1:
  JMP AssignBulletState1
BulletState2:
  JMP AssignBulletState2
BulletState3:
  JMP AssignBulletState3

AssignBulletState0:
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

AssignBulletState1:
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

AssignBulletState2:
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

AssignBulletState3:
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
  LDA #BULLET0                ; Set pointerLo to first bullet
  STA pointerLo
  LDX #0                      ; Set up counter
  STX bulletCount
UpdateBulletPosLoop:
  ; Move bullet
  LDY #$00
  LDA [pointerLo], Y
  CLC
  ADC #SPDSLOW
  STA [pointerLo], Y
  LDY #SPRITEX
  LDA [pointerLo], Y
  SEC
  SBC #SPDSLOW
  STA [pointerLo], Y

  ; Update bullet sprites
  LDX #$02                    ; Sprite is 2 tiles high
  JSR UpdateSpriteLayout      ; Pointer is in correct spot already, and will
                              ; be incremented to next bullet by the layout
  LDX bulletCount
  INX
  STX bulletCount
  CPX #BULLETCOUNT
  BNE UpdateBulletPosLoop
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
  LDY #$00                    ; Row y0
  LDA spriteLayoutOriginY     ; Set sprite Y's
  STA [pointerLo], Y
  LDY #$04                    ; Row y1
  STA [pointerLo], Y
  ; Set X's
  LDY #SPRITEX                ; Set sprite X's
  LDA spriteLayoutOriginX
  STA [pointerLo], Y
  CLC
  ADC #TILEW
  LDY #SPRITE2X
  STA [pointerLo], Y
  ; Increment Y for next loop
  LDA pointerLo   ; Increment low address for next loop
  CLC
  ADC #$08                    ; 8 bytes for 2 tile wide sprites
  STA pointerLo
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
