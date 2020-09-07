; main.asm contains game lifecycle code
;     RESET and init
;     NMI interrupt
;     Game loop
; Note: variables and constants defined in vars.asm

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
  CPX #$20                    ; Copying 16 sprite + 16 background palette bytes
  BNE LoadPalettesLoop        ; Branch to LoadPalettesLoop if loop not done

LoadSprites:
  LDX #$00                    ; start at 0
LoadSpritesLoop:
  LDA sprites, x              ; load data from address (sprites +  x)
  STA $0200, x                ; store into RAM address ($0200 + x)
  INX
  CPX #$28                    ; Compare X to hex $18, decimal 28
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
  JSR ReadController1
  JSR TestMoveLeft
  JSR TestMoveRight
  JSR TestMoveUp
  JSR TestMoveDown
  JSR UpdatePlayerSprites
  JSR UpdateBullets
  RTS

CountAnimation:
  LDX animTick
  INX
  STX animTick
  RTS

; Controller reading
LatchController:
  LDA #$01
  STA CONTROLLER1
  LDA #$00
  STA CONTROLLER1             ; tell both the controllers to latch buttons
  LDX #$08                    ; set up counter
  RTS

ReadController1:
  JSR LatchController
ReadControllerLoop1:
  LDA CONTROLLER1
  LSR A                       ; bit0 -> carry
  ROL buttons                 ; bit0 <- carry
  DEX
  BNE ReadControllerLoop1
  RTS

TestMoveLeft:
  LDA buttons
  AND #BUTTONL
  BNE MoveLeft
  RTS
MoveLeft:
  LDA $0203                   ; Sprite 0 x position
  JSR SubSpeed
  STA $0203
  RTS

TestMoveRight:
  LDA buttons
  AND #BUTTONR
  BNE MoveRight
  RTS
MoveRight:
  LDA $0203                   ; Sprite 0 x position
  JSR AddSpeed
  STA $0203
  RTS

TestMoveUp:
  LDA buttons
  AND #BUTTONU
  BNE MoveUp
  RTS
MoveUp:
  LDA $0200
  JSR SubSpeed
  STA $0200
  RTS

TestMoveDown:
  LDA buttons
  AND #BUTTOND
  BNE MoveDown
  RTS
MoveDown:
  LDA $0200
  JSR AddSpeed
  STA $0200
  RTS

AddSpeed:
  TAX                         ; Put sprite location A in x
  LDA buttons
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
  LDA buttons
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

UpdatePlayerSprites:
  ; Sprite y
  LDA PLAYER0                 ; Sprite 0 y
  STA PLAYER1                 ; Sprite 1 y
  CLC
  ADC #TILEW                  ; Add 1 tile
  STA PLAYER2                 ; Sprite 2 y
  STA PLAYER3                 ; Sprite 3 y
  ADC #TILEW                  ; Add 1 more tile
  STA PLAYER4                 ; Sprite 4 y
  STA PLAYER5                 ; Sprite 5 y
  ; Sprite x
  LDX #SPRITEX                ; x coord address offset
  LDA PLAYER0, X              ; Sprite 0 x
  STA PLAYER2, X              ; Sprite 2 x
  STA PLAYER4, X              ; Sprite 4 x
  CLC
  ADC #TILEW                  ; Add 1 tile
  STA PLAYER1, X              ; Sprite 1 x
  STA PLAYER3, X              ; Sprite 3 x
  STA PLAYER5, X              ; Sprite 5 x
  RTS

UpdateBullets:
  LDA #%00000011              ; Animate every other tick
  AND animTick
  BEQ UpdateBulletAnim
  JSR UpdateBulletPos
  RTS
UpdateBulletAnim:
  ; Update anim
  LDX bulletAnim0             ; Increment bullet anim
  INX
  STX bulletAnim0
  LDX #SPRITEATT              ; Sprite attrib offset in X
  LDY #SPRITETIL              ; Sprite tile offset in Y
  ; Bullet0
  LDA #BULLSTATE              ; Compare the first two bits
  AND bulletAnim0
  CMP #$03                    ; State3
  BEQ BulletState3
  CMP #$02                    ; State2
  BEQ BulletState2
  CMP #$01                    ; State1
  BEQ BulletState1
  ; TODO move down the line for other bullets
; BulletState0
  ; Sprite0, 4x
  LDA #BULLFRAME0
  STA BULLET00, Y             ; Store tiles
  STA BULLET01, Y
  STA BULLET02, Y
  STA BULLET03, Y
  ; Flip on 1,2,3
  JSR BulletNoFlip
  RTS
BulletState1:
  ; Sprite1, Sprite2
  ; Sprite2, Sprite1
  LDA #BULLFRAME1             ; Bullet1
  STA BULLET00, Y             ; Store tiles
  STA BULLET03, Y
  LDA #BULLFRAME2             ; Bullet2
  STA BULLET01, Y
  STA BULLET02, Y
  ; Flip on bottom
  LDA #BULLETNOFL
  STA BULLET00, X
  STA BULLET01, X
  LDA #BULLETFLXY
  STA BULLET02, X
  STA BULLET03, X
  RTS
BulletState2:
  ; Sprite3, 4x
  LDA #BULLFRAME3             ; Bullet3
  STA BULLET00, Y
  STA BULLET01, Y
  STA BULLET02, Y
  STA BULLET03, Y
  ; Flip on 1,2,3
  JSR BulletNoFlip
  RTS
BulletState3:
  ; Sprite1, Sprite2
  ; Sprite2, Sprite1
  LDA #BULLFRAME2             ; Bullet2
  STA BULLET00, Y             ; Store tiles
  STA BULLET03, Y
  LDA #BULLFRAME1             ; Bullet1
  STA BULLET01, Y
  STA BULLET02, Y
  ; Flip x top, flip y bottom
  LDA #BULLETFLX
  STA BULLET00, X
  STA BULLET01, X
  LDA #BULLETFLY
  STA BULLET02, X
  STA BULLET03, X
  RTS

BulletNoFlip:
  LDA #BULLETNOFL
  STA BULLET00, X
  LDA #BULLETFLX
  STA BULLET01, X
  LDA #BULLETFLY
  STA BULLET02, X
  LDA #BULLETFLXY
  STA BULLET03, X
  RTS

UpdateBulletPos:
  ; Bullet Y
  LDA BULLET00
  CLC
  ADC #SPDBULLET
  STA BULLET00
  STA BULLET01
  ADC #TILEW
  STA BULLET02
  STA BULLET03
  ; BulletX
  LDX #SPRITEX
  LDA BULLET00, X
  SEC
  SBC #SPDSLOW
  STA BULLET00, X
  STA BULLET02, X
  CLC
  ADC #TILEW
  STA BULLET01, X
  STA BULLET03, X
  RTS
