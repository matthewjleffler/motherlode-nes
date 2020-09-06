; Vblank setup wait
vblankwait:
  BIT $2002
  BPL vblankwait
  RTS

RESET:
  SEI             ; disable IRQs
  CLD             ; disable decimal mode
  LDX #$40
  STX $4017       ; disable APU frame IRQ
  LDX #$FF
  TXS             ; Set up stack
  INX             ; now X = 0
  STX $2000       ; disable NMI
  STX $2001       ; disable rendering
  STX $4010       ; disable DMC IRQs

  JSR vblankwait  ; First vblankwait

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

  JSR vblankwait  ; Second vblankwait, PPU is ready now

LoadPalettes:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$3F
  STA $2006             ; write the high byte of $3F00 address
  LDA #$00
  STA $2006             ; write the low byte of $3F00 address
  LDX #$00              ; start out at 0
LoadPalettesLoop:
  LDA palette, x        ; load data from address (palette + the value in x)
                          ; 1st time through loop it will load palette+0
                          ; 2nd time through loop it will load palette+1
                          ; 3rd time through loop it will load palette+2
                          ; etc
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$20              ; Compare X to hex $10, decimal 16 - copying 16 bytes = 4 sprites
  BNE LoadPalettesLoop  ; Branch to LoadPalettesLoop if compare was Not Equal to zero
                        ; if compare was equal to 32, keep going down



LoadSprites:
  LDX #$00              ; start at 0
LoadSpritesLoop:
  LDA sprites, x        ; load data from address (sprites +  x)
  STA $0200, x          ; store into RAM address ($0200 + x)
  INX                   ; X = X + 1
  CPX #$10              ; Compare X to hex $10, decimal 16
  BNE LoadSpritesLoop   ; Branch to LoadSpritesLoop if compare was Not Equal to zero
                        ; if compare was equal to 16, keep going down



LoadBackground:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$20
  STA $2006             ; write the high byte of $2000 address
  LDA #$00
  STA $2006             ; write the low byte of $2000 address

  LDA #$00
  STA pointerLo       ; put the low byte of the address of background into pointer
  LDA #HIGH(background)
  STA pointerHi       ; put the high byte of the address into pointer

  LDX #$00            ; start at pointer + 0
  LDY #$00
OutsideLoop:

InsideLoop:
  LDA [pointerLo], y  ; copy one background byte from address in pointer plus Y
  STA $2007           ; this runs 256 * 4 times

  INY                 ; inside loop counter
  CPY #$00
  BNE InsideLoop      ; run the inside loop 256 times before continuing down

  INC pointerHi       ; low byte went 0 to 256, so high byte needs to be changed now

  INX
  CPX #$04
  BNE OutsideLoop     ; run the outside loop 256 times before continuing down




  LDA #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  STA $2000

  LDA #%00011110   ; enable sprites, enable background, no clipping on left side
  STA $2001

Forever:
  JMP Forever     ;jump back to Forever, infinite loop


NMI:
  LDA #$00
  STA $2003       ; set the low byte (00) of the RAM address
  LDA #$02
  STA $4014       ; set the high byte (02) of the RAM address, start the transfer

  ;;This is the PPU clean up section, so rendering the next frame starts properly.
  LDA #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  STA $2000
  LDA #%00011110   ; enable sprites, enable background, no clipping on left side
  STA $2001
  LDA #$00        ;;tell the ppu there is no background scrolling
  STA $2005
  STA $2005
  ;;Graphics done

  ; Game Loop

  JSR ReadController1
  JSR TestMoveLeft
  JSR TestMoveRight
  JSR TestMoveUp
  JSR TestMoveDown

  JSR UpdateSpritePos

  RTI             ; Return from interrupt

LatchController:
  LDA #$01
  STA CONTROLLER1
  LDA #$00
  STA CONTROLLER1 ; tell both the controllers to latch buttons
  LDX #$08        ; set up counter
  RTS

ReadController1:
  JSR LatchController
ReadControllerLoop1:
  LDA CONTROLLER1
  LSR A           ; bit0 -> carry
  ROL buttons     ; bit0 <- carry
  DEX
  BNE ReadControllerLoop1
  RTS

TestMoveLeft:
  LDA buttons
  AND #BUTTONL
  BNE MoveLeft
  RTS
MoveLeft:
  LDA $0203       ; Sprite 0 x position
  JSR SubSpeed
  STA $0203
  RTS

TestMoveRight:
  LDA buttons
  AND #BUTTONR
  BNE MoveRight
  RTS
MoveRight:
  LDA $0203       ; Sprite 0 x position
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
  TAX           ; Put sprite location A in x
  LDA buttons
  AND #BUTTONB  ; Are we holding B?
  CLC
  BNE AddSpeedFast
; AddSpeedSlow
  TXA           ; Move sprite location back to A
  ADC #SLOW     ; Add slow
  RTS
AddSpeedFast:
  TXA           ; Move sprite location back to A
  ADC #FAST     ; Add fast
  RTS

SubSpeed:
  TAX           ; Put sprite location A in x
  LDA buttons
  AND #BUTTONB  ; Are we holding B?
  SEC
  BNE SubSpeedFast
; AddSpeedSlow
  TXA           ; Move sprite location back to A
  SBC #SLOW     ; Sub slow
  RTS
SubSpeedFast:
  TXA           ; Move sprite location back to A
  SBC #FAST     ; Sub fast
  RTS


UpdateSpritePos:
  ; Sprite y
  LDA $0200       ; Sprite 0 y
  STA $0204       ; Sprite 1 y
  CLC
  ADC #$08        ; Add 1 tile
  STA $0208       ; Sprite 2 y
  STA $020C       ; Sprite 3 y
  ; Sprite x
  LDA $0203       ; Sprite 0 x
  STA $020B       ; Sprite 2 x
  CLC
  ADC #$08        ; Add 1 tile
  STA $0207       ; Sprite 1 x
  STA $020F       ; Sprite 3 x
  RTS

  ; sprite 2 is x, y + 8
  ; sprite 3 is x + 8, y + 8