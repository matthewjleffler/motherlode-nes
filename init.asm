; init.asm
;   init and reset code

; CONSTANTS

PLAYER_SPAWN_X    = $80
PLAYER_SPAWN_Y    = $80
PLAYERSIZE        = 6 * 4     ; player byte size
BG_HI             = $20       ; hi pointer to background table addresses

; SUBROUTINES

; RESET and init

vblankwait:                   ; VBLANK reset wait
  BIT $2002
  BPL vblankwait
  RTS

reenableppu:
  LDA #%10010000              ; enable NMI, sprites from Pattern Table 0,
                              ; background from Pattern Table 1
  ORA nametable               ; Inclusive or with nametable variable
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
  LDA #HIGH(gameplayBackground)
  STA pointerHi               ; put the high byte of the address into pointer
  LDX #$00                    ; start at pointer + 0
  LDY #$00
.loopX:
.loopY:
  LDA [pointerLo], y          ; copy one background byte from address in + Y
  STA $2007                   ; this runs 256 * 4 times
  INY                         ; inside loop counter
  CPY #$00
  BNE .loopY                  ; run the inside loop 256 times before continuing
  ; End inner loop
  INC pointerHi               ; low byte wrapped, increment hi byte
  INX
  CPX #$08                    ; 4 X loops, covers nametable 1 and 2
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
