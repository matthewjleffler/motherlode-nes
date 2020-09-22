; game.asm contains game lifecycle code
;     NMI interrupt
;     Game loop
; Note: variables and constants defined in vars.asm
;       tables and sprites are in tables.asm

  .include "init.asm"
  .include "player.asm"
  .include "playerbullet.asm"
  .include "enemy.asm"
  .include "utils.asm"
  .include "math.asm"

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
  JSR TestPlayerShootBullet
  JSR UpdatePlayerBullets
  JSR TestSpawnEnemies
  JSR UpdateEnemies
  JSR DrawScoreUpdate
  ; Add a trailing 0 to the end of the background update buffer
  LDA #0
  LDX bufferUpdateIndex
  STA backgroundBuffer, X
  RTS
