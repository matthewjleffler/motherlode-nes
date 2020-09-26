; game.asm
;   game lifecycle

  .include "init.asm"
  .include "player.asm"
  .include "playerbullet.asm"
  .include "enemy.asm"
  .include "enemybullet.asm"
  .include "utils.asm"
  .include "math.asm"

NMI:                          ; NMI frame interrupt
  JSR UpdateBackground
  LDA #$00                    ; Copy sprites through DMA
  STA $2003                   ; set the low byte (00) of the RAM address
  LDA #$02                    ; set the high byte (02) of the RAM address
  STA $4014                   ; start the transfer
  JSR reenableppu

  ; Reset loop content
  LDA #0                      ; Clear background update buffer, and count
  STA backgroundBuffer
  STA bufferUpdateIndex
  INC animTick                ; Increment animation tick
  STA scoreChanged
  JSR ReadControllers

  ; Check game state
  LDA gamestate
  CMP #GAME_PAUSE
  BEQ .statePause
  CMP #GAME_KILL
  BEQ .stateKill
  CMP #GAME_RUN
  BEQ .stateRun
; title
  JSR .titleLoop
  JMP .endLoop
.stateKill:
  ; TODO content
  JMP .endLoop
.stateRun:
  JSR .gameLoop
  JMP .endLoop
.statePause:
  ; TODO content
  JMP .endLoop

.endLoop:
  LDA #0                      ; Add a trailing 0 to the end of the background
  LDX bufferUpdateIndex       ;  update buffer
  STA backgroundBuffer, X
  RTI                         ; Return from interrupt

.titleLoop:
  LDA buttons1fresh
  AND #BUTTONSTA
  CMP #BUTTONSTA
  BEQ .startGame
  RTS
.startGame:
  LDA animTick                ; Init RNG with anim tick
  STA seed
  LDA #PLAYER_SPAWN_X         ; Set up player spawn position
  STA playerPosX+1
  LDA #PLAYER_SPAWN_Y
  STA playerPosY+1
  LDA #SPAWN_MIN_TICKS        ; Set up initial enemy spawn timer
  STA enemySpawnTimer
  LDA #GAME_RUN
  STA gamestate
  JSR SetGameState
  RTS

.gameLoop:
  JSR TestPlayerMove
  JSR UpdatePlayerSprites
  JSR TestPlayerSpecial
  JSR TestPlayerShootBullet
  JSR UpdatePlayerBullets
  JSR TestSpawnEnemies
  JSR UpdateEnemies
  JSR UpdateEnemyBullets
  JSR DrawScoreUpdate
  RTS
