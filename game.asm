; game.asm
;   game lifecycle

  .include "init.asm"
  .include "player.asm"
  .include "playerbullet.asm"
  .include "enemy.asm"
  .include "enemybullet.asm"
  .include "utils.asm"
  .include "math.asm"

; CONSTANTS

PLAYER_SPAWN_X    = $80
PLAYER_SPAWN_Y    = $80
STARTX            = 11
STARTY            = 20
STARTLEN          = 11        ; PRESS START text 11 tiles

; SUBROUTINES

NMI:                          ; NMI frame interrupt
  JSR UpdatePalettes
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
  CMP #GAME_RUN
  BEQ .stateRun
; title / kill
  JSR .titleLoop
  JMP .endNMI
.stateRun:
  JSR .gameLoop
  JMP .endNMI
.statePause:
  JSR .pauseLoop
  JMP .endNMI

.endNMI:
  LDA #0                      ; Add a trailing 0 to the end of the background
  LDX bufferUpdateIndex       ;  update buffer
  STA backgroundBuffer, X
  RTI                         ; Return from interrupt

.titleLoop:
  JSR .testStartPressed
  CMP #1
  BEQ .startGame
  ; TODO blink press start
  LDA #STARTX
  STA startX
  LDA #STARTY
  STA startY
  LDA #STARTLEN
  STA len
  JSR StartBackgroundUpdate
  LDX #0
.drawStartText:
  LDA pressStartText, X
  JSR AddBackgroundByte
  INX
  CPX #STARTLEN
  BNE .drawStartText
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
  LDA #0
  STA playerBulletStates      ; Clear player bullet states
  STA enemyBulletStates       ; Clear enemy bullet states lo
  STA enemyBulletStates+1     ; Clear enemy bullet states hi
  STA playerDodge             ; Clear player dodge
  LDX #0
.loopClearEnemy:
  STA enemyState, X
  INX
  CPX #ENEMYCOUNT
  BNE .loopClearEnemy
  LDX #0
.loopClearScore:
  STA score, X
  INX
  CPX #SCOREPLACES
  BNE .loopClearScore
; Set actual game state
  LDA #GAME_RUN
  STA gamestate
  JSR SetGameState
; Now that nametable is set, do some clearing
; Clear buttons
  LDA #3
  STA len
  LDA #STATUS_Y
  STA startY
  LDA #1
  STA startX
  JSR StartBackgroundUpdate
  LDA #STATUS_BUTT_ON
  JSR AddBackgroundByte
  LDA #CLEAR_TILE
  JSR AddBackgroundByte
  LDA #STATUS_BUTT_ON
  JSR AddBackgroundByte
; TODO clear health bar
  RTS

.gameLoop:
  JSR .testStartPressed
  CMP #1
  BNE .testPlayerHealth
  LDA #GAME_PAUSE
  STA gamestate
  JSR SetGameState
  JSR DarkenPalette
  RTS
.testPlayerHealth:
  LDA buttons1fresh           ; TODO actual player health
  AND #BUTTONSEL
  CMP #BUTTONSEL
  BNE .runGameLoop
  LDA #GAME_KILL
  STA gamestate
  JSR SetGameState
  RTS
.runGameLoop:
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

.pauseLoop:
  JSR .testStartPressed
  CMP #1
  BNE .endPauseLoop
  LDA #GAME_RUN
  STA gamestate
  JSR SetGameState
  JSR SetDefaultPalette
.endPauseLoop:
  RTS

; Returns 1 in A, otherwise 0
.testStartPressed:
  LDA buttons1fresh
  AND #BUTTONSTA
  CMP #BUTTONSTA
  BEQ .startPressed
  LDA #0
  RTS
.startPressed:
  LDA #1
  RTS
