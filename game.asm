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
FADESTEPS         = 6
FADETIME          = 5

; SUBROUTINES

NMI:                          ; NMI frame interrupt
  JSR UpdatePalettes
  JSR UpdateBackground
  LDA #$00                    ; Copy sprites through DMA
  STA $2003                   ; set the low byte (00) of the RAM address
  LDA #$02                    ; set the high byte (02) of the RAM address
  STA $4014                   ; start the transfer
  JSR reenableppu
  JSR SoundPlayFrame

  ; Reset loop content
  LDA #0                      ; Clear background update buffer, and count
  STA backgroundBuffer
  STA bufferUpdateIndex
  STA scoreChanged
  JSR .countTimers
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

.countTimers:
  INC animTick                ; Increment animation tick
  LDA monochromeTime
  BNE .countmono
  JMP .endCount
.countmono:
  DEC monochromeTime
  LDA monochromeTime
  BEQ .endmono
  AND #FRAME_MASK
  STA monochrome
  JMP .endCount
.endmono:
  LDA #0
  STA monochrome
  JMP .endCount
.endCount:
  RTS

.titleLoop:
  LDA fadeCount               ; Have we started fading?
  BEQ .testInput              ; No - check input
; Out transition happening
  DEC fadeTime
  LDA fadeTime
  BEQ .incrementFade
  JMP .doDarken
.incrementFade:
  LDA #FADETIME
  STA fadeTime
  INC fadeCount
  LDA fadeCount
  CMP #FADESTEPS
  BEQ .titleLoopDone
.doDarken:
  JSR SetDefaultPalette       ; Set default palette
  LDY fadeCount
.darkenLoop:
  JSR DarkenPalette
  DEY
  CPY #0
  BNE .darkenLoop
; Flicker text
  LDA animTick                ; Flicker start
  LSR A
  AND #FRAME_MASK
  BEQ .drawStartText
  JMP .clearStartText
.titleLoopDone:
  JMP .startGame
.testInput:
  JSR .testSound
  JSR .testStartPressed
  BEQ .testStartBlink         ; Didn't press start, blink the text
; Pressed start
  LDA #1
  STA fadeCount               ; Start fading
  LDA #FADETIME
  STA fadeTime
  RTS
.testStartBlink:
  LDA animTick                ; See if we're on a blink state
  JSR DivideBy32
  AND #FRAME_MASK
  BEQ .drawStartText
  JMP .clearStartText

.drawStartText:
  LDA #STARTX
  STA startX
  LDA #STARTY
  STA startY
  LDA #STARTLEN
  STA len
  JSR StartBackgroundUpdate
  LDX #0
.loopDrawStartText:
  LDA pressStartText, X
  JSR AddBackgroundByte
  INX
  CPX #STARTLEN
  BNE .loopDrawStartText
  RTS

.testSound:
  LDA buttons1fresh
  AND #BUTTONA
  CMP #BUTTONA
  BEQ .startSound
  RTS
.startSound:
  LDA #8
  JSR SoundLoad
  RTS

.clearStartText:
  LDA #STARTX
  STA startX
  LDA #STARTY
  STA startY
  LDA #STARTLEN
  STA len
  JSR StartBackgroundUpdate
  LDX #0
.loopClearStartText:
  LDA #CLEAR_TILE
  JSR AddBackgroundByte
  INX
  CPX #STARTLEN
  BNE .loopClearStartText
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
  STA playerAbility           ; Clear player ability timer
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
  LDA #PLAYER_HEALTH
  STA playerHealth            ; Set player health
  JSR DrawPlayerHealth
  LDA #FADESTEPS              ; Set up fade in
  STA fadeCount
  LDA #FADETIME
  STA fadeTime
  RTS

.gameLoop:
  LDA fadeCount               ; If we're fading in, do a more limited update
  BNE .fadeInLoop             ; Yes - loop
  LDA playerHealth
  BEQ .waitDeadLoop
  ; Not fading, do normal update
  JSR .testStartPressed       ; Pause pressed
  BEQ .runGameLoop            ; No, do normal update
  LDA #GAME_PAUSE             ; Yes, switch to pause
  STA gamestate
  JSR SetGameState
  JSR DarkenPalette
  RTS

.runGameLoop:
  JSR CountPlayerTime
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

.waitDeadLoop:
  JSR CountPlayerTime
  LDA playerDamageFlash       ; Don't progress until flashing is done
  BNE .wait                   ; Not yet, wait
  LDA #GAME_KILL              ; Equal to 0, kill player
  STA gamestate
  JSR SetGameState
.wait:
  RTS

.fadeInLoop:
  DEC fadeTime
  BNE .fadeInPalette
  ; Timer ran out, decrement fade count
  DEC fadeCount
  LDA fadeCount
  BEQ .fadeInDone             ; Are we done fading in?
  LDA #FADETIME               ; No, count another step
  STA fadeTime
  JMP .fadeInPalette
.fadeInDone:
  JSR SetDefaultPalette       ; Restore any palette changes
  RTS                         ; Done with the fade entirely, next loop is update
.fadeInPalette:
  JSR SetDefaultPalette
  LDY fadeCount
.fadeInDarken:
  JSR DarkenPalette
  DEY
  CPY #0
  BNE .fadeInDarken
  ; Limited update
  JSR UpdatePlayerSprites
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
