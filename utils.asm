; utils.asm
;   utility subroutines

; CONSTANTS

; Rendering
STATUS_Y          = 3         ; y tile for status bar
DEBUG_TILE        = 19        ; X position of the debug tile rendering
SCORE_TILE        = 23        ; X position of the score places
PAUSE_TILE        = 14        ; X position of the pause text
LOW_MASK          = %00001111
BG_TABLE0         = $20
BG_TABLE1         = $24
PALETTE_SIZE      = $20
PALETTE_WIDTH     = $10       ; Palette is 16 colors wide
PALETTE_TOP       = $30       ; Top row of color, for lightening
CLEAR_TILE        = $28       ; Empty bg tile
PAUSELEN          = 5         ; PAUSE text 5 tiles
COPYX             = 3
COPYY             = 25
COPYLEN           = 19        ; Copyright coverup len
SCOREX            = 9
SCOREY            = 24
SCORELEN          = 6         ; SCORE: text 6 tiles
HEALTHX           = 5

; Controllers
CONTROLHI         = $40       ; Pointer to controller hi address
CONTROLLO         = $16       ; Pointer to consroller lo address

; Gameplay
COLLISIONMASK     = %10000000 ; Mask for collision bit
SCOREPLACES       = 8         ; Score places
ENEMY_ALIVE_STATE = 10        ; Min enemy state alive

; Movement
NEG_SIGN          = %10000000 ; Indicates negative movement
MOV_MASK          = %01111111 ; Non-sign movement

; Tiles
STATUS_HEART_OFF  = $60
STATUS_HEART_ON   = $61

; SUBROUTINES

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
.controller:
  LDX #$08                    ; Setup counter
.button:
  LDA [pointerLo], Y
  LSR A                       ; bit0 -> carry
  ROL arg0                    ; bit0 <- carry
  DEX                         ; see if this loop is done
  BNE .button                 ; continue loop
  LDA arg0                    ; Load this frame's state to calculate freshness
  EOR buttons1, y             ; EOR to get changes
  AND arg0                    ; AND to only keep newly on bits
  STA buttons1fresh, y        ; Store the freshness var
  LDA arg0                    ; Now store actual held state back
  STA buttons1, y
  INY
  CPY #$01                    ; Are we on controller 2?
  BEQ .controller
  RTS

; Sets game state
; Expects gamestate to already be set, this just applies the proper other settings
SetGameState:
  LDA #0                      ; Clear fade counters when transitioning between
  STA fadeTime                ; Game states
  STA fadeCount
  LDA gamestate
  CMP #GAME_TITLE
  BEQ .testGameTitle
  CMP #GAME_RUN
  BEQ .testGameRun
  CMP #GAME_PAUSE
  BEQ .testGamePause
  CMP #GAME_KILL
  BEQ .testGameKill
.testGameTitle:
  JMP .stateTitle
.testGameRun:
  JMP .stateRun
.testGamePause:
  JMP .statePause
.testGameKill:
  JMP .stateKill

.stateTitle:
  LDA #1
  STA nametable               ; Set nametable to title
  RTS

.stateRun:
  ; TODO clear hud - button states, score
  LDA #0
  STA nametable               ; Set nametable to game
  LDA #PAUSELEN               ; Clear Pause text
  STA len
  LDA #STATUS_Y
  STA startY
  LDA #PAUSE_TILE
  STA startX
  JSR StartBackgroundUpdate
  LDX #0
  LDA #CLEAR_TILE
.loopClearPause:
  JSR AddBackgroundByte
  INX
  CPX #PAUSELEN
  BNE .loopClearPause
; Clear hud
  LDA #1
  STA scoreChanged
  JSR DrawScoreUpdate
  RTS

.statePause:
  LDA #0
  STA nametable               ; Set nametable to game
  LDA #PAUSELEN               ; Draw pause text
  STA len
  LDA #STATUS_Y
  STA startY
  LDA #PAUSE_TILE
  STA startX
  JSR StartBackgroundUpdate
  LDX #0
.loopDrawPause:
  LDA pauseText, X
  JSR AddBackgroundByte
  INX
  CPX #PAUSELEN
  BNE .loopDrawPause
  RTS

.stateKill:
  LDA #$FF                    ; Clear value
  LDX #0
  STX animTick                ; Clear anim tick too
  STX enemyAnim               ; Used to fade in/out
.loopClearSprites:
  STA $0200, X
  INX
  BNE .loopClearSprites       ; Loop until we go back to 0
  LDA #1
  STA nametable
; Clear copyright text
  LDA #COPYLEN
  STA len
  LDA #COPYX
  STA startX
  LDA #COPYY
  STA startY
  JSR StartBackgroundUpdate
  LDA #CLEAR_TILE
  LDX #0
.loopClearCopy
  JSR AddBackgroundByte
  INX
  CPX #COPYLEN
  BNE .loopClearCopy
; Draw score text
  LDA #SCORELEN
  STA len
  LDA #SCOREX
  STA startX
  LDA #SCOREY
  STA startY
  JSR StartBackgroundUpdate
  LDX #0
.loopScoreText:
  LDA scoreKillText, X
  JSR AddBackgroundByte
  INX
  CPX #SCORELEN
  BNE .loopScoreText
; Draw score value
  LDX #SCOREPLACES            ; We're drawing all score places
  STX len
  LDA #SCOREX + SCORELEN + 1  ; We start drawing at score place
  STA startX
  LDA #SCOREY
  STA startY
  JSR StartBackgroundUpdate
.drawScoreLoop:
  DEX                         ; Decrement X
  LDA score, X                ; Draw score place into buffer, reversing order
  JSR AddBackgroundByte
  CPX #0                      ; Did we draw the last place?
  BNE .drawScoreLoop          ; No, do next place
  RTS

DrawPlayerHealth:
  LDA #PLAYER_HEALTH
  STA len
  LDA #STATUS_Y
  STA startY
  LDA #HEALTHX
  STA startX
  JSR StartBackgroundUpdate
  LDX #0
.loopDrawHealth:
  TXA
  CMP playerHealth
  BCC .drawHeartOn
  LDA #STATUS_HEART_OFF
  JMP .addByte
.drawHeartOn:
  LDA #STATUS_HEART_ON
.addByte:
  JSR AddBackgroundByte
  INX
  CPX #PLAYER_HEALTH
  BNE .loopDrawHealth
  RTS

; Sets whatever is in A to the debug value, and draws it into the score
;
; Args:
;  A                          -> debug
DrawDebug:
  STA debug                   ; Store debug value - stop score from rendering
  LDA #2
  STA len                     ; 2 characters going to end of score
  LDA #STATUS_Y
  STA startY
  LDA #DEBUG_TILE
  STA startX                  ; Tile X beginning of score
  JSR StartBackgroundUpdate
  LDA debug                   ; Load debug
  LSR A                       ; Shift bottom 4 bits off
  LSR A
  LSR A
  LSR A
  JSR AddBackgroundByte       ; Put high half of debug in first slot
  LDA debug
  AND #LOW_MASK
  JSR AddBackgroundByte       ; Put low half of debug in second slot
  RTS

; PALETTES

SetDefaultPalette:
  LDX #0
.loop:
  LDA defaultPalette, X       ; Load default palette data
  STA palette, X              ; Store into palette buffer X
  INX
  CPX #PALETTE_SIZE
  BNE .loop
  RTS

DarkenPalette:
  LDX #0
.loop:
  LDA #PALETTE_WIDTH          ; Load up palette width
  CMP palette, X              ; Are we greater than the bottom row?
  BCC .darken
  JMP .black
.darken:
  LDA palette, X
  SEC
  SBC #PALETTE_WIDTH
  CMP #$0D                    ; Bad black, don't use
  BEQ .black
  JMP .increment
.black:
  LDA #$0F
.increment:
  STA palette, X
  INX
  CPX #PALETTE_SIZE
  BNE .loop
  RTS

LightenPalette:
  LDX #0
.loop:
  LDA palette, X              ; Are we lower than the top row?
  AND #LOW_MASK               ; Check low bits
  CMP #$0E                    ; Are we in the color range?
  BCC .testLighten            ; Yes, lighten
  JMP .increment              ; No, just increment
.testLighten:
  LDA palette, X
  CMP #PALETTE_TOP
  BCC .lighten
  JMP .white
.lighten:
  CLC
  ADC #PALETTE_WIDTH
  JMP .increment
.white:
  LDA #$30                    ; White
.increment:
  STA palette, X
  INX
  CPX #PALETTE_SIZE
  BNE .loop
  RTS

UpdatePalettes:
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
  CPX #PALETTE_SIZE           ; Size of all pallete bytes
  BNE .loop                   ; Branch to LoadPalettesLoop if loop not done
  RTS

; BACKGROUND

; Applies previously setup background buffer updates
;
; Local:
;  bgLo       - low bg access byte
;  bgHi       - hi bg access byte
UpdateBackground:
  LDY #0                      ; Count through the buffer
  LDA backgroundBuffer
  BNE .loopUpdate
  RTS
.loopUpdate:
  LDX backgroundBuffer, Y     ; Length we'll count through the buffer
  CPX #0                      ; Are we at the end of the buffer?
  BNE .startDraw              ; No, draw this buffer
  RTS                         ; Yes, Done
.startDraw:
  LDA $2002                   ; Read PPU status to reset the high/low latch
  ; Calculate offset for Y and X
  LDA #0                      ; Clear background pointer
  STA bgHi
  STA bgLo
  INY                         ; Increment buffer to Y
  LDA backgroundBuffer, Y     ; Get Y tile
  STA bgLo                    ; Store it in lo
  ASL bgLo                    ; Multiply by 32, shifting bits 5 times
  ROL bgHi                    ;  over into the hi address
  ASL bgLo
  ROL bgHi
  ASL bgLo
  ROL bgHi
  ASL bgLo
  ROL bgHi
  ASL bgLo
  ROL bgHi
  INY                         ; Increment buffer to X
  LDA backgroundBuffer, Y     ; Load X offset
  CLC                         ; Now add X offset to bglo
  ADC bgLo
  STA bgLo                    ; Store new bg lo
  LDA #0
  ADC bgHi                    ; Add carry to bgHi
  STA bgHi                    ; Store new bg hi
; Check nametable
  LDA nametable               ; Check which table we're writing to
  BNE .table1                 ; We're on table 1
  LDA #BG_TABLE0              ; Table 0
  JMP .setupBuffer
.table1:
  LDA #BG_TABLE1
.setupBuffer:
  CLC
  ADC bgHi                    ; Add nametable offset to bgHi
  STA $2006                   ; Store hi byte of bg index
  LDA bgLo
  STA $2006                   ; Store lo byte of status index row
  INY                         ; Increment buffer
.loopDraw:
  LDA backgroundBuffer, Y     ; Load tile to draw from buffer
  STA $2007                   ; Write tile to PPU
  INY                         ; Increment buffer to next len
  DEX                         ; Decrement length
  CPX #0                      ; Done with this buffer?
  BEQ .loopUpdate             ; Yes, draw next buffer
  JMP .loopDraw               ; No, continue this draw

; Sets up a background update into the buffer
; Assumes we're drawing into the status bar
;
; Args:
;  len                        - How many bytes to copy
;  startY                     - The Y tile to start the draw into
;  startX                     - The X tile to start the draw into
;  oldX                       - Preserve X loop when adding to buffer
StartBackgroundUpdate:
  LDA len
  JSR AddBackgroundByte
  LDA startY
  JSR AddBackgroundByte
  LDA startX
  JSR AddBackgroundByte
  RTS

; Adds a byte to the background update, make sure len
; has been set in StartBackgroundUpdate first
AddBackgroundByte:
  STX oldX                    ; Store X in oldX
  LDX bufferUpdateIndex
  STA backgroundBuffer, X
  INC bufferUpdateIndex
  LDX oldX                    ; Restore old X
  RTS

; COLLISION / ENEMY INTERACTION

StorePlayerPosForSearch:
  LDA playerPosX+1
  STA posX2
  LDA playerPosY+1
  STA posY2
  RTS

; Based on the source position, find the closest enemy
;
; Args:
;   posX                      - source X
;   posY                      - source Y
;
; Stores index in A, and enemyCount
; Stores distance in distance
FindClosestEnemyIndex:
  LDX #0                      ; Start count at 0
  LDA #$FF                    ; Set max distance in distance, to check against
  STA distance
  STA enemyCount              ; Set a sentinel value of FF in result index
.loop:
  LDA enemyState, X          ; Check enemy states to ensure it's a valid target
  CMP #ENEMY_ALIVE_STATE      ; Is it alive?
  BCC .count                  ; No - go to next enemy
  LDY positionOffset, X       ; Get position offset for enemy index
  LDA enemyPosX+1, Y          ; Load enemy X position
  STA posX2                   ; Store in x2 for manhattan distance
  LDA enemyPosY+1, Y          ; Load enemy Y position
  STA posY2                   ; Store in y2 for manhattan distance
  JSR ManhattanDistance       ; Get distance in A
  CMP distance                ; Compare to current lowest distance
  BCC .smallest
  JMP .count
.smallest:
  STA distance                ; What we have now is now smallest distance
  STX enemyCount              ; Store the new lowest index in enemyCount
.count:
  INX                         ; Increment X
  CPX #ENEMYCOUNT
  BNE .loop
  LDA enemyCount              ; Record the lowest index we got
  RTS

; Tests world collision at point
;
; Args:
;   posX                      - origin x
;   posY                      - origin y
;   tilesW                    - tiles w
;   tilesH                    - tiles h
;
; Local:
;   tilesX                    - store x tile after calculating
;   tilesWOriginal            - store original w
;
; A will be 0 if not colliding, 1 if colliding
; X and Y will be trashed
TestWorldCollision:
  LDA #HIGH(collision)        ; Setup pointer for collison table
  STA pointerColHi
  LDA tilesW                  ; Load tile width
  STA tilesWOriginal          ; Cache tile width in arg5
.loopY:
  LDA #LOW(collision)
  STA pointerColLo
  LDA posY                    ; Y pixel
  LSR A                       ; LSR 3 times to be y/8 to get tile
  LSR A
  LSR A                       ; Now we have the tile
  CLC
  ADC tilesH                  ; Add our current tile Y offset
  TAY                         ; Now we have a Y
  LDA pointerColLo            ; Look up the pointer offset for our Y
  CLC
  ADC collisionLookupY, Y
  STA pointerColLo
.loopX:
  LDA posX                    ; X pixel
  LSR A                       ; LSR 3 times to be x/8 to get tile
  LSR A
  LSR A                       ; Now we have the tile pos
  CLC
  ADC tilesW                  ; Add current tile W
  STA tilesX                  ; Store our tile back in arg4, we're going to find
  LSR A                       ;   the collision byte with another 3 LSRs to
  LSR A                       ;   be tileX/8
  LSR A
  TAY                         ; Now we have an offset for the actual collision
  LDA tilesX                  ; Load the tile back into A
  SEC
  SBC collisionLookupX, Y     ; Subtract tiles to be in the right quadrant
  TAX                         ; Number of tiles into this quadrant to count
  LDA [pointerColLo], Y       ; Load the collision data
.shiftBits:
  CPX #0                      ; See if the current left bit is the one to test
  BEQ .testBit
  ASL A                       ; Shift bits left
  DEX
  JMP .shiftBits
.testBit:
  AND #COLLISIONMASK          ; Mask collision at this tile
  CMP #COLLISIONMASK          ; Do we equal the collision mask? We're colliding
  BEQ .collision
; Are we done looping?
  DEC tilesW                  ; Decrease W
  LDA tilesW                  ; Test to see if we finished
  CMP #$FF                    ; Looped, done
  BEQ .testFinishY            ; This row is done, test if Y is done
  JMP .loopX                  ; Check next w
.testFinishY:
  DEC tilesH                  ; Decrease H
  LDA tilesH                  ; Test to see if we finished
  CMP #$FF                    ; Looped, done
  BEQ .finish
  LDA tilesWOriginal          ; Reset W
  STA tilesW
  JMP .loopY
.finish:
  LDA #0
  RTS
.collision:
  LDA #1
  RTS

; SCORING

; Requires X to be set up for the place to increment
AddScore:
  LDA #1
  STA scoreChanged            ; Mark that we've changed score this frame
  CPX #SCOREPLACES            ; Did we overflow the total loop? We're done
  BNE .doAdd                  ; No, continue
.fillScore:                   ; Yes, mark score full
  DEX
  LDA #9
  STA score, X                ; Put 9 in this place
  CPX #0                      ; Did we finish the last place?
  BNE .fillScore              ; No, continue filling
  RTS                         ; Yes, stop updating
.doAdd:
  INC score, X
  LDA score, X
  CMP #$A                     ; Do we have 10 in this place?
  BEQ .addPlace               ; Yes, increment place
  RTS                         ; No, our add is done
.addPlace:                    ; We overflowed, carry 1 up a place
  LDA #0                      ; Set zero in this place
  STA score, X
  INX                         ; Increment place
  JMP AddScore                ; Add one to new place

DrawScoreUpdate:
  LDA scoreChanged            ; Check if score changed
  BNE .doUpdate               ; Yes, draw change
  RTS                         ; No, nothing to draw
.doUpdate:
  LDX #SCOREPLACES            ; We're drawing all score places
  STX len
  LDA #SCORE_TILE             ; We start drawing at score place
  STA startX
  LDA #STATUS_Y
  STA startY
  JSR StartBackgroundUpdate
.drawScoreLoop:
  DEX                         ; Decrement X
  LDA score, X                ; Draw score place into buffer, reversing order
  JSR AddBackgroundByte
  CPX #0                      ; Did we draw the last place?
  BNE .drawScoreLoop          ; No, do next place
  RTS

; SPRITE UPDATES

; Set pointer for enemy sprite
SetPointerForEnemy:
  LDA #SPRITEHI
  STA pointerHi
  LDX enemyCount
  LDA spriteOffset, X         ; Get sprite offset for index
  CLC
  ADC #ENEMY0
  STA pointerLo               ; Store sprite at index in pointerLo
  RTS

; Set pointer for bullet sprite
SetPointerForBullet:
  LDX bulletCount
  LDA spriteOffset, X         ; Get sprite offset for index
  CLC
  ADC #PBULLET0
  STA pointerLo               ; Store sprite at index in pointerLo
  RTS

; For sprite updates, move pointerLo down by two sprites
MovePointerOneRow:
  LDA pointerLo
  CLC
  ADC #$08                    ; 8 bytes for 2 4 byte sprites
  STA pointerLo
  RTS

; Sets all sprites to hidden for a given layout
; Expects all sprites to be 2 tiles wide
; Expects pointerLo to be set for the top left sprite address
;
; Args:
;   tilesH                    - Height of sprite in tiles
HideSpriteLayout:
  LDA #$FF                    ; Store "offscreen" FF in sprite y
  LDY #0                      ; Store sprite y origin offset
  STA [pointerLo], Y          ; Set Y's
  LDY #4                      ; Next sprite's Y
  STA [pointerLo], Y
  JSR MovePointerOneRow       ; Increment pointer for next loop
  DEC tilesH
  LDA tilesH
  BNE HideSpriteLayout        ; Continue loop for next row
  RTS

; Update sprite layout for a group of sprites
; Expects pointerLo to be set to the top left sprite address, and the sprite
; to be placed where it should be in the world already
; Expects all sprites to be 2 tiles wide
;
; Args:
;   posX                      - TL sprite position X
;   posY                      - TL sprite position Y
;   tilesH                    - Number of tiles high the sprite is
;
; Trashes Y
UpdateSpriteLayout:
  LDA posY                    ; Set sprite Y's
  LDY #0                      ; Row y0
  STA [pointerLo], Y
  LDY #4                      ; Row y1
  STA [pointerLo], Y
  LDA posX                    ; Load X
  LDY #SPRITEX                ; Set sprite X's
  STA [pointerLo], Y
  CLC
  ADC #TILE_WIDTH
  LDY #SPRITEX+4              ; Second sprite's X
  STA [pointerLo], Y
  JSR MovePointerOneRow       ; Increment Y for next loop
  LDA posY                    ; Increment row Y
  CLC
  ADC #TILE_WIDTH
  STA posY
  DEC tilesH
  LDA tilesH
  BNE UpdateSpriteLayout
  RTS

; Takes pre-filled frames and attributes and applies them to the
; current sprite pointer
ApplySpriteSettings:
  LDX #0                      ; Starts our loop at 0
.loop:
  LDA spriteFrame, X
  LDY #SPRITETIL              ; Assign tile
  STA [pointerLo], Y
  LDY #SPRITEATT              ; Assign attributes
  LDA spriteAttr, X
  STA [pointerLo], Y
  LDA pointerLo               ; Increment pointer by 4 bytes to next sprite
  CLC
  ADC #$04
  STA pointerLo
  INX
  CPX #$04                    ; Check whether we're done with the loop
  BNE .loop
  RTS

; SUBPIXEL MOVEMENT

; In preparation of SubPixelMove, find sign on velocity
;
; Args:
;   velHi                     - hi velocity
; Sets velSign to hi velocity sign
StoreVelocitySign:
  LDA velHi                   ; Check hi velocity for sign
  AND #NEG_SIGN
  STA velSign                 ; Store sign
  LDA velHi                   ; Clear sign off of velocity
  AND #MOV_MASK
  STA velHi
  LDA velSign                 ; Load sign back up
  CMP #NEG_SIGN               ; Are we negative?
  BEQ .negative
  LDA #0                      ; Positive
  STA velSign                 ; Store result
  RTS
.negative:
  LDA #1                      ; Negative
  STA velSign                 ; Store result
  RTS

; Multiply the stored velocity by 4
; velHi     - hi velocity
QuadrupleVelocity:
  LDA velHi                   ; Load current velocity hi
  ASL A                       ; Multiply current by 4
  ASL A
  STA velHi                   ; Save scaled velocity
  RTS

; Move subpixel based on velocity
; pointerSubLo should be set up to subpixel
;
; Args:
;   velLo                     - lo velocity
;   velHi                     - hi velocity
;   velSign                   - Sign of velocity
;
; A is new pixel position at the end ; TODO it's not - why?
SubPixelMove:
  LDY #0                      ; Set Y to 0
  LDA velSign                 ; Check sign
  BEQ .add                    ; Positive Movement
  JMP .sub                    ; Negative Movement
.add:
  LDA [pointerSubLo], Y       ; Load subpixel
  CLC
  ADC velLo                   ; Add lo velocity
  STA [pointerSubLo], Y       ; Store subpixel
  LDY #1
  LDA [pointerSubLo], Y       ; Load pixel
  ADC velHi                   ; Add hi velocity with carry
  STA [pointerSubLo], Y       ; Store result
  RTS
.sub:
  LDA [pointerSubLo], Y       ; Load subpixel
  SEC
  SBC velLo                   ; Subtract lo speed
  STA [pointerSubLo], Y       ; Store subpixel
  LDY #1
  LDA [pointerSubLo], Y       ; Load pixel
  SBC velHi                   ; Subtract hi velocity with carry
  STA [pointerSubLo], Y       ; Store pixel
  RTS
