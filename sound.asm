; sound.asm
;  sound init, running, and loading routines

; CONSTANTS
SQUARE_1          = $00 ;these are channel constants
SQUARE_2          = $01
TRIANGLE          = $02
NOISE             = $03

MUSIC_SQ1         = $00 ;these are stream # constants
MUSIC_SQ2         = $01 ;stream # is used to index into variables
MUSIC_TRI         = $02
MUSIC_NOI         = $03
SFX_1             = $04
SFX_2             = $05

soundTemp1        = arg0
soundTemp2        = arg1

; SUBROUTINES

SoundInit:
  LDA #$0F
  STA $4015                   ; Enable Square 1, Square 2, Triangle and Noise channels
  LDA #$00
  STA soundDisable            ; Clear variables
  LDA #$FF
  STA soundSq1Old
  STA soundSq2Old
SoundSilence:
  LDA #$30
  STA soundApu                ; Square 1 volume 0
  STA soundApu+4              ; Square 2 volume 0
  STA soundApu+12             ; Set noise volume 0
  LDA #$80
  STA soundApu+8              ; Silence triangle
  RTS

; TODO remove this?
SoundDisable:
  LDA #$00
  STA $4015                   ; Disable all channels
  LDA #$01
  STA soundDisable
  RTS

; Prepares the sound engine to play a song or sfx
;
; A  - Song/Sfx number to play
SoundLoad:
  STA soundTemp1              ; Save song number
  ASL A                       ; Multiply by 2, indexing into a table of poitners
  TAY
  LDA song_headers, Y         ; Setup the pointer to our song header
  STA soundPointer
  LDA song_headers+1, Y
  STA soundPointer+1
  LDY #0
  LDA [soundPointer], Y       ; Read the first byte, # streams
  STA soundTemp2              ; Store in a temp variable, use it as a loop counter
  INY
.loop:
  LDA [soundPointer], Y       ; Stream number
  TAX                         ; Stream number is variable index
  INY
  ; Status byte
  LDA [soundPointer], Y       ; Status byte, 1 = enable, 0 = disable
  STA streamStatus, X
  BEQ .nextStream             ; If status byte is 0, stream disabled, done
  INY
  ; Channel number
  LDA [soundPointer], Y
  STA streamChannel, X
  INY
  ; Initial duty and volume settings
  LDA [soundPointer], Y
  STA streamVolDuty, X
  INY
  ; Volume Envelope
  LDA [soundPointer], Y
  STA streamVE, X
  INY
  ; Pointer to stream data lo
  LDA [soundPointer], Y
  STA streamPointerLo, X
  INY
  ; Pointer to stream data hi
  LDA [soundPointer], Y
  STA streamPointerHi, X
  INY
  ; Tempo
  LDA [soundPointer], Y
  STA streamTempo, X
  ; Finish stream
  LDA #$A0
  STA streamTickerTotal, X
  LDA #$01
  STA streamNoteLengthCount, X
  LDA #0
  STA streamVEIndex, X
  STA streamLoop1, X
  STA streamNoteOffset, X
.nextStream:
  INY
  LDA soundTemp1              ; Song number
  STA streamCurrentSound, X
  DEC soundTemp2              ; Loop counter
  BNE .loop
  RTS

SoundPlayFrame:
  LDA soundDisable            ; Don't play if disabled
  BNE .done
  JSR SoundSilence            ; Silence channels in case they're not used this frame
  LDX #0
.loop:
  LDA streamStatus, X
  AND #$01                    ; Is the stream active?
  BEQ .endLoop
  ; Add temp to ticker total
  LDA streamTickerTotal, X
  CLC
  ADC streamTempo, X
  STA streamTickerTotal, X
  BCC .setBuffer              ; Carry clear, no tick. No tick, we're done with this stream
  ; New tick
  DEC streamNoteLengthCount, X
  BNE .setBuffer              ; Counter non zero, note isn't finished
  LDA streamNoteLength, X     ; Note is finished, reload note length count
  STA streamNoteLengthCount, X
  JSR SoundFetchByte          ; Read the next byte from the data stream
.setBuffer:
  JSR SoundSetTempPorts       ; Copy the current stream's sound data for the current frame
.endLoop:
  INX
  CPX #6
  BNE .loop
  JSR SoundSetApu             ; Copy our APU buffer into the real APU
.done:
  RTS

; Reads one byte from the sound stream and handles it
;
; Args:
;  X - stream number
SoundFetchByte:
  LDA streamPointerLo, X
  STA soundPointer
  LDA streamPointerHi, X
  STA soundPointer+1
  LDY #0
.fetch:
  LDA [soundPointer], Y
  BPL .note                   ; < #$80, a note
  CMP #$A0
  BCC .noteLength             ; Else if M #$A0, it's a note length
.opcode:
  JSR SoundOpcodeLauncher
  INY                         ; Next position in data
  LDA streamStatus, X
  AND #%00000001
  BNE .fetch                  ; After our opcode is done, grab another byte unless the stream is disable
  RTS
.noteLength:
  AND #%01111111              ; Remove hi bit
  STY soundTemp1              ; Save Y
  TAY
  LDA note_length_table, Y    ; Get note length count value
  STA streamNoteLength, X
  STA streamNoteLengthCount, X  ; Store it in length counter
  LDY soundTemp1              ; Restore Y
  INY                         ; Next index in stream
  JMP .fetch                  ; Fetch next byte
.note:
  STA soundTemp2
  LDA streamChannel, X
  CMP #NOISE
  BNE .notNoise
  JSR SoundDoNoise
  JMP .resetVE
.notNoise:
  LDA soundTemp2
  STY soundTemp1              ; Save our index
  CLC
  ADC streamNoteOffset, X     ; Add note offset
  ASL A
  TAY
  LDA note_table, Y
  STA streamNoteLo, X
  LDA note_table+1, Y
  STA streamNoteHi, X
  LDY soundTemp1              ; Restore data stream index
  JSR SoundTestRest
.resetVE:
  LDA #0
  STA streamVEIndex, X
;.updatePointer:
  INY
  TYA
  CLC
  ADC streamPointerLo, X
  STA streamPointerLo, X
  BCC .end
  INC streamPointerHi, X
.end:
  RTS

SoundDoNoise:
  LDA soundTemp2
  AND #%00010000
  BEQ .mode0
  LDA soundTemp2
  ORA #%10000000              ; Bit 7 to mode 1
  STA soundTemp2
.mode0:
  LDA soundTemp2
  STA streamNoteLo, X
  RTS

SoundTestRest:
  LDA [soundPointer], Y       ; Read the note byte again
  CMP #REST
  BNE .notRest
  LDA streamStatus, X
  ORA #%00000010              ; Set the rest bit in the status byte
  BNE .store
.notRest:
  LDA streamStatus, X
  AND #%11111101              ; Clear the rest bit in the status byte
.store:
  STA streamStatus, X
  RTS

SoundOpcodeLauncher:
  STY soundTemp1              ; Save Y register
  SEC
  SBC #$A0                    ; Turn the opcode byte into a table index by subtracting $A0
  ASL A                       ; Multiply by 2 because we index into a table of addresses
  TAY
  LDA sound_opcodes, Y        ; Get lo byte of subroutine address
  STA soundPointer2
  LDA sound_opcodes+1, Y      ; Get hi byte of subroutine address
  STA soundPointer2+1
  LDY soundTemp1              ; Restore Y register
  INY
  JMP [soundPointer2]         ; Jump to opcode routine

; Copy a stream's sound data to APU buffer
; Args:
;   X - Stream number
SoundSetTempPorts:
  LDA streamChannel, X
  ASL A
  ASL A
  TAY
  JSR SoundSetStreamVolume
  LDA #$08
  STA soundApu+1, Y           ; Sweep
  LDA streamNoteLo, X
  STA soundApu+2, Y           ; Period Lo
  LDA streamNoteHi, X
  STA soundApu+3, Y           ; Period Hi
  RTS

SoundSetStreamVolume:
  STY soundTemp1              ; Save the index into soundApu
  LDA streamVE, X             ; Which volume envelope?
  ASL A                       ; Multiply by 2 to index into a table of addresse
  TAY
  LDA volume_envelopes, Y     ; Get the low byte of address from pointer table
  STA soundPointer
  LDA volume_envelopes+1, Y   ; Get hi byte of the address
  STA soundPointer+1
.readVE:
  LDY streamVEIndex, X        ; Current position within the volume envelope
  LDA [soundPointer], Y
  CMP #$FF
  BNE .setVol                 ; If not FF, set volume
  DEC streamVEIndex, X        ; Else if FF, go back one and read again
  JMP .readVE
.setVol:
  STA soundTemp2              ; Save our new volume value
  CPX #TRIANGLE
  BNE .squares                ; If not triangle channel, go ahead
  LDA soundTemp2
  BNE .squares                ; Else if volume not zero, go ahead
  LDA #$80
  BMI .storeVol               ; Silence the channel
.squares:
  LDA streamVolDuty, X        ; Get current vol/duty
  AND #$F0                    ; Zero out old volume
  ORA soundTemp2              ; OR new volume in
.storeVol:
  LDY soundTemp1              ; Get index for soundApu
  STA soundApu, Y             ; Store value into buffer
  INC streamVEIndex, X        ; Set our volume envelope index to the next index
; .restCheck
  LDA streamStatus, X         ; Check the reset flag
  AND #%00000010
  BEQ .done                   ; If clear, no rest
  LDA streamChannel, X
  CMP #TRIANGLE               ; Triangle? silence with #%80
  BEQ .triangle
  LDA #$30
  BNE .store
.triangle:
  LDA #$80
.store:
  STA soundApu, Y
.done:
  RTS

SoundSetApu:
; Square1
  LDA soundApu+0
  STA $4000
  LDA soundApu+1
  STA $4001
  LDA soundApu+2
  STA $4002
  LDA soundApu+3
  CMP soundSq1Old             ; Compare to the last write
  BEQ .square2                ; Don't write, it's the same value
  STA $4003
  STA soundSq1Old             ; Save the value for next check
.square2:
  LDA soundApu+4
  STA $4004
  LDA soundApu+5
  STA $4005
  LDA soundApu+6
  STA $4006
  LDA soundApu+7
  CMP soundSq2Old             ; Compare to last write
  BEQ .triangle               ; Don't write, it's the same value
  STA $4007
  STA soundSq2Old             ; Save the value for next check
.triangle:
  LDA soundApu+8
  STA $4008
  LDA soundApu+10
  STA $400A
  LDA soundApu+11
  STA $400B
.noise:
  LDA soundApu+12
  STA $400C
  LDA soundApu+14
  STA $400E
  LDA soundApu+15
  STA $400F
  RTS

; OPCODES
se_op_endsound:
  LDA streamStatus, X         ; End of stream, disable and silence
  AND #%11111110
  STA streamStatus, X         ; Clear enable flag in status byte
  LDA streamChannel, X
  CMP #TRIANGLE
  BEQ .silenceTriangle        ; Triangle is silenced differently
  LDA #$30                    ; Silence squares and noise
  BNE .silence
.silenceTriangle:
  LDA #$80                    ; Silence triangle
.silence:
  STA streamVolDuty, X        ; Store silence value
  RTS

se_op_infinite_loop:
  LDA [soundPointer], Y       ; Pointer Lo from data stream
  STA streamPointerLo, X      ; Update data position
  INY
  LDA [soundPointer], Y       ; Pointer Hi from data stream
  STA streamPointerHi, X      ; Update data position
  STA soundPointer+1          ; Update the pointer
  LDA streamPointerLo, X
  STA soundPointer
  LDY #$FF                    ; Reset Y to loop back to 0
  RTS

se_op_set_loop1_counter:
  LDA [soundPointer], Y       ; Read the argument (number of loops)
  STA streamLoop1, X          ; Store in the loop counter variable
  RTS

se_op_loop1:
  DEC streamLoop1, X          ; Decrement the counter
  LDA streamLoop1, X
  BEQ .lastIteration          ; If zero, done looping
  JMP se_op_infinite_loop     ; If not zero, jump back
.lastIteration:
  INY                         ; Skip first byte of the address
  RTS
