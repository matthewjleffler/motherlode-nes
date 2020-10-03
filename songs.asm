; songs.asm
;  contains sfx and sound data

; TODO remove most of these
song_headers:
  .dw song0_header            ; this is a silence song.
  .dw song1_header            ; The Guardian Legend Boss song
  .dw song2_header            ; a sound effect.  Try playing it over the other songs.
  .dw song3_header            ; Dragon Warrior overland song
  .dw song4_header            ; a new song taking advantage of note lengths and rests
  .dw song5_header            ; another sound effect played at a very fast tempo.
  .dw song6_header
  .dw song7_header
  .dw song8_header
  .dw enemy_hit_header
  .dw enemy_die_header

; SONG0 - silence
song0_header:
  .db 6                       ; 6 streams
  .db MUSIC_SQ1
  .db $00
  .db MUSIC_SQ2
  .db $00
  .db MUSIC_TRI
  .db $00
  .db MUSIC_NOI
  .db $00
  .db SFX_1
  .db $00
  .db SFX_2
  .db $00

; SONG1
song1_header:
  .db $04           ;4 streams
  .db MUSIC_SQ1     ;which stream
  .db $01           ;status byte (stream enabled)
  .db SQUARE_1      ;which channel
  .db $70           ;initial duty (01)
  .db ve_tgl_1      ;volume envelope
  .dw song1_square1 ;pointer to stream
  .db $53           ;tempo
  .db MUSIC_SQ2     ;which stream
  .db $01           ;status byte (stream enabled)
  .db SQUARE_2      ;which channel
  .db $B0           ;initial duty (10)
  .db ve_tgl_2      ;volume envelope
  .dw song1_square2 ;pointer to stream
  .db $53           ;tempo
  .db MUSIC_TRI     ;which stream
  .db $01           ;status byte (stream enabled)
  .db TRIANGLE      ;which channel
  .db $80           ;initial volume (on)
  .db ve_tgl_2      ;volume envelope
  .dw song1_tri     ;pointer to stream
  .db $53           ;tempo
  .db MUSIC_NOI     ;which stream
  .db $01           ;enabled
  .db NOISE
  .db $30           ;initial duty_vol
  .db ve_drum_decay ;volume envelope
  .dw song1_noise   ;pointer to stream
  .db $53           ;tempo

song1_square1:
  .db eighth
  .db set_loop1_counter, 14             ;repeat 14 times
.loop:
  .db A2, A2, A2, A3, A2, A3, A2, A3
  .db transpose                         ;the transpose opcode take a 2-byte argument
  .dw .lookup_table                     ;which is the address of the lookup table
  .db loop1                             ;finite loop (14 times)
  .dw .loop
  .db loop                              ;infinite loop
  .dw song1_square1
.lookup_table:
  .db 2, -1, -1, -1, -1, -1, -2
  .db -1, -1, 0, -1, 8, -8, 8       ;14 entries long, reverse order

song1_square2:
  .db sixteenth
  .db REST    ;offset for delay effect
  .db eighth
.loop_point:
  .db REST
  .db A4, C5, B4, C5, A4, C5, B4, C5
  .db A4, C5, B4, C5, A4, C5, B4, C5
  .db A4, C5, B4, C5, A4, C5, B4, C5
  .db A4, C5, B4, C5, A4, C5, B4, C5
  .db Ab4, B4, A4, B4, Ab4, B4, A4, B4
  .db B4, E5, D5, E5, B4, E5, D5, E5
  .db A4, Eb5, C5, Eb5, A4, Eb5, C5, Eb5
  .db A4, D5, Db5, D5, A4, D5, Db5, D5
  .db A4, C5, F5, A5, C6, A5, F5, C5
  .db Gb4, B4, Eb5, Gb5, B5, Gb5, Eb5, B4
  .db F4, Bb4, D5, F5, Gs5, F5, D5, As4
  .db E4, A4, Cs5, E5, A5, E5, sixteenth, Cs5, REST
  .db eighth
  .db Ds4, Gs4, C5, Ds5, Gs5, Ds5, C5, Gs4
  .db sixteenth
  .db G4, Fs4, G4, Fs4, G4, Fs4, G4, Fs4
  .db eighth
  .db G4, B4, D5, G5
  .db loop
  .dw .loop_point

song1_tri:
  .db eighth
  .db A5, C6, B5, C6, A5, C6, B5, C6 ;triangle data
  .db A5, C6, B5, C6, A5, C6, B5, C6
  .db A5, C6, B5, C6, A5, C6, B5, C6
  .db A5, C6, B5, C6, A5, C6, B5, C6
  .db Ab5, B5, A5, B5, Ab5, B5, A5, B5
  .db B5, E6, D6, E6, B5, E6, D6, E6
  .db A5, Eb6, C6, Eb6, A5, Eb6, C6, Eb6
  .db A5, D6, Db6, D6, A5, D6, Db6, D6
  .db A5, C6, F6, A6, C7, A6, F6, C6
  .db Gb5, B5, Eb6, Gb6, B6, Gb6, Eb6, B5
  .db F5, Bb5, D6, F6, Gs6, F6, D6, As5
  .db E5, A5, Cs6, E6, A6, E6, Cs6, A5
  .db Ds5, Gs5, C6, Ds6, Gs6, Ds6, C6, Gs5
  .db sixteenth
  .db G5, Fs5, G5, Fs5, G5, Fs5, G5, Fs5
  .db G5, B5, D6, G6, B5, D6, B6, D7
  .db loop
  .dw song1_tri

song1_noise:
  .db eighth, $04
  .db sixteenth, $04, $04, $04
  .db d_eighth, $04
  .db sixteenth, $04, $04, $04, $04
  .db eighth, $04, $04
  .db loop
  .dw song1_noise

; SONG2
song2_header:
  .db $01           ;1 stream
  .db SFX_1         ;which stream
  .db $01           ;status byte (stream enabled)
  .db SQUARE_2      ;which channel
  .db $70           ;duty (01)
  .db ve_battlekid_1b  ;volume envelope
  .dw song2_square2 ;pointer to stream
  .db $80           ;tempo

song2_square2:
  .db eighth, D3, D2
  .db endsound

; SONG3
song3_header:
  .db $04           ;4 streams
  .db MUSIC_SQ1     ;which stream
  .db $01           ;status byte (stream enabled)
  .db SQUARE_1      ;which channel
  .db $B0           ;initial duty (10)
  .db ve_tgl_1      ;volume envelope
  .dw song3_square1 ;pointer to stream
  .db $20           ;tempo
  .db MUSIC_SQ2     ;which stream
  .db $01           ;status byte (stream disabled)
  .db SQUARE_2
  .db $B0
  .db ve_tgl_2
  .dw song3_tri     ;pointer to stream
  .db $20
  .db MUSIC_TRI     ;which stream
  .db $00           ;status byte (stream enabled)
  .db MUSIC_NOI     ;which stream
  .db $00           ;disabled.  Our load routine will skip the

song3_square1:
  .db eighth
  .db D4, A4, F4, A4, D4, B4, G4, B4
  .db D4, C5, A4, C5, D4, As4, F4, As4
  .db E4, A4, E4, A4, D4, A4, Fs4, A4
  .db D4, A4, Fs4, A4, G4, As4, A4, C5
  .db D4, C5, A4, C5, D4, B4, G4, B4
  .db D4, B4, G4, B4, D4, As4, Gs4, As4
  .db Cs4, A4, E4, A4, D4, A4, E4, A4
  .db Cs4, A4, E4, A4, B3, A4, Cs4, A4
  .db loop
  .dw song3_square1

song3_tri:
  .db quarter, D6, A6, d_half, G6
  .db eighth, F6, E6, quarter, D6
  .db eighth, C6, As5, C6, A5
  .db quarter, E6, d_whole, D6
  .db quarter, A6, C7, d_half, B6
  .db eighth, G6, F6, quarter, E6
  .db eighth, F6, G6, whole, A6, A6
  .db loop
  .dw song3_tri

; SONG4
song4_header:
  .db $04           ;4 streams
  .db MUSIC_SQ1     ;which stream
  .db $01           ;status byte (stream enabled)
  .db SQUARE_1      ;which channel
  .db $B0           ;initial duty (10)
  .db ve_battlekid_1b  ;volume envelope
  .dw song4_square1 ;pointer to stream
  .db $60           ;tempo
  .db MUSIC_SQ2     ;which stream
  .db $01           ;status byte (stream enabled)
  .db SQUARE_2      ;which channel
  .db $30           ;initial duty (00)
  .db ve_short_staccato ;volume envelope
  .dw song4_square2 ;pointer to stream
  .db $60           ;tempo
  .db MUSIC_TRI     ;which stream
  .db $01           ;status byte (stream enabled)
  .db TRIANGLE      ;which channel
  .db $81           ;initial volume (on)
  .db ve_battlekid_1b  ;volume envelope
  .dw song4_tri     ;pointer to stream
  .db $60           ;tempo
  .db MUSIC_NOI     ;which stream
  .db $01
  .db NOISE
  .db 30
  .db ve_drum_decay
  .dw song4_noise
  .db $60

song4_square1:
  .db half, E4, quarter, G4, eighth, Fs4, E4, d_sixteenth, Eb4, E4, Fs4, t_quarter, REST, half, REST
  .db       Fs4, quarter, A4, eighth, G4, Fs4, d_sixteenth, E4, Fs4, G4, t_quarter, REST, half, REST
  .db       G4, quarter, B4, eighth, A4, G4, quarter, A4, B4, C5, eighth, B4, A4
  .db       B4, A4, G4, Fs4, Eb4, E4, Fs4, G4, Fs4, E4, d_half, REST
  .db loop
  .dw song4_square1

song4_square2:
  .db quarter
  .db E3, B3, B3, B3, B2, Fs3, Fs3, Fs3
  .db Fs3, A3, A3, A3, B2, E3, E3, E3
  .db E3, B3, B3, B3, B3, A3, G3, Fs3
  .db E3, B3, A3, Fs3, E3, E3, E3, E3;d_half, REST
  .db loop
  .dw song4_square2

song4_tri:
  .db half, E4, G4, B3, Eb4
  .db Fs4, A4, B3, E4
  .db G4, B4, quarter, A4, B4, half, C5
  .db eighth, E4, Fs4, G4, A4, B3, C4, D4, Eb4, A3, E4, d_half, REST
  .db loop
  .dw song4_tri

song4_noise:
  .db set_loop1_counter, 3
.loop_point:
  .db half, $0D, $07, $0D
  .db quarter, $07, $15
  .db loop1
  .dw .loop_point
  .db quarter, $0D, $0D, $07, $07
  .db d_half, $05
  .db sixteenth, $12, $13, $14, $15
  .db loop
  .dw song4_noise

; SONG5
song5_header:
  .db $01           ;1 stream
  .db SFX_1         ;which stream
  .db $01           ;status byte (stream enabled)
  .db SQUARE_2      ;which channel
  .db $70           ;initial duty (01)
  .db ve_short_staccato ;volume envelope
  .dw song5_square2 ;pointer to stream
  .db $FF           ;tempo..very fast tempo

song5_square2:
  .db thirtysecond, C4, D8, C5, D7, C6, D6, C7, D5, C8, D8 ;some random notes played very fast
  .db endsound

; SONG6
;Battle Kid theme.  Original by Sivak
song6_header:
  .db $04           ;4 streams
  .db MUSIC_SQ1     ;which stream
  .db $01           ;status byte (stream enabled)
  .db SQUARE_1      ;which channel
  .db $30           ;initial duty (01)
  .db ve_battlekid_1      ;volume envelope
  .dw song6_square1 ;pointer to stream
  .db $4C           ;tempo
  .db MUSIC_SQ2     ;which stream
  .db $01           ;status byte (stream enabled)
  .db SQUARE_2      ;which channel
  .db $30           ;initial duty (10)
  .db ve_battlekid_2      ;volume envelope
  .dw song6_square2 ;pointer to stream
  .db $4C           ;tempo
  .db MUSIC_TRI     ;which stream
  .db $01           ;status byte (stream enabled)
  .db TRIANGLE      ;which channel
  .db $80           ;initial volume (on)
  .db ve_short_staccato    ;volume envelope
  .dw song6_tri     ;pointer to stream
  .db $4C           ;tempo
  .db MUSIC_NOI     ;which stream
  .db $01
  .db NOISE
  .db $30
  .db ve_drum_decay
  .dw song6_noise
  .db $4C

song6_square1:
  .db sixteenth
  .db set_loop1_counter, 13
.intro_loop:
  .db A3, C4, E4, A4
  .db loop1
  .dw .intro_loop
  .db A3, E4, E3, E2
  .db duty, $B0
  .db volume_envelope, ve_battlekid_1b
  .db quarter, E4, E3
  .db duty, $70
  .db five_eighths, A3
  .db eighth, B3, C4, D4
  .db sixteenth, Ds4
  .db five_sixteenths, E4 ;original probably uses a slide effect.  I fake it here with odd note lengths
  .db d_quarter, A3
  .db quarter, D4
  .db five_eighths, A3
  .db eighth, B3, C4, D4, C4, B3, A3
  .db quarter, G3, E3
  .db eighth, B3
  .db five_eighths, A3
  .db eighth, B3, C4, D4
  .db sixteenth, Ds4
  .db five_sixteenths, E4
  .db d_quarter, A3
  .db quarter, D4
  .db five_eighths, A3
  .db eighth, B3, C4, D4, C4, B3, A3
  .db quarter, E4, E3
  .db eighth, E3
  .db duty, $30
  .db volume_envelope, ve_battlekid_1
  .db loop
  .dw song6_square1

song6_square2:
  .db sixteenth
  .db REST
.loop_point:
  .db set_loop1_counter, 13
.intro_loop:
  .db A3, C4, E4, A4
  .db loop1
  .dw .intro_loop
  .db A3, E4, E3, E2
  .db duty, $B0
  .db volume_envelope, ve_battlekid_2b
  .db quarter, E4, E3
  .db duty, $70
  .db five_eighths, A3
  .db eighth, B3, C4, D4
  .db sixteenth, Ds4
  .db five_sixteenths, E4
  .db d_quarter, A3
  .db quarter, D4
  .db five_eighths, A3
  .db eighth, B3, C4, D4, C4, B3, A3
  .db quarter, G3, E3
  .db eighth, B3
  .db five_eighths, A3
  .db eighth, B3, C4, D4
  .db sixteenth, Ds4
  .db five_sixteenths, E4
  .db d_quarter, A3
  .db quarter, D4
  .db five_eighths, A3
  .db eighth, B3, C4, D4, C4, B3, A3
  .db quarter, E4, E3
  .db eighth, E3
  .db duty, $30
  .db volume_envelope, ve_battlekid_2
  .db sixteenth
  .db loop
  .dw .loop_point

song6_tri:
  .db eighth
  .db set_loop1_counter, 4              ;repeat 4 times
.loop:
  .db A3, A3, A4, A4, A3, A3, A4, A4    ;series of notes to repeat
  .db adjust_note_offset, -2            ;go down a step
  .db loop1
  .dw .loop
  .db set_note_offset, 0                ;after 4 repeats, reset note offset to 0.
  .db loop                              ;infinite loop
  .dw song6_tri
  ;21 bytes

song6_noise:
  .db set_loop1_counter, 3
.loop_point1:
  .db quarter, $01, $03, $01
  .db eighth, $03, $05
  .db loop1
  .dw .loop_point1
  .db eighth, $01, $02, $03, $03
  .db quarter, $07, $07
  .db loop
  .dw song6_noise

; SONG7
song7_header:
  .db $01           ;1 stream
  .db SFX_1         ;which stream
  .db $01           ;status byte (stream enabled)
  .db SQUARE_2      ;which channel
  .db $70           ;initial duty (01)
  .db ve_short_staccato ;volume envelope
  .dw song7_square2 ;pointer to stream
  .db $FF           ;tempo..very fast tempo

song7_square2:
  .db set_loop1_counter, $08    ;repeat 8 times
.loop:
  .db thirtysecond, D7, D6, G6      ;play two D notes at different octaves
  .db adjust_note_offset, -4     ;go down 2 steps
  .db loop1
  .dw .loop
  .db endsound

; SONG8
song8_header:
  .db 1
  .db SFX_1
  .db $01
  .db NOISE
  .db $30
  .db ve_tgl_2
  .dw song8_noise
  .db $4C

song8_noise:
  .db quarter                  ;first play through as half notes
  .db set_loop1_counter, 2  ;play two times
.loop_point:
  .db $00, $01, $02, $03, $04, $05, $06, $07, $08
  .db $09, $0A, $0B, $0C, $0D, $0E, $0F
  .db $10, $11, $12, $13, $14, $15, $16, $17, $18
  .db $19, $1A, $1B, $1C, $1D, $1E, $1F
  .db volume_envelope, ve_drum_decay        ;change to 7-frame decay volume envelope
  .db eighth                               ;play quarter notes
  .db loop1                                 ;repeat
  .dw .loop_point
  .db endsound

; Song9
enemy_hit_header:
  .db 1
  .db SFX_1
  .db 1
  .db NOISE
  .db $30       ; ??
  .db ve_hit_decay
  .dw enemy_hit_noise
  .db $66
enemy_hit_noise:
  .db eighth, $0B, endsound

enemy_die_header:
  .db 1
  .db SFX_2
  .db 1
  .db NOISE
  .db 30
  .db ve_blip_echo
  .dw enemy_die_noise
  .db $80
enemy_die_noise:
  .db half, $03, endsound
