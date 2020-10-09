; songs.asm
;  contains sfx and sound data

SOUND_SILENT        = 0
MUSIC_TITLE         = 1
SFX_BULLET_HIT      = 2
SFX_BULLET_MISS     = 3
SFX_ENEMY_DIE       = 4
SFX_PLAYER_DIE      = 5
SFX_PLAYER_HURT     = 6
SFX_PLAYER_DODGE    = 7
SFX_PLAYER_ABIL     = 8

song_headers:
  .dw silent_header           ; this is a silence song.
  .dw title_header
  .dw bullet_hit_header
  .dw bullet_miss_header
  .dw enemy_die_header
  .dw player_die_header
  .dw player_hurt_header
  .dw player_dodge_header
  .dw player_ability_header

; SONG0 - silence
silent_header:
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

; Title Song
title_header:
  .db 1                       ; 3 streams
  .db MUSIC_SQ1               ; First stream
  .db 1                       ; Status enabled
  .db SQUARE_1                ; Square 1 channel
  .db $B0                     ; Initial duty (10)
  .db ve_vol_chord            ; Volume envelope
  .dw title_square1           ; Pointer to stream
  .db 40                      ; Tempo

title_square1:
  .db thirtysecond

  .db set_loop1_counter, 8
.gs3_1:
  .db Gs2,  B2, Ds3, loop1
  .dw .gs3_1

  .db set_loop1_counter, 8
.fs3_1:
  .db Fs2, As2, Cs3, loop1
  .dw .fs3_1

  .db set_loop1_counter, 8
.e3_1:
  .db  E2, Gs2,  B2, loop1
  .dw .e3_1

  .db set_loop1_counter, 8
.cs3_1:
  .db Cs2,  E2, Gs2, loop1
  .dw .cs3_1

  .db set_loop1_counter, 8
.gs3_2:
  .db Gs2,  B2, Ds3, loop1
  .dw .gs3_2

  .db set_loop1_counter, 8
.fs3_2:
  .db Fs2, As2, Cs3, loop1
  .dw .fs3_2

  .db set_loop1_counter, 8
.e3_2:
  .db  E2, Gs2,  B2, loop1
  .dw .e3_2

  .db set_loop1_counter, 8
.fs3_3:
  .db Fs2, As2, Cs3, loop1
  .dw .fs3_3

  .db loop
  .dw title_square1

; Bullet Hit Sound
bullet_hit_header:
  .db 1
  .db SFX_1
  .db 1
  .db NOISE
  .db $30
  .db ve_hit_decay
  .dw bullet_hit_noise
  .db $66
bullet_hit_noise:
  .db thirtysecond, $0B, endsound

; Bullet Miss Sound
bullet_miss_header:
  .db 1
  .db SFX_1
  .db 1
  .db NOISE
  .db $30
  .db ve_miss_decay
  .dw bullet_miss_noise
  .db $90
bullet_miss_noise:
  .db thirtysecond, $04, endsound

; Enemy Death Sound
enemy_die_header:
  .db 1
  .db SFX_1
  .db 1
  .db NOISE
  .db $30
  .db ve_rising_decay
  .dw enemy_die_noise
  .db $80
enemy_die_noise:
  .db eighth, $0A, endsound

; Player death sound
player_die_header:
  .db 1
  .db SFX_2
  .db 1
  .db NOISE
  .db $30
  .db ve_rising_decay
  .dw player_die_stream
  .db $60
player_die_stream:
  .db half, $1C, endsound

; Player hurt sound
player_hurt_header:
  .db 1
  .db SFX_2
  .db 1
  .db NOISE
  .db $30
  .db ve_rising_decay
  .dw player_hurt_stream
  .db $90
player_hurt_stream:
  .db quarter, $1C, endsound

; Player dodge sound
player_dodge_header:
  .db 1
  .db SFX_2
  .db 1
  .db SQUARE_2
  .db $B0
  .db ve_vol_chord
  .dw player_dodge_stream
  .db $90
player_dodge_stream:
  .db thirtysecond
  .db set_loop1_counter, 2
.loop:
  .db Fs4, Gs4, As4, B4, loop1
  .dw .loop
  .db endsound

; Player ability sound
player_ability_header:
  .db 1
  .db SFX_2
  .db 1
  .db SQUARE_2
  .db $B0
  .db ve_vol_chord
  .dw player_ability_stream
  .db $90
player_ability_stream:
  .db thirtysecond
  .db set_loop1_counter, 2
.loop:
  .db Fs3, Fs4, Fs5, Fs4, Fs3, Fs4, loop1
  .dw .loop
  .db endsound
