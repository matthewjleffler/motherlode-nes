; soundtables.asm
;  Contains tables for the sound engine

; AUDIO TABLES

; Note constants
A1    = $00                   ; the "1" means Octave 1
As1   = $01                   ; the "s" means "sharp"
Bb1   = $01                   ; the "b" means "flat"  A# == Bb, so same value
B1    = $02

C2    = $03
Cs2   = $04
Db2   = $04
D2    = $05
Ds2   = $06
Eb2   = $06
E2    = $07
F2    = $08
Fs2   = $09
Gb2   = $09
G2    = $0A
Gs2   = $0B
Ab2   = $0B
A2    = $0C
As2   = $0D
Bb2   = $0D
B2    = $0E

C3    = $0F
Cs3   = $10
Db3   = $10
D3    = $11
Ds3   = $12
Eb3   = $12
E3    = $13
F3    = $14
Fs3   = $15
Gb3   = $15
G3    = $16
Gs3   = $17
Ab3   = $17
A3    = $18
As3   = $19
Bb3   = $19
B3    = $1a

C4    = $1b
Cs4   = $1c
Db4   = $1c
D4    = $1d
Ds4   = $1e
Eb4   = $1e
E4    = $1f
F4    = $20
Fs4   = $21
Gb4   = $21
G4    = $22
Gs4   = $23
Ab4   = $23
A4    = $24
As4   = $25
Bb4   = $25
B4    = $26

C5    = $27
Cs5   = $28
Db5   = $28
D5    = $29
Ds5   = $2a
Eb5   = $2a
E5    = $2b
F5    = $2c
Fs5   = $2d
Gb5   = $2d
G5    = $2e
Gs5   = $2f
Ab5   = $2f
A5    = $30
As5   = $31
Bb5   = $31
B5    = $32

C6    = $33
Cs6   = $34
Db6   = $34
D6    = $35
Ds6   = $36
Eb6   = $36
E6    = $37
F6    = $38
Fs6   = $39
Gb6   = $39
G6    = $3a
Gs6   = $3b
Ab6   = $3b
A6    = $3c
As6   = $3d
Bb6   = $3d
B6    = $3e

C7    = $3f
Cs7   = $40
Db7   = $40
D7    = $41
Ds7   = $42
Eb7   = $42
E7    = $43
F7    = $44
Fs7   = $45
Gb7   = $45
G7    = $46
Gs7   = $47
Ab7   = $47
A7    = $48
As7   = $49
Bb7   = $49
B7    = $4a

C8    = $4b
Cs8   = $4c
Db8   = $4c
D8    = $4d
Ds8   = $4e
Eb8   = $4e
E8    = $4f
F8    = $50
Fs8   = $51
Gb8   = $51
G8    = $52
Gs8   = $53
Ab8   = $53
A8    = $54
As8   = $55
Bb8   = $55
B8    = $56

C9    = $57
Cs9   = $58
Db9   = $58
D9    = $59
Ds9   = $5a
Eb9   = $5a
E9    = $5b
F9    = $5c
Fs9   = $5d
Gb9   = $5d

REST  = $5e

; Note length
thirtysecond        = $80
sixteenth           = $81
eighth              = $82
quarter             = $83
half                = $84
whole               = $85
d_sixteenth         = $86
d_eighth            = $87
d_quarter           = $88
d_half              = $89
d_whole             = $8A
t_quarter           = $8B
five_eighths        = $8C
five_sixteenths     = $8D

; Opcodes
endsound            = $A0
loop                = $A1
volume_envelope     = $A2
duty                = $A3
set_loop1_counter   = $A4
loop1               = $A5
set_note_offset     = $A6
adjust_note_offset  = $A7
transpose           = $A8

note_table:
  .dw                                                                $07F1, $0780, $0713 ; A1-B1 ($00-$02)
  .dw $06AD, $064D, $05F3, $059D, $054D, $0500, $04B8, $0475, $0435, $03F8, $03BF, $0389 ; C2-B2 ($03-$0E)
  .dw $0356, $0326, $02F9, $02CE, $02A6, $027F, $025C, $023A, $021A, $01FB, $01DF, $01C4 ; C3-B3 ($0F-$1A)
  .dw $01AB, $0193, $017C, $0167, $0151, $013F, $012D, $011C, $010C, $00FD, $00EF, $00E2 ; C4-B4 ($1B-$26)
  .dw $00D2, $00C9, $00BD, $00B3, $00A9, $009F, $0096, $008E, $0086, $007E, $0077, $0070 ; C5-B5 ($27-$32)
  .dw $006A, $0064, $005E, $0059, $0054, $004F, $004B, $0046, $0042, $003F, $003B, $0038 ; C6-B6 ($33-$3E)
  .dw $0034, $0031, $002F, $002C, $0029, $0027, $0025, $0023, $0021, $001F, $001D, $001B ; C7-B7 ($3F-$4A)
  .dw $001A, $0018, $0017, $0015, $0014, $0013, $0012, $0011, $0010, $000F, $000E, $000D ; C8-B8 ($4B-$56)
  .dw $000C, $000C, $000B, $000A, $000A, $0009, $0008                                    ; C9-F#9 ($57-$5D)
  .dw $0000 ;rest

; Volume envelopes
; TODO remove these?
ve_short_staccato   = $00
ve_fade_in          = $01
ve_blip_echo        = $02
ve_tgl_1            = $03
ve_tgl_2            = $04
ve_battlekid_1      = $05
ve_battlekid_1b     = $06
ve_battlekid_2      = $07
ve_battlekid_2b     = $08
ve_drum_decay       = $09
ve_hit_decay        = $0A
ve_hit_long_decay   = $0B
ve_vol_chord        = $0C

volume_envelopes:
  .dw se_ve_1
  .dw se_ve_2
  .dw se_ve_3
  .dw se_ve_tgl_1
  .dw se_ve_tgl_2
  .dw se_battlekid_loud
  .dw se_battlekid_loud_long
  .dw se_battlekid_soft
  .dw se_battlekid_soft_long
  .dw se_drum_decay
  .dw se_hit_decay
  .dw se_hit_long_decay
  .dw se_vol_chord

note_length_table:
  .db $01                     ; 32nd note
  .db $02                     ; 16th note
  .db $04                     ; 8th note
  .db $08                     ; quarter note
  .db $10                     ; half note
  .db $20                     ; whole note
  .db $03                     ; dotted 16th note
  .db $06                     ; dotted 8th note
  .db $0C                     ; dotted quarter note
  .db $18                     ; dotted half note
  .db $30                     ; dotted whole note?
  .db $07                     ; modified quarter to fit after d_sixteenth triplets
  .db $14                     ; 2 quarters plus an 8th
  .db $0A

; Opcode jump table
sound_opcodes:
  .dw se_op_endsound          ; $A0
  .dw se_op_infinite_loop     ; $A1
  .dw se_op_change_ve         ; $A2
  .dw se_op_duty              ; $A3
  .dw se_op_set_loop1_counter ; $A4
  .dw se_op_loop1             ; $A5
  .dw se_op_set_note_offset   ; $A6
  .dw se_op_adjust_note_offset  ; $A7
  .dw se_op_transpose         ; $A8

; Volume envelopes
se_ve_1:
  .db $0F, $0E, $0D, $0C, $09, $05, $00
  .db $FF

se_ve_2:
  .db $01, $01, $02, $02, $03, $03, $04, $04, $07, $07
  .db $08, $08, $0A, $0A, $0C, $0C, $0D, $0D, $0E, $0E
  .db $0F, $0F
  .db $FF

se_ve_3:
  .db $0D, $0D, $0D, $0C, $0B, $00, $00, $00, $00, $00
  .db $00, $00, $00, $00, $06, $06, $06, $05, $04, $00
  .db $FF

se_ve_tgl_1:
  .db $0F, $0B, $09, $08, $07, $06, $00
  .db $FF

se_ve_tgl_2:
  .db $0B, $0B, $0A, $09, $08, $07, $06, $06, $06, $05
  .db $FF

se_battlekid_loud:
  .db $0f, $0e, $0c, $0a, $00
  .db $FF

se_battlekid_loud_long:
  .db $0f, $0e, $0c, $0a, $09
  .db $FF

se_battlekid_soft:
  .db $09, $08, $06, $04, $00
  .db $FF

se_battlekid_soft_long:
  .db $09, $08, $06, $04, $03
  .db $FF

se_drum_decay:
  .db $0E, $09, $08, $06, $04, $03, $02, $01, $00
  .db $FF

se_hit_decay:
  .db $08, $06, $04, $02, $00
  .db $FF

se_hit_long_decay:
  .db $06, $06, $05, $04, $03, $02, $01, $00, $FF

se_vol_chord:
  .db 7, $FF                  ; Sustained volume
