; main.asm
;   enntry point, program layout

; Header
  .inesprg 1                  ; 1x 16KB PRG code
  .ineschr 1                  ; 1x  8KB CHR data
  .inesmap 0                  ; Mapper 0 = NROM, no bank swapping
  .inesmir 1                  ; Background mirroring

; Variables
  .include "vars.asm"         ; Variable definitions
  .include "const.asm"        ; Global constnat definitions

; Banks
  .bank 0
  .org $C000
  .include "game.asm"         ; Game loop

  .bank 1
  .include "tables.asm"       ; Tables, attributes, palettes, sprites
  .include "soundtables.asm"  ; Sound tables
  .include "songs.asm"        ; Song/sfx tables

  .org $FFFA                  ; First of three vectors start here
  .dw NMI                     ; NMI interrupt
  .dw RESET                   ; RESET interrupt
  .dw 0                       ; External interrupt IRQ (unused here)

  .bank 2
  .org $0000
  .incbin "motherlode.chr"    ; Includes 8KB graphics file
