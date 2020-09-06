; Header
  .inesprg 1                  ; 1x 16KB PRG code
  .ineschr 1                  ; 1x  8KB CHR data
  .inesmap 0                  ; Mapper 0 = NROM, no bank swapping
  .inesmir 1                  ; Background mirroring

; Variables
  .include "vars.asm"         ; Variable definitions

; Banks
  .bank 0
  .org $C000
  .include "game.asm"         ; Main program

  .bank 1
  .org $E000                  ; Align background so the lower address is $00
  .include "tables.asm"       ; Tables, attributes, palettes, sprites

  .org $FFFA                  ; First of three vectors start here
  .dw NMI                     ; NMI interrupt
  .dw RESET                   ; RESET interrupt
  .dw 0                       ; External interrupt IRQ (unused here)

  .bank 2
  .org $0000
  .incbin "mario.chr"         ; Includes 8KB graphics file
