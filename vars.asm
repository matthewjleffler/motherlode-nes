; VARIABLES

  .rsset $0000       ; put pointers in zero page

pointerLo   .rs 1   ; pointer variables are declared in RAM
pointerHi   .rs 1   ; low byte first, high byte immediately after
buttons     .rs 1   ; buttons

; CONSTANTS
CONTROLLER1 = $4016
BUTTONA     = %10000000
BUTTONB     = %01000000
BUTTONSEL   = %00100000
BUTTONSTA   = %00010000
BUTTONU     = %00001000
BUTTOND     = %00000100
BUTTONL     = %00000010
BUTTONR     = %00000001

; Move Speed
SLOW        = $01
FAST        = $03
