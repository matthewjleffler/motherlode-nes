; math.asm
;   various helpfum math subroutines

; CONSTANTS

; Atan2 / Math
octant            = arg4
angle             = arg4      ; Reuse arg4 after finishing with octant
sum               = arg4
distance          = arg9

; SUBROUTINES

; Returns a random 8-bit number in A (0-255), clobbers arg0
RNG:
  LDA seed+1
  STA arg0 ; store copy of high byte
  ; compute seed+1 ($39>>1 = %11100)
  LSR A ; shift to consume zeroes on left...
  LSR A
  LSR A
  STA seed+1 ; now recreate the remaining bits in reverse order... %111
  LSR A
  EOR seed+1
  LSR A
  EOR seed+1
  EOR seed+0 ; recombine with original low byte
  STA seed+1
  ; compute seed+0 ($39 = %111001)
  LDA arg0 ; original high byte
  STA seed+0
  ASL A
  EOR seed+0
  ASL A
  EOR seed+0
  ASL A
  ASL A
  ASL A
  EOR seed+0
  STA seed+0
  RTS

; Calclulate distance between two points
;
; Args:
;   posX                      - x1
;   posY                      - y1
;   posX2                     - x2
;   posY2                     - y2
; Local:
;   sum
ManhattanDistance:
;findX
  LDA posX                    ; Load x1
  CLC
  CMP posX2                   ; Compare x2
  BCC .x1less
;x2less
  SEC                         ; x1 still in A
  SBC posX2                   ; subtract x2
  STA sum                     ; store result
  JMP .findY
.x1less:
  LDA posX2                   ; Load x2
  SEC
  SBC posX                    ; subtract x1
  STA sum
.findY:
  LDA posY                    ; Load y1
  CLC
  CMP posY2                   ; Compare y2
  BCC .y1less
;y2less
  SEC                         ; y1 still in A
  SBC posY2                   ; subtract y2
  JMP .sum
.y1less:
  LDA posY2                   ; Load y2
  SEC
  SBC posY                    ; subtract y1
.sum:
  CLC                         ; y is in A
  ADC sum                     ; add with x
  BCC .finish                 ; if we didn't overflow, we're done
  LDA #$FF                    ; Just set result to full if we overflowed
.finish:
  STA sum                     ; Store final sum
  CLC
  RTS

; from https://codebase64.org/doku.php?id=base:8bit_atan2_8-bit_angle
;
; Args:
;   posX                      - x1
;   posY                      - y1
;   posX2                     - x2
;   posY2                     - y2
;
; Local:
;   octant
; A will be the 256 degree angle, also in angle
; X and Y will be trashed
Atan2:
  LDA #0
  STA octant
  LDA posX
  SEC
  SBC posX2
  BCS .o1
  EOR #$ff
.o1:
  TAX
  ROL octant
  LDA posY
  SEC
  SBC posY2
  BCS .o2
  EOR #$ff
.o2:
  TAY
  ROL octant
  LDA log2_tab,x
  SEC
  SBC log2_tab,y
  BCC .o3
  EOR #$ff
.o3:
  TAX
  LDA octant
  ROL A
  AND #%111
  TAY
  LDA atan_tab,x
  EOR octant_adjust,y
  STA angle
  RTS

Divide32:
  LSR A
  LSR A
  LSR A
  RTS

Divide16:
  LSR A
  LSR A
  LSR A
  LSR A
  RTS

Divide8:
  LSR A
  LSR A
  LSR A
  LSR A
  LSR A
  RTS

Divide4:
  LSR A
  LSR A
  LSR A
  LSR A
  LSR A
  LSR A
  RTS

; Atan2 divided down to 32 degrees
Atan232:
  JSR Atan2
  JSR Divide32
  RTS

; Atan2 divided down to 16 degrees
Atan216:
  JSR Atan2
  JSR Divide16
  RTS
