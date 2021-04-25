  processor 6502
  ORG $0000
  lda 5
  sta $FF
  lda 7
  lda $FF
  clc
  bcc point
  lda #66
  BRK
point:
  lda #30
  sta $0100
  inc $0100
  inc $0100
  inc $0100
  inc $0100
  dec $0100
  lda $0100
  pha
  lda #100
  pla
  SED
  lda #0
  adc #$22
  adc #$22
  ORG $FFFC
  .byte #0
  .byte #0
