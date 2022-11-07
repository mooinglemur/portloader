.scope DOSx

.pushseg
.segment "ZEROPAGE"
ptr:
    .res 2

.segment "BSS"
errorno:
    .res 1
diropen:
    .res 1
tmp0:
    .res 1
tmp1:
    .res 1
tmp2:
    .res 1
crc16_state:
    .res 2
dirent_name:
    .res 16
dirent_type:
    .res 1
dirent_size:
    .res 2
at_eof:
    .res 1
.popseg

; +-------+
; + close +
; +-------+
;
; Inputs: .A = LFN
; Affects: .A .X .Y
;
; Close existing handle if it's open

.proc close
    beq end

    jsr X16::Kernal::CLOSE

    jsr X16::Kernal::CLRCHN
end:
    rts
.endproc


; +----------+
; + closedir +
; +----------+
;
; Inputs: none
; Affects: .A .X .Y
;
; Close existing handle if it's open

.proc closedir
    lda diropen
    beq end

    lda #1
    jsr X16::Kernal::CLOSE
    stz diropen

    jsr X16::Kernal::CLRCHN
end:
    rts
.endproc

; +------------+
; + crc16_file +
; +------------+
;
; Inputs: .A r0 r1
; Affects: .A .X .Y
; Returns: .X .Y .C
;
; Description: takes the open LFN indicated by A, and reads in
; the number of bytes indicated by the 32-bit value in r0 (low) and r1 (high).
; The CRC-16 is reflected in the return value of X and Y.  If carry is set,
; there was an error in reading the file.

.proc crc16_file
    tax
    beq error
    jsr X16::Kernal::CHKIN
    bcs error
    stz at_eof
    lda #$FF
    sta crc16_state
    sta crc16_state+1
loop:
    lda X16::Reg::r0L
    ora X16::Reg::r0H
    ora X16::Reg::r1L
    ora X16::Reg::r1H
    beq end

    lda at_eof
    bne eof

    jsr X16::Kernal::CHRIN
    pha
    jsr X16::Kernal::READST
    sta tmp0

    pla
    jsr crc16


    ; fast 32-bit decrement
    lda X16::Reg::r0L
    bne dec0L
    lda X16::Reg::r0H
    bne dec0H
    lda X16::Reg::r1L
    bne dec1L
    dec X16::Reg::r1H
dec1L:
    dec X16::Reg::r1L
dec0H:
    dec X16::Reg::r0H
dec0L:
    dec X16::Reg::r0L

    bra loop
eof:
    pla
error:
    jsr X16::Kernal::CLRCHN
    sec
    rts
end:
    jsr X16::Kernal::CLRCHN
    clc
    ldx crc16_state
    ldy crc16_state+1
    rts

.endproc


; +------+
; + open +
; +------+
;
; Inputs: .A = LFN/SA (the same for convenience, valid values 2-14)
;         .X = low byte of optional null-terminated filename
;         .Y = high byte of optional null-terminated filename
; Affects: .A .X .Y
; Returns: .C clear on success, set on error, .A/errno contains error number

.proc open
    pha
    stx tmp0
    tya
    ora tmp0
    bne :+
        lda #1
        bra error
    :
    stx ptr
    sty ptr+1

    pla
    sta tmp0

    jsr close

    jsr get_ptr_length
    ldx ptr
    ldy ptr+1
    jsr X16::Kernal::SETNAM

    lda tmp0
    ldx #8
    ldy tmp0
    jsr X16::Kernal::SETLFS

    jsr X16::Kernal::OPEN
    bcs error

    clc
    bra end
error:
    sta errorno
    lda tmp0
    jsr close
    sec
end:
    rts
.endproc



; +---------+
; + opendir +
; +---------+
;
; Inputs: .X = low byte of optional null-terminated directory string (0 for default)
;         .Y = high byte of optional null-terminated directory string (0 for default)
; Affects: .A .X .Y
; Returns: .C clear on success, set on error, .A/errno contains error number

.proc opendir
    stx tmp0
    tya
    ora tmp0
    beq is_default_dir
    stx ptr
    sty ptr+1
    bra cont
is_default_dir:
    ldx #<default_dir
    stx ptr
    ldy #>default_dir
    sty ptr+1
cont:
    jsr closedir
    inc diropen

    jsr get_ptr_length
    ldx ptr
    ldy ptr+1
    jsr X16::Kernal::SETNAM

    lda #1
    ldx #8
    ldy #0
    jsr X16::Kernal::SETLFS

    jsr X16::Kernal::OPEN
    bcs error

    clc
    bra end
error:
    sta errorno
    jsr closedir
    sec
end:
    rts
.endproc

; +---------+
; + readdir +
; +---------+
;
; Inputs: none
; Affects: .A .X .Y
; Returns: values in dirent_*
;          .C is set on error or EOF with A/errno set
;          .C is clear on success
.proc readdir
    lda diropen
    bne :+
        jmp end
    :
    ldx #1
    jsr X16::Kernal::CHKIN
    bcc :+
        jmp error
    :

    filename_offset = tmp0
    state = tmp1
    skip_entry = tmp2

next_entry:
    stz filename_offset
    stz state
    stz skip_entry

loop:
    jsr X16::Kernal::CHRIN
    pha
    jsr X16::Kernal::READST
    cmp #0
    beq :+
        jmp eof
    :

    lda state
    asl
    tax
    pla
    jmp (state_tbl,x)
    
state_tbl:
    .word state_skip, state_skip, state_size_l, state_size_h
    .word state_await_open_quote, state_await_close_quote
    .word state_null_term, state_await_type, state_await_eol


state_size_l:
    sta dirent_size
state_skip:
    inc state
    bra loop

state_size_h:
    sta dirent_size+1
    inc state
    bra loop

state_await_open_quote:
    cmp #$22
    bne loop
    inc state
    bra loop

state_await_close_quote:
    cmp #$22
    beq state_skip

    ldy filename_offset
    cpy #14
    beq assert_skip_entry

    sta dirent_name,y
    inc filename_offset
    bra loop

state_null_term:
    pha
    lda #0
    ldy filename_offset
    sta dirent_name,y
    inc state
    pla
state_await_type:
    cmp #$20
    beq loop
    sta dirent_type
    inc state
    cmp #'F'
    beq assert_skip_entry
    cmp #'H'
    beq assert_skip_entry
    bra loop

state_await_eol:
    cmp #0
    beq success
    bra loop

assert_skip_entry:
    inc skip_entry
    jmp loop

success:
    lda skip_entry
    beq :+
        jmp next_entry
    :
    jsr X16::Kernal::CLRCHN
    lda dirent_type
    beq error
    clc
    rts
eof:
    pla
    lda #13 
error:
    sta errorno
    jsr closedir
    sec
end:
    ldx #0
    ldy #0
    rts
.endproc

; +------+
; + seek +
; +------+
;
; Inputs: .A = LFN/SA (the same for convenience, valid values 2-14)
;         r0+r1 = 32 bit absolute file offset
; Affects: .A .X .Y
; Returns: .C clear on success, set on error, .A/errno contains error number

.proc seek
    sta seekfn+1
    lda X16::Reg::r0L
    sta seekfn+2
    lda X16::Reg::r0H
    sta seekfn+3
    lda X16::Reg::r1L
    sta seekfn+4
    lda X16::Reg::r1H
    sta seekfn+5

    lda #6
    ldx #<seekfn
    ldy #>seekfn
    jsr X16::Kernal::SETNAM

    lda #15
    ldx #8
    ldy #15
    jsr X16::Kernal::SETLFS
    bcs error

    jsr X16::Kernal::OPEN
    bcs error
    bra end
error:
    sta errorno
    lda #15
    jsr close
    sec
    rts
end:
    lda #15
    jsr close
    clc
    rts

seekfn:
    .byte "Pxxxxx",$00

.endproc


; ##############################
; # Private subs and variables #
; ##############################

.proc crc16
    ldx #8
    eor crc16_state+1
rotate_loop:
    asl crc16_state
    rol
    bcc clear
    tay
    lda crc16_state
    eor #$21
    sta crc16_state
    tya
    eor #$10
clear:
    dex
    bne rotate_loop
    sta crc16_state+1
    rts
.endproc

.proc get_ptr_length
    ldy #0
loop:
    lda (ptr),y
    beq end
    iny
    bne loop
end:
    tya
    rts
.endproc


default_dir:
    .byte "$",$00

.endscope