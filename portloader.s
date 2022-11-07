.pc02

; Add the PRG header
.segment "HEADER"
.word $0801

.segment "CODE"
; Add a BASIC startline
.word entry-2
.byte $00,$00,$9e
.byte "2061"
.byte $00,$00,$00

; Entry point at $080d
entry:
    jmp x16init

.include "x16.inc"
.include "dosx.s"

.struct Patchset
    patchfile     .byte 16
    romfile       .byte 16
    size_blocks   .byte 2
    crc16         .word
    crc16offset   .byte 3
    crc16length   .byte 3
    magicoffset   .byte 3
    magiclen      .byte 1
    magicdata     .byte 16
    description   .byte 32
.endstruct

.struct Dirlist
    filename      .byte 16
    type          .byte 1
    size_blocks   .byte 2
.endstruct

GOLDENRAM = $0400

.segment "ZEROPAGE"
ptr:
    .res 2
.segment "BSS"
next_patch_idx:
    .res 1
patch_idx:
    .res 1
next_dirlist_idx:
    .res 1
dirlist_idx:
    .res 1
tmp0:
    .res 1
tmp1:
    .res 1
dirlist:
    .res 256*.sizeof(Dirlist)
patchset:
    .res 32*.sizeof(Patchset)


.segment "CODE"

x16init:
    ; initialize memory
    lda #<patchset
    sta X16::Reg::r0L
    lda #>patchset
    sta X16::Reg::r0H
    lda #<(32*.sizeof(Patchset))
    sta X16::Reg::r1L
    lda #>(32*.sizeof(Patchset))
    sta X16::Reg::r1H
    lda #0

    jsr X16::Kernal::MEMORY_FILL

    jsr populate_patchset_dirlist

    jsr X16::Kernal::CLRCHN

    jsr display_heading

    jsr display_patchlist

    lda next_patch_idx
    beq return

    jsr input_numbers

    pha

    lda #$0D
    jsr X16::Kernal::CHROUT

    pla
    dec
    cmp next_patch_idx
    bcs return

    sta patch_idx

    ; try to find the ROM

    jsr findrom
    bcs notfound

    ; just in case we EOFed
    lda #2
    jsr DOSx::close

    ldx dirlist_idx
    ldy #Dirlist::filename
    jsr point_to_dirlist_field

    ldx ptr
    ldy ptr+1
    lda #2
    jsr DOSx::open

    ; At this point LFN 2 is the ROM file

    ldx patch_idx
    ldy #Patchset::patchfile
    jsr point_to_patchset_field

    ldx ptr
    ldy ptr+1
    lda #3
    jsr DOSx::open


    lda #3
    jsr patch_ff_past_meta

    jmp chain_to_goldenram_loader

notfound:
    ; Display message "NO ROM FOUND"
    lda #<msg_no_rom
    sta ptr
    lda #>msg_no_rom
    sta ptr+1

    jsr print_string

return:
    rts

msg_no_rom:
    .byte "NO MATCHING ROM FOUND",$0D,$00

.proc findrom
    stz dirlist_idx

rom_loop:
    lda dirlist_idx
    cmp next_dirlist_idx
    bne :+
        jmp end
    :

    ; Display message "CHECKING CANDIDATE FILE: "
    lda #<msg_checking_rom
    sta ptr
    lda #>msg_checking_rom
    sta ptr+1

    jsr print_string

    ldx dirlist_idx
    ldy #Dirlist::filename
    jsr point_to_dirlist_field

    jsr print_string

    lda #$0D
    jsr X16::Kernal::CHROUT

    ; Check size
    ldx patch_idx
    ldy #Patchset::size_blocks
    jsr point_to_patchset_field

    lda (ptr)
    sta tmp0
    inc ptr
    bne :+
        inc ptr+1
    :
    
    lda (ptr)
    ora tmp0
    beq no_size ; patch didn't specify size if this is true
    lda (ptr)
    pha

   ; Display message "CHECKING SIZE: "
    lda #<msg_size
    sta ptr
    lda #>msg_size
    sta ptr+1

    jsr print_string


    ldx dirlist_idx
    ldy #Dirlist::size_blocks
    jsr point_to_dirlist_field

    pla
    ldy #1
    cmp (ptr),y
    beq :+
        ; Display message "NOT A MATCH"
        lda #<msg_nope
        sta ptr
        lda #>msg_nope
        sta ptr+1

        jsr print_string

        jmp next_rom_loop
    :
    lda tmp0
    cmp (ptr)
    beq :+
        ; Display message "NOT A MATCH"
        lda #<msg_nope
        sta ptr
        lda #>msg_nope
        sta ptr+1

        jsr print_string

        jmp next_rom_loop
    :

    ; Display message "OK"
    lda #<msg_ok
    sta ptr
    lda #>msg_ok
    sta ptr+1

    jsr print_string

no_size:
    ldx dirlist_idx
    ldy #Dirlist::filename
    jsr point_to_dirlist_field

    ; open file
    lda #2
    ldx ptr
    ldy ptr+1
    jsr DOSx::open
    bcc :+
        jmp error
    :

    ; Check magic
    ldx patch_idx
    ldy #Patchset::magiclen
    jsr point_to_patchset_field

    lda (ptr)
    beq no_magic

    ; Display message "CHECKING MAGIC: "
    lda #<msg_magic
    sta ptr
    lda #>msg_magic
    sta ptr+1

    jsr print_string


    ldx patch_idx
    ldy #Patchset::magicoffset
    jsr point_to_patchset_field
    
    lda (ptr)
    sta X16::Reg::r0L
    ldy #1
    lda (ptr),y
    sta X16::Reg::r0H
    iny
    lda (ptr),y
    sta X16::Reg::r1L
    stz X16::Reg::r1H

    lda #2
    jsr DOSx::seek
    bcc :+
        jmp error
    :

    ldx #2
    jsr X16::Kernal::CHKIN

    ldx patch_idx
    ldy #Patchset::magiclen
    jsr point_to_patchset_field

    lda (ptr)
    sta tmp0

    ldx patch_idx
    ldy #Patchset::magicdata
    jsr point_to_patchset_field

    ldy #0
magicdata_loop:
    lda (ptr),y
    sta tmp1
    jsr X16::Kernal::CHRIN
    
    pha
    jsr X16::Kernal::READST
    cmp #0
    beq :+
        jmp error
    :
    pla

    cmp tmp1
    beq :+
        ; Display message "NOT A MATCH"
        lda #<msg_nope
        sta ptr
        lda #>msg_nope
        sta ptr+1
        jsr print_string
        jmp moveon
    :

    iny
    cpy tmp0
    bcc magicdata_loop

    ; Display message "OK"
    lda #<msg_ok
    sta ptr
    lda #>msg_ok
    sta ptr+1

    jsr print_string


no_magic:
    ; check CRC16

    ; seek first to the offset so that we'll be at the right place once we have the length
    ldx patch_idx
    ldy #Patchset::crc16offset
    jsr point_to_patchset_field

    lda (ptr)
    sta X16::Reg::r0L
    ldy #1
    lda (ptr),y
    sta X16::Reg::r0H
    iny
    lda (ptr),y
    sta X16::Reg::r1L
    stz X16::Reg::r1H

    lda #2
    jsr DOSx::seek
    bcc :+
        jmp error
    :


    ldx patch_idx
    ldy #Patchset::crc16length
    jsr point_to_patchset_field

    lda (ptr)
    sta tmp0
    sta X16::Reg::r0L
    ldy #1
    lda (ptr),y
    sta X16::Reg::r0H
    ora tmp0
    sta tmp0
    iny
    lda (ptr),y
    sta X16::Reg::r1L
    ora tmp0
    beq no_crc16
    stz X16::Reg::r1H

    ;;;
    ; Display message "CHECKING CRC-16: "
    lda #<msg_crc16
    sta ptr
    lda #>msg_crc16
    sta ptr+1

    jsr print_string



    ldx patch_idx
    ldy #Patchset::crc16
    jsr point_to_patchset_field

    lda #2
    jsr DOSx::crc16_file

    txa
    cmp (ptr)
    beq :+
        ; Display message "NOT A MATCH"
        lda #<msg_nope
        sta ptr
        lda #>msg_nope
        sta ptr+1
        jsr print_string
        bra moveon
    :

    tya
    ldy #1
    cmp (ptr),y
    beq :+
        ; Display message "NOT A MATCH"
        lda #<msg_nope
        sta ptr
        lda #>msg_nope
        sta ptr+1
        jsr print_string
        bra moveon
    :
    
    ; Display message "OK"
    lda #<msg_ok
    sta ptr
    lda #>msg_ok
    sta ptr+1

    jsr print_string


no_crc16:
    ; found!
    
    clc
    rts

next_rom_loop:
    inc dirlist_idx
    jmp rom_loop
moveon:
    lda #2
    jsr DOSx::close
    ; Display message "NOT A MATCH"
    lda #<msg_nope
    sta ptr
    lda #>msg_nope
    sta ptr+1

    bra next_rom_loop
error:
    lda #2
    jsr DOSx::close
end:
    sec
    rts

msg_checking_rom:
    .byte "CHECKING CANDIDATE FILE: ",$00
msg_size:
    .byte $AD,"CHECKING SIZE: ",$00
msg_magic:
    .byte $AD,"CHECKING MAGIC: ",$00
msg_crc16:
    .byte $AD,"CHECKING CRC-16: ",$00
msg_ok:
    .byte "OK",$0D,$00
msg_nope:
    .byte "NOT A MATCH",$0D,$00


.endproc

.proc input_numbers
    stz tmp0
    stz tmp1
    ; grab a line from the screen
    ; loop until we find a digit, store up to two, convert them to binary
    ; and return the result in A
loop1:
    jsr X16::Kernal::CHRIN
    cmp #$0D
    beq return
    cmp #$30
    bcc loop1
    cmp #$3A
    bcs loop1
    sec
    sbc #$30
    sta tmp0
loop2:
    jsr X16::Kernal::CHRIN
    cmp #$0D
    beq return
    cmp #$30
    bcc loop3
    cmp #$3A
    bcs loop3
    sec
    sbc #$30
    ldx tmp0
    stx tmp1
    sta tmp0
loop3:
    jsr X16::Kernal::CHRIN
    cmp #$0D
    beq return
    bra loop3
return:
    lda tmp1
    asl
    asl
    asl
    adc tmp0
    sta tmp0
    lda tmp1
    asl
    adc tmp0

    rts


.endproc


.proc display_heading

    lda #<message1
    sta ptr
    lda #>message1
    sta ptr+1

    jsr print_string

    lda next_patch_idx
    beq end

    lda #<message2
    sta ptr
    lda #>message2
    sta ptr+1

    jsr print_string

end:
    rts
message1:
    .byte $93
    .byte $0D,$0D,"PORTLOADER V0.1",$0D,$0D
    .byte "THIS UTILITY LOADS GAMES THAT HAVE BEEN PORTED TO THE COMMANDER X16",$0D
    .byte "FROM OTHER SYSTEMS BY WAY OF THE ORIGINAL ROM IMAGE AND A .PAT FILE",$0D
    .byte "PLACE BOTH FILES IN THE SAME DIRECTORY AS THIS PROGRAM.",$0D,$0D,$00
message2:
    .byte "SELECT A PATCH FILE FROM THE LIST BELOW",$0D,$00
.endproc

.proc display_patchlist
    stz patch_idx

    lda next_patch_idx
    beq nopatches

loop:
    lda patch_idx
    inc
    jsr a_to_dec ; 10s in x, 1s in a

    pha
    cpx #0
    beq :+
        txa
        clc
        adc #$30
        jsr X16::Kernal::CHROUT
    :
    pla
    clc
    adc #$30
    jsr X16::Kernal::CHROUT
    lda #'.'
    jsr X16::Kernal::CHROUT
    lda #' '
    jsr X16::Kernal::CHROUT
    ldx patch_idx
    ldy #Patchset::description
    jsr point_to_patchset_field

    jsr print_string

    lda #' '
    jsr X16::Kernal::CHROUT


    lda #'['
    jsr X16::Kernal::CHROUT

    ldx patch_idx
    ldy #Patchset::patchfile
    jsr point_to_patchset_field

    jsr print_string

    lda #']'
    jsr X16::Kernal::CHROUT

    lda #$0D
    jsr X16::Kernal::CHROUT

    lda patch_idx
    inc 
    sta patch_idx
    cmp next_patch_idx
    bcc loop

    rts
nopatches:
    lda #<message
    sta ptr
    lda #>message
    sta ptr+1

    jsr print_string

    rts
message:
    .byte "NO PATCH FILES FOUND.",$0D,$00
.endproc

.proc print_string

    ldy #0

    :
        lda (ptr),y
        beq :+
        phy
        jsr X16::Kernal::CHROUT
        ply
        iny
        bne :-
    :

    rts
.endproc

.proc a_to_dec
    ldx #0
tens_loop:
    sec
    sbc #10
    bmi ones
    inx
    bra tens_loop
ones:
    adc #10
    rts
    

.endproc

.proc populate_patchset_dirlist
    stz next_patch_idx
    stz next_dirlist_idx

    ldx #0
    ldy #0
    jsr DOSx::opendir
    bcs error

dirloop:
    jsr DOSx::readdir
    bcs enddir

    jsr print_dirent

    jsr is_dirent_a_patch
    bcc :+
        jsr save_dirlist_ent
        bra dirloop
    :

    jsr save_patch_name

    inc next_patch_idx
    bra dirloop
enddir:
    jsr DOSx::closedir
    stz patch_idx
patchloop:
    jsr save_patch_meta

    lda patch_idx
    inc
    sta patch_idx
    cmp next_patch_idx
    bcc patchloop

    rts
error:
    jsr DOSx::closedir
    sec
    rts
.endproc

.proc point_to_dirlist_field
    ; x = index of entry
    ; y = field offset

    lda #<dirlist
    sta ptr
    lda #>dirlist
    sta ptr+1

    cpx #0
    beq after_entryloop
entryloop:
    lda #.sizeof(Dirlist)
    clc
    adc ptr
    sta ptr
    lda ptr+1
    adc #0
    sta ptr+1
    dex
    bne entryloop
after_entryloop:
    tya
    clc
    adc ptr
    sta ptr
    lda ptr+1
    adc #0
    sta ptr+1
end:
    rts
.endproc

.proc point_to_patchset_field
    ; x = index of entry
    ; y = field offset

    lda #<patchset
    sta ptr
    lda #>patchset
    sta ptr+1

    cpx #0
    beq after_entryloop
entryloop:
    lda #.sizeof(Patchset)
    clc
    adc ptr
    sta ptr
    lda ptr+1
    adc #0
    sta ptr+1
    dex
    bne entryloop
after_entryloop:
    tya
    clc
    adc ptr
    sta ptr
    lda ptr+1
    adc #0
    sta ptr+1
end:
    rts
.endproc

.proc save_dirlist_ent
    ldx next_dirlist_idx
    ldy #0
    jsr point_to_dirlist_field

    ldy #Dirlist::filename
    ldx #0
nameloop:
    lda DOSx::dirent_name,x
    sta (ptr),y
    beq end_nameloop
    iny
    inx
    cpx #.sizeof(Dirlist::filename)
    bcc nameloop
end_nameloop:
    ldy #Dirlist::size_blocks
    lda DOSx::dirent_size
    sta (ptr),y
    iny
    lda DOSx::dirent_size+1
    sta (ptr),y
    ldy #Dirlist::type
    lda DOSx::dirent_type
    sta (ptr),y
    inc next_dirlist_idx
    
    rts
.endproc

.proc chain_to_goldenram_loader
    lda #<loader_code
    sta X16::Reg::r0L
    lda #>loader_code
    sta X16::Reg::r0H

    lda #<GOLDENRAM
    sta X16::Reg::r1L
    lda #>GOLDENRAM
    sta X16::Reg::r1H

    lda #<(after_loader_code-loader_code)
    sta X16::Reg::r2L
    lda #>(after_loader_code-loader_code)
    sta X16::Reg::r2H

    jsr X16::Kernal::MEMORY_COPY

    jmp ldr_loader
.endproc

.pushseg
.segment "RELOCCODE"

loader_code: ; should have the address in regular RAM (from the PRG) where our loader is
.org GOLDENRAM ; loader code will get copied to golden RAM

.proc ldr_loader ; this should assemble to a golden RAM address
    ldx #<msg_intro
    ldy #>msg_intro
    jsr ldr_print_string

loader_loop:
    ldx #3 ; switch to patch
    jsr X16::Kernal::CHKIN

    jsr X16::Kernal::CHRIN
    pha
    jsr X16::Kernal::READST
    bne eof
    pla
    cmp #$10
    beq copy
    cmp #$11
    bne :+
        jmp supp
    :
    cmp #$1F
    bne :+
        jmp bank
    :
    ; unsupported
    ldx #<msg_ERR
    ldy #>msg_ERR
    jsr ldr_print_string
end:
    lda #2
    jsr X16::Kernal::CLOSE
    lda #3
    jsr X16::Kernal::CLOSE
    jsr X16::Kernal::CLRCHN

    rts
eof:
    pla
done:
    ldx #<msg_END
    ldy #>msg_END
    jsr ldr_print_string
    jsr X16::Kernal::CLRCHN

    lda #$0D
    jsr X16::Kernal::KBDBUF_PUT
    lda #'R'
    jsr X16::Kernal::KBDBUF_PUT
    lda #'U'
    jsr X16::Kernal::KBDBUF_PUT
    lda #'N'
    jsr X16::Kernal::KBDBUF_PUT
    lda #$0D
    jsr X16::Kernal::KBDBUF_PUT


    bra end
copy:
    lda #'C'
    jsr X16::Kernal::CHROUT

    jsr X16::Kernal::CHRIN
    sta seek_fn+2
    jsr X16::Kernal::CHRIN
    sta seek_fn+3
    jsr X16::Kernal::CHRIN
    sta seek_fn+4
    jsr seek

    ldx #3 ; switch to patch
    jsr X16::Kernal::CHKIN

    jsr X16::Kernal::CHRIN
    sta X16::Reg::r0L
    jsr X16::Kernal::CHRIN
    sta X16::Reg::r0H

    jsr X16::Kernal::CHRIN
    sta ptr
    jsr X16::Kernal::CHRIN
    sta ptr+1

    ldx #2 ; switch to ROM
    jsr X16::Kernal::CHKIN

copy_loop:
    lda X16::Reg::r0L
    ora X16::Reg::r0H
    bne :+
        jmp loader_loop
    :
    jsr X16::Kernal::CHRIN
    sta (ptr)
    inc ptr
    bne :+
        inc ptr+1
    :

    lda X16::Reg::r0L
    bne :+
        dec X16::Reg::r0H
    :
    dec X16::Reg::r0L
    
    bra copy_loop
supp:
    lda #'S'
    jsr X16::Kernal::CHROUT

    jsr X16::Kernal::CHRIN
    sta X16::Reg::r0L
    jsr X16::Kernal::CHRIN
    sta X16::Reg::r0H

    jsr X16::Kernal::CHRIN
    sta ptr
    jsr X16::Kernal::CHRIN
    sta ptr+1

    bra copy_loop ; it's the same from here
bank:
    lda #'B'
    jsr X16::Kernal::CHROUT

    jsr X16::Kernal::CHRIN
    sta X16::Reg::RAMBank

    jmp loader_loop

seek_fn:
    .byte "P",$02,"xxx",$00
msg_intro:
    .byte "NOW LOADING ROM WITH PATCH",$0D,$00
msg_ERR:
    .byte "ERROR",$0D,$00
msg_END:
    .byte $0D,"DONE",$0D,$00

seek:
    lda #6
    ldx #<seek_fn
    ldy #>seek_fn
    jsr X16::Kernal::SETNAM

    lda #15
    ldx #8
    ldy #15
    jsr X16::Kernal::SETLFS

    jsr X16::Kernal::OPEN
    lda #15
    jsr X16::Kernal::CLOSE

    rts
.endproc

.proc ldr_print_string
    stx ptr
    sty ptr+1
    ldy #0

    :
        lda (ptr),y
        beq :+
        phy
        jsr X16::Kernal::CHROUT
        ply
        iny
        bne :-
    :

    rts
.endproc

.reloc
after_loader_code:

.popseg

.proc is_dirent_a_patch
    ldy #$ff
null_loop:
    iny
    lda DOSx::dirent_name,y
    bne null_loop

    dey
    beq no
    lda DOSx::dirent_name,y
    cmp #'T'
    bne no

    dey
    beq no
    lda DOSx::dirent_name,y
    cmp #'A'
    bne no

    dey
    beq no
    lda DOSx::dirent_name,y
    cmp #'P'
    bne no

    dey
    beq no
    lda DOSx::dirent_name,y
    cmp #'.'
    bne no
yes:
    clc
    rts
no:
    sec
    rts
.endproc

.proc print_dirent
    ldx #0
    :
        lda DOSx::dirent_name,x
        beq :+
        phx
        jsr X16::Kernal::CHROUT
        plx
        inx
        bne :-
    :
    lda #13
    jsr X16::Kernal::CHROUT

    rts
.endproc

.proc patch_ff_past_meta
    pha
    tax
    jsr X16::Kernal::CHKIN

header_loop:
    jsr X16::Kernal::CHRIN
    pha
    jsr X16::Kernal::READST
    cmp #0
    bne eof
    pla
    beq error ; zero, unrecognized
    dec
    beq version 
    dec
    beq description
    dec
    beq magic
    dec
    beq error ; CRC32, is unsupported
    dec
    bne :+
        jmp crc16
    :
    dec
    bne :+
        jmp size
    :
    cmp #9 ; End of metadata, the last valid tag, is #$0f, but we've done 6 DECs
    bne error
end:
    pla
    clc
    rts
eof:
    pla
error:
    pla
    jsr DOSx::close
    sec
    rts
version:
    jsr X16::Kernal::CHRIN
    cmp #1
    bne error
    bra header_loop
description:
    stz tmp0
    jsr X16::Kernal::CHRIN
    sta tmp1
    cmp #.sizeof(Patchset::description)
    bcs error
@1:
    jsr X16::Kernal::CHRIN
    ldy tmp0
    iny
    sty tmp0
    cpy tmp1
    bcc @1
    bra header_loop
magic:
    ; offset
    jsr X16::Kernal::CHRIN
    jsr X16::Kernal::CHRIN
    jsr X16::Kernal::CHRIN

    ; length
    jsr X16::Kernal::CHRIN
    sta tmp1
    stz tmp0
    ; data
@1:
    jsr X16::Kernal::CHRIN
    ldy tmp0
    inc tmp0
    cpy tmp1
    bcc @1
    jmp header_loop
crc16:
    ; offset
    jsr X16::Kernal::CHRIN
    jsr X16::Kernal::CHRIN
    jsr X16::Kernal::CHRIN

    ; length
    jsr X16::Kernal::CHRIN
    jsr X16::Kernal::CHRIN
    jsr X16::Kernal::CHRIN

    ; crc16
    jsr X16::Kernal::CHRIN
    jsr X16::Kernal::CHRIN
    jmp header_loop
size:
    jsr X16::Kernal::CHRIN
    jsr X16::Kernal::CHRIN
    jmp header_loop
.endproc

.proc save_patch_name
    lda #1
    sta X16::Reg::RAMBank

    ldx next_patch_idx
    ldy #Patchset::patchfile
    jsr point_to_patchset_field

    ldy #0
patchname_loop:
    lda DOSx::dirent_name,y
    sta (ptr),y
    beq end_patchname
    iny
    cpy #.sizeof(Patchset::patchfile)
    bcc patchname_loop
end_patchname:
    rts
.endproc


.proc save_patch_meta
    ldx patch_idx
    ldy #Patchset::patchfile
    jsr point_to_patchset_field

    jsr X16::Kernal::CLRCHN

    lda #2
    ldx ptr
    ldy ptr+1
    jsr DOSx::open
    bcs error

    ldx #2
    jsr X16::Kernal::CHKIN

header_loop:
    jsr X16::Kernal::CHRIN
    pha
    jsr X16::Kernal::READST
    cmp #0
    bne eof
    pla
    beq error ; zero, unrecognized
    dec
    beq version 
    dec
    beq description
    dec
    beq magic
    dec
    beq error ; CRC32, is unsupported
    dec
    bne :+
        jmp crc16
    :
    dec
    bne :+
        jmp size
    :
    cmp #9 ; End of metadata, the last valid tag, is #$0f, but we've done 6 DECs
    bne error
end:
    lda #2
    jsr DOSx::close
    clc
    rts
eof:
    pla
error:
    lda #2
    jsr DOSx::close
    sec
    rts
version:
    jsr X16::Kernal::CHRIN
    cmp #1
    bne error
    bra header_loop
description:
    ldx patch_idx
    ldy #Patchset::description
    jsr point_to_patchset_field
    stz tmp0
    jsr X16::Kernal::CHRIN
    sta tmp1
    cmp #.sizeof(Patchset::description)
    bcs error
@1:
    jsr X16::Kernal::CHRIN
    ldy tmp0
    sta (ptr),y
    iny
    sty tmp0
    cpy tmp1
    bcc @1
    ldy tmp0 ; null terminate
    lda #0
    sta (ptr),y
    bra header_loop
magic:
    ldx patch_idx
    ldy #Patchset::magicoffset
    jsr point_to_patchset_field

    jsr X16::Kernal::CHRIN
    sta (ptr)
    jsr X16::Kernal::CHRIN
    ldy #1
    sta (ptr),y
    jsr X16::Kernal::CHRIN
    ldy #2
    sta (ptr),y

    ldx patch_idx
    ldy #Patchset::magiclen
    jsr point_to_patchset_field

    jsr X16::Kernal::CHRIN
    sta (ptr)
    sta tmp1
    cmp #.sizeof(Patchset::magicdata)
    bcs error
    stz tmp0

    ldx patch_idx
    ldy #Patchset::magicdata
    jsr point_to_patchset_field
@1:
    jsr X16::Kernal::CHRIN
    ldy tmp0
    sta (ptr),y
    inc tmp0
    cpy tmp1
    bcc @1
    ldy tmp0 ; null terminate
    lda #0
    sta (ptr),y
    jmp header_loop
crc16:
    ldx patch_idx
    ldy #Patchset::crc16offset
    jsr point_to_patchset_field

    jsr X16::Kernal::CHRIN
    sta (ptr)
    jsr X16::Kernal::CHRIN
    ldy #1
    sta (ptr),y
    jsr X16::Kernal::CHRIN
    ldy #2
    sta (ptr),y

    ldx patch_idx
    ldy #Patchset::crc16length
    jsr point_to_patchset_field

    jsr X16::Kernal::CHRIN
    sta (ptr)
    jsr X16::Kernal::CHRIN
    ldy #1
    sta (ptr),y
    jsr X16::Kernal::CHRIN
    ldy #2
    sta (ptr),y

    ldx patch_idx
    ldy #Patchset::crc16
    jsr point_to_patchset_field

    jsr X16::Kernal::CHRIN
    sta (ptr)
    jsr X16::Kernal::CHRIN
    ldy #1
    sta (ptr),y
    jmp header_loop
size:
    ldx patch_idx
    ldy #Patchset::size_blocks
    jsr point_to_patchset_field

    jsr X16::Kernal::CHRIN
    sta (ptr)
    jsr X16::Kernal::CHRIN
    ldy #1
    sta (ptr),y
    jmp header_loop
.endproc