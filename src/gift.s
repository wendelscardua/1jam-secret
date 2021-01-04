.include "constants.inc"
.include "mmc3-constants.inc"
.include "header.inc"
.include "charmap.inc"
.include "vram-buffer.inc"

.feature force_range
.linecont +

; famitone2 config
FT_PAL_SUPPORT=0
FT_NTSC_SUPPORT=1
FT_SFX_ENABLE=1
FT_THREAD=1
FT_DPCM_ENABLE=0
FT_SFX_STREAMS=4
FT_DPCM_OFF=$c000

; music/sfx constants
.enum music_track
  CanonInD
.endenum

.enum sfx
  Collision
.endenum

.macro SFX effect, channel
  save_regs
  LDA #sfx::effect
  LDX #.ident ( .concat( "FT_SFX_", .string(channel) ) )
  JSR FamiToneSfxPlay
  restore_regs
.endmacro

.macro PLAY track
.local skip
  save_regs
  LDA #music_track::track
  JSR FamiToneMusicPlay
  restore_regs
.endmacro

.macro SCREEN_OFF
  LDA #$00
  STA PPUCTRL ; disable NMI
  STA PPUMASK ; disable rendering
.endmacro

.macro SCREEN_ON
  LDA #%10001000  ; turn on NMIs, sprites use second pattern table
  STA PPUCTRL
  LDA #%00011110  ; turn on screen
  STA PPUMASK
.endmacro

.macro VERTICAL_PPUADDR
  LDA #%10001100
  STA PPUCTRL
.endmacro

.macro HORIZONTAL_PPUADDR
  LDA #%10001000
  STA PPUCTRL
.endmacro

; game config

LAST_FRAME = 240
NUM_CHARACTERS = 9
FRAMES_PER_ALETHIOSCOPE_MINUTE = 60

.enum direction
  up
  down
  left
  right
.endenum

.segment "ZEROPAGE"
FT_TEMP: .res 3
.segment "FAMITONE"
FT_BASE_ADR: .res 186
.segment "CODE"
.include "famitone2.s"

.segment "OAM"
.struct Sprite
  ycoord .byte
  tile .byte
  flag .byte
  xcoord .byte
.endstruct

oam_sprites:
  .repeat 64
    .tag Sprite
  .endrepeat
.zeropage

.importzp buttons
.importzp last_frame_buttons
.importzp released_buttons
.importzp pressed_buttons
.importzp rng_seed
.importzp rle_ptr

; zp vars
addr_ptr: .res 2 ; generic address pointer
ppu_addr_ptr: .res 2
sprite_ptr: .res 2 ; metasprite pointer

.export nmis
.export old_nmis

nmis: .res 1
old_nmis: .res 1

language: .res 1
.enum languages
  english
  portuguese
.endenum

sprite_counter: .res 1

; room stuff
current_room: .res 1

; alethioscope stuff
alethioscope_character: .res 1
alethioscope_current_frame: .res 1
alethioscope_target_frame: .res 1
alethioscope_frame_ptr: .res 2
alethioscope_frame_counter: .res 1

; character stuff
character_room: .res NUM_CHARACTERS
character_x: .res NUM_CHARACTERS
character_sx: .res NUM_CHARACTERS
character_y: .res NUM_CHARACTERS
character_sy: .res NUM_CHARACTERS
character_delta_sx: .res NUM_CHARACTERS
character_delta_sy: .res NUM_CHARACTERS
character_target_x: .res NUM_CHARACTERS
character_target_y: .res NUM_CHARACTERS

;;;; investigation stuff
detective_room: .res 1
; HACK: same data order as in room metadata (so it's easier to load)
begin_investigation_stuff:
; character present in room
room_character: .res 1
room_character_x: .res 1
room_character_y: .res 1
; detective coordinates
detective_x: .res 1
detective_y: .res 1
; collision boxes for room exits (leading to map menu)
exit_x1: .res 4
exit_y1: .res 4
exit_x2: .res 4
exit_y2: .res 4
end_investigation_stuff:

; temp string

clock_string: .res 6

; temp stuff
temp_x: .res 1
temp_y: .res 1
temp_acc: .res 1
temp_tile: .res 1
temp_flag: .res 1

.enum game_states
  waiting_to_start
  prologue
  help
  investigating
  alethioscoping
  game_over
.endenum

game_state: .res 1

.segment "BSS"
; non-zp RAM goes here

.segment "PRGRAM"


.segment "CODE"

.import reset_handler
.import readjoy
.import rand
.import unrle

.import music_data
.import sfx_data

.macro KIL ; pseudo instruction to kill the program
  .byte $12
.endmacro

.macro VBLANK
  .local vblankwait
vblankwait:
  BIT PPUSTATUS
  BPL vblankwait
.endmacro

.macro save_regs
  PHA
  TXA
  PHA
  TYA
  PHA
.endmacro

.macro restore_regs
  PLA
  TAY
  PLA
  TAX
  PLA
.endmacro

.proc irq_handler
  RTI
.endproc

.proc nmi_handler
  save_regs
  INC nmis
  JSR flush_vram_buffer
  JSR refresh_oam
  ; reset ppuaddr
  BIT PPUSTATUS
  JSR set_scroll
  JSR FamiToneUpdate
  restore_regs
  RTI
.endproc

.export main
.proc main
  SEI         ; ignore IRQs
  CLD         ; disable decimal mode
  LDX #$40
  STX $4017   ; disable APU frame IRQ
  LDX #$ff
  TXS         ; Set up stack
  INX         ; now X = 0
  STX PPUCTRL ; disable NMI
  STX PPUMASK ; disable rendering
  STX $4010   ; disable DMC IRQs

  LDX #0
clear_ram:
  LDA #$00
  STA $0000,X
  STA $0100,X
  STA $0300,X
  STA $0400,X
  STA $0500,X
  STA $0600,X
  STA $0700,X
  LDA #$fe
  STA $0200,X
  INX
  BNE clear_ram

  ; horizontal mirroring (fix for everdrive)
  LDA #%00000001
  STA $a000

  ; enable PRG RAM
  LDA #%10000000
  STA $a001

  reset_vram_stack

  SCREEN_ON

  LDX #<music_data
  LDY #>music_data
  LDA #1
  JSR FamiToneInit

  ; init FamiTone SFX
  LDX #<sfx_data
  LDY #>sfx_data
  LDA #1
  JSR FamiToneSfxInit

  ; init rng
  LDA #$a9
  STA rng_seed
  LDA #$73
  STA rng_seed+1

  SEI ; disable interrupts

  ; prepare clock string
  LDA #$1a
  STA clock_string+2
  LDA #$00
  STA clock_string+5

  JSR go_to_title

forever:
  LDA nmis
  CMP old_nmis
  BEQ etc
  STA old_nmis
  ; new frame code
  JSR game_state_handler
  JSR slow_updates
etc:
  JMP forever
.endproc

.proc refresh_oam
  ; Refresh OAM
  LDA #$00
  STA OAMADDR
  LDA #$02
  STA OAMDMA
  RTS
.endproc

.proc set_scroll
  LDA #$00
  STA PPUSCROLL
  STA PPUSCROLL
  RTS
.endproc

.proc load_palettes
  ; cobbles Y
  LDY PPUSTATUS
  LDY #$3f
  STY PPUADDR
  LDY #$00
  STY PPUADDR
:
  LDA palettes,Y
  STA PPUDATA
  INY
  CPY #$20
  BNE :-
  RTS
.endproc

.proc load_default_chr
  ; bg
  LDA #0
  STA BANK_SELECT
  LDA #0
  STA BANK_DATA
  LDA #1
  STA BANK_SELECT
  LDA #2
  STA BANK_DATA

  ; sprites
  LDA #2
  STA BANK_SELECT
  LDA #4
  STA BANK_DATA

  LDA #3
  STA BANK_SELECT
  LDA #5
  STA BANK_DATA

  LDA #4
  STA BANK_SELECT
  LDA #6
  STA BANK_DATA

  LDA #5
  STA BANK_SELECT
  LDA #7
  STA BANK_DATA
  RTS
.endproc

.proc game_state_handler
  LDX game_state
  LDA game_state_handlers_h, X
  PHA
  LDA game_state_handlers_l, X
  PHA
  RTS
.endproc

.proc slow_updates
  JSR rand
  RTS
.endproc

.proc go_to_title
  LDA #game_states::waiting_to_start
  STA game_state

  ; erase sprites
  LDX #$00
  LDA #$F0
:
  STA oam_sprites+Sprite::ycoord, X
  .repeat .sizeof(Sprite)
  INX
  .endrepeat
  BNE :-

  SCREEN_OFF

  JSR load_palettes

  JSR load_default_chr

  LDA PPUSTATUS
  LDA #$20
  STA PPUADDR
  LDA #$00
  STA PPUADDR

  LDA #<nametable_title
  STA rle_ptr
  LDA #>nametable_title
  STA rle_ptr+1
  JSR unrle

  VBLANK

  SCREEN_ON

  ; PLAY CanonInD

  RTS
.endproc

.proc go_to_prologue
  LDA #game_states::prologue
  STA game_state

  ; erase sprites
  LDX #$00
  LDA #$F0
:
  STA oam_sprites+Sprite::ycoord, X
  .repeat .sizeof(Sprite)
  INX
  .endrepeat
  BNE :-

  SCREEN_OFF

  JSR load_palettes

  JSR load_default_chr

  LDA PPUSTATUS
  LDA #$20
  STA PPUADDR
  LDA #$00
  STA PPUADDR

  LDA #<nametable_prologue
  STA rle_ptr
  LDA #>nametable_prologue
  STA rle_ptr+1
  JSR unrle

  VBLANK

  SCREEN_ON

  RTS
.endproc

.proc go_to_help
  LDA #game_states::help
  STA game_state

  ; erase sprites
  LDX #$00
  LDA #$F0
:
  STA oam_sprites+Sprite::ycoord, X
  .repeat .sizeof(Sprite)
  INX
  .endrepeat
  BNE :-

  SCREEN_OFF

  JSR load_palettes

  JSR load_default_chr

  LDA PPUSTATUS
  LDA #$20
  STA PPUADDR
  LDA #$00
  STA PPUADDR

  LDA #<nametable_help
  STA rle_ptr
  LDA #>nametable_help
  STA rle_ptr+1
  JSR unrle

  VBLANK

  SCREEN_ON

  RTS
.endproc

.proc go_to_investigating
  SCREEN_OFF

  LDA #game_states::investigating
  STA game_state

  ; erase sprites
  LDX #$00
  LDA #$F0
:
  STA oam_sprites+Sprite::ycoord, X
  .repeat .sizeof(Sprite)
  INX
  .endrepeat
  BNE :-

  JSR load_palettes

  JSR load_default_chr

  LDA detective_room
  JSR load_room
  JSR load_investigation_stuff

  ; PLAY CanonInD

  RTS
.endproc

; at least alethioscope_character and alethioscope_current_frame must've been set beforehand
.proc go_to_alethioscoping
  LDA #game_states::alethioscoping
  STA game_state

  ; erase sprites
  LDX #$00
  LDA #$F0
:
  STA oam_sprites+Sprite::ycoord, X
  .repeat .sizeof(Sprite)
  INX
  .endrepeat
  BNE :-

  JSR load_palettes

  JSR load_default_chr

  JSR load_alethioscope_current_frame

  ; game setup here

  ; PLAY CanonInD

  RTS
.endproc

.proc go_to_game_over
  LDA #game_states::game_over
  STA game_state
  RTS
.endproc

.proc waiting_to_start
  JSR readjoy
  LDA pressed_buttons
  AND #BUTTON_START
  BEQ :+

  ; TODO selectable language, english text
  LDA #languages::portuguese
  STA language
  JSR go_to_prologue
:
  RTS
.endproc

.proc prologue
  JSR readjoy
  LDA pressed_buttons
  AND #(BUTTON_START|BUTTON_SELECT|BUTTON_A|BUTTON_B)
  BEQ :+
  JSR go_to_help
:
  RTS
.endproc

.proc help
  JSR readjoy
  LDA pressed_buttons
  AND #(BUTTON_START|BUTTON_SELECT|BUTTON_A|BUTTON_B)
  BEQ :+

  ; TODO selectable language, english text
  LDA #languages::portuguese
  STA language

  JSR game_setup

  JSR go_to_investigating
  ; LDA #4
  ; STA alethioscope_character
  ; LDA #((21-18)*60+10)
  ; STA alethioscope_current_frame
  ; LDA #240
  ; STA alethioscope_target_frame
  ; JSR go_to_alethioscoping
:
  RTS
.endproc

.proc game_over
  ; erase sprites
  LDX #$00
  LDA #$F0
:
  STA oam_sprites+Sprite::ycoord, X
  .repeat .sizeof(Sprite)
  INX
  .endrepeat
  BNE :-
  STX sprite_counter

  JSR readjoy
  LDA pressed_buttons
  AND #(BUTTON_START|BUTTON_SELECT)
  BEQ :+
  JSR go_to_title
:
  RTS
.endproc

; things to set before first call to "go_to_investigating"
.proc game_setup
  ; initial room
  LDA #2 ; hall
  STA detective_room
  RTS
.endproc

.proc investigating
  LDA #0
  STA sprite_counter
  LDX room_character

  LDA character_sprites_l, X
  STA addr_ptr
  LDA character_sprites_h, X
  STA addr_ptr+1
  LDA room_character_x, X
  STA temp_x
  LDA room_character_y, X
  STA temp_y
  JSR display_metasprite

  LDA #<detective_metasprite
  STA addr_ptr
  LDA #>detective_metasprite
  STA addr_ptr+1
  LDA detective_x, X
  STA temp_x
  LDA detective_y, X
  STA temp_y
  JSR display_metasprite

  JSR erase_remaining_sprites

  RTS
.endproc

.proc alethioscoping
  LDA #0
  STA sprite_counter
  
  JSR render_characters
  JSR erase_remaining_sprites

  DEC alethioscope_frame_counter
  BEQ next_alethioscope_frame
  BMI next_alethioscope_frame

  JSR alethioscope_animation

  RTS
next_alethioscope_frame:
  INC alethioscope_current_frame
  LDA alethioscope_current_frame
  CMP alethioscope_target_frame
  BCS :+
  JSR load_alethioscope_current_frame
  RTS
:
  JSR go_to_investigating
  RTS
.endproc

.proc alethioscope_animation
  LDX #0
loop:
  LDA character_x, X
  CMP character_target_x, X
  BCS decrement_x
  LDA character_sx, X
  CLC
  ADC character_delta_sx, X
  STA character_sx, X
  BCC check_y
  INC character_x, X
  JMP check_y
decrement_x:
  LDA character_sx, X
  SEC
  SBC character_delta_sx, X
  STA character_sx, X
  BCS check_y
  DEC character_x, X
check_y:

  LDA character_y, X
  CMP character_target_y, X
  BCS decrement_y
  LDA character_sy, X
  CLC
  ADC character_delta_sy, X
  STA character_sy, X
  BCC next
  INC character_y, X
  JMP next
decrement_y:
  LDA character_sy, X
  SEC
  SBC character_delta_sy, X
  STA character_sy, X
  BCS next
  DEC character_y, X
next:
  INX
  CPX #NUM_CHARACTERS
  BNE loop
  RTS  
.endproc

.proc render_characters
  LDX #0
loop:
  LDA character_room, X
  CMP current_room
  BNE next
  LDA character_sprites_l, X
  STA addr_ptr
  LDA character_sprites_h, X
  STA addr_ptr+1
  LDA character_x, X
  STA temp_x
  LDA character_y, X
  STA temp_y
  TXA
  PHA
  JSR display_metasprite
  PLA
  TAX
next:
  INX
  CPX #NUM_CHARACTERS
  BNE loop
  RTS
.endproc

; input: (addr_ptr) = metasprite pointer
;        temp_x and temp_y = screen position for metasprite origin
; cobbles X, Y
.proc display_metasprite
  LDY #0
  LDX sprite_counter
loop:
  LDA (addr_ptr),Y ; delta x
  CMP #128
  BEQ return
  INY
  CLC
  ADC temp_x

  ;  trying to skip offscreen tiles
  BCC :+
  INY
  INY
  INY
  JMP loop
:

  STA oam_sprites+Sprite::xcoord,X
  LDA (addr_ptr),Y ; delta y
  INY
  SEC
  SBC #$01
  CLC
  ADC temp_y

  ; trying to skip offscreen tiles
  CMP #$20
  BCS :+
  INY
  INY
  JMP loop
:

  STA oam_sprites+Sprite::ycoord,X
  LDA (addr_ptr),Y ; tile
  INY
  STA oam_sprites+Sprite::tile,X
  LDA (addr_ptr),Y ; flags
  INY
  STA oam_sprites+Sprite::flag,X
  .repeat .sizeof(Sprite)
  INX
  .endrepeat
  JMP loop
return:
  STX sprite_counter
  RTS
.endproc

.proc erase_remaining_sprites
  LDX sprite_counter
  LDA #$F0
:
  STA oam_sprites+Sprite::ycoord, X
  .repeat .sizeof(Sprite)
  INX
  .endrepeat
  BNE :-
  RTS
.endproc

.proc load_alethioscope_current_frame
  LDA #FRAMES_PER_ALETHIOSCOPE_MINUTE
  STA alethioscope_frame_counter
  LDX alethioscope_current_frame
  LDA keyframe_lt_l, X
  STA alethioscope_frame_ptr
  LDA keyframe_lt_h, X
  STA alethioscope_frame_ptr+1
  LDY #0
  LDX #0
loop:
  LDA (alethioscope_frame_ptr), Y
  INY
  STA character_room, X
  LDA (alethioscope_frame_ptr), Y
  INY
  STA character_x, X
  LDA #0
  STA character_sx, X
  LDA (alethioscope_frame_ptr), Y
  INY
  STA character_y, X
  LDA #0
  STA character_sy, X
  LDA (alethioscope_frame_ptr), Y
  INY
  STA character_delta_sx, X
  LDA (alethioscope_frame_ptr), Y
  INY
  STA character_delta_sy, X  
  INX
  CPX #NUM_CHARACTERS
  BNE loop

  LDA alethioscope_current_frame
  CMP #LAST_FRAME
  BEQ skip_read_target

  ; keep reading next frame data for target_x/target_y
  LDX #0
next_frame_loop: 
  INY ; skip room

  LDA (alethioscope_frame_ptr), Y
  INY
  STA character_target_x, X

  LDA (alethioscope_frame_ptr), Y
  INY
  STA character_target_y, X

  INY ; skip delta sx
  INY ; skip delta sy

  INX
  CPX #NUM_CHARACTERS
  BNE next_frame_loop

skip_read_target:
  LDX alethioscope_character
  LDA character_room, X
  JSR load_room

  LDX alethioscope_current_frame
  LDA clock_digits_0, X
  STA clock_string+0
  LDA clock_digits_1, X
  STA clock_string+1
  LDA clock_digits_2, X
  STA clock_string+3
  LDA clock_digits_3, X
  STA clock_string+4

  write_string_to_vram $2057, clock_string

  RTS
.endproc

; input: A = desired room
; if current_room != A, change it to A then loads nametable
.proc load_room
  CMP current_room
  BNE :+
  RTS
:
  STA current_room
  SCREEN_OFF

  LDA PPUSTATUS
  LDA #$20
  STA PPUADDR
  LDA #$00
  STA PPUADDR

  LDX current_room
  LDA room_pointers_l, X
  STA rle_ptr
  LDA room_pointers_h, X
  STA rle_ptr+1
  JSR unrle

  vram_buffer_alloc 5
  LDA #($20 | $80)
  STA vram_buffer, X
  INX
  LDA #$44
  STA vram_buffer, X
  INX
  LDA current_room
  ASL
  CLC
  ADC language
  TAY
  LDA room_strings_l, Y
  STA vram_buffer, X
  INX
  LDA room_strings_h, Y
  STA vram_buffer, X
  INX
  LDA #$00
  STA vram_buffer, X
  STX vram_buffer_sp

  VBLANK

  SCREEN_ON

  RTS
.endproc

.proc load_investigation_stuff
  LDX detective_room
  LDA room_metadata_pointers_l, X
  STA addr_ptr
  LDA room_metadata_pointers_h, X
  STA addr_ptr+1
  LDY #(end_investigation_stuff-begin_investigation_stuff-1)
loop:
  LDA (addr_ptr), Y
  STA begin_investigation_stuff, Y
  DEY
  BPL loop
  RTS
.endproc

.segment "VECTORS"
.addr nmi_handler, reset_handler, irq_handler

.segment "RODATA"

.define game_state_handlers waiting_to_start-1, prologue-1, help-1, investigating-1, alethioscoping-1, game_over-1

game_state_handlers_l: .lobytes game_state_handlers
game_state_handlers_h: .hibytes game_state_handlers

palettes:
.incbin "../assets/bg-palettes.pal"
.incbin "../assets/sprite-palettes.pal"

nametable_title: .incbin "../assets/nametables/title.rle"
nametable_main: .incbin "../assets/nametables/main.rle"
nametable_prologue: .incbin "../assets/nametables/prologue.rle"
nametable_help: .incbin "../assets/nametables/help.rle"

; room names
.define room_strings $0000, $0000, \
                     string_study_en, string_study_pt, \
                     string_hall_en, string_hall_pt, \
                     string_lounge_en, string_lounge_pt, \
                     string_library_en, string_libray_pt, \
                     string_dining_room_en, string_dining_room_pt, \
                     string_billiard_room_en, string_billiard_room_pt, \
                     string_conservatory_en, string_conservatory_pt, \
                     string_ballroom_en, string_ballroom_pt, \
                     string_kitchen_en, string_kitchen_pt

room_strings_l: .lobytes room_strings
room_strings_h: .hibytes room_strings

string_study_en: .byte "Study", $00
string_study_pt: .byte "Escritorio", $00 ; TODO diacritics
string_hall_en:
string_hall_pt: .byte "Hall", $00
string_lounge_en: .byte "Lounge", $00
string_lounge_pt: .byte "Sala de estar", $00
string_library_en: .byte "Library", $00
string_libray_pt: .byte "Biblioteca", $00
string_dining_room_en: .byte "Dining room", $00
string_dining_room_pt: .byte "Sala de jantar", $00
string_billiard_room_en: .byte "Billiard room", $00
string_billiard_room_pt: .byte "Salao de jogos", $00 ; TODO diacritics
string_conservatory_en: .byte "Conservatory", $00
string_conservatory_pt: .byte "Estufa", $00
string_ballroom_en: .byte "Ballroom", $00
string_ballroom_pt: .byte "Salao de festas", $00 ; TODO diacritics
string_kitchen_en: .byte "Kitchen", $00
string_kitchen_pt: .byte "Cozinha", $00

string_now_en: .byte $01, "Present", $00
string_now_pt: .byte "Presente", $00

; rooms:
.define room_pointers $0000, \
                      nametable_study, nametable_hall, nametable_lounge, nametable_library, \
                      nametable_dining_room, nametable_billiard_room, nametable_conservatory, nametable_ballroom, \
                      nametable_kitchen
room_pointers_l: .lobytes room_pointers
room_pointers_h: .hibytes room_pointers

nametable_study: .incbin "../assets/nametables/study.rle"
nametable_hall: .incbin "../assets/nametables/hall.rle"
nametable_lounge: .incbin "../assets/nametables/lounge.rle"
nametable_library: .incbin "../assets/nametables/library.rle"
nametable_dining_room: .incbin "../assets/nametables/dining-room.rle"
nametable_billiard_room: .incbin "../assets/nametables/billiard-room.rle"
nametable_conservatory: .incbin "../assets/nametables/conservatory.rle"
nametable_ballroom: .incbin "../assets/nametables/ballroom.rle"
nametable_kitchen: .incbin "../assets/nametables/kitchen.rle"

; rooms - investigation metadata
.define room_metadata_pointers $0000, \
                               study_metadata, hall_metadata, lounge_metadata, library_metadata, \
                               dining_room_metadata, billiard_room_metadata, conservatory_metadata, ballroom_metadata, \
                               kitchen_metadata
room_metadata_pointers_l: .lobytes room_metadata_pointers
room_metadata_pointers_h: .hibytes room_metadata_pointers

; metadata format:
; hitboxes for exits (4 x1, 4 x2, 4 y1, 4y2)
; room character, followed by x, y coordinates
; detective initial x, y coordinates

study_metadata:
  .byte $1, $60, $68
  .byte $b0, $b0
  .byte $a0, $00, $00, $00
  .byte $cf, $00, $00, $00
  .byte $d0, $00, $00, $00
  .byte $e8, $00, $00, $00
hall_metadata:
  .byte $0, $c0, $a0
  .byte $70, $80
  .byte $00, $60, $00, $00
  .byte $1f, $9f, $00, $00
  .byte $80, $d0, $00, $00
  .byte $af, $e8, $00, $00
lounge_metadata:
  .byte $9, $70, $80
  .byte $40, $b0
  .byte $30, $00, $00, $00
  .byte $6f, $00, $00, $00
  .byte $d0, $00, $00, $00
  .byte $e8, $00, $00, $00
library_metadata:
  .byte $2, $b0, $50
  .byte $c0, $90
  .byte $60, $e0, $00, $00
  .byte $9f, $ff, $00, $00
  .byte $d0, $60, $00, $00
  .byte $e8, $9f, $00, $00 
dining_room_metadata:
  .byte $3, $88, $60
  .byte $30, $40
  .byte $00, $30, $00, $00
  .byte $1f, $5f, $00, $00
  .byte $50, $20, $00, $00
  .byte $7f, $3f, $00, $00 
billiard_room_metadata:
  .byte $4, $20, $80
  .byte $38, $50
  .byte $30, $e0, $00, $00
  .byte $5f, $ff, $00, $00
  .byte $20, $a0, $00, $00
  .byte $3f, $bf, $00, $00
conservatory_metadata:
  .byte $5, $58, $b0
  .byte $b8, $60
  .byte $e0, $00, $00, $00
  .byte $ff, $00, $00, $00
  .byte $40, $00, $00, $00
  .byte $6f, $00, $00, $00
ballroom_metadata:
  .byte $6, $a0, $c0
  .byte $30, $50
  .byte $00, $e0, $30, $b0
  .byte $1f, $ff, $4f, $cf
  .byte $70, $70, $20, $20
  .byte $9f, $9f, $3f, $3f
kitchen_metadata:
  .byte $7, $b0, $98
  .byte $40, $50
  .byte $30, $00, $00, $00
  .byte $5f, $00, $00, $00
  .byte $20, $00, $00, $00
  .byte $3f, $00, $00, $00

.include "../assets/metasprites.inc"

; A..H + V + ded V
.define character_sprites metasprite_0_data, metasprite_1_data, metasprite_2_data, metasprite_3_data, \
                          metasprite_4_data, metasprite_5_data, metasprite_6_data, metasprite_7_data, \
                          metasprite_8_data, metasprite_9_data

character_sprites_l: .lobytes character_sprites
character_sprites_h: .hibytes character_sprites

detective_metasprite = metasprite_10_data

.include "../assets/char-positions.inc"

; masks for more efficient ranged rand
rand_mask:
.byte %00000000 ; <= 0
.byte %00000001 ; <= 1
.repeat 2
.byte %00000011 ; <= 2, 3
.endrepeat
.repeat 4
.byte %00000111 ; <= 4..7
.endrepeat
.repeat 8
.byte %00001111 ; <= 8..15
.endrepeat
.repeat 16
.byte %00011111 ; <= 16..31
.endrepeat
.repeat 32
.byte %00111111 ; <= 32..65
.endrepeat
.repeat 64
.byte %01111111 ; <= 64..127
.endrepeat
.repeat 128
.byte %11111111 ; <= 128..255
.endrepeat

.segment "CHR"
.incbin "../assets/chr/bg-4k-main.chr"
.incbin "../assets/chr/sprites-4k.chr"

; 1k blocks
;  0 : bg-main
;  1 : bg-main
;  2 : bg-main
;  3 : bg-main
;  4 : sp
;  5 : sp
;  6 : sp
;  7 : sp

; banks
; 0 : 2k bg
; 1 : 2k bg
; 2 : 1k sp
; 3 : 1k sp
; 4 : 1k sp
; 5 : 1k sp
