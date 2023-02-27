; -----------------------------------------------------------------------------
; Simple Racketball Game Demo
;
;
; Author:  Shirobon
; Date:    2023/02/11
; -----------------------------------------------------------------------------

    .include "sfr_def.s"        ; Contains definitions of hardware registers

; -----------------------------------------------------------------------------
; Game Variable Addresses and Constants
; -----------------------------------------------------------------------------
v_btn       = $10   ; Current active state of buttons
v_btn_old   = $11   ; Previous state of buttons [UNUSED]
v_btn_chg   = $12   ; Which buttons have changed from v_btn_old to v_btn [UNUSED]
v_gameover  = $13   ; Flag for gameover

v_pad_x     = $14   ; X position of paddle (The leftmost edge)
v_ball_x    = $15   ; X position of ball
v_ball_y    = $16   ; Y position of ball
v_ball_xv   = $17   ; X velocity of ball
v_ball_yv   = $18   ; Y velocity of ball

b_sleep     = $7    ; Sleep Button Position
b_mode      = $6    ; Mode Button Position
b_b         = $5    ; B
b_a         = $4    ; A
b_r         = $3    ; Right
b_l         = $2    ; Left
b_d         = $1    ; Down
b_u         = $0    ; Up

; -----------------------------------------------------------------------------
; Interrupt Vectors
; -----------------------------------------------------------------------------

    .org $00        ; Reset Vector
    jmpf __main     ; Jump far past header to entry point
    
    .org $03        ; INT0 Interrupt (External)
    jmp __nop_vec   ; Nothing to do for this interrupt, just return
    
    .org $0B        ; INT1 Interrupt (External)
    jmp __nop_vec   ; Nothing to do for this interrupt, just return
    
    .org $13        ; INT2 Interrupt (External) or T0L Overflow
    jmp __nop_vec   ; Nothing to do for this interrupt, just return
    
    .org $1B        ; INT3 Interrupt (External) or Base Timer Overflow
    jmp __time_vec  ; Every time this interrupt is risen, call firmware to update time
    
    .org $23        ; T0H Overflow
    jmp __nop_vec   ; Nothing to do for this interrupt, just return
    
    .org $2B        ; T1H or T1L Overflow
    jmp __nop_vec   ; Nothing to do for this interrupt, just return
    
    .org $33        ; SIO0 Interrupt
    jmp __nop_vec   ; Nothing to do for this interrupt, just return
    
    .org $3B        ; SIO1 Interrupt
    jmp __nop_vec   ; Nothing to do for this interrupt, just return
    
    .org $43        ; RFB Interrupt
    jmp __nop_vec   ; Nothing to do for this interrupt, just return
    
    .org $4B        ; P3 Interrupt
    jmp __nop_vec   ; Nothing to do for this interrupt, just return

__nop_vec:
    reti            ; Just return, used for unnecessary ISRs
    
    .org $130       ; Firmware Entry Vector - Update System Time
__time_vec:
    push ie         ; Save current Interrupt Enable settings
    clr1 ie, 7      ; Block all maskable interrupts
    not1 ext, 0     ; NOT1 EXT, 0, followed by a jmpf to a vector
    jmpf __time_vec ; will call the firmware function, and when done, return here
    pop ie          ; Restore previous Interrupt Enable settings
    reti
    
    .org $1F0       ; Firmware Entry Vector - Leave Game Mode
__leave_vec:
    not1 ext, 0     ; Same as __time_vec; call the firmware
    jmpf __leave_vec

; -----------------------------------------------------------------------------
; VMS File Header
; -----------------------------------------------------------------------------
    .org $200       ; For games, header begins at offset $200
    
                    ; 16 bytes of file description (on the VMS)
    .text 16 "RACKETBALL"
                    ; 32 bytes of file description (on the Dreamcast)
    .text 32 "VMU RACKETBALL - (C) SHIROBON"
    
    .string 16 ""   ; Identifier of application that created the file (just skip)
    
                    ; Use nifty Waterbear directive to directly generate the icon data :)
    .include icon "./asset/icon.png"    
    
; -----------------------------------------------------------------------------
; Main Program Entry Point
; -----------------------------------------------------------------------------
    .org $680       ; Main entry point begins at $680
__main:
    clr1 ie, 7      ; Disable interrupts while we initialize hardware
    mov #$81, ocr   ; Setup Oscillation Control Register
                    ; Bit 7 - Clock Divisor. 0=Divide by 12, 1=Divide by 6
                    ; Bit 5 - Set Subclock Mode (32kHz), preserve battery
                    ; Bit 0 - Disable Main Clock. Set to 1 when VMU undocked
    mov #$09, mcr   ; Setup Mode Control Register (LCD Operation)
                    ; Bit 4 - Set to 0, set refresh rate to 83Hz
                    ; Bit 3 - Set to 1 to enable refresh (0 stops LCD)
                    ; Bit 0 - Set to 1 for graphics mode
    mov #$80, vccr  ; Setup LCD Contrast Control Register
                    ; Bit 7 - Set to 1 to enable LCD display (after enabling refresh)
    clr1 p3int, 0   ; Clear bit 0 in Port 3 Interrupt Control Register
                    ; This register sets up interrupts to happen on button press
                    ; Bit 0 - Enable interrupts (which sets bit 1 of p3int on press)
                    ; But, we won't use it (set to 0), because they suck ass
                    ; and don't report when the button was unpressed
    clr1 p1, 7      ; Clear bit 7 in Port 1 Latch
                    ; I don't know why this is necessary, Marcus documents don't
                    ; even show this bit is used for anything. But he does this 
                    ; in his example tetris demo.
    mov #$FF, p3    ; Port 3 is the buttons on the VMU. 0=pressed, 1=unpressed
                    ; Set all to unpressed
    
    clr1 psw, 4     ; Indirect address register bank bits of PSW register
    clr1 psw, 3     ; 00 makes R0-R3 correspond to address 000-003
                    ; Read the docs on indirect addressing
    
    mov #$82, $2    ; Address $2 in RAM stores the offset in which we will
                    ; indirectly address. Address $2 in RAM corresponds to the
                    ; $100 to $1FF range in SFR space.
                    ; Therefore, since we want to write to $182 in XRAM,
                    ; We put $82 in address $2, to designate a write to SFR $182
    mov #2, xbnk    ; LCD Framebuffer (XRAM) is divided into 3 banks. Bank 0 is the
                    ; upper half of LCD, 1 is the bottom half, and 2 is the
                    ; 4 icons on the bottom of the screen.
    xor acc         ; I don't like the game icon, so clear it by writing 0
                    ; to address $182 in bank 2
    st @R2          ; Write that shit.
    
    set1 ie, 7      ; Restore interrupts after initializing all hardware

                    ; Get initial set of buttons
    call __pollbuttons
    
.main:
    call __drawtitle; Draw title screen for most epic VMU racketball game :)

.wait_for_start:    ; Wait until A is pressed on the title screen to begin
    call __pollbuttons
    bn v_btn, b_a, .wait_for_start
                                    
    mov #18, v_pad_x; After A pressed, initialize game variables, and begin game
    mov #24, v_ball_x
    mov #30, v_ball_y
    mov #1, v_ball_xv
    mov #255, v_ball_yv
    mov #0, v_gameover
    
.game_loop:         ; Main game loop. First poll input, then do game logic
    call __pollbuttons
    bpc v_btn, b_l, .move_pad_left
    bpc v_btn, b_r, .move_pad_right

.game_logic:        ; Game logic sections, update variables and render
    call __clearvf  ; Clear Framebuffer at beginning of frame
    
    call __dophysics; Run the game logic
    
    ld v_gameover   ; Check if ball miss and is a gameover
    be #1, __gameover
    
    call __drawfield; Draw the playfield, game entities, then commit framebuffer
    call __drawentities
    call __commitvf

    br .game_loop   ; When finished, do the next frame

.move_pad_left:     ; Move pad left. Check if hit edge, and then go to game logic
    ld v_pad_x      ; Load pad X to acc for compare. Hit edge at x=2, otherwise decrement
    be #2, .game_logic
    dec v_pad_x     ; Hit edge at x=2, otherwise decrement 
    br .game_logic
    
.move_pad_right:    ; Move pad right. Check if hit edge, and then go to game logic
    ld v_pad_x      ; Load pad X to acc for compare. Hit edge at x=36, otherwise increment
    be #36, .game_logic
    inc v_pad_x     ; ^ Since pad width is 10px, and border is 2px, 48-10-2 is 36.
    br .game_logic
    
    
    
    
    
.loop:
    jmp .loop

; -----------------------------------------------------------------------------
; __draw_title - Draws the Title Screen
; Clobbered: acc, b, c, trh, trl
; -----------------------------------------------------------------------------
__drawtitle:
                    ; Draw the first part of the title, [trl+trh] point to it
    mov #<title1, trl
    mov #>title1, trh
    inc trl         ; Increment address by 2 since Waterbear includes the
    inc trl         ; dimensions of the image before the data (disable-able?)
    call __copytovf ; Copy image to virtual framebuffer
    call __commitvf ; Copy virtual framebuffer to real framebuffer
    
    
                    ; Simple delay loop before showing second part of title
    mov #255, b     ; Register B is counter for inner loop
    mov #255, c     ; Register C is counter for outer loop
.outer:
    dec c           ; Decrement outer counter
    ld c            ; Test against 0, if 0, get out of delay loop
    be #0, .done
.inner:             ; If C not 0, do another set of counting in inner loop
    dec b           ; Decrement inner counter
    ld b            ; Test against 0, if 0, get out of inner loop
    be #0, .outer
    br .inner
.done               ; Draw the second part of the title
    mov #<title2, trl
    mov #>title2, trh
    inc trl
    inc trl
    call __copytovf
    call __commitvf

    ret
    
; -----------------------------------------------------------------------------
; __gameover - Draw Game Over screen and return to beginning of program
; THIS IS NOT CALLED, it is jumped to.
; Clobbered: acc, b, c, trh, trl
; -----------------------------------------------------------------------------
__gameover:
    mov #<gameover, trl
    mov #>gameover, trh
    inc trl         ; Increment address by 2 since Waterbear includes the
    inc trl         ; dimensions of the image before the data (disable-able?)
    call __copytovf ; Copy image to virtual framebuffer
    call __commitvf ; Copy virtual framebuffer to real framebuffer
    
    
                    ; Simple Delay loop to show screen for a bit of time
    mov #255, b     ; Register B is counter for inner loop
    mov #255, c     ; Register C is counter for outer loop
.outer:
    dec c           ; Decrement outer counter
    ld c            ; Test against 0, if 0, get out of delay loop
    be #0, .done
.inner:             ; If C not 0, do another set of counting in inner loop
    dec b           ; Decrement inner counter
    ld b            ; Test against 0, if 0, get out of inner loop
    be #0, .outer
    br .inner
.done:              ; We don't call this since we return to __main, and don't
    jmpf __main     ; want to reest our call stack.

; -----------------------------------------------------------------------------
; __drawfield - Draws the game field
; Clobbered: trh, trl
; -----------------------------------------------------------------------------
__drawfield:
                    ; Draw field, [trl+trh] points to it
    mov #<field, trl
    mov #>field, trh
    inc trl         ; Increment address by 2 since Waterbear includes the
    inc trl         ; dimensions of the image before the data (disable-able?)
    call __copytovf ; Copy image to virtual framebuffer
    ret

; -----------------------------------------------------------------------------
; __dophysics - Performs game physics
; Clobbered: acc, b, c
; -----------------------------------------------------------------------------
__dophysics:
    ld v_ball_x     ; First add the velocities to the ball positions
    add v_ball_xv
    st v_ball_x     ; and write the new position back
    ld v_ball_y
    add v_ball_yv
    st v_ball_y
.checkcollision:    ; Check collisions against all edges
    mov #2, acc     ; Left wall
    be v_ball_x, .leftcollision
.a: mov #45, acc    ; Right Wall
    be v_ball_x, .rightcollision
.b: mov #2, acc     ; Top Wall
    be v_ball_y, .topcollision
.c: mov #30, acc    ; Bottom Collision
    be v_ball_y, .bottomcollision
.d: ret
    
.leftcollision:
    mov #1, acc     ; Reverse velocity to the right
    st v_ball_xv
    br .a           ; Check against the next case
.rightcollision:
    mov #255, acc   ; Reverse velocity to the left
    st v_ball_xv
    br .b           ; Check against the next case
.topcollision:
    mov #1, acc     ; Reverse velocity towards down direction
    st v_ball_yv
    br .c           ; Check against the next case
.bottomcollision:   ; For bottom collision, check if it hit the paddle or the bottom
    ld v_ball_x     ; Get ball X in accumulator
    sub v_pad_x     ; Subtract paddle position from ball position (Bx-Px)
    sub #11         ; If (Bx-Px) is between 0-10, it was a hit
                    ; Subtract result by 11. If the original value was 0-10, it should borrow
    bn psw, 7, .miss; If there was no borrow, the ball missed
            
    mov #255, acc   ; Reverse velocity towards up direction
    st v_ball_yv
    br .d           ; Go back and return
.miss
    mov #1, v_gameover
    ret
; -----------------------------------------------------------------------------
; __drawentities - Draws entities on the screen. Paddle, Ball
; Clobbered: acc, b, c
; -----------------------------------------------------------------------------
__drawentities:
.drawpaddle:        ; Draw the paddle first
    ld v_pad_x      ; Load acc with paddle x. Corresponds to the far left edge of paddle.
    st b            ; Store in B (since we are going to draw it)
    mov #10, acc    ; Paddle width is 6 pixels, store in acc as a counter
.paddleloop:
    push acc        ; Save counter
    push b          ; Save B since __setvfpixel clobbers it
    push b          ; Save B twice to draw 2 lines
    mov #30, c      ; Draw paddle on line 30
    call __setvfpixel
    mov #31, c      ; Draw second lind of paddle on line 31
    pop b
    call __setvfpixel
    pop b
    pop acc
    inc b           ; Move to next pixel in paddle
    dec acc         ; Decrement counter until entire paddle width is drawn
    bne #0, .paddleloop
    
.drawball:          ; Draw the ball, it is a 3px by 3px cross with position in middle    
    ld v_ball_x     ; Load positions into b and c for drawing onto virtual framebuffer
    st b
    ld v_ball_y
    dec acc         ; Start at top (1 pixel above middle) for the cross
    st c
    
    push b          ; Just unroll this logic, no need to make a loop for something this small
    push c          ; But oh boy, does it look stoopid :marcel:
    call __setvfpixel
    pop c
    pop b
    
    inc c           ; Move 1 pixel down and draw middle
    push b
    push c
    call __setvfpixel
    pop c
    pop b
    
    inc c           ; Move 1 pixel down and draw bottom
    push b
    push c
    call __setvfpixel
    pop c
    pop b

    dec c           ; Revert Y coordinate back to center
    dec b           ; Move X coordinate 1 to the left (to draw left pixel of ball)
    push b
    push c
    call __setvfpixel
    pop c
    pop b
    
    inc b           ; Already drew the middle, so skip to the right
    inc b
    call __setvfpixel
    
    ret
    
    
; -----------------------------------------------------------------------------
; __pollbuttons - Poll the Buttons and store state in variable
; Clobbered: None
; -----------------------------------------------------------------------------
__pollbuttons:
    bp p7, 0, .quit ; When the VMU is plugged into the controller, this bit goes high
    push acc        ; Save accumulator so we can use it for reading and storing
    ld v_btn        ; The current set of buttons is now the old set
    st v_btn_old
    ld p3           ; Read value of Port 3 (Buttons)
    bn acc, 6, .quit; Bit 6 is the MODE button, which should also trigger a quit
    xor #$FF        ; Invert button state, so 1=Pressed 0=Unpressed
    st v_btn        ; The current set of buttons is now the new set just read
    xor v_btn_old   ; XOR the new set with the old set will show which buttons changed
    st v_btn_chg
    pop acc
    ret
.quit:              ; When the VMU is plugged in, we should quit the game
    jmp __leave_vec
    
; -----------------------------------------------------------------------------
; __setvfpixel - Set Virtual Framebuffer Pixel at (x,y)
; Register b = X coordinate
; Register c = Y coordinate
; Clobbered: ACC, B, C, vrmad1/2, vtrbf, Work RAM Registers Modified
; Screen is 48x32 pixels. 1 bpp. 6 bytes horizontal.
; -----------------------------------------------------------------------------    
__setvfpixel:       ; Algorithm: Set ((y*6)+(x/8))th byte's (x%8)-1 pixel
    push b          ; Save X coordinate since B is used for multiplication
    xor acc         ; Clear accumulator
    mov #6, b       ; MUL is kinda reest. {ACC, C} form a 16 bit multiplicand
                    ; which is multiplied with register B to form a 24 bit
                    ; result in {B, ACC, C}
    mul             ; ACC = 0, C = Y coordinate. {B,ACC,C} contains Ycoord*6.
    pop acc         ; Restore X coordinate in accumulator and keep for transfer
    push c          ; Ycoord*6 will never be above 256, so just save lowest 8 bits
    st c            ; Move Xcoord from accumulator to C since it is the dividend
    xor acc         ; Division is also reest. 16-bit divident in {ACC,C}
    mov #8, b       ; 8-bit divisor in Register B
    div             ; Do {ACC,C}/B -> C contains X/8, B contains remainder
    xor acc         ; Clear accumulator just in case
    add c           ; Accumulator = X/8
    pop c           ; Restore multiplication result Ycoord*6
    add c           ; Accumulator is now ((Y*6) + (X/8)), the correct byte in framebuffer
    
    mov #$0, vrmad2 ; VRMAD (9 bit register) holds address of Work RAM (256 byte area)
    st vrmad1       ; which will be accessed through VTRBF (to hold virtual framebuffer)
                    ; Acc holds the byte offset of our virtual pixel group, 
                    ; so set VRMAD1 to that
    clr1 vsel, 4    ; VSEL Bit 4 - If set autoincrement VRMAD on every VRTBF access.
                    ; Disable it, we don't want autoincrement
                    
    ld b            ; Move B (contains the bit offset into the group of pixels)
                    ; to accumulator for comparison
                    
                    ; Because this reest processor doesn't have a way to programatically
                    ; set a bit, we gotta get a little stoopid here and unroll it...
                    
.b0:bne #0, .b1     ; If 0, store in bit 7 (MSB, leftmost) , otherwise check again
    ld vtrbf        ; Load what is already in the virtual framebuffer to acc
    set1 acc, 7     ; Set the pixel
    st vtrbf        ; Store it back into virtual framebuffer
    ret
.b1:bne #1, .b2     ; If 1, store in bit 6 (next bit from MSB), otherwise check again
    ld vtrbf
    set1 acc, 6
    st vtrbf
    ret
.b2:bne #2, .b3     ; Keep doing this ...
    ld vtrbf
    set1 acc, 5
    st vtrbf
    ret
.b3:bne #3, .b4     ; Getting a little stoopid...
    ld vtrbf
    set1 acc, 4
    st vtrbf
    ret
.b4:bne #4, .b5     ; Rhymeはお辞め　博士に任せ I get STOOPID　涙の出る馬鹿さ加減
    ld vtrbf
    set1 acc, 3
    st vtrbf
    ret
.b5:bne #5, .b6     ;　炎の詩人　止まらん火災　これこそが Worldhights
    ld vtrbf
    set1 acc, 2
    st vtrbf
    ret
.b6:bne #6, .b7     ; 俺の Mind State は Nine-Eight (Like the Dreamcast Release Year)
    ld vtrbf
    set1 acc, 1
    st vtrbf
    ret
.b7:                ; 進むつもり So we don't stop ...
    ld vtrbf
    set1 acc, 0
    st vtrbf
    ret

; -----------------------------------------------------------------------------
; __commitvf - Commit Virtual Framebuffer to Real Framebuffer
; Clobbered: Nothing
; -----------------------------------------------------------------------------
__commitvf:
    push acc        ; Save registers so the application code doesn't need to worry
    push xbnk
    push $2         ; Will use this as pointer for framebuffer
    push vsel
    push vrmad1
    push vrmad2

.begin:
    mov #$80, $2    ; Framebuffer starts at address $180, so we put $80 into $2 for
                    ; Indirect SFR addressing
    xor acc         ; Set acc to zero
    st xbnk         ; Select first half of framebuffer
    st vrmad1       ; Start at first byte in virtual framebuffer
    st vrmad2
    set1 vsel, 4    ; Enable autoincrement address for sequential copy

.loop:
    ld vtrbf        ; Get a byte from Work RAM Virtual Framebuffer
    st @R2          ; Store in real framebuffer
    inc $2          ; Increment to next framebuffer
    ld $2           ; Load value to accumulator for testing
    and #$0F        ; Test if address is divisible by 12
    bne #$0C, .skip ; Since after 2 lines (12 bytes) there are 4 empty bytes need to skip over
    ld $2           ; If address is divisible by 12 then we copied 2 lines
    add #4          ; So add 4 more to skip over the unused bytes
    st $2           ; This is the new address into the framebuffer
    bnz .skip       ; If the address was 0, we have rolled over past 256 and need to
    inc xbnk        ; write to the next bank (since 1 XBNK only contains half the framebuffer)
    mov #$80, $2    ; Reset framebuffer address to point to beginning of next bank
.skip:
    ld vrmad1       ; Get current Work RAM (Virtual Framebuffer) Address
    bne #$C0, .loop ; If we haven't copied the whole virtual framebuffer yet, go back and copy more
    
    pop vrmad2      ; Restore clobbered registers
    pop vrmad1 
    pop vsel
    pop $2
    pop xbnk
    pop acc
    ret

; -----------------------------------------------------------------------------
; __copytovf - Copy a bitmap from TRH/TRL to Virtual Framebuffer
; Clobbered: Nothing
; -----------------------------------------------------------------------------
__copytovf:
    push acc        ; Save registers so the application code doesn't need to worry
    push c
    push vsel
    push vrmad1
    push vrmad2
    
    xor acc         ; Set initial counter to 0
    st c            ; Register C is counter
    st vrmad1       ; Start at first byte in virtual framebuffer
    st vrmad2
    set1 vsel, 4    ; Enable autoincrement address for sequential copy

.loop:
    ldc             ; Get a byte from [(trh:trl)+c]
    st vtrbf        ; Store the byte in virtual framebuffer, and autoincrement
    inc c           ; Increment counter and check if we copied 6*32 = $C0 bytes
    ld c
    bne #$C0, .loop ; If not, then copy more
.done:
    pop vrmad2      ; Restore clobbered registers
    pop vrmad1
    pop vsel
    pop c
    pop acc
    ret

; -----------------------------------------------------------------------------
; __clearvf - Clear the Virtual Framebuffer
; Clobbered: None
; -----------------------------------------------------------------------------
__clearvf:
    push acc        ; Save registers so the application code doesn't need to worry
    push c
    push vsel
    push vrmad1
    push vrmad2
    
    xor acc         ; Set initial counter to 0
    st c            ; Register C is counter
    st vrmad1       ; Start at first byte in virtual framebuffer
    st vrmad2
    set1 vsel, 4    ; Enable autoincrement address for sequential copy

.loop:
    xor acc         ; Clear accumulator and use that to clear virtual framebuffer
    st vtrbf        ; Store the byte in virtual framebuffer, and autoincrement
    inc c           ; Increment counter and check if we copied 6*32 = $C0 bytes
    ld c
    bne #$C0, .loop ; If not, then copy more
.done:
    pop vrmad2      ; Restore clobbered registers
    pop vrmad1
    pop vsel
    pop c
    pop acc
    ret    
    
; -----------------------------------------------------------------------------
; Graphical Assets
; -----------------------------------------------------------------------------

title1:
    .include sprite "asset/title1.png"
    
title2:
    .include sprite "asset/title2.png"
    
field:
    .include sprite "asset/field.png"
    
gameover:
    .include sprite "asset/gameover.png"

    
; -----------------------------------------------------------------------------
; THE END, HOPE YOU ENJOYED! ;)
; -----------------------------------------------------------------------------
    .cnop 0, $200   ; Pad binary to an even number of blocks.