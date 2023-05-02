; ISR_example.asm: a) Increments/decrements a BCD variable every half second using
; an ISR for timer 2; b) Generates a 2kHz square wave at pin P1.1 using
; an ISR for timer 0; and c) in the 'main' loop it displays the variable
; incremented/decremented using the ISR for timer 2 on the LCD.  Also resets it to 
; zero if the 'BOOT' pushbutton connected to P4.5 is pressed.
$NOLIST
$MODLP51RC2
$include(math32.inc)
$include(LCD_4bit.inc)
$LIST

CLK  		   EQU 22118400
BAUD 		   EQU 115200
BRG_VAL 	   EQU (0x100-(CLK/(16*BAUD)))
TIMER0_RATE    EQU 4096     ; 2048Hz squarewave (peak amplitude of CEM-1203 speaker)
TIMER0_RELOAD  EQU ((65536-(CLK/TIMER0_RATE)))
TIMER2_RATE    EQU 1000     ; 1000Hz, for a timer tick of 1ms
TIMER2_RELOAD  EQU ((65536-(CLK/TIMER2_RATE)))

; Reset vector
org 0x0000
    ljmp main

; External interrupt 0 vector (not used in this code)
org 0x0003
    reti

; Timer/Counter 0 overflow interrupt vector
org 0x000B
    ljmp Timer0_ISR

; External interrupt 1 vector (not used in this code)
org 0x0013
	reti

; Timer/Counter 1 overflow interrupt vector (not used in this code)
org 0x001B
	reti

; Serial port receive/transmit interrupt vector (not used in this code)
org 0x0023 
	reti
	
; Timer/Counter 2 overflow interrupt vector
org 0x002B
	ljmp Timer2_ISR

DSEG at 40H
; In the 8051 we can define direct access variables starting at location 0x30 up to location 0x7F dseg at 0x30
x: 			  ds 4
y: 			  ds 4
bcd: 		  ds 5
temp: 		  ds 4
pwr: 		  ds 4
sec: 		  ds 4
Count1ms:     ds 2 
BCD_counter:  ds 1 ; The BCD counter incrememted in the ISR and displayed in the main loop
onoff:		  ds 1
state: 		  ds 1
temp_soak: 	  ds 1
time_soak: 	  ds 1
temp_refl: 	  ds 1
time_refl: 	  ds 1
time_cool:	  ds 1
temp_cool:	  ds 1

cseg
; These 'equ' must match the hardware wiring
LCD_RS 		   EQU p3.2
;LCD_RW equ PX.X ; Not used in this code, connect the pin to GND
LCD_E  		   EQU p3.3
LCD_D4 		   EQU p3.4
LCD_D5 		   EQU p3.5
LCD_D6 		   EQU p3.6
LCD_D7 		   EQU p3.7
CE_ADC    	   EQU p0.5
MY_MOSI   	   EQU p0.4 
MY_MISO  	   EQU p0.2
MY_SCLK   	   EQU p0.1
SHIFT_PB       EQU p2.7
TEMP_SOAK_PB   EQU p4.4
TIME_SOAK_PB   EQU p0.6
TEMP_REFL_PB   EQU p0.3
TIME_REFL_PB   EQU p0.0
START_PB       EQU p1.7


; In the 8051 we have variables that are 1-bit in size.  We can use the setb, clr, jb, and jnb
; instructions with these variables.  This is how you define a 1-bit variable:
BSEG
mf: dbit 1
one_second_flag: dbit 1 ; Set to one in the ISR every time 500 ms had passed
alarm_flag: dbit 1

;                     1234567890123456    <- This helps determine the location of the counter
Time:  		db 'Time  ', 0
Colon: 		db ':', 0
space: 		db ' ', 0
Temperature: 	db  'Temperature is: ', 0
Blank: 		db  '                ', 0
Initial_Message: db  'TS  ts  RT  rt  ', 0

;---------------------------------;
;Initialize SPI                   ;
;---------------------------------;
INIT_SPI: 
    setb MY_MISO    ; Make MISO an input pin 
    clr MY_SCLK     ; For mode (0,0) SCLK is zero 
    ret 
 
;---------------------------------;
;Do SPI                           ;
;---------------------------------; 
DO_SPI_G: 
    push acc 
    mov R1, #0      ; Received byte stored in R1 
    mov R2, #8      ; Loop counter (8-bits) 
DO_SPI_G_LOOP: 
    mov a, R0       ; Byte to write is in R0 
    rlc a           ; Carry flag has bit to write 
    mov R0, a 
    mov MY_MOSI, c 
    setb MY_SCLK    ; Transmit 
    mov c, MY_MISO  ; Read received bit 
    mov a, R1       ; Save received bit in R1 
    rlc a 
    mov R1, a 
    clr MY_SCLK 
    djnz R2, DO_SPI_G_LOOP 
    pop acc 
    ret 

;---------------------------------;
;Initialize Serial Port           ;
;---------------------------------;
; Configure the serial port and baud rate
InitSerialPort:
    ; Since the reset button bounces, we need to wait a bit before
    ; sending messages, otherwise we risk displaying gibberish!
    mov R1, #222
    mov R0, #166
    djnz R0, $   ; 3 cycles->3*45.21123ns*166=22.51519us
    djnz R1, $-4 ; 22.51519us*222=4.998ms
    ; Now we can proceed with the configuration
    orl	PCON,#0x80
    mov	SCON,#0x52
    mov	BDRCON,#0x00
    mov	BRL,#BRG_VAL
    mov	BDRCON,#0x1E ; BDRCON=BRR|TBCK|RBCK|SPD;
    ret
    
;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 0                     ;
;---------------------------------;
Timer0_Init:
	mov a, TMOD
	anl a, #0xf0 ; 11110000 Clear the bits for timer 0
	orl a, #0x01 ; 00000001 Configure timer 0 as 16-timer
	mov TMOD, a
	mov TH0, #high(TIMER0_RELOAD)
	mov TL0, #low(TIMER0_RELOAD)
	; Set autoreload value
	mov RH0, #high(TIMER0_RELOAD)
	mov RL0, #low(TIMER0_RELOAD)
	; Enable the timer and interrupts
    setb ET0  ; Enable timer 0 interrupt
    setb TR0  ; Start timer 0
	ret

;---------------------------------;
; ISR for timer 0.  Set to execute;
; every 1/4096Hz to generate a    ;
; 2048 Hz square wave at pin P1.1 ;
;---------------------------------;
Timer0_ISR:
	clr TF0  ; According to the data sheet this is done for us already.
	jnb alarm_flag, no
	cpl SOUND_OUT ; Connect speaker to P1.1!
no:
	reti

;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 2                     ;
;---------------------------------;
Timer2_Init:
	mov T2CON, #0 ; Stop timer/counter.  Autoreload mode.
	mov TH2, #high(TIMER2_RELOAD)
	mov TL2, #low(TIMER2_RELOAD)
	; Set the reload value
	mov RCAP2H, #high(TIMER2_RELOAD)
	mov RCAP2L, #low(TIMER2_RELOAD)
	; Init One millisecond interrupt counter.  It is a 16-bit variable made with two 8-bit parts
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Enable the timer and interrupts
    setb ET2  ; Enable timer 2 interrupt
    setb TR2  ; Enable timer 2
	ret

;---------------------------------;
; ISR for timer 2                 ;
;---------------------------------;
Timer2_ISR:
	clr TF2  ; Timer 2 doesn't clear TF2 automatically. Do it in ISR
	;cpl P1.0 ; To check the interrupt rate with oscilloscope. It must be precisely a 1 ms pulse.
	
	; The two registers used in the ISR must be saved in the stack
	push acc
	push psw
	
	; Increment the 16-bit one mili second counter
	inc Count1ms+0    ; Increment the low 8-bits first
	mov a, Count1ms+0 ; If the low 8-bits overflow, then increment high 8-bits
	jnz Inc_Done
	inc Count1ms+1

Inc_Done:
	; Check if one second has passed
	mov a, Count1ms+0
	cjne a, #low(1000), Timer2_ISR_done ; Warning: this instruction changes the carry flag!
	mov a, Count1ms+1
	cjne a, #high(1000), Timer2_ISR_done	
	; 1000 milliseconds have passed.  Set a flag so the main program knows	
	setb one_second_flag ; Let the main program know one second has passed  
	cpl TR0 ; Enable/disable timer/counter 0. This line creates a beep-silence-beep-silence sound.
	; Reset to zero the milli-seconds counter, it is a 16-bit variable
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Increment the BCD counter
	mov a, BCD_counter
	add a, #0x01
	da a ; Decimal adjust instruction.  Check datasheet for more details!
	mov BCD_counter, a
	lcall Timer
Timer2_ISR_done:
	pop psw
	pop acc
	reti

;---------------------------------;
; Left Blank MAC                  ;
;---------------------------------;
Left_blank mac
	mov a, %0
	anl a, #0xf0
	swap a
	jz Left_blank_%M_a
	ljmp %1
	Left_blank_%M_a:
	Display_char(#' ')
	mov a, %0
	anl a, #0x0f
	jz Left_blank_%M_b
	ljmp %1
	Left_blank_%M_b:
	Display_char(#' ')
endmac
    
;---------------------------------;
; Putchar                         ;
;---------------------------------;  	
; Send a character using the serial port
putchar:
    jnb TI, putchar
    clr TI
    mov SBUF, a
    ret
    
;---------------------------------;
; Send String                     ;
;---------------------------------; 
; Send a constant-zero-terminated string using the serial port
SendString:
    clr A
    movc A, @A+DPTR
    jz SendStringDone
    lcall putchar
    inc DPTR
    sjmp SendString
SendStringDone:
    ret

;---------------------------------;
; Read_ADC_Channel                ;
;---------------------------------; 
Read_ADC_Channel MAC
	mov b, #%0
	lcall _Read_ADC_Channel
ENDMAC

_Read_ADC_Channel:
	clr CE_ADC
	mov R0, #00000001B
	lcall DO_SPI_G
	mov a, b
	swap a
	anl a, #0F0H
	setb acc.7
	mov R0, a
	lcall DO_SPI_G
	mov a, R1
	anl a, #00000011B
	mov R7, a
	mov R0, #55H
	lcall DO_SPI_G
	mov a, R1
	mov R6, a
	setb CE_ADC
	clr a

;---------------------------------;
; Convert                         ;
;---------------------------------; 
Convert:
	; Copy the 10-bits of the ADC conversion into the 32-bits of 'x'
	mov x+0, r6
	mov x+1, r7 
	mov x+2, #0
	mov x+3, #0
	; Multiply by 410
	load_Y(410)
	lcall mul32
	; Divide result by 1023
	load_Y(1023)
	lcall div32
	; Subtract 273 from result
	load_Y(273)
	lcall sub32
	; The 4-bytes of x have the temperature in binary	
	
;---------------------------------;
; Display_10_digit_BCD            ;
;---------------------------------; 
; Sends 10-digit BCD number in bcd to the LCD
Display_10_digit_BCD:
	Set_Cursor(2, 7)
	Display_BCD(bcd+4)
	Display_BCD(bcd+3)
	Display_BCD(bcd+2)
	Display_BCD(bcd+1)
	Display_BCD(bcd+0)
	; Replace all the zeros to the left with blanks
	Set_Cursor(2, 7)
	Left_blank(bcd+4, skip_blank)
	Left_blank(bcd+3, skip_blank)
	Left_blank(bcd+2, skip_blank)
	Left_blank(bcd+1, skip_blank)
	mov a, bcd+0
	anl a, #0f0h
	swap a
	jnz skip_blank
	Display_char(#' ')
skip_blank:
	ret
	
;---------------------------------;
; Send_BCD                        ;
;---------------------------------; 
; Send a BCD number to PuTTY in ASCIII
Send_BCD mac
	push ar0
	mov r0, %0
	lcall ?Send_BCD
	pop ar0
endmac

?Send_BCD:
	push acc
	; Send most significant digit
	mov a, r0
	swap a
	anl a, #0fh
	orl a, #30h
	lcall putchar
	; Send least significant digit
	mov a, r0
	anl a, #0fh
	orl a, #30h
	lcall putchar
	pop acc
	ret
	
;---------------------------------;
; Send_To_Serial_Port             ;
;---------------------------------; 
; Send eight bit number via serial port, passed in ’a’.
SendToSerialPort:
	mov b, #100
	div ab
	orl a, #0x30 ; Convert hundreds to ASCII
	lcall putchar ; Send to PuTTY/Python/Matlab
	mov a, b ; Remainder is in register b
	mov b, #10
	div ab
	orl a, #0x30 ; Convert tens to ASCII
	lcall putchar ; Send to PuTTY/Python/Matlab
	mov a, b
	orl a, #0x30 ; Convert units to ASCII
	lcall putchar ; Send to PuTTY/Python/Matlab
	ret

;---------------------------------;
; Send_To_LCD                     ;
;---------------------------------; 
; Eight bit number to display passed in ’a’.
; Sends result to LCD
SendToLCD:
	mov b, #100
	div ab
	orl a, #0x30 ; Convert hundreds to ASCII
	lcall ?WriteData ; Send to LCD
	mov a, b ; Remainder is in register b
	mov b, #10
	div ab
	orl a, #0x30 ; Convert tens to ASCII
	lcall ?WriteData; Send to LCD
	mov a, b
	orl a, #0x30 ; Convert units to ASCII
	lcall ?WriteData; Send to LCD
	ret
	
;---------------------------------;
; Write Data to AT8               ;
;---------------------------------; 
loadbyte mac
	mov a, %0
	movx @dptr, a
	inc dptr
endmac

Save_Configuration:
    push IE ; Save the current state of bit EA in the stack
    clr EA ; Disable interrupts
    mov FCON, #0x08 ; Page Buffer Mapping Enabled (FPS = 1)
    mov dptr, #0x7f80 ; Last page of flash memory
    ; Save variables
    loadbyte(temp_soak) ; @0x7f80
    loadbyte(time_soak) ; @0x7f81
    loadbyte(temp_refl) ; @0x7f82
    loadbyte(time_refl) ; @0x7f83
    loadbyte(#0x55) ; First key value @0x7f84
    loadbyte(#0xAA) ; Second key value @0x7f85
    mov FCON, #0x00 ; Page Buffer Mapping Disabled (FPS = 0) 
    orl EECON, #0b01000000 ; Enable auto-erase on next write sequence  
    mov FCON, #0x50 ; Write trigger first byte
    mov FCON, #0xA0 ; Write trigger second byte
    ; CPU idles until writing of flash completes.
    mov FCON, #0x00 ; Page Buffer Mapping Disabled (FPS = 0)
    anl EECON, #0b10111111 ; Disable auto-erase
    pop IE ; Restore the state of bit EA from the stack
    ret

;----------------------------------------;
; Read From Flash, Get Saved Values      ;
;----------------------------------------; 
getbyte mac
	clr a
	movc a, @a+dptr
	mov %0, a
	inc dptr
Endmac

Load_Configuration:
	mov dptr, #0x7f84 ; First key value location.
	getbyte(R0) ; 0x7f84 should contain 0x55
	cjne R0, #0x55, Load_Defaults
	getbyte(R0) ; 0x7f85 should contain 0xAA
	cjne R0, #0xAA, Load_Defaults
	; Keys are good. Get stored values.
	mov dptr, #0x7f80
	getbyte(temp_soak) ; 0x7f80
	getbyte(time_soak) ; 0x7f81
	getbyte(temp_refl) ; 0x7f82
	getbyte(time_refl) ; 0x7f83
	ret

;---------------------------------;
; Change Data                     ;
;---------------------------------; 
Change_8bit_Variable MAC
	jb %0, %2
	Wait_Milli_Seconds(#50) ; de-bounce
	jb %0, %2
	jnb %0, $
	jb SHIFT_PB, skip%Mb
	dec %1
	sjmp skip%Ma
	skip%Mb:
	inc %1
	skip%Ma:
	ENDMAC
	
;---------------------------------;
; Timer			                  ;
;---------------------------------;
Timer: 
	mov a, sec
	add a, #1
	da a
	mov sec, a
	reti

;---------------------------------;
; Load Default Values	          ;
;---------------------------------;
Load_Defaults: 
	; Load defaults if 'keys' are incorrect
	mov temp_soak, #150
	mov time_soak, #45
	mov temp_refl, #225
	mov time_refl, #30
	ret

Python: 
	lcall InitSerialPort 
	lcall SendToSerialPort
   	Send_BCD(bcd)	
   	mov a, #'\r'
   	lcall putchar
   	mov a, #'\n'
  	lcall putchar
  	;lcall hex2bcd ; converts binary in x to BCD in BCD
   	;lcall Display_10_digit_BCD  
   	lcall SendToLCD
   	
;---------------------------------;
; Main program. Includes hardware ;
; initialization and 'forever'    ;
; loop.                           ;
;---------------------------------;
main:
    mov SP, #7FH ; Set the stack pointer to the beginning of idata   
   	lcall Timer0_Init
   	lcall Timer2_Init
   	; In case you decide to use the pins of P0, configure the port in bidirectional mode:
   	;mov P0M0, #0 ;!!!! Maybe comment this out
   	;mov P0M1, #0 ;Same with this idk check piazza post
	lcall Load_Configuration
	
   	setb EA   ; Enable Global interrupts
	lcall LCD_4BIT
   	Set_Cursor(1, 1)
   	Send_Constant_String(#Initial_Message)
	
Loop: 
	clr alarm_flag
	
	Change_8bit_Variable(TEMP_SOAK_PB, temp_soak, loop_a)
	Set_Cursor(2, 1)
	mov a, temp_soak
	lcall SendToLCD
	lcall Save_Configuration
loop_a:
	Change_8bit_Variable(TIME_SOAK_PB, time_soak, loop_b)
	Set_Cursor(2, 5)
	mov a, time_soak
	lcall SendToLCD
	lcall Save_Configuration
loop_b:
	Change_8bit_Variable(TEMP_REFL_PB, temp_refl, loop_c)
	Set_Cursor(2, 9)
	mov a, temp_refl
	lcall SendToLCD
	lcall Save_Configuration
loop_c:
	Change_8bit_Variable(TIME_REFL_PB, time_refl, loop_d)
	Set_Cursor(2, 13)
	mov a, time_refl
	lcall SendToLCD
	lcall Save_Configuration
loop_d:

FSM1:
	mov a, state
	
state0:
	cjne a, #0, state1
	mov pwr, #0
	jb START_PB, state0_done
	Wait_Milli_Seconds(#50)
	jnb START_PB, $ ; Wait for key release
	mov state, #1
	state0_done:
	ljmp FSM1
	
state1:
	cjne a, #1, state2
	mov pwr, #100
	mov sec, #0
	mov a, temp_soak
	clr c
	lcall Python
	Wait_Milli_Seconds(#50)
	subb a, temp
	jnc state1_done
	mov state, #2
	state1_done:
	ljmp FSM1
	
state2:
	cjne a, #2, state3
	mov pwr, #20
	mov a, time_soak
	clr c
	lcall Python
	Wait_Milli_Seconds(#50)
	subb a, sec
	jnc state2_done
	mov state, #3
	state2_done:
	ljmp FSM1
	
state3:
	cjne a, #3, state4
	mov pwr, #100
	mov sec, #0
	mov a, temp_refl
	clr c
	lcall Python
	Wait_Milli_Seconds(#50)
	subb a, temp
	jnc state3_done
	mov state, #4
	state3_done:
	ljmp FSM1
	
state4:
	cjne a, #4, state5
	mov pwr, #20
	mov a, time_refl
	clr c
	lcall Python
	Wait_Milli_Seconds(#50)
	subb a, sec
	jnc state3_done
	mov state, #5
	state4_done:
	ljmp FSM1
	
state5:
	cjne a, #5, state0
	mov pwr, #0
	mov sec, #0
	mov a, time_cool
	clr c
	lcall Python
	Wait_Milli_Seconds(#50)
	subb a, temp
	jnc state5_done
	mov state, #0
	state5_done:
	ljmp FSM1

COLD_JUNCTION:             
	Read_ADC_Channel(1)
	; Copy the 10-bits of the ADC conversion into the 32-bits of 'x'
	mov x+0, r6
	mov x+1, r7 
	mov x+2, #0
	mov x+3, #0
	; Multiply by 410
	load_Y(410)
	lcall mul32
	; Divide result by 1023
	load_Y(1023)
	lcall div32
	; Subtract 273 from result
	load_Y(273)
	lcall sub32
	
	mov temp+0, x+0
	mov temp+1, x+1
	mov temp+2, #0
	mov temp+3, #0

HOT_JUNCTION:
	Read_ADC_Channel(0)
	load_Y(temp)
	lcall add32

	ljmp Loop
	
END
