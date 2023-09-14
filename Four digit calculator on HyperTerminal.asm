***********************************************************************
*
* Title:          Simple Calculator
*
* Objective:      CMPEN 472 Homework 7
*
* Revision:       V2.1
*
* Date:	          March 15, 2023
*
* Programmer:     Sai Narayan
*
* Company:        The Pennsylvania State University
*                 Department of Computer Science and Engineering
*
* Program:        Simple SCI Serial Port I/O and Demonstration
*                 Calculator
*                                  
*
* Algorithm:      Simple Serial I/O use, typewriter, ASCII and hex conversions
*                 Arithmetic calls, overflow checking, negative checking
*
* Register use:	  A: Serial port data
*                 B: misc data
*                 X: character buffer
*                 Y: misc uses and buffer
*
* Memory use:     RAM Locations from $3000 for data, 
*                 RAM Locations from $3100 for program
*
*	Input:			    Parameters hard-coded in the program - PORTB, 
*                 Terminal connected over serial
* Output:         
*                 Terminal connected over serial
*                 PORTB bit 7 to bit 4, 7-segment MSB
*                 PORTB bit 3 to bit 0, 7-segment LSB
*
* Observation:    This is a menu-driven program that prints to and receives
*                 data from a terminal, and will do different things based 
*                 on user input. It can do +, -, *, and /.
*
***********************************************************************
* Parameter Declearation Section
*
* Export Symbols
            XDEF        pstart       ; export 'pstart' symbol
            ABSENTRY    pstart       ; for assembly entry point
  
* Symbols and Macros
PORTB       EQU         $0001        ; i/o port B addresses
DDRB        EQU         $0003

SCIBDH      EQU         $00C8        ; Serial port (SCI) Baud Register H
SCIBDL      EQU         $00C9        ; Serial port (SCI) Baud Register L
SCICR2      EQU         $00CB        ; Serial port (SCI) Control Register 2
SCISR1      EQU         $00CC        ; Serial port (SCI) Status Register 1
SCIDRL      EQU         $00CF        ; Serial port (SCI) Data Register

CR          equ         $0d          ; carriage return, ASCII 'Return' key
LF          equ         $0a          ; line feed, ASCII 'next line' character
SPACE       equ         $20          ; space character

***********************************************************************
* Data Section: address used [ $3000 to $30FF ] RAM memory
*
            ORG         $3000        ; Reserved RAM memory starting address 
                                     ;   for Data for CMPEN 472 class

CCount      DS.B        $0001        ; Number of chars in buffer
HCount      DS.B        $0001        ; number of ASCII characters to be converted to hex
DCount      DS.B        $0001        ; number of ASCII chars to be converted to decimal
DCount1     DS.B        $0001        ; number of decimal digits in Arg1
DCount2     DS.B        $0001        ; number of decimal digits in Arg2
Hex         DS.B        $0002        ; stores a hex number with leading 0s

InputBuff   DS.B        $0009        ; The actual command buffer

DecBuff     DS.B        $0004        ; used to store Hex -> Decimal -> ASCII conversion, terminated with NULL

Arg1ASCII   DS.B        $0004        ; Arg1 in ASCII-formatted decimal
Arg2ASCII   DS.B        $0004        ; Arg2 in ASCII-formatted decimal

HexArg1     DS.B        $0002        ; stores first argument in expression (hex number with leading 0s)
HexArg2     DS.B        $0002        ; stores second argument in expression (hex number with leading 0s)
Temp        DS.B        $0001        
Operation   DS.B        $0001        ; stores what operation was requested (0 for +, 1 for -, 2 for *, 3 for /)                            
err         DS.B        $0001        ; error flag (0 for no error, 1 for error)
negFlag     DS.B        $0001        ; negative answer flag (0 for positive, 1 for negative)
; Each message ends with $00 (NULL ASCII character) for your program.
;
; There are 256 bytes from $3000 to $3100.  If you need more bytes for
; your messages, you can put more messages 'msg3' and 'msg4' at the end of 
; the program - before the last "END" line.
                                     ; Remaining data memory space for stack,
                                     ;   up to program memory start

*
***********************************************************************
* Program Section: address used [ $3100 to $3FFF ] RAM memory
*
            ORG        $3100        ; Program start address, in RAM
pstart      LDS        #$3100       ; initialize the stack pointer

            LDAA       #%11111111   ; Set PORTB bit 0,1,2,3,4,5,6,7
            STAA       DDRB         ; as output

            LDAA       #%00000000
            STAA       PORTB        ; clear all bits of PORTB

            ldaa       #$0C         ; Enable SCI port Tx and Rx units
            staa       SCICR2       ; disable SCI interrupts

            ldd        #$0001       ; Set SCI Baud Register = $0001 => 2M baud at 24MHz (for simulation)
;            ldd        #$0002      ; Set SCI Baud Register = $0002 => 1M baud at 24MHz
;            ldd        #$000D      ; Set SCI Baud Register = $000D => 115200 baud at 24MHz
;            ldd        #$009C      ; Set SCI Baud Register = $009C => 9600 baud at 24MHz
            std        SCIBDH       ; SCI port baud rate change
            
            jsr   menu               ; print the menu messages, 'Welcome...'
                                   
main        
            ldx   #prompt            ; print the prompt message
            jsr   printmsg
            
            ldx   #InputBuff         ; cmd buffer init
            clr   CCount
            clr   HCount
            jsr   clrBuff            ; clear out old buffer data to prevent garbage second arguments
            ldx   #InputBuff         ; cmd buffer init

cmdLoop     jsr   getchar            ; type writer - check the key board
            cmpa  #$00               ;  if nothing typed, keep checking
            beq   cmdLoop
                                     
            cmpa  #CR
            beq   noReturn
            jsr   putchar            ; is displayed on the terminal window - echo print

noReturn    staa  1,X+               ; store char in buffer
            inc   CCount             ; 
            ldab  CCount
            cmpb  #$0A               ; max # chars in buffer is 10, including Enter
            lbhi   Error              ; user filled the buffer
            cmpa  #CR
            bne   cmdLoop            ; if Enter/Return key is pressed, move the
            ;ldaa  #LF                ; cursor to next line
            ;jsr   putchar
            
            
            ldab  CCount
            cmpb  #$04               ; min # chars in buffer is 4, including Enter
            lblo   Error              ; user didn't write enough
            
            
CmdChk                
            ldaa  #CR                ; move the cursor to beginning of the line
            jsr   putchar            ;  Return/Enter key                    
            ldaa  #LF                ; for checking
            jsr   putchar
            
            jsr   parser             ; parse the user input
            ldaa  err                ; check the error flag,
            cmpa  #$01               ;  branch to error handler if flag set
            lbeq   Error
            
            ldx   #Hex
            clr   1,X+
            clr   1,X+
            
            ldy   #HexArg1
            ldx   #Arg1ASCII
            ldaa  DCount1
            staa  DCount
            jsr   asciiDec2Hex       ; convert ASCII-formatted arg1 into hex
            ldaa  err                ; check the error flag,
            cmpa  #$01               ; branch to error handler if flag set
            lbeq   Error
            sty   HexArg1
            
            ldx   #Hex
            clr   1,X+
            clr   1,X+
            
            ldy   #HexArg2
            ldx   #Arg2ASCII
            ldaa  DCount2
            staa  DCount
            jsr   asciiDec2Hex       ; convert ASCII-formatted arg2 into hex
            ldaa  err                ; check the error flag,
            cmpa  #$01               ;  branch to error handler if flag set
            lbeq   Error
            sty   HexArg2
            
            
            ldaa  Operation          ; Operation switch statement
            cmpa  #$00
            beq   opAdd
            cmpa  #$01
            beq   opMinus
            cmpa  #$02
            beq   opMult
            cmpa  #$03
            beq   opDiv
            bra   Error              ; if somehow operation variable is invalid, error out
            
opAdd       ldd   HexArg1            ; Load first hexadecimal argument into D
            addd  HexArg2            ; Add second hexadecimal argument to D
            std   Hex                ; Store result in Hex
            bra   answer             ; Jump to answer section to print result

opMinus     ldd   HexArg1            ; Load first hexadecimal argument into D
            cpd   HexArg2            ; Compare D to second hexadecimal argument
            blt   negative           ; If first argument is less than the second, branch to 'negative'
            subd  HexArg2            ; Subtract second argument from D
            std   Hex                ; Store result in Hex
            bra   answer             ; Jump to answer section to print result
negative    ldd   HexArg2            ; So do second argument minus first 
            subd  HexArg1
            std   Hex
            ldaa  #$01
            staa  negFlag            ; Set negative flag
            bra   answer             ; Jump to answer section to print result

opMult      ldd   HexArg1            ; Load first hexadecimal argument into D
            ldy   HexArg2            ; Load second hexadecimal argument into Y
            emul                     ; Multiply D by Y
            bcs   Overflow           ; If the carry flag is set, branch to 'Overflow'
            cpy   #$00               ; Check if the upper byte of the answer is 0
            bne   Overflow           ; If not, then we overflowed, branch to 'Overflow'
            std   Hex
            bra   answer             ; Jump to answer section to print result

opDiv       ldd   HexArg1            ; Load first hexadecimal argument into D
            ldx   HexArg2            ; Load second hexadecimal argument into X
            cpx   #$0000             ; Compare X to 0 to prevent divide by zero
            beq   Error              ; If X is 0, branch to 'Error'
            idiv                     ; Perform division, result stored in X
            stx   Hex
            ;bra  answer


answer                               ; Print the answer
            ldx   #equals
            jsr   printmsg           ; Print the '='
            ldd   Hex
            jsr   hex2asciiDec       ; Convert answer to ascii
            ldaa  negFlag
            cmpa  #$01               ; Check if answer is negative
            bne   pozz               
            ldx   #minus
            jsr   printmsg
            
pozz        ldx   #DecBuff
            jsr   printmsg
            ldaa    #CR                ; Move the cursor to the beginning of the line
            jsr     putchar            ;   Carriage Return/Enter key
            ldaa    #LF                ; Move the cursor to the next line, Line Feed                                 
            jsr     putchar            
            clr    negFlag           ; Reset negative flag
            lbra   main               ; Loop back to the main routine

Error                                ; No recognized command entered, print error message
            ldaa  #CR                ; Move the cursor to the beginning of the line
            jsr   putchar            ;   Carriage Return/Enter key                    
            ldaa  #LF                ; Move the cursor to the next line, Line Feed
            jsr   putchar
            ldx   #error1              ; Print the error message
            jsr   printmsg
            ldaa  #CR                ; Move the cursor to the beginning of the line
            jsr   putchar            ;   Carriage Return/Enter key
            ldaa  #LF                ; Move the cursor to the next line, Line Feed                                 
            jsr   putchar
            clr   err                ; Reset error flag
            lbra  main               ; Loop back to the main routine

Overflow                             ; Result of multiplication is too big
            ldaa  #CR                ; Move the cursor to the beginning of the line
            jsr   putchar            ;   Carriage Return/Enter key                    
            ldaa  #LF                ; Move the cursor to the next line, Line Feed
            jsr   putchar
            ldx   #error2              ; Print the overflow error message
            jsr   printmsg
            ldaa  #CR                ; Move the cursor to the beginning of the line
            jsr   putchar            ;   Carriage Return/Enter key
            ldaa  #LF                ; Move the cursor to the next line, Line Feed                                 
            jsr   putchar
            clr   err                ; Reset error flag
            lbra  main               ; Loop back to the main routine
    
;subroutine section below

;***********printmsg***************************
;* Program: Output character string to SCI port, print message
;* Input:   Register X points to ASCII characters in memory
;* Output:  message printed on the terminal connected to SCI port
;* 
;* Registers modified: CCR
;* Algorithm:
;     Pick up 1 byte from memory where X register is pointing
;     Send it out to SCI port
;     Update X register to point to the next byte
;     Repeat until the byte data $00 is encountered
;       (String is terminated with NULL=$00)
;**********************************************
NULL           equ     $00
printmsg       psha                   ;Save registers
               pshx
printmsgloop   ldaa    1,X+           ;pick up an ASCII character from string
                                       ;   pointed by X register
                                       ;then update the X register to point to
                                       ;   the next byte
               cmpa    #NULL
               beq     printmsgdone   ;end of strint yet?
               jsr     putchar        ;if not, print character and do next
               bra     printmsgloop

printmsgdone   pulx 
               pula
               rts
;***********end of printmsg********************


;***************putchar************************
;* Program: Send one character to SCI port, terminal
;* Input:   Accumulator A contains an ASCII character, 8bit
;* Output:  Send one character to SCI port, terminal
;* Registers modified: CCR
;* Algorithm:
;    Wait for transmit buffer become empty
;      Transmit buffer empty is indicated by TDRE bit
;      TDRE = 1 : empty - Transmit Data Register Empty, ready to transmit
;      TDRE = 0 : not empty, transmission in progress
;**********************************************
putchar        brclr SCISR1,#%10000000,putchar   ; wait for transmit buffer empty
               staa  SCIDRL                      ; send a character
               rts
;***************end of putchar*****************


;***************echoPrint**********************
;* Program: makes calls to putchar but ends when CR is passed to it
;* Input:   ASCII char in A
;* Output:  1 char is displayed on the terminal window - echo print
;* Registers modified: CCR
;* Algorithm: if(A==CR) return; else print(A);
;**********************************************
echoPrint      cmpa       #CR       ; if A == CR, end of string reached
               beq        retEcho   ; return
               
               jsr        putchar
               
retEcho        rts
;***************end of echoPrint***************


;****************getchar***********************
;* Program: Input one character from SCI port (terminal/keyboard)
;*             if a character is received, otherwise return NULL
;* Input:   none    
;* Output:  Accumulator A containing the received ASCII character
;*          if a character is received.
;*          Otherwise Accumulator A will contain a NULL character, $00.
;* Registers modified: CCR
;* Algorithm:
;    Check for receive buffer become full
;      Receive buffer full is indicated by RDRF bit
;      RDRF = 1 : full - Receive Data Register Full, 1 byte received
;      RDRF = 0 : not full, 0 byte received
;**********************************************
getchar        brclr SCISR1,#%00100000,getchar7
               ldaa  SCIDRL
               rts
getchar7       clra
               rts
;****************end of getchar**************** 


;***************menu***************************
;* Program: Print the menu UI
;* Input:   
;* Output:  Prints a menu to the terminal
;* Registers modified: X, A
;**********************************************
menu            ldx   #menu1             ; print the first message, 'Welcome...'
                jsr   printmsg
            
                ldaa  #CR                ; move the cursor to beginning of the line
                jsr   putchar            ;   Cariage Return/Enter key
                ldaa  #LF                ; move the cursor to next line, Line Feed
                jsr   putchar
                
                ldx   #menu2             ; print the second message
                jsr   printmsg
                
                ldaa  #CR                
                jsr   putchar            
                ldaa  #LF                
                jsr   putchar
                ldaa  #CR                
                jsr   putchar            
                ldaa  #LF                
                jsr   putchar
                
                
                ldx   #menu3             ; print the third menu item
                jsr   printmsg
                
                ldaa  #CR                
                jsr   putchar            
                ldaa  #LF                
                jsr   putchar

                ldx   #menu4             ; print the fourth menu item
                jsr   printmsg
                
                ldaa  #CR                
                jsr   putchar            
                ldaa  #LF                
                jsr   putchar
                
                
                ldx   #menu5             ; print the fifth menu item
                jsr   printmsg
                
                ldaa  #CR                
                jsr   putchar            
                ldaa  #LF                
                jsr   putchar
                
                ldaa  #CR                
                jsr   putchar            
                ldaa  #LF                
                jsr   putchar
                rts
;***************end of menu********************


 ;***********clrBuff****************************
;* Program: Clear out command buff
;* Input:   
;* Output:  buffer is filled with zeros
;* 
;* Registers modified: X,A,B,CCR
;* Algorithm: set each byte (10 total) in InputBuff to $00
;************************************************
clrBuff
            ldab    #$0A        ; number of bytes allocated
clrLoop
            cmpb    #$00        ; standard while loop
            beq     clrReturn
            ldaa    #$00
            staa    1,X+        ; clear current byte
            decb                ; B = B-1
            bra     clrLoop     ; loop thru whole buffer

clrReturn   rts                            
            
;***********clrBuff*****************************


;***********parser****************************
;* Program: parse user input and echo back to terminal
;* Input: 2 ASCII-formatted decimal nums, separated 
;*        by a math operator, in #InputBuff 
;* Output: 2 hex nums in HexArg1 and HexArg2, 
;*          operator code stored in Operation variable
;*          with error flag set if error detected 
;* 
;* Registers modified: X,Y,A,B,CCR
;* Algorithm: iterate through buffer and extract each char,
;*            checking for legality along the way
;************************************************
parser      ldx     #indent     ; Load X register with the address of indent string
            jsr     printmsg    ; Jump to printmsg function which prints the indent string
            ldx     #InputBuff  ; Load X register with the address of the input buffer
            ldy     #Arg1ASCII  ; Load Y register with the address of the first argument buffer
            clrb                ; Clear the B register which will be used as a counter for the number of digits in the first argument
            
loopArg1    ldaa    1,X+        ; Load A register with the value pointed by X and then increment X
            jsr     echoPrint   ; Jump to echoPrint function which prints the character in A register
            
            cmpa    #$39        ; Compare the value in A register with ASCII value of 9
            bhi     parseErr    ; If A > 9, branch to parseErr, i.e., invalid character
            
            cmpa    #$30        ; Compare the value in A register with ASCII value of 0
            blo     opChk       ; If A < 0, branch to opChk, i.e., it's not a numeric digit but could be an operator
            
            cmpb    #$04        ; Compare the value in B register with 4
            bhi     parseErr    ; If B > 4, branch to parseErr, i.e., more than 4 digits have been encountered in first argument which is not allowed
            
            staa    1,Y+        ; Store the value in A register in the location pointed by Y and then increment Y
            incb                ; Increment the B register
            bra     loopArg1    ; Branch to loopArg1 to process the next character
            
opChk       cmpb    #$04        ; Compare the value in B register with 4
            bhi     parseErr    ; If B > 4, branch to parseErr, i.e., more than 4 digits in first argument
            tstb                ; Test the value in B register
            beq     parseErr    ; If B = 0, branch to parseErr, i.e., there were no digits before the operator
            
            stab    DCount1     ; Store the value in B register in DCount1, i.e., store the number of digits in first argument
            clrb                ; Clear the B register to reuse as a counter for the number of digits in the second argument
            stab    0,Y         ; Store the value in B register in the location pointed by Y, i.e., null-terminate the first argument string
            
            cmpa    #$2B        ; Compare the value in A register with ASCII value of '+'
            bne     chkMinus    ; If A != '+', branch to chkMinus to check if it's '-'
            ldaa    #$00        ; Load A register with 0
            staa    Operation   ; Store the value in A register in Operation, i.e., set operation as addition
            bra     Arg2        ; Branch to Arg2 to start processing the second argument
            
chkMinus    cmpa    #$2D        ; Compare the value in A register with ASCII value of '-'
            bne     chkMult     ; If A != '-', branch to chkMult to check if it's '*'
            ldaa    #$01        ; Load A register with 1
            staa    Operation   ; Store the value in A register in Operation, i.e., set operation as subtraction
            bra     Arg2        ; Branch to Arg2 to start processing the second argument
            
chkMult     cmpa    #$2A        ; Compare the value in A register with ASCII value of '*'
            bne     chkDiv      ; If A != '*', branch to chkDiv to check if it's '/'
            ldaa    #$02        ; Load A register with 2
            staa    Operation   ; Store the value in A register in Operation, i.e., set operation as multiplication
            bra     Arg2        ; Branch to Arg2 to start processing the second argument
            
chkDiv      cmpa    #$2F        ; Compare the value in A register with ASCII value of '/'
            bne     parseErr    ; If A != '/', branch to parseErr, i.e., invalid operator
            ldaa    #$03        ; Load A register with 3
            staa    Operation   ; Store the value in A register in Operation, i.e., set operation as division
            
Arg2        ldy     #Arg2ASCII  ; Load Y register with the address of the second argument buffer

loopArg2    ldaa    1,X+        ; Load A register with the value pointed by X and then increment X
            jsr     echoPrint   ; Jump to echoPrint function which prints the character in A register
            
            cmpa    #CR         ; Compare the value in A register with ASCII value of Carriage Return
            beq     parseRet    ; If A = CR, branch to parseRet, i.e., end of input buffer
            
            cmpa    #$39        ; Compare the value in A register with ASCII value of 9
            bhi     parseErr    ; If A > 9, branch to parseErr, i.e., invalid character
            cmpa    #$30        ; Compare the value in A register with ASCII value of 0
            blo     parseErr    ; If A < 0, branch to parseErr, i.e., invalid character
            
            cmpb    #$04        ; Compare the value in B register with 4
            bhi     parseErr    ; If B > 4, branch to parseErr, i.e., more than 4 digits in second argument
            
            staa    1,Y+        ; Store the value in A register in the location pointed by Y and then increment Y
            incb                ; Increment the B register
            bra     loopArg2    ; Branch to loopArg2 to process the next character
            
parseRet    cmpb    #$04        ; Compare the value in B register with 4
            bhi     parseErr    ; If B > 4, branch to parseErr, i.e., more than 4 digits in second argument
            tstb                ; Test the value in B register
            beq     parseErr    ; If B = 0, branch to parseErr, i.e., there were no digits in the second argument
            
            stab    DCount2     ; Store the value in B register in DCount2, i.e., store the number of digits in second argument
            clrb                ; Clear the B register
            stab    0,Y         ; Store the value in B register in the location pointed by Y, i.e., null-terminate the second argument string
            
            rts                 ; Return from subroutine
            
parseErr    ldaa    #$01        ; Load A register with 1
            staa    err         ; Store the value in A register in err, i.e., set the error flag
            rts                 ; Return from subroutine

;***********parser*****************************


;****************asciiDec2Hex******************
;* Program: converts ascii-formatted decimal (up to 3 digits) to hex
;*             
;* Input: ascii-formatted decimal, number of digits      
;* Output: hex number in buffer (#Hex) and Y
;*          
;*          
;* Registers modified: X,Y,A,B,CCR
;* Algorithm: from hw6 aid pdf   
;**********************************************
asciiDec2Hex
    ldaa  0,X     ; Load the most significant digit from the memory address pointed by X into the A register.
    ldab  DCount  ; Load the number of digits into the B register.
    cmpb  #$04    ; Compare the value in the B register with 4 (Are there 4 digits?).
    bne   STAGE1  ; If B is not equal to 4, branch to STAGE1.
    dec   DCount  ; Decrement DCount, leaving 3 digits.
    suba  #$30    ; Subtract ASCII value of 0 from A to convert ASCII digit to actual number.
    TFR   A,B     ; Transfer A register value to B. Now, D register contains 16 bit value (as D = [B,A]).
    CLRA          ; Clear A register.
    LDY   #1000   ; Load Y with 1000 which is the weight of the most significant digit in a 4-digit number.
    EMUL          ; Multiply D by Y. The result is stored in D register.
    std   Hex     ; Store the result from D register into Hex.
    inx           ; Increment X register (moves to next digit).
    ldaa  0,X     ; Load the next digit into A.
    ldab  DCount  ; Load the number of digits into B.

STAGE1
    cmpb  #$03    ; Compare the value in B with 3 (Are there 3 digits left?).
    bne   STAGE2  ; If B is not equal to 3, branch to STAGE2.
    dec   DCount  ; Decrement DCount, leaving 2 digits.
    suba  #$30    ; Subtract ASCII value of 0 from A to convert ASCII digit to actual number.
    ldab  #100    ; Load B with 100 which is the weight of the most significant digit in a 3-digit number.
    mul           ; Multiply A by B. The result is stored in D register.
    addd  Hex     ; Add the value in Hex to D.
    std   Hex     ; Store the result from D register into Hex.
    inx           ; Increment X register (moves to next digit).
    ldaa  0,X     ; Load the next digit into A.
    ldab  DCount  ; Load the number of digits into B.

STAGE2
    cmpb  #$02    ; Compare the value in B with 2 (Are there 2 digits left?).
    bne   STAGE3  ; If B is not equal to 2, branch to STAGE3
    dec   DCount  ; Decrement DCount, leaving 1 digit.
    suba  #$30    ; Subtract ASCII value of 0 from A to convert ASCII digit to actual number.
    ldab  #10     ; Load B with 10 which is the weight of the second most significant digit in a 2-digit number.
    mul           ; Multiply A by B. The result is stored in D register.
    addd  Hex     ; Add the value in Hex to D.
    std   Hex     ; Store the result from D register into Hex.
    inx           ; Increment X register (moves to next digit).
    ldaa  0,X     ; Load the least significant digit into A.
    ldab  DCount  ; Load the number of digits into B.

STAGE3
    cmpb  #$01    ; Compare the value in B with 1 (Is there 1 digit left?).
    bne  ad2hErr  ; If B is not equal to 1, branch to ad2hErr label.
    dec   DCount  ; Decrement DCount, leaving 0 digits.
    suba  #$30    ; Subtract ASCII value of 0 from A to convert ASCII digit to actual number.
    ldab  #1      ; Load B with 1 which is the weight of the least significant digit in a single-digit number.
    mul           ; Multiply A by B. The result is stored in D register.
    addd  Hex     ; Add the value in Hex to D.
    std   Hex     ; Store the result from D register into Hex.
    inx           ; Increment X register.
    ldy   Hex     ; Load the result into Y from Hex.
    rts           ; Return from subroutine.

ad2hErr
    ldaa  #$01    ; Load A register with 1 to set error flag.
    staa  err     ; Store the value in A register into err.
    rts           ; Return from subroutine.


;************end of asciiDec2Hex*************** 


;****************hex2asciiDec******************
;* Program: converts a hex number to ascii-formatted decimal, max. 5 digits
;*             
;* Input:  a hex number in D     
;* Output: that same number in ascii-formatted decimal in DecBuff 
;*          
;*          
;* Registers modified: A, B, X, CCR
;* Algorithm: read the comments
;   
;**********************************************
hex2asciiDec    clr   HCount    ; Clear the HCount to reset the loop counter
                cpd   #$0000    ; Check if the hex number is 0; if it is, we can skip the conversion process
                lbeq  PSU       ; If the hex number is zero, branch to PSU (Power Start-Up)

preConvLoop     ldy   #DecBuff  ; Load the effective address of DecBuff into Y
convertLoop     ldx   #10       ; Set the divisor to 10
                idiv            ; Divide the hex number by 10
                  
                stab  1,Y+      ; Store the quotient of the division into the decimal buffer
                inc   HCount    ; Increment HCount, marking one division operation done and one remainder obtained
                tfr   X,D       ; Transfer the division result to D
                tstb            ; Check if the result was 0
                bne   convertLoop; If the result was not 0, repeat the conversion loop
                
reverse         ldaa  HCount    ; Load the accumulator A with HCount
                cmpa  #$05      ; Check the number of remainders calculated, indicating the length of the decimal number
                beq   five      ; If there were five remainders, branch to the 'five' block
                cmpa  #$04      ; If there were four remainders, branch to the 'four' block
                beq   four
                cmpa  #$03      ; If there were three remainders, branch to the 'three' block
                lbeq   three
                cmpa  #$02      ; If there were two remainders, branch to the 'two' block
                lbeq   two
                                ; If there was only one remainder, we can convert it directly
                ldx   #DecBuff  ; Reload the address of the decimal buffer into X
                ldaa  0,X       ; Load the least significant digit (1s place) into A
                adda  #$30      
                staa  1,X+      ; Store the converted 1s place (now an ASCII char) into the buffer and increment X
                ldaa  #$00      ; Load a NULL value into A
                staa  1,X+      ; Store the null terminator into the buffer and increment X
                rts             ; Return from subroutine

five            ldx   #DecBuff
                ldaa  1,X+      ; load the 1s place remainder into A
                inx
                inx
                inx
                ldab  0,X       ; load the 10000s place remainder into B
                staa  0,X       ; put the 1s place into the 1s place
                ldx   #DecBuff
                stab  0,X       ; put the 10000s place into the 10000s place
                
                inx             ; move to 1000s place
                ldaa  1,X+      ; load current 1000s place (supposed to be 10s) and do X++
                inx             ; skip current 100s place
                ldab  0,X       ; load current 10s place (supposed to be 1000s)
                staa  0,X       ; put current 1000s into 10s place
                ldx   #DecBuff  ; reload buff
                inx             ; move to 1000s place
                stab  0,X       ; put proper 1000s place (former 10s) into 1000s place
                
                ldx   #DecBuff  ; reload buff
                ldaa  0,X       ; load 10000s place into A
                adda  #$30      ;add ASCII offset
                staa  1,X+      ; store converted 10000s place and do X++
                ldaa  0,X       ; load 1000s place into A
                adda  #$30      ;add ASCII offset
                staa  1,X+      ; store converted 1000s place and do X++
                ldaa  0,X       ; load 100s place into A
                adda  #$30      ;add ASCII offset
                staa  1,X+      ; store converted 100s place and do X++
                ldaa  0,X       ; load 10s place into A
                adda  #$30
                staa  1,X+      ; store converted 10s place, X++
                ldaa  0,X       ; load 1s place
                adda  #$30      
                staa  1,X+      ; store converted 1s place, X++
                ldaa  #$00      ; load NULL into A
                staa  1,X+      ; store null terminator
                rts


four            ldx   #DecBuff
                ldaa  1,X+      ; load the 1s place remainder into A
                inx
                inx
                ldab  0,X       ; load the 1000s place remainder into B
                staa  0,X       ; put the 1s place into the 1s place
                ldx   #DecBuff
                stab  0,X       ; put the 1000s place into the 1000s place
                
                inx             ; move to 100s place
                ldaa  1,X+      ; load current 100s place (supposed to be 10s) and do X++
                ldab  0,X       ; load current 10s place (supposed to be 100s)
                staa  0,X       ; put current 100s into 10s place
                ldx   #DecBuff  ; reload buff
                inx             ; move to 100s place
                stab  0,X       ; put proper 100s place (former 10s) into 100s place
                
                ldx   #DecBuff  ; reload buff
                ldaa  0,X       ; load 1000s place into A
                adda  #$30      ;add ASCII offset
                staa  1,X+      ; store converted 1000s place and do X++
                ldaa  0,X       ; load 100s place into A
                adda  #$30      ;add ASCII offset
                staa  1,X+      ; store converted 100s place and do X++
                ldaa  0,X       ; load 10s place into A
                adda  #$30
                staa  1,X+      ; store converted 10s place, X++
                ldaa  0,X       ; load 1s place
                adda  #$30      
                staa  1,X+      ; store converted 1s place, X++
                ldaa  #$00      ; load NULL into A
                staa  1,X+      ; store null terminator
                rts


three           ldx   #DecBuff
                ldaa  1,X+      ; load the 1s place remainder into A
                inx
                ldab  0,X       ; load the 100s place remainder into B
                staa  0,X       ; put the 1s place into the 1s place
                ldx   #DecBuff
                stab  0,X       ; put the 100s place into the 100s place
                
                ldaa  0,X       ; load 100s place into A
                adda  #$30      ;add ASCII offset
                staa  1,X+      ; store converted 100s place and do X++
                ldaa  0,X       ; load 10s place into A
                adda  #$30
                staa  1,X+      ; store converted 10s place, X++
                ldaa  0,X       ; load 1s place
                adda  #$30      
                staa  1,X+      ; store converted 1s place, X++
                ldaa  #$00      ; load NULL into A
                staa  1,X+      ; store null terminator
                rts
                

two             ldx   #DecBuff
                ldaa  1,X+      ; load the 1s place remainder into A
                ldab  0,X       ; load the 10s place remainder into B
                staa  0,X       ; put the 1s place into the 1s place
                ldx   #DecBuff  
                stab  0,X       ; put the 10s place into the 10s place
                
                ldaa  0,X       ; load 10s place into A
                adda  #$30      ;add ASCII offset
                staa  1,X+      ; store converted 10s place and do X++
                ldaa  0,X       ; load 1s place into A
                adda  #$30
                staa  1,X+      ; store converted 1s place, X++
                ldaa  #$00      ; load NULL into A
                staa  1,X+      ; store null terminator
                rts

               
PSU             ldx   #DecBuff  ; Load the address of the decimal buffer into X
                ldaa  #$30      ; Load ASCII character '0' into A
                staa  1,X+      ; Store '0' into the buffer and increment X
                ldaa  #$00      ; Load a NULL value into A
                staa  1,X+      ; Store the null terminator into the buffer and increment X
                rts             ; Return from subroutine

;************end of hex2asciiDec***************


;OPTIONAL
;more variable/data section below
; This section is after the program code section
; of the RAM. The RAM ends at $3FFF
; in the MC9S12C128 chip

prompt         DC.B    'ECalc> ', $00 ; Define the string for the prompt to be displayed on the console
indent         DC.B    '       ', $00 ; Define a string for indenting output
equals         DC.B    '=', $00 ; Define a byte for the equals character
minus          DC.B    '-', $00 ; Define a byte for the minus character

error1         DC.B    '       Invalid input format', $00 ; Define a string for an invalid input error message
error2         DC.B    '       Overflow error', $00 ; Define a string for an overflow error message

menu1          DC.B    'ECalc Program for 4-digits!  Choose an operation (+, -, *, /) and enter', $00 ; Define the first line of the welcome message
menu2          DC.B    'your expression below (example shown below) and hit Enter.', $00 ; Define the second line of the welcome message
menu3          DC.B    'No parentheses. Only 1 operation per expression. Max number of digits is 3. No negatives. Only use base-10 numbers.', $00 ; Define the third line of the welcome message
menu4          DC.B    'Ecalc> 123+4', $00 ; Define an example input to be displayed on the console
menu5          DC.B    '       123+4=127', $00 ; Define an example output to be displayed on the console

               END               ; This is the end of assembly source file
                                 ; Any lines below this will be ignored and not assembled/compiled
