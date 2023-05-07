; prog4 (file reader/writer) 
; J. George
; Machine Organization
; Professor T. Larue 
; Date of last update: 04/30/2023
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
MyStack SEGMENT STACK				;
	
	DW 256 DUP	(?)			;
	
MyStack ENDS					;
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

MyData SEGMENT		

	screenLine	DB	1			; Variable used to control the line I'm 
							; printing error statements to console
	
	startTicks	DW	0			; 
	endTicks	DW	0			; 
	
	outFile		DB	"output.dat", 0 	; 
	
	errorCreate	DB	"failed to create outfile", '$' ; 
	
	inFileHandle	DW	?			; 
	outFileHandle	DW	?			; 
	
	fileName	DB	120 DUP (0) 		; array containing the file name after
							; fetching the info from the command line
							
	controlVarStr	DB	8 DUP	('$') 		; string containing the control variable 
							; after seperating it from other arguments 
							; from the command line 
							
	cntVariable	DW	0			; 
	
	bufferSize	DW	64			; 
	bufferRead	DB	64 DUP	(?) 		; 
	
	bufferOffset	DW	0			; 
	
	actNumRead	DW	0			; 
	
	tempName	DB	30 DUP	(0) 		; 
	str_len		DW	0			; 
	tempValue	DB	8 DUP	(0)		;
	
	personVal	DW	0			; 
	
	ScreenMem	EQU	0B800h			;
	
	openF		EQU	3Dh			; 
	
	inFileOpen	DB    "File Opened...", '$'	; 
	
	errorMsg	DB    "Error opening file! ", 0   ; Message to be displayed in the event
							  ; of an error loading the file 
							  
	end_prog	DB    "Your file has been processed and outfile.dat exists. Check your folder.", '$' ; 
							  
	time_msg	DB    "Time elapsed to the nearest tenth of a second:", '$'	; 

MyData  ENDS						  ;
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
MyCode SEGMENT					
						; Upon Entry: This program accepts two arguments
						; via the commmand line to be entered as such:
						; prog5.exe <file name you wish to open>.<type of file>
						; <integer string> with one white space between each
						; It separates them into a fileName variable, 
						; which will be the name of file the user wishes to 
						; open and read from, and a controlVarStr which is a 
						; string representing an integer value. This program is used 
						; to process a file of names and integer values, compare
						; the value to the command argument, and write the name if 
						; greater or equal to the argument. If it has come to the
						; end of the input file, the program will display the
						; amount of time elapsed in tenths of a second, close both
						; files, and terminate. 
						; Note: If there is no command argument, or the file does 
						; not exist, the program will display an error message. 
						
						; Upon Exit: The file has been processed and an outfile, 
						; titled Outfile.txt will now exist in the folder containing
						; this program =] 
						; If error occurs, refer to note above. 
						
	ASSUME  CS:MyCode, DS: MyData		; Tell the assembler which segment registers
						; should be used to access MyCode and MyData
						; in memory 

mainProg 	PROC				; Begin Main PROC 
	
	MOV 	AX, MyData			; Move Data segment into AX
	MOV 	DS, AX				; Move AX, containing MyData, into DS.
	
	CALL	getCmdTail			; Call to subroutine which fetches the 
						; command tail and stores it in two separate 
						; strings. 

	MOV	AX, ScreenMem			; Move screen memory variable into AX
	MOV	ES, AX				; Move AX into ES, allowing us to use ES 
						; for output to video memory 
	XOR	AX, AX				; Bit comparison to set AX to zero 
	
	CALL	clsScr				; Call to subroutine which clears the screen 
	CALL	convertString			; Call to subroutine which converts command tail
						; arguments to an integer 
	CALL	getStartTicks			; Call to subroutine which gets the current
						; ticks from the clock 
	
	CALL	openFile			; Call to subroutine which opens the input file
	CALL	createFile			; Call to subroutine which creates the output file
	CALL	readBlock			; Call to subroutine which reads a block of 64 bytes
						; into our readBuffer array 
parse_person:
	CALL	skip_White			; Call to subroutine to skip any white space 
	CALL	read_Name			; Call to subroutine which stores the person's name 
						; in tempName array 
	CALL	skip_White			; Call to subroutine which skips any white space 
	CALL	read_Int			; Call to subroutine which stores the integer in
						; tempValue array 
	CALL	convertTempNum			; Call to subroutine which converts tempValue to
						; an integer 
	MOV	BX, [personVal]			; Move the converted integer into BX 
	CMP	BX, [cntVariable]		; Compare it to the converted argument from the
						; command tail 
	JB	parse_person			; If the value is below, jump to parse_person
						; label to get the next person from the buffer
	CALL	writeFile			; Otherwise, call writeFile subroutine to write
						; the person's name to the out file 
	JMP	parse_person			; Then jump to parse_person to get the next name
						; and number from the file/buffer 
	
mainProg ENDP					; End MainProg PROC 
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clsScr		PROC				; ON entry: This PROC receives no arguments
						; nor returns any value. This proc is used 
						; upon running the program to clear the 
						; native writing from the screen
						
						; On Exit: All 1000 cells of screen memory
						; will be set to a white on black space 
						; character, essentially clearing the screen 

	PUSH	AX CX DI 			; Push AX BX CX DI ES onto the stack to preserve 
						; their value 		
						
	MOV	CX, 2000			; Move 2000 into CX to control the Loop
	MOV	DI, 0				; Move DI, index register, to 0 to point at
						; the first cell in video memory 
	MOV	AH, 07h				; MOV black background/white foreground into AH
	XOR	AL, AL				; Set AL value to zero with exclusive or bit comparison 
clrLoop:
	MOV	ES:[DI], AX			; Begin Loop, move AX into video memory at position DI
	ADD	DI, 2				; Add 2 to DI to move to the next cell in video memory 
	LOOP	clrLoop				; Use CX built in loop function to loop 
	
	POP	DI CX AX			; Pop ES DI CX BX AX to restore their preserved
						; value from the stack
	RET					; Return to main PROC 

clsScr		ENDP				; End of PROC 

;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
getCmdTail		PROC			; Upon Entry: This PROC takes no 
                        			; parameters and returns nothing.
                        			
						; Upon Exit: the ASCIIZ string "fileName" 
						; will contain the characters which
						; represent the text file entered with 
						; the command tail. "controlVarStr" will
						; contain a string which needs converted 
						; to an integer, which represents the 
						; "perameter" to be checked before 
						; printing to the outfile 
						
						
	PUSH	BX CX SI DI			; PUSH registers on to the stack to
						; preserve their values
	MOV	CL, ES:[80h]			; Move the size of the command tail into CL
	MOV	CH, 0				; Set CH to zero to clear it's value, if any 
	CMP	CX, 0				; Check if there is a size to the command tail 
	JE	noTail				; if no tail, jump to label 
						
	MOV 	SI, 82h 			; SI Points to the beginning of the 
						; command tail, skipping one space. 
						
	MOV 	DI, OFFSET fileName		; DI points fileName variable. We'll store
						; the command tail here until we see a
						; black space. 
getCMDLoop:
	MOV	BL, ES:[SI]			; Move the character from the command tail
						; at position SI into BL. 
	CMP	BL, ' '				; Compare the value in BL to ' ' space. 
	JE	getInt				; If even, jump to getInt label to begin
						; getting the rest of the command tail 
	MOV	[DI], BL			; Otherwise, move BL into fileName at 
						; position [DI] 
	INC	SI				; Move SI to the next postion in the 
						; command tail 
	INC	DI				; Move DI to the next Byte in memory 
	LOOP	getCMDLoop			; Use CX built in loop function to loop 
						; through the characters at SI and move
						; them into fileName at postion DI. 
getInt:	
	MOV	BL, 0				; Move a null terminator into BL
	MOV	[DI], BL			; Move the null terminator into the array at 
						; current position of DI
	LEA	DI, controlVarStr		; Load effective address of controlVarStr into
						; DI
	DEC	CX
	INC	SI				; Increment SI to move to the next address 
						; in the array 
intLoop:
	MOV	BL, ES:[SI]			; Move character at position SI into BL
	CMP	BL, ' '	 			; Check that BL is not a blank space
	JLE	endTail				; If so, jump to endTail label
	MOV	[DI], BL			; Otherwise, Move the character in BL into 
						; the array at position DI
	INC	SI				; Increment SI to move to the next character
	INC	DI				; Increment DI to move to the next space in array
	LOOP	intLoop				; absolute jump to intLoop label 
endTail:
	MOV	BL, 0				; MOV null terminator into BL
	MOV	[DI], BL			; Move null terminator into array at position DI
	POP	DI SI CX BX			; Pop registers off the stack to restore their value
	RET					; Return to main PROC
noTail:	
	CALL	errorHandling			; Called in the event the command tail does not exist
	
getCmdTail ENDP
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
openFile	PROC				; Upon Entry: AX contains 0. This PROC is 
						; designed to open a file from a string of
						; characters representing the file name 
						
						; Upon Exit: The file is open or the file open
						; has failed, in which case the program will 
						; terminate 
						
	PUSH	AX CX DX			; Push AX CX DX onto the stack to preserve 
						; their values 
	MOV	AH, openF			; Move 3Dh into AH. This is the "open" file 
						; function 
	LEA	DX, fileName			; Move OFFSET of the file name into DX  
	MOV	CX, 0				; Move 0 into CX to indicate the file should
						; be read only 
	INT	21h				; BIOS interupt to open the file 
	JC	errorOpen			; If carry flag, file did not open/Does not exist
	
	MOV	[inFileHandle], AX		; Otherwise, move AX into inFileHandle variable 
	POP	DX CX AX			; Pop DX CX AX off the stack to restore their preserved values 
	RET					; Return to previous routine (Main) 
errorOpen:					
	CALL	errorHandling			; Call errorHandling PROC to handle the error/terminate
						; program

openFile ENDP					; End openFile PROC 
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
createFile	PROC				; Upon Entry: This proc accepts no arguments
						; nor returns a value. Subroutine to create
						; an output file to save the processed names
						; as the program processes the infile 
						
						; Upon Exit: The outfile is either created,
						; in which case the outFileHandle variable 
						; will contain the handle for the output file
	PUSH	AX CX DX 			; Push registers on to the stack to preserve 
						; their current value 
	
	MOV	AH, 3CH				; Move the code to create a file into AH 
	MOV	DX, OFFSET outFile		; Move the location of the file name into DX 
	MOV	CX, 0				; Move CX to 0 (no special file attributes)
	INT	21h				; DOS interrupt to create the new out file 
	JC	failCreate			; If an error occurs, jump to failCreate label
	MOV	[outFileHandle], AX		; Otherwise, move AX (contains file handle) into
						; address of outFileHandle 
	POP	DX CX AX 			; Pop registers to restore their preserved values 
	RET					; Return to main PROC 
failCreate:
	CALL	failCreateDisplay		; Sunroutine to print an error message to the console
	
createFile	ENDP				; End createFile PROC 
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
errorHandling	PROC				; Upon Entry: This PROC takes no perameters
						; and returns nothing. Subroutine for printing
						; the error code in case the command tail does 
						; not exist. 
						
						; Upon exit: Error message has been printed to
						; console. Program Terminates upon failure 

	PUSH	SI DI AX BX			; Push SI DI DX AX on to the stack  to 
						; preserve their values
	MOV	AX, ScreenMem			; Move EQU of video memory variable into AX
	MOV	ES, AX				; Move video memory value into ES 
	LEA	SI, errorMsg			; Load address of errorMsg into SI( Source
						; index register ) 
	CALL	clsScr				; function to clear the screen
	XOR 	BX, BX				; Bit comparison to set BX to zero 

	MOV	DI, (160 * 14 + 58)		; Move DI to the first position in video memory
	MOV	AH, 47h				; Move AH to 47h to change the text color to
						; white on red 
	
errorPrint:
	MOV	AL, [SI]			; Move the character at position SI into AL 
	CMP	AL, 0				; Compare AL to 0 (null string terminator) 
	JE	endErrorPrint			; Jump if AX = 0 
	
	MOV	ES:[DI], AX			; Otherwise, move AX into video memory at 
						; position DI 
	INC	SI				; Increment SI to point to the next character
						; in the array 
	ADD	DI, 2				; Add 2 to DI to move to the next position in 
						; video memory 
	JMP	errorPrint			; Jump to errorPrint while AL != 0 
	
endErrorPrint:	
	POP	BX AX DI SI			; If AX = 0, pop registers to restore preserved
						; value from the stack
	MOV	AH, 4Ch				; Move AH to 4Ch to terminate the program 
	INT	21h				; Interrupt to terminate the program   

errorHandling ENDP				; End of errorHandling PROC 
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

failCreateDisplay	PROC			; Upon Entry: This PROC receives no arguments
						; nor returns a value. Subroutine for printing
						; in the event the outfile was not created. 
						;
						; Upon exit: Outfile has not been created. 
						; Error message has been printed to screen.
						; Program terminates. 
						
	PUSH	DX BX AX			; Push registers onto the stack to preserve 
						; value
	MOV	AH, 02h				; Move	02h into AH, the OP code to move the 
						; screen pointer
	MOV	BH, 0				; Move BH to zero to indicate the page number 
	MOV	DH, screenLine			; Move screenLine variable into DH to control
						; which line the output will appear on 
	MOV	DL, 0				; Move DL to 0 (column adjustment) 
	INT	10h				; BIOS interrupt to set the video memory pointer 
	
	ADD	[screenLine], 1			; Add 1 to screenLine variable 
	
	MOV	AH, 09h				; Move code to display a string into AH 
	MOV	DX, OFFSET errorCreate		; Move the address of the string to DX 
	INT	21h				; DOS interrupt to display the string 
	
	MOV	AH, 4Ch				; Move code to terminate the program into AH 
	INT	21h				; DOS interrupt to terminate the program and 
						; restore functionality to the operating system
	POP	AX BX DX			; Pop registers off the stack to restore their 
						; perserved values 
	RET					; Return to previous Procedure 
	
failCreateDisplay	ENDP			; End failCreateDisplay 
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
convertString	PROC				; Upon Entry: This PROC accepts no arguments
						; nor returns a value. The purpose is to 
						; convert the ctrVarStr from an ASCII string
						; to an integer so we can use it to process 
						; the in-file. 
						
						; Upon Exit: The ctrVarStr has been converted
						; to an integer and stored in the variable
						; cntVariable 

	PUSH	SI AX BX CX DX			; Push registers on to the stack to preserve their values
	MOV	[cntVariable], 0		; Re-initialize cntVariable to zero, since
						; we are reusing it, just to be safe 
	MOV 	SI, OFFSET controlVarStr 	; Load string address into SI
	XOR 	AX, AX 				; Clear accumulator
	MOV 	CX, 10 				; Initialize multiplier
       	
convert_loop:
        MOV 	BL, [SI] 			; Load ASCII code of current digit
        CMP 	BL, 0	 			; Check for end of string
        JE 	done_convert			; If end of string, jump to done_convert label
        SUB 	BL, '0' 			; Otherwise, convert ASCII character to decimal value
        MUL 	CX 				; Multiply AX by CX to make room for the next
        					; digit 
        ADD 	AX, BX 				; Add current digit to accumulator
        INC 	SI 				; Increments SI to move to next digit in string
        JMP 	convert_loop			; Absolute jump to convert_loop label 
        
done_convert:
	MOV 	[cntVariable], AX 		; Store result in cntVariable
	
	POP	DX CX BX AX SI			; Pop registers off the stack to restore their 
						; preserved values
        RET					; Return to Main PROC 
        
convertString ENDP 				; End convertString PROC 
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
convertTempNum	PROC				; Upon Entry: This PROC accepts no arguments
						; nor returns a value. The purpose is to 
						; convert the ctrVarStr from an ASCII string
						; to an integer so we can use it to process 
						; the in-file. 
						
						; Upon Exit: The ctrVarStr has been converted
						; to an integer and stored in the variable
						; personVal 

	PUSH	SI AX BX CX DX			; Push registers on to the stack to preserve
						; their values
	MOV 	SI, OFFSET tempValue		; Load string address into SI
	XOR 	AX, AX 				; Clear accumulator
	XOR	BX, BX				; Clear trasfer register, just to be safe 
	MOV 	CX, 10 				; Initialize multiplier 
	
convert_Value:
        MOV 	BL, [SI] 			; Load ASCII code of current digit
        CMP 	BL, '$'	 			; Check for end of string
        JE 	done_Value			; If equal, jump to done_value label 
        SUB 	BL, '0' 			; Otherwise, convert ASCII code to decimal value
        MUL 	CX 				; Multiply AX by CX to make room for the next
        					; digit 
        ADD 	AX, BX 				; Add current digit to accumulator
        INC 	SI 				; Increment SI to move to next digit in the string
        JMP 	convert_Value			; Absolute jump to convert_value label. 
        
done_Value:
	MOV 	[personVal], AX 		; Store result in personValue variable 
	POP	DX CX BX AX SI			; Pop the registers to restore their value from the stack 
        RET					; return to Main PROC 
        
convertTempNum ENDP 				; End convertTempNum PROC 
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
readBlock	PROC				; Upon Entry: This proc accepts no arguments
						; nor returns a value. The purpose is to read
						; a "block"v (64 bytes) of data from a file 
						; and store it in the buffer array (bufferRead) 
						
						; Upon Exit: the buffer is filled with the number of
						; bytes read from the file (default 64 bytes hardcoded) 
						; and AX contains the actual number of bytes read. We
						; store that value in actNumRead variable for later
						; comparison and error handling =] 

	PUSH	AX BX CX DX 			; Push Registers onto the stack to preserve their value
	
	MOV	AH, 3Fh				; MOV AH to DOS code to read from file  
	MOV	BX, inFileHandle		; Put the inFileHandle(contains inFile handle)
						; into BX for the interrupt 
	MOV	CX, bufferSize			; Move bufferSize into CX to control the number
						; of bytes read 
	MOV	DX, OFFSET bufferRead		; Load the address of the buffer into DX for the 
						; read interrupt 
	INT	21h				; Read 64 bytes, or whatever amount are left in the
						; file	
	JC	end_block			; Jump if there is a carry (Error reading from file) 
	CMP	AX, 0				; Otherwise, compare AX (number of bytes read) to 0 
	JG	end_read			; IF AX is less or equal to O, jump to end_read label
						; otherwise, continue to end_block label 
	
end_block:
	MOV	AH, 3Eh				; Move the DOS code to close the file into AH
	MOV	BX, inFileHandle		; Move in file handle into BX
	INT	21h				; DOS interrupt to close the in file 
	
	MOV	AH, 3Eh				; Move the DOS code to close the file into AH
	MOV	BX, outFileHandle		; Move out file handle into BX
	INT	21h				; DOS interrupt to close the in file 
	
	CALL	getEndTicks			; Call subroutine to process the number of ticks elapsed
	CALL	end_display			; 
	CALL	time_display			; Subroutine to display time_msg
	CALL	convert_Ticks			; Subroutine to convert ticks to tenths of a second
						; and display it to screen 
	
	MOV	AH, 4Ch				; Move 4Ch into AH to terminate the program 
	INT	21h				; DOS interrupt to terminate the program and restore 
						; functionality to the operating system. 
end_read:
	MOV	[actNumRead], AX		; If AX is greater than zero, and no error occurs while
						; reading from the in file, move AX (bytes read) into
						; actNumRead variable
	MOV	[bufferOffset], 0		; Set bufferOffset variable to zero, since the buffer is now
						; "fresh" 
	POP	DX CX BX AX 			; Pop registers to restore their previous value from the stack
	RET					; Return to previous PROC 

readBlock	ENDP				; End readBlock PROC 
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
getNextByte	PROC				; Upon Entry: This PROC receives no arguments
						; Subroutine to fetch the next byte of data 
						; from the buffer array and place it into AX. 
						; Returns the character by passing AX
						
						; Upon Exit: AX contains the next character 
						; from the buffer to be processed. 
		PUSH	SI BX 			; Push SI BX on to the stack to preserve their value
		
		MOV	BX, [bufferOffset]	; move bufferOffset to BX
		CMP	BX, [actNumRead]	; Is it equal to the actNumRead variable (hold the
						; actual number of bytes read) 
		JE	fix_buffer		; If even, jump to fix_buffer label to read into 
						; the buffer 
fixed_it:
		MOV	SI, OFFSET bufferRead	; Otherwise, Mov buffer address into SI
		ADD	SI, BX			; Add the offset (current position in the buffer) to SI
		MOV	AL, [SI]		; Move the character at position SI to AL
		INC	BX			; Increment BX by one to account for the new position
		MOV	[bufferOffset], BX	; Move BX into bufferOffset variable to update the value
		POP	BX SI			; Pop BX SI to restore their previously preserved values 
		RET				; Return to PROC  
		
fix_buffer:	
		CALL	readBlock		; Call to subroutine used to read into our in buffer
						; variable "bufferRead" 
		MOV	BX, [bufferOffset]	; Move the updated offset of the buffer into BX 
		JMP	fixed_it		; Jump to fixed_it label with the buffer reloaded
		
getNextByte	ENDP				; End getNextByte PROC
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
skip_White	PROC				; Upon Entry: This PROC receives no arguments. 
						; Subroutine for skipping any white space contained
						; in the in file. Returns a character value in the 
						; AX register (character will be in AL) 
						
						; Upon Exit: AX contains either a character or a digit
						; from the buffer array. Any white space between names 
						; and integer value, or integer value and string, have 
						; been skipped. 
top_skip:
	CALL	getNextByte			; Call subroutine to fetch the next byte of data from
						; the buffer
	CMP	AL, 20h				; Is this a space? 
	JLE	top_skip			; If the character is a space, or less (include tab, CR, LF)
						; jump to top_skip label 
endSkip:
	RET					; Otherwise, return to MAIN with the character in AX 
		
skip_White	ENDP				; End skip_white PROC 
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
read_Name	PROC				; Upon Entry: AX contains a non-whitespace character
						; assumed to be the beginning of a name. 
						; This PROC will store the names into a temp string 
						; to process later in the program. 
						
						; Upon Exit: tempName will contain one person's
						; name from out buffer array 
						
	PUSH	DI 				; Push AX onto the Stack to preserve it's value 
	MOV	DI, OFFSET tempName		; Load the address of the tempName into Di to
						; store the name from the buffer array  
	MOV	[str_len], 1			; Set str_len to 1(account for assumed character).  
						; This variable controls our output to our Outfile 
						; (need # of bytes to be printed to write to outfile,
						; not including null terminator)
	MOV	[DI], AL			; Move the assumed character into tempName array at
						; position DI 
	INC	DI				; Increment DI to point to the next byte in the array 
	
top_name:
	CALL	getNextByte			; Call to subroutine which fetches the next byte of
						; data from our buffer array 
	CMP	AL, 20h 			; Is the character a blank space? 
	JLE	end_name			; If so, jump to end name.
	MOV	[DI], AL			; Other wise, move the character into tempName array
	INC	DI				; Increment DI to move to the next space in tempName 
	ADD	[str_len], 1			; Add 1 to str_len variable to account for the character
						; added 
	JMP	top_name			; Jump to top_name label to process the next character 
end_name:
	MOV	AL, 0Dh				; Move carriage return character into AL
	MOV	[DI], AL			; Move the carriage return character into the array at DI
	INC	DI				; Increment DI to move to the next space in tempName 
	MOV	AL, 0Ah				; Move line feed character into AL 
	MOV	[DI], AL			; Move line feed character into the array at position DI 
	INC	DI				; Increment DI to move to the next space in tempName 
	ADD	[str_len], 2			; Add 2 to name length to account for the two added characters
	MOV	AL, 0				; Since we have the whole name, move 0 into AL
	MOV	[DI], AL			; Move 0 into DI (null terminate our string)
	POP	DI				; POP AX off the stack to restore it's value
	RET					; return to main proc 
	
read_Name	ENDP				; Return to Main PROC 
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_Int	PROC				; Upon Entry: AX contains a non-whitespace character
						; assumed to be the beginning of an integer value. 
						; This PROC will store the value into a temp string 
						; to process later in the program. 
						
						; Upon Exit:  will contain one person's
						; name. SI will be incremented and our counter, 
						; will be decremented. 

	PUSH	BX DI 				; Push AX onto the Stack to preserve it's value 
	MOV	DI, OFFSET tempValue		; Load the address of the tempValue into Di to
						; store the number from the file 
	
	MOV	[DI], AL			; Move the assumed character into the array at DI 
	INC	DI				; Increment DI to point to the next position in
						; the array 
top_Int:
	CALL	getNextByte			; Call to subroutine which fetches the next byte of
						; data from our buffer array 
	CMP	AL, 20h 			; Is the character a null terminator? 
	JLE	end_Int				; If so, jump to end Int.
	MOV	[DI], AL			; Other wise, move the character into tempName array
	INC	DI				; Increment DI to move to the next space in tempName 
	JMP	top_Int				; Jump to process the next character 
	
end_Int:
	MOV	BL, '$'
	MOV	[DI], BL			; Move 0 into DI (null terminate our string)
	POP	DI BX				; POP AX off the stack to restore it's value
	RET					; return to main proc 
	
read_Int	ENDP				; Return to Main PROC 
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
writeFile	PROC				; Upon Entry: This PROC receives no arguemnts
						; nor returns a value. Subroutine for writing 
						; to the outfile in the event the person's value
						; was greater than or even to the control variable
						; entered on the command line
						
						; Upon Exit: The person's name has been printed to
						; the outfile

	PUSH	AX BX CX DX			; Push the address of AX CX DX on to the 
						; stack to preserve their previous values 
	
	MOV 	AH, 40h    			; DOS write to file function
	MOV 	BX, outFileHandle	      	; outFileHandle In BX to control output to file
	MOV 	CX, [str_len] 			; length of the string
	MOV	DX, OFFSET tempName		; Move DX to point at the begining of name 	
	INT 	21h        			; write the string to the file
	
	POP	DX CX BX AX 			; Pop registers to restore their preserved values 
						; from the stack
	RET					; Return to Main PROC
writeFile	ENDP				; End writeFile PROC

;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
getStartTicks 	PROC				; Upon Entry: All registers are null. 
						; This PROC accepts no arguments nor returns
						; a value. Performs the RTC interupt to 
						; fetch the current time from the clock,
						; then stores the value in startTicks
						
						; Upon Exit: startTicks variable is updated
						; to contain the time the program began. 
		
	PUSH	AX CX DX			; Push the address of AX CX DX on to the 
						; stack to preserve their previous values 
	MOV	AH, 00h				; Move OOh into AH in preparation of 
						; getting the system ticks
	INT	1Ah				; Gets system ticks and stores them in CX:DX
	MOV	startTicks, DX			; Move DX (ticks) into startShiftTicks variable 
	POP	DX CX AX			; POP DX CX AX to restore their previous value 
						; which we pushed on the stack 
	RET					; Return to previous PROC/Main 

getStartTicks ENDP				; End PROC 
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
getEndTicks 	PROC				; Upon Entry: All registers are null. 
						; This PROC accepts no arguments nor returns
						; a value. Performs the RTC interupt to 
						; fetch the current time from the clock,
						; then subtracts that value from startTicks,
						; then places that value into endTicks variable
						
						; Upon Exit: endTicks contains the time 
						; elapsed (in ticks) from when the program 
						; began and when it finished.

	PUSH	AX BX CX DX			; Push the address of AX CX DX on to the 
						; stack to preserve their previous values 
	MOV	AH, 00h				; Sets AH to 00h, the Real time clock interupt
	INT	1Ah				; Software interupt to get time from clock 
	MOV	BX, startTicks			; Move startTicks variable into BX 
	SUB	DX, BX				; Subtract current ticks form startTicks to
						; get the amount of time that has passed 
	MOV	endTicks, DX			; Move BX, containing ticks elapsed, into endTicks
						; variable. 
	POP	DX CX BX AX			; POP DX CX AX to restore their previous value 
						; which we pushed on the stack 
	RET					; Return to previous PROC/Main 
	
getEndTicks ENDP				; END PROC 
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
convert_Ticks	PROC				; Upon Entry: This PROC receives no arguments
						; nor returns a value. Subroutine to convert
						; our personVal variable from a string to an 
						; integer, and then place it in screen memory
						; to inform user 
						
						; Upon Exit: personVal has been converted from
						; string to an integer. It has also been placed 
						; in screen memory on line 24
						
	PUSH	AX BX CX DX DI			; Push registers on to the stack to preserve
						; their values
						
	MOV	DI, (160 * 1 + 158)		; Move DI to point to the last cell in screen mem 
	MOV	CX, 10				; Move 10 into DX in preparation of division 
	MOV	BX, 18				; Move 18 into BX in preparation for multiplication 
	MOV	AX, endTicks			; Move endTicks variable into AX to begin processing
	
	MUL	CX				; Multiply AX by CX (NumOfTicks * 10 tenths/second) 
	DIV	BX				; Divde AX by BX. AX now contains our whole and 				
		
						; tenths of a second. We are ready to convert to int
	XOR	DX, DX				; Bit comparison to set DX to zero 
	DIV	CX				; Divide AX by CX to get the first digit
	ADD	DL,'0'				; Add '0' to convert the digit to a character 
	MOV	ES:[DI], DL			; Move the character into screen memory at posiiton DI 
	SUB	DI, 2				; Subtract 2 from DI (each cell is two bytes) 
	XOR	DX, DX				; Bit comparison to set DX to zero 
	
	MOV	DL, '.'				; Move our decimal point into DL 
	MOV	ES:[DI], DL			; Move the decimal point into screen mem at position DI
	SUB	DI, 2				; Subtract 2 from DI
	XOR	DX, DX				; Bit comparison to set DX to zero 
	
	DIV	CX				; Divide AX by CX to get the next digit
	ADD	DL,'0'				; Add '0' to convert the digit to a character 
	MOV	ES:[DI], DL			; Move the decimal point into screen mem at position DI
	SUB	DI, 2				; Subtract 2 from DI
	XOR	DX, DX				; Bit comparison to set DX to zero 
	
	DIV	CX				; Divide AX by CX to get the first digit
	ADD	DL,'0'				; Add '0' to convert the digit to a character 
	MOV	ES:[DI], DL			; Move the decimal point into screen mem at position DI
	SUB	DI, 2				; Subtract 2 from DI
	
	XOR	DX, DX				; Bit comparison to set DX to zero 
	XOR	AX, AX				;
	XOR	BX, BX				; 
	
	MOV	AH, 02h				; Move 02h, the hex code to move the pointer within
						; video functions
	MOV	BH, 0				; BH represents the 'page' number we'd like to
						; move the pointer to 
	MOV	DH, 23		 		; Represents the line number you wish to move to
	MOV	DL, 0				; Represents the offset of the current position 
	INT	10h				; Interrupt which moves the point to the specified
						; position

	POP	DI DX CX BX AX			; Pop registers to restore their preserved values from
						; the stack
	RET					; Return to PROC
	
convert_Ticks	ENDP				; End convert_Ticks PROC
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
time_display	PROC				; Upon Entry: This PROC receives no arguments
						; nor returns any value. Subroutine that transfers
						; an ASCII string to screen memory. The string 
						; informs the user how long it took the program
						; to process the file
						;
						; Upon Exit: The ASCII string has been placed in 
						; screen memory.
						
	PUSH	DX AX BX			; Push registers on to the stack to preserve
						; their values
	MOV	AH, 02h				; Move 02h, the hex code to move the pointer within
						; video functions
	MOV	BH, 0				; BH represents the 'page' number we'd like to
						; move the pointer to 
	MOV	DH, 1		 		; Represents the line number you wish to move to
	MOV	DL, 0				; Represents the offset of the current position 
	INT	10h				; Interrupt which moves the point to the specified
						; position
						
	MOV	AH, 09h				; Move code to print a '$' terminated string into
						; AH
	MOV	DX, OFFSET time_msg		; Loads the address of the string into DX
	INT	21h				; BIOS interrupt to print the string to screen mem
	
	POP	BX AX DX			; POP register values of the stack to restore their 
						; value
	RET					; Returns to previous PROC
	
time_display	ENDP				; End time_display PROC
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
end_display	PROC				; Upon Entry: This PROC receives no arguments
						; nor returns any value. Subroutine that transfers
						; an ASCII string to screen memory. The string 
						; informs the user that their file has been processed
						; and that the outfile.txt file now exists. 
						
						; Upon Exit: The ASCII string has been placed in 
						; screen memory.
						
	PUSH	DX AX BX			; Push registers on to the stack to preserve
						; their values
	MOV	AH, 02h				; Move 02h, the hex code to move the pointer within
						; video functions
	MOV	BH, 0				; BH represents the 'page' number we'd like to
						; move the pointer to 
	MOV	DH, 0		 		; Represents the line number you wish to move to
	MOV	DL, 0				; Represents the offset of the current position 
	INT	10h				; Interrupt which moves the point to the specified
						; position
						
	MOV	AH, 09h				; Move code to print a '$' terminated string into
						; AH
	MOV	DX, OFFSET end_prog		; Loads the address of the string into DX
	INT	21h				; BIOS interrupt to print the string to screen mem
	
	POP	BX AX DX			; POP register values of the stack to restore their 
						; value
	RET					; Returns to previous PROC
	
end_display	ENDP				; End time_display PROC

;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
MyCode ENDS					; End of MyCode

END mainProg					; End of Program 