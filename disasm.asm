.model small
.stack 100h
.data
	fileErrorRead db "Nepavyko atidaryti failo skaitymui", '$'
	fileErrorWrite db "Nepavyko atidaryti failo rasymui", '$'
	fileReadingError db "Nepavyko nuskaityti failo", '$'
	fileWritingError db "Nepavyko irasyti i faila", '$'
	noInput db "Nera ivesties", '$'
	help db "Disasembleris (visos komandos be ESC)", '$'

	unknownInstruction db "Nezinoma instrukcija", 0
	
	instructionBuff db 64 dup(?)
	bytesBuff db 16 dup (?)
	buffer db 512 dup (?)
	writeBuffer db 512 dup (?)
	
	readHandle dw ?
	writeHandle dw ?
	fileName db 255 dup (0)
	offsetSymbol db ":	", 0
	isSegment db 0
	d db ?
	w db ?
	md db ?
	reg db ?
	rm db ?
	leftBytes dw ?
	opOffset dw 100h
	code db ?
	canWrite db 1
	bytesBuffIndex dw 0
	instructionBuffIndex dw 0
	
	isPrefix db 0
	currPrefix db 3 dup(?), 0
	firstTime db 1
	endl db 13, 10, 0
	comma db ', ', 0
	
	oneByteInstructions db 6h, "push ES", 0, 7h, "pop ES", 0, 0Eh, "push CS", 0, 16h, "push SS", 0, 17h, "pop SS", 0, 1Eh, "push DS", 0, 1Fh, "pop DS", 0, 37h, "aaa", 0, 3Fh, "aas", 0, 27h, "daa", 0, 2Fh, "das", 0, 98h, "cbw", 0, 99h, "cwd", 0, 9Bh, "wait", 0, 9Ch, "pushf", 0, 9Dh, "popf", 0, 9Eh, "sahf", 0, 9Fh, "lahf", 0, 0A4h, "movsb", 0, 0A5h, "movsw", 0, 0A6h, "cmpsb", 0, 0A7h, "cmpsw", 0, 0AAh, "stosb", 0, 0ABh, "stosw", 0, 0ACh, "lodsb", 0, 0ADh, "lodsw", 0, 0AEh, "scasb", 0, 0AFh, "scasw", 0, 0C3h, "ret", 0, 0CBh, "ret", 0, 0CCh, "int 3h", 0, 0CEh, "into", 0, 0CFh, "iret", 0, 0D7h, "xlat", 0, 0ECh, "in al, dx", 0, 0EDh, "in ax, dx", 0, 0EEh, "out dx, al", 0, 0EFh, "out dx, ax", 0, 0F0h, "lock", 0, 0F2h, "repne", 0, 0F3h, "rep", 0, 0F4h, "hlt", 0, 0F5h, "cmc", 0, 0F8h, "clc", 0, 0F9h, "stc", 0, 0FAh, "cli", 0, 0FBh, "sti", 0, 0FCh, "cld", 0, 0FDh, "std", 0
	twoByteInstructions db 0D4h, "aam", 0, 0D5h, "aad", 0
	endsReg16Instructions db 40h, "inc ", 0, 48h, "dec ", 0, 50h, "push ", 0, 58h, "pop ", 0, 90h, "xchg ax, ", 0
	startsRegInstructions db 0B0h, "mov ", 0
	dwmdregrmInstructions db 0h, "add ", 0, 08h, "or ", 0, 10h, "adc ", 0, 18h, "sbb ", 0, 20h, "and ", 0, 28h, "sub ", 0, 30h, "xor ", 0, 38h, "cmp ", 0, 88h, "mov ", 0
	alAxExceptionInstructions db 4h, "add ", 0, 0Ch, "or ", 0, 14h, "adc ", 0, 1Ch, "sbb ", 0, 24h, "and ", 0, 2Ch, "sub ", 0, 34h, "xor ", 0, 3Ch, "cmp ", 0, 0A8h, "test ", 0
	givenreg80Instructions db 000b, "add ", 0, 001b, "or ", 0, 010b, "adc ", 0, 011b, "sbb ", 0, 100b, "and ", 0, 101b, "sub ", 0, 110b, "xor ", 0, 111b, "cmp ", 0
	wmdregrmLoHiInstructions db 84h, "test ", 0, 86h, "xchg ", 0
	segRegInstructions db 8Bh, "mov ", 0
	modregrmInstructions db 8Dh, "lea ", 0, 0C4h, "les ", 0, 0C5h, "lds ", 0
	givenreg8FInstructions db 000b, "pop ", 0
	alAxmemInstructions db 0A0h, "mov ", 0
	givenregC6Instructions db 000b, "mov ", 0
	data8Instructions db 0C2h, "ret ", 0, 0CAh, "ret ", 0, 0CDh, "int ", 0
	givenregD0Instructions db 000b, "rol ", 0, 001b, "ror ", 0, 010b, "rcl ", 0, 011b, "rcr ", 0, 100b, "shl ", 0, 101b, "shr ", 0, 111b, "sar ", 0
	inOutInstructions db 0E4h, "in ", 0, 0E6h, "out ", 0
	givenregF6Instructions db 000b, "test", 0, 010b, "not ", 0, 011b, "neg ", 0, 100b, "mul ", 0, 101b, "imul ", 0, 110b, "div ", 0, 111b, "idiv ", 0
	givenregFEInstructions db 000b, "inc ", 0, 001b, "dec ", 0, 010b, "call ", 0, 011b, "call ", 0, 100b, "jmp ", 0, 101b, "jmp ", 0, 110b, "push ", 0
	endsWithSegmentInstructions db 06h, "push ", 0, 07h, "pop ", 0
	ipInc8Instructions db 70h, "jo ", 0, 71h, "jno ", 0, 72h, "jb ", 0, 73h, "jnb ", 0, 74h, "je ", 0, 75h, "jne ", 0, 76h, "jna ", 0, 77h, "ja ", 0, 78h, "js ", 0, 79h, "jns ", 0, 7Ah, "jp ", 0, 7Bh, "jnp ", 0, 7Ch, "jl ", 0, 7Dh, "jnl ", 0, 7Eh, "jle ", 0, 7Fh, "jg ", 0, 0E0h, "loopne ", 0, 0E1h, "loope ", 0, 0E2h, "loop ", 0, 0E3h, "jcxz ", 0, 0EBh, "jmp ", 0 
	farProcInstructions db 9Ah, "call ", 0, 0EAh, "jmp ", 0
	nearInstructions db 0E8h, "call ", 0, 0E9h, "jmp ", 0
	prefixInstructions db 26h, "es:", 0, 2Eh, "cs:", 0, 36h, "ss:", 0, 3Eh, "ds:", 0
	
	segments db "ES", 0, "CS", 0, "SS", 0, "DS", 0
	registers16 db "ax", 0, "cx", 0, "dx", 0, "bx", 0, "sp", 0, "bp", 0, "si", 0, "di", 0
	registers8 db "al", 0, "cl", 0, "dl", 0, "bl", 0, "ah", 0, "ch", 0, "dh", 0, "bh", 0
	
	mods db 000b, "bx+si", 0, 001b, "bx+di", 0, 010b, "bp+si", 0, 011b, "bp+di", 0, 100b, "si", 0, 101b, "di", 0, 110b, "bp", 0, 111b, "bx", 0
	
	opWordPtr db "word ptr ", 0
	opBytePtr db "byte ptr ", 0
.code

printerror macro error
	mov dx, offset error
	jmp errorMsg
endm

fputc macro char
	mov al, char
	call fPutChar
endm

fputstr macro string
	lea bx, string
	call fPutString
endm

notGivenReg macro instructionArray, arraySize, instructionWidth, procedure
	local checkInstructions, next
	mov cx, arraySize
	lea bx, instructionArray
	
	checkInstructions:
	cmp dh, [bx]
	jb next
	mov al, [bx]
	add al, instructionWidth
	cmp dh, al
	ja next
	
	mov al, [bx]
	mov code, al
	
	inc bx
	call fPutString
	xor ah, ah
	mov al, dh
	sub al, code
	
	call procedure
	
	jmp AnalyzeNextBytes
	next:
	call getNextInstruction
		
	loop checkInstructions
endm

givenReg macro instructionArray, arraySize, instructionStart, instructionWidth, procedure
	local checkInstructions, next, exit
	mov cx, arraySize
	lea bx, instructionArray
	
	cmp dh, instructionStart
	jb exit
	cmp dh, instructionStart+instructionWidth
	ja exit
	
	call getByte
	call getModRegRm
	
	checkInstructions:
	
	mov al, [bx]
	cmp reg, al
	jne next
	inc bx
	call fPutString
	call procedure	
	jmp AnalyzeNextBytes
	
	next:
	call getNextInstruction
		
	loop checkInstructions
	exit:
endm

mov ax, @data
mov ds, ax

call getCommandLineParameters

xor si, si
mov di, 512
AnalyzeNextBytes:
	cmp firstTime, 1
	je noEndl
	call printBytesBuff
	fputstr endl
	noEndl:
	call printOffset
	mov firstTime, 0
	werePrefix:
	call getByte
	call getDW

	notGivenReg oneByteInstructions 49 0 doNothing
	notGivenReg twoByteInstructions 2 0 getByte
	notGivenReg endsReg16Instructions 5 7 register16Only
	notGivenReg startsRegInstructions 1 15 movRegImm
	notGivenReg dwmdregrmInstructions 9 3 allParameters
	notGivenReg alAxExceptionInstructions 9 1 registerImm
	givenReg givenreg80Instructions 8 80h 3 parametersImm
	notGivenReg wmdregrmLoHiInstructions 2 1 allParameters
	notGivenReg modregrmInstructions 3 0 loadGroup
	notGivenReg segRegInstructions 1 3 dsegment
	givenReg givenreg8FInstructions 1 08Fh 0 analyzeEA
	notGivenReg alAxmemInstructions 1 3 axAlAllParameters
	givenreg givenregC6Instructions 1 0C6h 1 parametersImm
	notGivenReg data8Instructions 3 0 directImm
	givenreg givenregD0Instructions 7 0D0h 4 rotateGroup
	notGivenReg ipInc8Instructions 21 0 shortGroup
	notGivenReg nearInstructions 2 0 nearGroup
	notGivenReg farProcInstructions 2 0 farGroup
	givenreg givenregF6Instructions 7 0F6h 5 EAPlusException
	givenreg givenregFEInstructions 7 0FEh 1 analyzeEA	
	notGivenReg inOutInstructions 2 1 inOutGroup
	
	cmp isPrefix, 1
	jne wereNotPrefix
	jmp werePrefix
	wereNotPrefix:
	fputstr unknownInstruction
	jmp AnalyzeNextBytes

	
EOF:
	call writeBufferToFile
	mov ah, 3eh
	mov bx, readHandle
	int 21h
	mov bx, writeHandle
	int 21h

jmp exit

proc doNothing
	ret
endp

proc directImm
	xor w, 1
	call chooseImm
	ret
endp 

proc EAPlusException
	cmp reg, 000b
	je exception
	call analyzeEA
	ret
	exception:
	call parametersImm
	ret
endp

proc register16Only
	push bx
	mov bl, 3
	mul bl
	mov bx, ax
	fputstr [registers16+bx]
	pop bx
	ret
endp

proc movRegImm
	push bx
	mov bx, 8
	div bl
	mov w, al
	mov al, ah
	xor ah, ah
	mov bl, 3
	mul bl
	mov bl, al
	
	call chooseRegister
	fputstr comma
	call chooseImm
	
	registerEnd:
	pop bx
	ret
endp

proc allParameters
	call getByte
	call getModRegRm
	call checkReverse
	ret
endp

proc registerImm
	push bx
	mov bx, 0
	call chooseRegister
	fputstr comma
	call chooseImm
	pop bx
	ret
endp

proc dsegment
	call getByte
	call getModRegRm
	call checkReverseWithSegments
	ret
endp

proc loadGroup
	mov d, 1
	mov w, 1
	call allParameters
	ret
endp

proc axAlAllParameters
	push bx
	mov bx, 0
	call getModRegRm
	call checkReverse
	pop bx
	ret
endp 

proc rotateGroup
	call analyzeEA
	fputstr comma
	cmp d, 0
	je opCl
	fputc '1'
	jmp rotateGroupend
	opCl:
	fputstr [registers8+3]
	rotateGroupend:
	ret
endp

proc parametersImm
	call analyzeEA
	fputstr comma
	call chooseImm
	ret
endp

proc shortGroup
	call getByte
	mov dl, dh
	xor dh, dh
	cmp dx, 80h
	jb keepSameDx
	sub dx, 100h
	keepSameDx:
	add dx, opOffset
	call convertDhToHex
	mov dh, dl
	call convertDhToHex
	ret
endp

proc farGroup
	call getByte
	push dx
	call getByte
	push dx
	call getByte
	push dx
	call getByte
	call convertDhToHex
	pop dx
	call convertDhToHex
	fputc ':'
	pop dx
	call convertDhToHex
	pop dx
	call convertDhToHex
	ret
endp

proc nearGroup
	call getByte
	mov dl, dh
	call getByte
	cmp dx, 8000h
	jb keepSameDx1
	sub dx, 0FFFFh
	sub dx, 1
	keepSameDx1:
	add dx, opOffset
	call convertDhToHex
	mov dh, dl
	call convertDhToHex
	ret
endp

proc inOutGroup
	push bx
	mov bx, 0
	
	cmp d, 0
	jne reverse2

	call chooseRegister
	mov w, 0
	fputstr comma
	call chooseImm
	jmp endInOut
	
	reverse2:
	mov al, w
	push ax
	mov w, 0
	call chooseImm
	pop ax
	mov w, al
	
	fputstr comma
	mov bx, 0
	call chooseRegister
	endInOut:
	pop bx
	ret
endp

proc printOffset
	push dx
	mov dx, opOffset
	call convertDhToHex
	mov dh, dl
	call convertDhToHex
	fputstr offsetSymbol
	pop dx
	mov canWrite, 0
	ret
endp

proc getNextInstruction
	skip:
	inc bx
	cmp byte ptr [bx], 0
	jne skip
	inc bx
	ret
endp

proc checkPrefix
	push si
	mov si, 0
	mov cx, 4
	lea bx, prefixInstructions
	checkPrefixInstructions:
		cmp dh, [bx]
		jne nextInstruction
		inc bx
		mov cx, 3
		fillBuff:
			mov al, [bx]
			mov currPrefix[si], al
			inc bx
			inc si
		loop fillBuff
		mov isPrefix, 1
		jmp pExit
		
	nextInstruction:
	call getNextInstruction
	loop checkPrefixInstructions
	pExit:
	pop si
	ret
endp

proc getCommandLineParameters
	xor cx, cx
	mov cl, es:[80h]
	cmp cx, 0
	jne notError1
	printerror noInput
	
	notError1:
		mov bx, 81h
		xor si, si
		call skipSpaces
		
	checkHelp:
		cmp cx, 2
		jb checkHelpEnd
		cmp es:[bx], '?/'
		jne checkHelpEnd
		printerror help
	checkHelpEnd:
	
	getReadFileNameToBuff:
		mov al, es:[bx+si]
		cmp al, ' '
		je readFileHandle
		mov fileName[si], al
		inc si
		loop getReadFileNameToBuff
		
	readFileHandle:
		call getReadFileHandle
		call skipSpaces
		
	resetIndex:
		add bx, si
		xor si, si
		
	getWriteFileNameToBuff:
		mov al, es:[bx+si]
		mov fileName[si], al
		inc si
		loop getWriteFileNameToBuff
		
	writeFileHandle:
		mov fileName[si], 0
		call getWriteFileHandle
	ret
endp

proc chooseImm
	cmp w, 0
	jne insertWord
	
	call putHexByte
	ret
	
	insertWord:
	call putHexWord
	ret
endp

proc chooseRegister
	cmp w, 0
	jne register16_
	fputstr [registers8+bx]
	jmp endAnalyzeReg
	
	cmp isSegment, 1
	jne register16_
	fputstr [segments+bx]
	mov isSegment, 0
	jmp endAnalyzeReg
	
	register16_:
	fputstr [registers16+bx]
	
	endAnalyzeReg:
	ret
endp

proc analyzeReg
	xor ah, ah
	mov al, reg
	
	push bx
	mov bl, 3
	mul bl
	mov bx, ax
	
	call chooseRegister
	pop bx
	ret
endp

proc analyzeSeg
	mov isSegment, 1
	call analyzeReg
	ret
endp

proc checkReverse
	cmp d, 0
	jne reverse

	call analyzeEA
	fputstr comma
	call analyzeReg
	ret
	
	reverse:
	call analyzeReg
	fputstr comma
	call analyzeEA
	ret
endp

proc checkReverseWithSegments
	mov w, 1
	mov isSegment, 1
	call checkReverse
	ret
endp

proc analyzeEA
	cmp md, 11b
	jne notRegister
	mov ah, rm
	mov reg, ah
	call analyzeReg
	ret
	
	notRegister:
	cmp w, 0
	jne wordPtr
	fputstr opBytePtr
	jmp prefixCheck
	wordPtr:
	fputstr opWordPtr
	prefixCheck:
	
	cmp isPrefix, 0
	je noPrefix
	
	fputstr currPrefix
	mov isPrefix, 0
	
	noPrefix:
	fputc '['
	
	mov cx, 8
	lea bx, mods
	rmCheck:
		mov ah, [bx]
		cmp rm, ah
		jne rnext
		cmp rm, 110b
		jne notException
		cmp md, 00b
		jne notException
		call putHexWord
		jmp m1
		notException:
		inc bx
		fputstr [bx]
		jmp m1
		rnext:
		call getNextInstruction
	loop rmCheck
	
	m1:
	cmp md, 01b
	jne m2
	fputc '+'
	call putHexByte
	jmp endAnalyze
	
	m2:
	cmp md, 10b
	jne endAnalyze
	fputc '+'
	call putHexWord
	jmp endAnalyze
	
	endAnalyze:
	fputc ']'
	ret
endp

proc getByte
	cmp di, 512
	je reset
	
	cmp leftBytes, 0
	jne notreset
	jmp EOF
	notreset:
	mov dh, buffer[di]
	inc di
	dec leftBytes
	inc opOffset
	
	push bx
	mov bx, bytesBuffIndex
	mov bytesBuff[bx], dh
	inc bytesBuffIndex
	call checkPrefix
	pop bx
	ret
	
	reset:
	push ax bx cx dx
	mov bx, readHandle
	mov ah, 3fh
	mov cx, 512
	mov dx, offset buffer
	int 21h
	jnc notError4
	printerror fileReadingError
	notError4:
	xor di, di
	mov leftBytes, ax
	pop dx cx bx ax
	
	call getByte
	ret
endp

proc getModRegRm
	mov rm, dh
	and rm, 00000111b
	mov reg, dh
	shr reg, 3
	and reg, 00000111b
	mov md, dh
	shr md, 6
	and md, 00000011b
	ret
endp

proc getDW
	push dx
	rcr dh, 1
	mov w, 0
	adc w, 0
	rcr dh, 1
	mov d, 0
	adc d, 0
	pop dx
	ret
endp

proc putHexWord
	call getByte
	push dx
	call getByte
	cmp dh, 0A0h
	jb notPrefixw
	fputc '0'
	
	notPrefixw:
	call convertDhToHex
	pop dx
	call convertDhToHex
	fputc 'h'
	ret
endp

proc putHexByte
	call getByte
	cmp dh, 0A0h
	jb notPrefixb
	fputc '0'
	
	notPrefixb:
	call convertDhToHex
	fputc 'h'
	ret
endp

proc convertDhToHex
	push cx dx
	mov cx, 2
	getDigits:
		xor dl, dl
		rol dx, 4
		cmp dl, 9
		ja hexLetter
		add dl, 48
		jmp writeToBuffer
		hexLetter:
			add dl, 55
		writeToBuffer:
			fputc dl
	loop getDigits
	pop dx cx
	ret
endp

proc printBytesBuff
	push cx dx bx
	
	mov canWrite, 1
	xor bx, bx
	mov cx, bytesBuffIndex
	
	getBuffDigits:
	mov dh, bytesBuff[bx]
	call convertDhToHex
	inc bx
	loop getBuffDigits
	mov cx, 8
	sub cx, bytesBuffIndex
	shl cx, 1
	printSpaces:
		fputc ' '
		loop printSpaces
	mov bytesBuffIndex, 0
	
	mov bx, instructionBuffIndex
	mov instructionBuff[bx], 0
	fputstr instructionBuff
	mov instructionBuffIndex, 0

	pop bx dx cx
	ret
endp

proc writeBufferToFile
	push ax bx cx dx
	mov ah, 40h
	mov bx, writeHandle
	mov cx, si
	mov dx, offset writeBuffer
	int 21h
	jnc notError5
	mov dx, offset fileWritingError
	jmp errorMsg
	notError5:
		pop dx cx bx ax
		ret
endp

proc getWriteFileHandle
	mov ah, 3Ch
	mov cx, 2
	mov dx, offset fileName
	int 21h
	jnc notError3
	mov dx, offset fileErrorWrite
	jmp errorMsg
	notError3:
		mov writeHandle, ax
	ret
endp

proc getReadFileHandle
	mov ax, 3d00h
	mov dx, offset fileName
	int 21h
	jnc notError2
	mov dx, offset fileErrorRead
	jmp errorMsg
	notError2:
		mov readHandle, ax
	ret
endp

proc skipSpaces
	skipSpaces_:
		mov al, es:[bx+si]
		cmp al, ' '
		jne skipSpaces_End
		inc bx
		loop skipSpaces_
	skipSpaces_End:
		ret
endp

proc fPutChar
	cmp canWrite, 1
	jne writeToInstructionsBuff
	mov writeBuffer[si], al
	inc si
	call checkIfFull
	ret
	writeToInstructionsBuff:
	push bx
	mov bx, instructionBuffIndex
	mov instructionBuff[bx], al
	inc instructionBuffIndex
	pop bx
	
	ret
endp

proc fPutString
	push ax
	getCharacters:
		mov al, [bx]
		cmp al, 0
		je getCharactersEnd	
		fputc al
		inc bx
		jmp getCharacters
	getCharactersEnd:
	pop ax
	ret
endp

proc checkIfFull
	cmp si, 512
	jne notFull
	call writeBufferToFile
	xor si, si
	notFull:
		ret
endp

errorMsg:
	mov ah, 9
	int 21h
exit:
	mov ah, 4ch
	int 21h
end