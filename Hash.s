		AREA    ALLDATA, DATA, READWRITE

TABLE_SIZE DCD 10
whatToSearchSuccess DCB "TITN"
whatToSearchFail 	DCB "TITI"
; [[[[Word index]]]]
; original string key = key to search for each data ex. "TOON"
; int data = data of each key ex. 0x45 is int data of original string key "TOON"
; hashed key = integer key from calculating original string key by hashFn, range [0,TABLE_SIZE)
dataArr	DCB "TOON" 	; hashed key = 0 
		DCD 0x45 
		DCB "DROp"	; hashed key = 1 
		DCD 0xEEEE 
		DCB "KONG"	; hashed key = 3 
		DCD 0x15 
		DCB "NEEN"	; hashed key = 4 
		DCD 0x40 
		DCB "king"	; hashed key = 5
		DCD 0xFFF	
		DCB "2!=4"	; hashed key = 6
		DCD 0x12345678 
		DCB "LUIS"	; hashed key = 7
		DCD 0x428
		DCB "boAT"	; hashed key = 8
		DCD 0x777	
		DCB "*UCK"	; hashed key = 9
		DCD 0x658874		
		DCB "TITN"	; hashed key = 9->0->1->2
		DCD 0xAAA 
		DCB "TITN"	; update data for hashed key = 9->0->1->2
		DCD 0xBBB 
		DCB "GAME"	; hashed key = 2 (but already full, ignore)
		DCD 0xBDC
		DCB 0 ;0 = EOF

hashTableData 	SPACE 44	; 4*TABLE_SIZE = 40 + Offset(4)
hashTableKey 	SPACE 44	; 4*TABLE_SIZE = 40 + Offset(4)

;=========================================================
		AREA    HASHING, CODE, READONLY
 
		ENTRY
		EXPORT main
 
main
	;--------------------------------------------------
	; HASH ALL DATA		
		BL hashALL
	;-----------------------------------------------------
	; Search for 'TITN' data (0xBBB), result saved in r1
		LDR r12,=whatToSearchSuccess 	; PASS string to search addr as parameter via r12 (pass by ref.) 
		MOV r5,#0						; reset attempt
		BL search
	; result 	r1 = 0xBBB,
	;			r0 = address of r1 data,
	;			r2 = 1 success flag
	;--------------------------------------------------
	; Search for 'TITI' data , fail
		LDR r12,=whatToSearchFail 	; PASS string to search addr as parameter via r12 (pass by ref.) 
		MOV r5,#0					; reset attempt
		BL search
	; result 	r0 = -1,
	;			r2 = 0 not success flag
	;--------------------------------------------------
		B END_OF_PROGRAM
;===========================================
hashALL
		;r0 = hashed key (-1: error)
		;r2 = first key char of each data (for EOF checking) 
		;r10 = hashTableKey address
		;r11 = hashTableData address
		;r12 = address of dataArr
		LDR r12,=dataArr
		LDR r11,=hashTableData
		LDR r10,=hashTableKey
		STR	lr, [sp, #0] 		; Save return address
		
loopHash
		LDRB r2,[r12]  			; LOAD first char (Byte) ; 0 = EOF
		CMP r2,#0				; if EOF, then do all ...EQ operations
		LDREQ	lr, [sp, #0] 	; Load return address
		MOVEQ 	pc,lr 			; then return
		
		MOV r5,#0				; reset attempt
		BL	hashFn 				; if not, then hash next data
		BL  keyCheck			; then check key if collide or no space left
		CMP r0,#-1				; if error, skip 
		BLNE  saveData			; if not, save to table
		
		B loopHash
;==============================================================		
hashFn
		;r0 = hashed key
		;r1 = int data
		;r2 = i runner
		;r3 = TABLE_SIZE
		;r4 = imm #4 (for MUL)
		;r5 = attempt counter	
		;r6 = temp register
		;r9 = address of dataArr (first address)
		;r10 = hashTableKey address
		;r11 = hashTableData address
		;r12 = address of dataArr (now pointing string key) (incremental r9)
		MOV r0,#0				; prepare r0 for key calculation
		MOV r2,#0				; prepare runner
		MOV r9,r12				; Save address of first character in string
loopFn
		LDRB r1,[r12],#1 	; LOAD each char of name (Byte) & Shift 1 Byte
		ADD r0,r0,r1		; r0 += r1 ascii value
		ADD r2,r2,#1		; i++
		CMP r2,#4			; check if r2 == 4 ?
		BNE loopFn			; if not, go to next char
		
		; r0 MOD TABLE_SIZE
		LDR r3,=TABLE_SIZE
		LDR r3,[r3]
		UDIV r6, r0, r3      ; r6 <- r0 / TABLE_SIZE 
		MLS  r0, r3, r6, r0  ; r0 <- r0 - (r3 * r6)
		
		; Now r0 = hashed key [0,TABLE_SIZE) 
		MOV r4,#4
		MUL r0,r0,r4 ; x4 (Byte to Word)
		MOV pc,lr
;==============================================================			
keyCheck
		;r0 = hashed key
		;r1 = existing original string key in hashTableKey[r0]
		;r2 = new original key to check
		;r3 = TABLE_SIZE
		;r4 = imm #4 (for MUL)
		;r5 = attempt counter	
		;r6-7 = temp register
		;r9 = address of dataArr (first address)
		;r10 = hashTableKey address
		;r11 = hashTableData address
		;r12 = address of dataArr (now pointing string key) (incremental r9)

		; Check if same key already exist (UPDATE CHECK)
		LDR r1,[r10,r0]		; LOAD whole exist original string key from hashTableKey[r0]
		LDR r2,[r9] 		; LOAD whole new original string key value (Word)
		CMP r1,r2			; if exist==new, then OK, return
		MOVEQ pc,lr
		
		; Check if exist string is empty (COLLISION CHECK)
		CMP r1,#0
		MOVEQ pc,lr		; if == 0, no collision, then OK, return
						; else, Collision,Linear probling (+1)
		ADD r0,r0,#4 	; (+4 ,Byte to Word)
		ADD r5,r5,#1 	; attempt++
		
		; Space available check
		LDR r7,=TABLE_SIZE
		LDR r6,[r7] 	
		CMP r5,r6		; if (used == TABLE_SIZE)
		MOVEQ r0,#-1	; set key = -1 (as error flag)
		ADDEQ r12,#4	; skip that int
		MOVEQ pc,lr 	; no space left, return with no change & error
		
		; Out of bound check (if r0 >= TABLE_SIZE , then back to 0)
		MOV r4,#4
		UDIV r0,r0,r4		; (r0/4 , Word to Byte)
		LDR r3,=TABLE_SIZE
		LDR r3,[r3]
		CMP r0,r3
		MOVGE r0,#0			; back to 0
		MUL r0,r0,r4		; (r0*4 , Byte to Word)
		B keyCheck
		
;==============================================================
saveData	
		;r0 = hashed key
		;r1 = int data
		;r2 = original string key
		;r9 = original string key address
		;r10 = hashTableKey address
		;r11 = hashTableData address
		;r12 = address of dataArr (now pointing int data)
		
		; Save key to hashTablekey[r0]
		LDR r2,[r9] 	
		STR r2,[r10,r0] 	; STORE string value -> hashTableKey[r0]
		
		; Save data to hashTableData[r0]	
		LDR r1,[r12],#4 	; LOAD int data (Word) & Shift 4 Byte
		STR r1,[r11,r0] 	; STORE int value -> hashTableData[r0]
		
		MOV pc,lr			; return
		
;==============================================================
search
		;r0 = return result (address)
		;r1 = return result (int data)
		;r2 = result flag (0:fail to search,1:success)
		;r11 = hashTableData address
		STR	lr, [sp, #0] 		; Save return address
		BL hashFn
		LDR r11,=hashTableData
		BL  keyCheck			; check key if match
		CMP r0,#-1				
		MOVEQ r2,#0			; if key not found, set r2 = 0 as not success flag
		MOVNE r2,#1				; else , r2 = 1 as success flag
		LDRNE r1,[r11,r0]		; if found, LOAD result from  hashTableData[r0]
		ADDNE r0,r11			; r0 + hashTableData address
		LDR	lr, [sp, #0] 		; LOAD return address
		MOV pc,lr				; return

END_OF_PROGRAM		
		END	;End of the program