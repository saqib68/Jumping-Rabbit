 [org 0x0100]

jmp start
message: db 'Score : ' ; string to be printed 
length: dw 8
over: db'GAMEOVER'
len: dw 8

load: db'LOADING'
lenload: dw 7

res: db '>Press SHIFT TO CONTINUE'
lenres: dw 24

res1: db '>Press Escape TO Exit'
lenres1: dw 21

nam: db ' NAME : Saqib Hassan'
lennam: dw 20

inst: db 'Intructions'
leninst: dw 11

inst1: db'> Press up key to jump'
leninst1: dw 24


gamename: db'Jumping Rabbit'
namel: dw 14
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;printing num;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
sound:	
IN AL, 61h  ;Save state
PUSH AX  

; Game Over Sound (Modified)

; Load the counter 2 value for a lower frequency
mov al, 0FFh
out 43h, al

mov ax, 0FCFh ; Adjust frequency for a lower tone
out 42h, al
mov al, ah
out 42h, al

; Turn the speaker on
in al, 61h
mov ah, al
or al, 3h
out 61h, al
call delay
mov al, ah
out 61h, al

call delay

; Load the counter 2 value for a higher frequency
mov ax, 0E2Ch
out 42h, al
mov al, ah
out 42h, al

; Turn the speaker on
in al, 61h
mov ah, al
or al, 3h
out 61h, al
call delay
mov al, ah
out 61h, al

call delay

; Load the counter 2 value for another frequency
mov ax, 0D59h
out 42h, al
mov al, ah
out 42h, al

; Turn the speaker on
in al, 61h
mov ah, al
or al, 3h
out 61h, al
call delay
mov al, ah
out 61h, al



   

POP AX;restore Speaker state
OUT 61h, AL
ret
printstr: push bp 
 mov bp, sp 
 push es 
 push ax 
 push cx 
 push si 
 push di 
 mov ax, 0xb800 
 mov es, ax ; point es to video base 
 mov di, [bp+8] ; point di to top left column 
 mov si, [bp+6] ; point si to string 
 mov cx, [bp+4] ; load length of string in cx 
 mov ah,[bp+10] ; normal attribute fixed in al 
nextchar1: mov al, [si] ; load next char of string 
 mov [es:di], ax ; show this char on screen 
 add di, 2 ; move to next screen location 
 add si, 1 ; move to next char in string 
 loop nextchar1 ; repeat the operation cx times 
 pop di 
 pop si 
 pop cx 
 pop ax 
 pop es 
 pop bp 
 ret 8
 
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;printing Text;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
 printnum: push bp 
 mov bp, sp 
 push es 
 push ax 
 push bx 
 push cx 
 push dx 
 push di 
 mov ax, 0xb800 
 mov es, ax ; point es to video base 
 mov ax, [bp+4] ; load number in ax 
 mov bx, 10 ; use base 10 for division 
 mov cx, 0 ; initialize count of digits 
nextdigit: mov dx, 0 ; zero upper half of dividend 
 div bx ; divide by 10 
 add dl, 0x30 ; convert digit into ascii value 
 push dx ; save ascii value on stack 
 inc cx ; increment count of values 
 cmp ax, 0 ; is the quotient zero 
 jnz nextdigit ; if no divide it again 
 mov di,[bp+6] 
 nextpos: pop dx ; remove a digit from the stack 
 mov dh, 0x4f ; use normal attribute 
 mov [es:di], dx ; print char on screen 
 add di, 2 ; move to next screen location 
 loop nextpos ; repeat for all digits on stack
 pop di 
 pop dx 
 pop cx 
 pop bx 
 pop ax 
 pop es 
 pop bp 
 ret 4
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;Rabbit Fall
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
 RabbitFall:
 pusha
 
 mov word [Br1],4
 
 
 
 mov cx,5
 lrd:
 add word[rabbit_start],160
 call Game
 call delay
  call delay
   call delay
    call delay
 
 loop lrd
 
 


 popa
 ret
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;Delay;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;
delay:
       push cx
       mov cx,0xffff
loop1: loop loop1
       mov cx,0xffff
loop2: loop loop2
       pop cx
       ret
clear:	
		push ax
		push di
		push es
		mov ax, 0xB800 
		mov es, ax 
		mov di, 0 
	nextchar: 
			mov word [es:di], 0x0720
			add di, 2
			cmp di, 4000 
			jne nextchar 
	popping:
			pop es
			pop di
			pop ax
		ret      


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


animation: 

       push bp
       mov bp,sp
       push di 
       push ax
       push es
       push cx
       push si
       push ds

       mov ax,0xb800
       mov es,ax  
       mov ds,ax


       cld								 ;auto increment
       mov di,0
       mov si,2
       mov bx,0
       mov cx,0

outerloop1:
       push cx                           ;counts the number of rows
       mov cx,158                     
       mov ax,[es:bx]                    ;start loc to move at the end

   l11: 
        movsw	
        sub cx,2
        add bx,2
        cmp cx,0
        jne  l11
        
        mov [es:bx],ax                       ;copying end loc with start
        add bx,2                             ;come downwards
      
        pop dx 
        inc dx                               ;increment the number of rows done
       mov di,0
       mov si,2
       mov cx,dx
 lx:                                         ;adds til the desired row comes
      add di,160
      add si,160 
      loop lx

      mov cx,dx
      cmp cx,8
      jne outerloop1




;CODE FOR SHIFTING SHIPS TO RIGHT

       std 
       mov di,1598			 ;coordinate of right end below buildings
       mov si,1596				 ;coordinate -2 of right end below buildings
       mov bx,1598				 ;saving the value of first coordinate so that it wont get overwritten
       mov cx,12					 ;the row number is 12th

outerloop2:							 ;THIS LOOP MOVES THE SHIPS TO THE RIGHT
       push cx						 ;for loop that shifts row
       push di						 ;it will run the loop
       push si						 ;for loop running
       mov cx,158					 ;counter for loop
       mov ax,[es:bx]				 ;saved the first coordinate of row to ax register so that it wont get overwritten
	 
   l22: 
        movsw
        sub cx,2 
        sub bx,2					 ;subtracting bx also because we have to place the above saved value in last location loop reaches
        cmp cx,0					 
        jne  l22					 
        							 
        mov [es:bx],ax				 ;placing saved value to last location that loop reached
       pop si						 ;getting back original value of si
       pop di						 ;getting back original value of di
       mov bx,di					 ;for saving first coordinate of respective index
       add di,160					 ;moving down to next row
       add si,160					 ;moving down to next row
       pop cx						 ;getting back value of cx
       inc cx						 ;increments value of cx as 1 row completes
      cmp cx,17					 ;for running the loop to 25th row times
      jne outerloop2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
exit:
       pop ds  
       pop si           
       pop cx
       pop es
       pop ax
       pop di
       pop bp

       ret 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                  ;PRINTS THE SKY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
sky:

pusha
push es
mov ax,0xB800
mov es,ax
mov di,0

s1:
	mov word[es:di],0x3000
	add di,2
	cmp di,1280
	jnz s1

pop es
popa
ret

				
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;SHIP PRINTING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ship:
   push bp
   mov bp,sp
   push ax
   push es
   push di
   push cx
   push bx
   mov di,[bp+6]
   mov cx,[bp+4]
   mov ax,0xb800
   mov es,ax
   push di
   sh1:
   mov word[es:di],0x0020
    add di,2
	cmp di,cx
	jnz sh1
	pop di
   sub cx,158
   sub di,162
   push di
      sh2:
   mov word[es:di],0x6620
    add di,2
	cmp di,cx
	jnz sh2
	pop di

	 sub cx,158
	sub di,162
	   push di
      sh3:
   mov word[es:di],0x6620
    add di,2
	cmp di,cx
	jnz sh3
	pop di

		sub cx,166
	sub di,154
	 push di
      sh4:
   mov word[es:di],0x7720
    add di,2
	cmp di,cx
	jnz sh4
	pop di


	pop bx
	pop cx
	pop di
	pop es
	pop ax
	pop bp

	ret 4




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				;PRINTS SHORE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
shore:
	push bp
	mov bp,sp
	push ax
	push es
	push di
	mov ax,0xB800
	mov es,ax
	mov di,1280
	printshore:
		mov word[es:di],0x222d			
		add di,2
		cmp di,1440
		jnz printshore

	pop di
	pop es
	pop ax
	pop bp
	ret 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			;PRINTS THE BUILDINGS AT TOP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


buildings:
	push bp
	mov bp,sp
	push ax
	push es
	push di
	push cx
	push dx
	mov ax, 0xB800 
	mov es, ax

  ;Roof;	
    mov di,[bp+14]				    
	mov cx,[bp+12]			    	
	
  

	L1:
		mov word[es:di],0x7720   
		add  di,2
		cmp di,cx
		jne L1
;RIGHT WALL;
	
		mov di,[bp+12]				           
	    mov cx,[bp+10]				           
						        
									        
	L2: 							        
		mov word[es:di],0x7720 ;|	        
		add di,160					        
		cmp di,cx					        
		jnz L2						        
			
	;;LEFT WALL;;			
    	mov di,[bp+14]				           
	    mov cx,[bp+8]				            



	L3:
		mov word[es:di],0x7720	;|
		add di,160
		cmp di,cx
		jnz L3
		
		
		
		;CENTER WALL;
		
		mov di,[bp+6]						
		mov cx,[bp+4]							
	

	L4:
		
			mov word[es:di],0x7720
			add di,160
			cmp di,cx
			jnz L4
     mov di,[bp+8]				                 ;L5 START = L3 END
	 mov cx,[bp+10]				                 ;L5 END = L2 END
	 add cx,2		
	 
	 ;BASE OF BUILDING

	 L5:
	 
			mov word[es:di],0xff20
			add di,2
			cmp di,cx
			jnz L5
					
	;filling out 

	;filling left;
			mov di,[bp+14]					;L1 START i.e roof
			add di,162						;next row+2 of roof
			mov cx,[bp+6]					;Ending point i.e. start of Center wall (L4)
			mov ax,di
			mov dx,cx
			sub word[bp+4],160				;Ending of center wall

			outer:
			mov di,ax ;strt
			mov cx,dx ;end
			L6:
			mov word[es:di],0x032d
			add di,2
			cmp di,cx
			jnz L6
			add ax,160
			add dx,160
			cmp cx,[bp+4]					;end of center wall
			jnz outer

			

			;filling right;
			xor ax,ax
			xor dx,dx
			mov ax,[bp+6]					;Ending point i.e. start of Center wall (L4)
			add ax,2
			mov dx,[bp+12]					;end of roof loc
			add dx,160
			sub word[bp+10],160				;end loc of left wall-160
			outer2:
				mov di,ax ;strt
				mov cx,dx ;end
			L7:	
				mov word[es:di],0x032D
				add di,2
				cmp di,cx
				jnz L7
		
			add ax,160
			add dx,160
			cmp cx,[bp+10]
			jnz outer2
	 pop dx		
	 pop cx
	 pop di
	 pop es
	 pop ax
	 pop bp
	 ret 12


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				;PRINTS THE SEA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
sea:
	
	push bp
	mov bp,sp
	push ax
	push es
	push di
	mov ax, 0xB800 
	mov es, ax 
	mov di,1440


print:
		
		mov word[es:di],0x1720
		add di,2
		cmp di,2240
		jne print

		pop di
		pop es
		pop ax
		pop bp
		ret 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Scenery

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

scene:
pusha


        
		
		call sea 
		call sky

		call shore		
	
		mov di,4
		push di 
		mov di,20
		push di
		mov di,1140
		push di 
		mov di,1124
		push di
		mov di,172
		push di
		mov di,1132
		push di
		call buildings
		mov di,36
		push di
		mov di,52
		push di
		mov di,1172
		push di
		mov di,1156
		push di
		mov di,204   
		push di
		mov di,1164
		push di
		call buildings
		mov di,68
		push di
		mov di,84
		push di
		mov di,1204
		push di
 		mov di,1188
		push di
		mov di,236
		push di
		mov di,1196
		push di
		call buildings
		mov di,100
		push di
		mov di,116
		push di
		mov di,1236
		push di
		mov di,1220
		push di
		mov di,268
		push di
		mov di,1228
		push di
		call buildings
		mov di,132
		push di
		mov di,148
		push di
		mov di,1268
		push di
		mov di,1252
		push di
		mov di,300
		push di
		mov di,1260
		push di
		call buildings
		mov di,2020
        push di
        mov di,2048
        push di

        call ship
		
		mov di,1940
        push di
        mov di,1960
        push di
		call ship
		
		popa
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Rabbit

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


rabbit:
push bp

mov bp,sp
push ax
push es
mov ax,0xB800
mov es,ax

push di
mov di,[bp+4]

mov word[es:di],0x7000
add di,2
mov word[es:di],0x7000
add di,2
mov word[es:di],0x705e
add di,2
mov word[es:di],0x7000
add di,2

mov word[es:di],0x7000
sub di,168
mov word[es:di],0x7000
add di,2
mov word[es:di],0x742e
add di,2
mov word[es:di],0x7000
add di,2
mov word[es:di],0x742e
add di,2
mov word[es:di],0x7000
sub di,162
mov word [es:di],0x702f
sub di,4
mov word [es:di],0x705c



pop di
pop es
pop ax
pop bp
ret 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;Bricks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Bricks:


push bp
mov bp,sp
push ax
push es
push di
push cx

mov ax,0xB800
mov es,ax

mov di,[bp+4]
mov cx,di
add cx,26
mov ax,[bp+6]

cmp ax,1
je blue
cmp ax,2
je orange
cmp ax ,3
je purple
cmp ax,4
je nobrick

cmp ax,0
je base

base:

call Base
jmp endi

blue:
lb:
mov word[es:di],0x102d
add di,2
cmp di,cx
jne lb
jmp endi


orange:
lb1:
mov word[es:di],0x602d
add di,2
cmp di,cx
jne lb1
jmp endi

purple:
lb2:
mov word[es:di],0x502d
add di,2
cmp di,cx
jne lb2
jmp endi

nobrick:
lbn:
mov word[es:di],0x3300
add di,2
cmp di,cx
jne lbn
jmp endi



endi:
pop cx
pop di
pop es
pop ax
pop bp
ret 4

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;;;;Section 3;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Section3:


	push ax
	push es
	push di
	mov ax, 0xB800 
	mov es, ax 
	mov ax,0x3302
	mov di,2240


print11:
		
		mov word[es:di],ax
		add di,2
		cmp di,4000
		jne print11


pop di
pop es
pop ax
ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Carraot;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

drawcarrot:
push bp
mov bp,sp
push ax
push es

mov ax,0xB800
mov es,ax
mov di,[bp+4]

mov word[es:di],0x4C7c

pop es
pop ax
pop bp
ret 2





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;Game
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


Game:


push bp
	mov bp,sp
	push ax
	push es
	push di
	push bx
	mov ax, 0xB800 
	mov es, ax 
	mov di,2400



call Section3

;;rabit


push word [rabbit_start]
call rabbit
;;;;carrot

cmp byte[boolCarrot],1
jne SK

mov di,3284
push di
call drawcarrot

SK:
;;puprle tile
push word [Br3]
push  word [Br3Str]
call Bricks

;;; brown tile

push word [Br2]
push  word [Br2Str]
call Bricks

;green tile


push word [Br1]
push  word [Br1Str]

call Bricks
call animation  ; bg animation
mov ax,2392
push ax
mov ax,2378
push ax
call PrintScore






		pop bx
		pop di
		pop es
		pop ax
		pop bp
		ret 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;rabbit Jump
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



				
				
				
	
		
rabbit_jump:		
                pusha
				push es
				push di
				push si
				
				mov word[cs:seconds],0
				mov bx,160
				mov cx,5
				lg:	
				sub word[rabbit_start],bx
						cmp cx,2
						je CheckCarrot
						
						todraw:
						
						 call Game ;rabbit jump animation
						
						 call delay
						 call delay
						
					
						
			
					
						

			
						sub cx,1
						jnz lg
					exiit:	
				pop si
				pop di
				pop es
				popa
				ret
				

				
				
				CheckCarrot:
			mov ax,0xb800
			mov es,ax
			mov di,[rabbit_start]
			add di,160
			
			mov si,5
			mov dl,0x7c
			l15:	cmp byte[es:di],dl
					je	break
					add di,2
					dec si
					jnz l15
			jmp todraw
			break:
			inc byte[score]
			dec byte[boolCarrot]
			jmp todraw			
				
				
				
				
				
				


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;play
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

play:


push di
push si
pusha
push es
push ds

lgame:

mov cl,[key]
cmp cl,0
je drawresume

cmp cl,1
jne skip

 
 call rabbit_jump
 call CheckPosition

 cmp byte [gameEnd],0
 je termin


  
 
 call scroll_down
 
mov byte[key],3
 push 2
 call Rand
 mov cl,byte[Random]
 mov byte[boolCarrot],cl
 

skip:


call BrickMovement

call Game
 cmp word[blueend],0
 je termin
call delay
call delay






jmp lgame
;;;;;;;;;;;;;;;;;;;;;;
;temp terminate;;;;;

;;;;;;;;;;;;;;;;;;;

drawresume:
call resume

mov byte[key],3
mov word[terminating],1


tempterminate:




mov cl,[key]
cmp cl,2
je rescene
cmp cl,0
je teri

jmp tempterminate

rescene:

call scene
mov word[terminating],0
jmp lgame

termin:



 call RabbitFall
 

teri:



call GAMEOVER
mov cx,7


mov cx,1
call delay
call delay
call delay
call delay
call delay
call delay
call delay
call delay
call delay
call delay
call delay
call delay
call delay
call delay
call delay
call delay

pop ds
pop es
popa
pop si
pop di

ret



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;TIMER;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Timer:



cmp word [Br1],1
jne skipt2

cmp word[terminating],0
jne skipt2

inc word[cs:tickcount]
cmp word[cs:tickcount],18
jne skipt1
mov word[cs:tickcount],0
inc word[seconds]
cmp word[seconds],3
jne skipt1
mov word[blueend],0




skipt2:
mov word[cs:seconds],0

skipt1:
push ds
		push bx
		push cs
		pop ds ; initialize ds to data segment
		mov bx, [currenttask] ; read index of current in bx
		shl bx,5 ; multiply by 32 for pcb start
		mov [pcb+bx+0], ax ; save ax in current pcb
		mov [pcb+bx+4], cx ; save cx in current pcb
		mov [pcb+bx+6], dx ; save dx in current pcb
		mov [pcb+bx+8], si ; save si in current pcb
		mov [pcb+bx+10], di ; save di in current pcb
		mov [pcb+bx+12], bp ; save bp in current pcb
		mov [pcb+bx+24], es ; save es in current pcb
		pop ax ; read original bx from stack
		mov [pcb+bx+2], ax ; save bx in current pcb
		pop ax ; read original ds from stack
		mov [pcb+bx+20], ax ; save ds in current pcb
		pop ax ; read original ip from stack
		mov [pcb+bx+16], ax ; save ip in current pcb
		pop ax ; read original cs from stack
		mov [pcb+bx+18], ax ; save cs in current pcb
		pop ax ; read original flags from stack
		mov [pcb+bx+26], ax ; save flags in current pcb
		mov [pcb+bx+22], ss ; save ss in current pcb
		mov [pcb+bx+14], sp ; save sp in current pcb
		mov bx, [pcb+bx+28] ; read next pcb of this pcb
		mov [currenttask], bx ; update current to new pcb
		mov cl, 5
		shl bx, cl ; multiply by 32 for pcb start
		mov cx, [pcb+bx+4] ; read cx of new process
		mov dx, [pcb+bx+6] ; read dx of new process
		mov si, [pcb+bx+8] ; read si of new process
		mov di, [pcb+bx+10] ; read di of new process
		mov bp, [pcb+bx+12] ; read bp of new process
		mov es, [pcb+bx+24] ; read es of new process
		mov ss, [pcb+bx+22] ; read ss of new process
		mov sp, [pcb+bx+14] ; read sp of new process
		push word [pcb+bx+26] ; push flags of new process
		push word [pcb+bx+18] ; push cs of new process
		push word [pcb+bx+16] ; push ip of new process
		push word [pcb+bx+20] ; push ds of new process
		mov al, 0x20
		out 0x20, al ; send EOI to PIC
		mov ax, [pcb+bx+0] ; read ax of new process
		mov bx, [pcb+bx+2] ; read bx of new process
		pop ds ; read ds of new process
		iret ; return to new process

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;kbisr;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				
				
kbisr:		push ax
			push es
			PUSH CX
			mov ax, 0xb800
			mov es, ax						; point es to video memory
			mov ch,0
			;************************************
			; TEST YOUR CONCEPTS
			; WHAT IS FOLLOWING LINE DOING

			in al, 0x60						; read a char from keyboard port, scancode

	
			cmp al,1					; is the key left shift
			jne nextcmp					; no, try next comparison
			mov cx,0
			mov [key],cl; Attribute Byte for L
			jmp nomatch						; leave interrupt routine

nextcmp:	cmp al, 0x48					; is the key right shift
			jne nextcmp1					; no, leave interrupt routine
			mov cx,1
			mov [key],cl
			
			
			nextcmp1:	cmp al, 0x36				; is the key right shift
			jne nomatch					; no, leave interrupt routine
			mov cx,2
			mov [key],cl

		

nomatch:
pop cx
			pop es
			pop ax
		

	mov al, 0x20
			out 0x20, al					; send EOI to PIC
			
		iret
			


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Scroll down;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

scroll_down:
pusha
push si
push di
push es
push ds

mov cx,5
mov di,0
lsd:
add word[Br1Str],160
add word[Br2Str],160
add word[Br3Str],160
add word[rabbit_start],160
 call Game  ; brick scroll down 
 call delay
 call delay
loop lsd
 ;swapping brick index
        mov cx,[Br2Str]
		mov [Br1Str],cx
		mov cx,[Br3Str]
		mov [Br2Str],cx
		
		lran:
		push 5
		call Rand
		mov ax, [Random]
		mov di,ax
	shr ax,1
		jnc go
		
		add di,1
		
		go:
	
		mov ax,  [postions+di]
		mov word [Br3Str],ax
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Swaping direction



		mov cl,[Dir_B2]
		mov [Dir_B1],cl
		mov cl,[Dir_B3]
		mov [Dir_B2],cl
			mov cx,[direction]
		mov [Dir_B3],cl

;;;;;;;;;;;swaping colors
	mov ax,[Br3]
		mov bx,[Br2]
		mov [Br2],ax
		mov [Br1],bx
		mov ax,[BrickColor]
		mov [Br3],ax




en:

push 3
call Rand
mov cl,byte[Random]
add cl,1


mov byte[BrickColor],cl



mov byte[direction],cl
		
		
		
		u2:
		
		mov byte [boolCarrot],1
	
pop ds
pop es
pop di
pop si
popa

ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Movement of BRicks;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BrickMovement:


		push si
		push di
		push es
		push ds
		pusha
		
		
		mov al,[Dir_B1]
		mov bx,[Br1Str]
		cmp al,1
		je bbb2
		cmp al,2
		je r
		mov ax, 0
		call MoveLeft
		sub word[rabbit_start],2
		cmp bx,3890 ;check to move return position
		jne bbb2
		mov bh,2
		mov byte[Dir_B1],bh
		jmp bbb2
		
		r: 
		mov ax,0
			call MoveRight
			add word[rabbit_start],2
			cmp bx,3930
			jne bbb2
			mov bh,3
			mov byte[Dir_B1],bh
			
		bbb2:
		mov al,[Dir_B2]
		mov bx,[Br2Str]
		cmp al,1
		je bbb3
		cmp al,2
		je r1
		mov ax,1
		call MoveLeft
		cmp bx,3090
		jne bbb3
		mov bh,2
		mov byte[Dir_B2],bh
		jmp bbb3
		
		r1: 
		mov ax,1
			call MoveRight
			cmp bx,3120
			jne bbb3
			mov bh,3
			mov byte[Dir_B2],bh
		
		bbb3:
		mov al,[Dir_B3]
		mov bx,[Br3Str]
		cmp al,1
		je end6
		cmp al,2
		je r2
		mov ax,2
		call MoveLeft
		cmp bx,2290
		jne end6
		mov bh,2
		mov byte[Dir_B3],bh
		jmp end6
		
		r2: mov ax, 2
			call MoveRight
			cmp bx,2320
			jne end6
			mov bh,3
			mov byte[Dir_B3],bh
		
		end6:	
		popa
		pop ds
		pop es
		pop di
		pop si
		
		ret		


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Check Rabbit position;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

CheckPosition:

pusha
push di
push si
push es
mov ax,0xb800
mov es,ax
mov cx,5
mov di,[rabbit_start]
add di,160

lch:
cmp word [es:di],0x3302
jne skiip
add di,2
loop lch

mov byte[gameEnd],0


skiip:

pop es
pop si
pop di
popa
ret









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Move Right;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
MoveRight:		
               push dx
			   push bx
			   push cx
               push bp
				mov bp,sp
				
				
				mov dx,ax
				cmp dx,0
				je bbrr1
				cmp dx,1
				je bbrr2
				add word[Br3Str],2
				jmp endiii
				
				bbrr1:	add word [Br1Str],2
					jmp endiii
					
				bbrr2:	
				add word[Br2Str],2
					jmp endiii
				
				endiii:
				
				
				pop bp
				pop cx
				pop bx
				pop dx
				ret 
				
MoveLeft:		
               push dx
			   push bx
			   push cx
               push bp
				mov bp,sp
				
				
				mov dx,ax
				
				cmp dx,0
				je bl1
				cmp dx,1
				je bl2
				sub word[Br3Str],2
				jmp endl				
				bl1:	sub word [Br1Str],2
					jmp endl
					
				bl2:	sub word[Br2Str],2
					jmp endl
				
				endl:
				
				
				pop bp
				pop cx
				pop bx
				pop dx
				ret 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;BASE;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Base:
pusha
push di
push es

mov ax,0xB800
mov es,ax
mov di,3840
l23:

mov word[es:di],0x202d
add di,2
cmp di,4000
jne l23




pop es
pop di
popa
ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;Print Score;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PrintScore:
push bp
mov bp,sp
pusha
push di
push es
mov ax,0x4f
push ax
mov bx,[bp+4]
push bx
mov ax,message
push ax
push word[length]
call printstr

mov ax,[bp+6]
push ax
push word[score]
call printnum





pop es
pop di
popa
pop bp
ret 4

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Random number;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Rand:		push bp
			mov bp,sp
			pushad
			
			rdtsc
			xor  dx, dx
			mov  cx, [bp+4]    
			div  cx       ; here dx contains the remainder of the division - from 0 to 9
			mov [Random],dl
			
			popad
			pop bp
			ret 2
;;;;;;;;;;;;;;;;;;;;
;GAMEOVER;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;

GAMEOVER:

	push ax
	push es
	push di
	mov ax, 0xB800 
	mov es, ax 
	mov ax,0x0002
	mov di,0


prin:
		
		mov word[es:di],ax
		add di,2
		cmp di,4000
		jne prin
mov ax,0x40
push ax
mov ax,2150
push ax
mov ax,over
push ax
push word[len]
call printstr
mov ax,2484
push ax

mov ax,2470
push ax
call PrintScore

pop di
pop es
pop ax
ret

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;RESUMESCREEN;;;;;;;;;;;;;;;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	resume:
	pusha
	push es
	
	mov ax,0xB800
	mov es,ax
	mov dx,12
	
	
	mov di,1160
	lresu:
	mov cx,40
	
	lres:
	
	mov word[es:di],0x0002
	add di,2
	loop lres
	add di,160
	sub di,80
	sub dx,1
	jnz lresu
	

	
mov ax,0x02
push ax
mov ax,1810
push ax
mov ax,res
push ax
push word[lenres]
call printstr

mov ax,0x04
push ax
mov ax,2290
push ax
mov ax,res1
push ax
push word[lenres1]
call printstr
	
	pop es
	popa
	
	ret
;\;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Instruction Screen;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
InstructionScreen:
pusha

push es

mov ax,0xB800
mov es,ax

call scene
call  Game



	mov dx,15
	
	
	mov di,990
	linst:
	mov cx,50
	
	linst1:
	
	mov word[es:di],0x7000
	add di,2
	loop linst1
	add di,160
	sub di,100
	sub dx,1
	jnz linst

mov ax,0x40
push ax
mov ax,1174
push ax
mov ax,inst
push ax
push word[leninst]
call printstr

mov ax,0x70
push ax
mov ax,1498
push ax
mov ax,inst1
push ax
push word[leninst1]
call printstr

mov ax,0x70
push ax
mov ax,1978
push ax
mov ax,nam
push ax
push word[lennam]
call printstr


mov ax,0x8c
push ax
mov ax,2614
push ax
mov ax,res
push ax
push word[lenres]
call printstr


lwait:
mov cl,[key]
cmp cl,2
je exit12


jmp lwait

exit12:





pop es
popa

ret

Music:

mov si, 0 ; current index for music_data
		
	.next_note:
	
		; 3) the first byte is the opl2 register
		;    that is selected through port 388h
		mov dx, 388h
		mov al, [si + music_data + 0]
		out dx, al
		
		; 4) the second byte is the data need to
		;    be sent through the port 389h
		mov dx, 389h
		mov al, [si + music_data + 1]
		out dx, al
		
		; 5) the last 2 bytes form a word
		;    and indicate the number of waits (delay)
		mov bx, [si + music_data + 2]
		
		; 6) then we can move to next 4 bytes
		add si, 4
		
		; 7) now let's implement the delay
		
	.repeat_delay:	
		mov cx, 600 ; <- change this value according to the speed
		              ;    of your computer / emulator
	.delay:
	
		; if keypress then exit
		mov ah, 1
		int 16h
		jnz isend
		
		loop .delay
		
		dec bx
		jg .repeat_delay
		
		; 8) let's send all content of music_data
		cmp si, [music_length]
		jb .next_note
		
		mov si,0
		jmp .next_note




loadingscreen:
pusha
push es
mov ax,0xb800
mov es,ax

mov cx,14
mov di,2166

lll:
mov ax,0x9a
push ax
mov ax,1510
push ax
mov ax,gamename
push ax
push word[namel]
call printstr

mov ax,0x07
push ax
mov ax,2150
push ax
mov ax,load
push ax
push word[lenload]
call printstr

mov word[es:di],0x7700
add di,2

call delay
call delay
call delay
call delay
call delay
call delay
loop lll


pop es
popa
ret


start:
     
;Multitasking
mov word[pcb+16],main
mov word[pcb+18],cs
mov word[pcb+26],0x0200
mov word[pcb+28],1
			
mov word[pcb+12],bp
mov word[pcb+14],sp
mov word[pcb+20],ds
mov word[pcb+22],ss
mov word[pcb+24],es
			
mov word[pcb+32+16],Music
mov word[pcb+32+18],cs
mov word[pcb+32+26],0x0200
mov word[pcb+32+28],0
			
mov word[pcb+32+12],bp
mov word[pcb+32+14],sp
mov word[pcb+32+20],ds
mov word[pcb+32+22],0x4000
mov word[pcb+32+24],es
mov word[currenttask],0

xor ax, ax
mov es, ax				
mov ax, [es:9*4]
mov [oldisr], ax								
mov ax, [es:9*4+2]
mov [oldisr+2], ax	
cli						

			


mov word [es:9*4], kbisr		; store offset at n*4....... csabc:kbisr	
mov [es:9*4+2], cs			
			
			

sti
 
mov ax, [es:8*4]
mov [oldisr1], ax								
mov ax, [es:8*4+2]
mov [oldisr1+2], ax		 
	 
cli						

			


mov word [es:8*4], Timer		; store offset at n*4....... csabc:kbisr	
mov [es:8*4+2], cs			
			
			

sti
	 
	main:
	 call clear
	 
	call loadingscreen
	 
	 call InstructionScreen
	 
	 
	 


	call scene
	 
	call Game
	call play
	
endgame:
	mov ax,0
	mov es,ax
	mov ax, [oldisr1]								; read old offset in ax
mov bx,[oldisr1+2]
cli												; disable interrupts
			mov [es:8*4], ax								; restore old offset from ax
			mov [es:8*4+2], bx								; restore old segment from bx
			sti	
	
mov ax, [oldisr]								; read old offset in ax
mov bx,[oldisr+2]
cli												; disable interrupts
			mov [es:9*4], ax								; restore old offset from ax
			mov [es:9*4+2], bx								; restore old segment from bx
			sti
	isend:	
	  call clear
		mov ax,0x4c00
		int 0x21
		
		
		
		
blueend:dw 1	
score: db 0
BaseStart:	dw 3840
Br1:	dw 0
Br2:	dw 2
Br3: dw 3

Br1Str:	dw 3910  
Br2Str:	dw 3110
Br3Str:	dw 2310

rabbit_start:	dw 3758
boolCarrot:	db 1
gameEnd:	db 1
BrickColor: dw 1
direction: dw 1
Dir_B1: db 1
Dir_B2: db 2
Dir_B3: db 3
key:	db 3
oldisr: dd 0
oldisr1: dd 0
tickcount: dw 0
seconds:dw 0
terminating: dw 0
Random: dw 0
postions: dw 2304,2312,2316,2324,2328
pcb : times 32*16 dw 0
currenttask: dw 0
music_length dw 8472
music_data incbin "roster.imf"