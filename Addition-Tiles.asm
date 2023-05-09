;COAL PROJECT 01
;21l-7694 
[org 0x100]
jmp start
;______________________GLOSSARY FOR KEYBOARD________________
;Up: 0x48
;Left: 0x4B
;Right: 0x4D
;Down: 0x50
;Enter: 0x1C 
;________________________________________________________________
oldisr1: dd 0 		;oldisr for hooking

c equ 40 		; red color i think

;_______________________________________________________________________________________________________________________________________________________________________________________________
;____clrscr
clrscr:
		
    mov     ah, 0fh       
    int     10h   
    
    mov     ah, 0
    int     10h
   
    ret
;_____________end of clrscr
;_______________________________________________________________________________________________________________________________________________________________________________________________

;_____start of keyboard interrupt
; keyboard interrupt service routine
kbisr: push ax
push es
mov ax, 0xb800
mov es, ax			 ;point es to video memory
in al, 0x60 			; read a char from keyboard port


cmp al, 0x50 			; down arrow
jne nextcmp 			; no, try next comparison

mov byte [es:6], 'D'		 ; call down move
call nsd
jmp nomatch 			; leave interrupt routine
nextcmp: cmp al, 0x4B		 ; left arrow 


jne cmpr 		
mov byte [es:6], 'L' 		;call left move
call nsl
jmp nomatch

cmpr:
cmp al, 0x4D			;right arrow
jne cmpu

mov byte[es:6], 'R'		 ;call right move
call nonselr

jmp nomatch


cmpu:
cmp al, 0x48	;up arrow 
jne cmpe
mov byte[es:6], 'U' 		;call up move
call nsu
jmp nomatch

cmpe: cmp al, 0x1C		;enter key
jne nomatch
mov byte[es:6], 'E'


			

nomatch:
; mov al, 0x20
;out 0x20, al 			; send EOI to PIC
pop es
pop ax
jmp far [cs:oldisr1]
;iret


;_____________end of keyboard interrupt 1________________________________

;_______________________________________________________________________________________________________________________________________________________________________________________________
;_______________________________________________________________________________________________________________________________________________________________________________________________
;_______________________________________________________________________________________________________________________________________________________________________________________________
;_______________________________________________________________________________________________________________________________________________________________________________________________
;_______________________________________________________________________________________________________________________________________________________________________________________________



;right addition without selection ;__________________________________________


nonselr:
pusha
push di
push es

mov di, 1296			;point to the start of grid
mov bh, 04h			;store the red attribute we are using to distinguish pointer

f: 
mov ax, [es:di]			;mov current value at di to ax
cmp bh, ah			;compare the attribute of ax with the red attribute 
je ff				;ax has a red attribute ie it is the pointed value. now we leave this search loop
add di, 2			;if not red then move to the next location
cmp di, 2592			;check if end of grid surpasses
ja lve				;if grid has ended, exit since the pointer is nowhere 
jmp f				;still inside grid and red not founf
ff:


;border checks 
cmp di, 1310
je mover			;takes the pointer to the other side of the grid
cmp di, 1470
je mover
cmp di, 1630
je mover
cmp di, 1790
je mover
cmp di, 1950
je mover
cmp di, 2110
je mover
cmp di, 2270
je mover
cmp di, 2430
je mover
cmp di, 2590
je mover

mov ax, [es:di]			;put the current value in ax


add di, 2			;put the right (position) value in bx
mov bx, [es:di]


sub di, 2			; go back to pointing to ax's place
cmp al, bl			;compare the values to check if they are both the same digit

jne mover2			;if they are unequal, move to the routine that simply check for empty space and if we can move there 

cmp bl, 0x20			;check if bl is empty ie if it is a space. this check was added because otherwise, it would add even two consecutive empty spaces 
jne addd			;if bx is not empty, we go to add the two numbers 

cmp al, 0x20			;if ax is space then both bx and ax are spaces hence we will simply traverse
je mover2

;add
addd:				
mov cx, bx			
xor ch, ch			;clear attributes
mov dx, ax
xor dh, dh			;clear attribute
add cx, dx			;add cx=cx+dx where they have bx and ax's values
sub cl, 30h			;since it is in hex, we have to subtract 30 to reach the decimal number
mov ch, 0x04			;red attribute
mov ax, cx


mov bx, 0720h			
mov [es:di], bx			;empty the place
add di, 2	
mov [es:di], ax			;put the added value in right place
jmp lve 				;leave the routine

mover2: call sllide		;moving tile through empty spots



mover: call rightmov		;traversal
lve:
pop es
pop di
popa
ret

sllide: 
mov ax, [es:di]
add di, 2
cmp word[es:di], 0720h
jne emptynt			;it is not an empty spot so we cannot move there hence we will leave the routine
mov [es:di], ax
sub di, 2			;not empty so we move
mov word[es:di], 0720h
add di,2
jmp rett

emptynt:
sub di, 2			; point back to original place. this is a precaution 

rett:
ret

; non select right ends


;-----NS LEFT------------------======================
nsl:
pusha
push di
push es
mov di, 1296
mov bh, 04h

;the logic is the exact same as right one.
;only the calculation changes so refer to right (name) for explanation 

fl: 
mov ax, [es:di]
cmp bh, ah
je ffl
add di, 2
cmp di, 2592
ja lvel
jmp fl
ffl:


;border checks 
cmp di, 1296
je moverl
cmp di, 1456
je moverl
cmp di, 1616
je moverl
cmp di, 1776
je moverl
cmp di, 1936
je moverl
cmp di, 2096
je moverl
cmp di, 2256
je moverl
cmp di, 2416
je moverl
cmp di, 2576
je moverl

mov ax, [es:di]


sub di, 2
mov bx, [es:di]


add di, 2
cmp al, bl

jne mover2l

cmp bl, 0x20
jne subl

cmp al, 0x20
je mover2l

;sub
subl:
mov cx, bx
xor ch, ch
mov dx, ax
xor dh, dh
add cx, dx
sub cl, 30h
mov ch, 0x04
mov ax, cx


mov bx, 0720h
mov [es:di], bx
sub di, 2
mov [es:di], ax
jmp lvel 

mover2l: call sllidel



moverl: call leftmov
lvel:
pop es
pop di
popa
ret

sllidel: 
mov ax, [es:di]
sub di, 2
cmp word[es:di], 0720h
jne emptynl
mov [es:di], ax
add di, 2
mov word[es:di], 0720h
sub di,2
jmp retl

emptynl:
add di, 2

retl:
ret
;	END OF NSL ------------------------------------------------

;	 	NS UP ------------------------------------------------

nsu:
pusha
push di
push es
mov di, 1296
mov bh, 04h
;the logic is the exact same as right one.
;only the calculation changes so refer to right (name) for explanation 
fu: 
mov ax, [es:di]
cmp bh, ah
je ffu
add di, 2
cmp di, 2592
ja lveu
jmp fu
ffu:


;border checks 
cmp di, 1310
je moveru
cmp di, 1308
je moveru
cmp di, 1306
je moveru
cmp di, 1304
je moveru
cmp di, 1302
je moveru
cmp di, 1300
je moveru
cmp di, 1298
je moveru
cmp di, 1296
je moveru


mov ax, [es:di]


sub di, 160
mov bx, [es:di]


add di, 160
cmp al, bl

jne mover2u

cmp bl, 0x20
jne subu

cmp al, 0x20
je mover2u

;add
subu:
mov cx, bx
xor ch, ch
mov dx, ax
xor dh, dh
add cx, dx
sub cl, 30h
mov ch, 0x04
mov ax, cx


mov bx, 0720h
mov [es:di], bx
sub di, 160
mov [es:di], ax
jmp lveu

mover2u: call sllideu



moveru: call upmov
lveu:
pop es
pop di
popa
ret

sllideu: 
mov ax, [es:di]
sub di, 160
cmp word[es:di], 0720h
jne emptynu
mov [es:di], ax
add di, 160
mov word[es:di], 0720h
sub di,160
jmp retu

emptynu:
add di, 160

retu:
ret
;	END OF NS UP------------------------------------------------

;         NS DOWN--------------------------------------------------------
nsd:
pusha
push di
push es
mov di, 1296
mov bh, 04h

;the logic is the exact same as right one.
;only the calculation changes so refer to right (name) for explanation 

fd: 
mov ax, [es:di]
cmp bh, ah
je ffd
add di, 2
cmp di, 2592
ja lved
jmp fd
ffd:


;border checks 
cmp di, 2590
je moverd
cmp di, 2588
je moverd
cmp di, 2586
je moverd
cmp di, 2584
je moverd
cmp di, 2582
je moverd
cmp di, 2580
je moverd
cmp di, 2578
je moverd
cmp di, 2576
je moverd


mov ax, [es:di]


add di, 160
mov bx, [es:di]


sub di, 160
cmp al, bl

jne mover2d

cmp bl, 0x20
jne adddd

cmp al, 0x20
je mover2d

;add
adddd:
mov cx, bx
xor ch, ch
mov dx, ax
xor dh, dh
add cx, dx
sub cl, 30h
mov ch, 0x04
mov ax, cx


mov bx, 0720h
mov [es:di], bx
add di, 160
mov [es:di], ax
jmp lved 

mover2d: call sllided



moverd: call downmov
lved:
pop es
pop di
popa
ret

sllided: 
mov ax, [es:di]
add di, 160
cmp word[es:di], 0720h
jne emptynd
mov [es:di], ax
sub di, 160
mov word[es:di], 0720h
add di,160
jmp retd

emptynd:
sub di, 160

retd:
ret


;  	END OF NS DOWN
;____________________________________________________________________________
;__________________________________________________________________________
;____________________________________________________________________________
;____________________________________________________________________________
;##### right move  starts here
rightmov: 
push bx
push ax
push di

mov di, 1296
mov bh, 04h

find: 
mov ax, [es:di]
cmp bh, ah
je found
add di, 2
cmp di, 2590
ja exit
jmp find

found:
mov ah, 07
mov [es:di], ax
add di, 2

;the checks for borders

cmp di, 1312
je nextline
cmp di, 1472
je nextline 
cmp di, 1632
je nextline 
cmp di, 1792
je nextline 
cmp di, 1952
je nextline 
cmp di, 2112
je nextline
cmp di, 2272

je nextline
cmp di, 2432
je nextline
cmp di, 2592 
je zero

store:
mov ax, [es:di]
mov ah, 04
mov [es:di], ax



call dl1
;mov byte[es:328], 'H' ;test 
;call dl1

exit:
pop di
pop ax
pop bx

ret
nextline: add di, 144
jmp store
zero: mov di, 1296
jmp store
;########end of right move

;######start of left move############################
leftmov:
push bx
push ax
push di
mov di, 1296
mov bh, 04h

findl: 
mov ax, [es:di]
cmp bh, ah
je foundl
add di, 2
cmp di, 2590
ja exit
jmp findl

foundl:
mov ah, 07
mov [es:di], ax
sub di, 2
;the checks for borders


cmp di, 2574
je prevline
cmp di, 2414
je prevline 
cmp di, 2254
je prevline 
cmp di, 2094
je prevline 
cmp di, 1934
je prevline
cmp di, 1774

je prevline
cmp di, 1614
je prevline
cmp di, 1454
je prevline

cmp di, 1294
jbe last
storer:

mov ax, [es:di]
mov ah, 04
mov [es:di], ax



call dl1
mov byte[es:328], 'H' ;test 
call dl1

exitl:
pop di
pop ax
pop bx

ret
prevline: sub di, 144
jmp storer
last: mov di, 2590
jmp storer
;#########end of left move ########################

;####UP MOVE 

upmov:

push bx
push ax
push di
mov di, 1296
mov bh, 04h

findu: 
mov ax, [es:di]
cmp bh, ah
je foundu
add di, 2
cmp di, 2590
ja exitu
jmp findu

foundu:
mov ah, 07
mov [es:di], ax
sub di, 160
;the checks for borders


cmp di, 1150
je prevlineu
cmp di, 1148
je prevlineu 
cmp di, 1146
je prevlineu 
cmp di, 1144
je prevlineu 
cmp di, 1142
je prevlineu
cmp di, 1140

je prevlineu
cmp di, 1138
je prevlineu
cmp di, 1136
je prevlineu


storeu:

mov ax, [es:di]
mov ah, 04
mov [es:di], ax



call dl1
mov byte[es:328], 'H' ;test 
call dl1

exitu:
pop di
pop ax
pop bx

ret
prevlineu: add di, 1440
jmp storeu




;####	END OF UP ########################

;#### DOWN MOVE #####
downmov:
push bx
push ax
push di
mov di, 1296
mov bh, 04h

findd: 
mov ax, [es:di]
cmp bh, ah
je foundd
add di, 2
cmp di, 2590
ja exitd
jmp findd

foundd:
mov ah, 07
mov [es:di], ax
add di, 160
;the checks for borders


cmp di, 2750
je prevlined
cmp di, 2748
je prevlined 
cmp di, 2746
je prevlined 
cmp di, 2744
je prevlined 
cmp di, 2742
je prevlined
cmp di, 2740

je prevlined
cmp di, 2738
je prevlined
cmp di, 2736
je prevlined


stored:

mov ax, [es:di]
mov ah, 04
mov [es:di], ax



call dl1
mov byte[es:328], 'H' ;test 
call dl1

exitd:
pop di
pop ax
pop bx

ret
prevlined: sub di, 1440
jmp stored


;#### END OF DOWN MOVE #################

;_______GRID BUILDING draft 

BuildGrid:
push ax

mov ax, 0xb800
mov es, ax

mov ah, 07h
mov al, 32h
mov di, 1296
p1:

call prnt
add di, 144
cmp di, 2590
jb p1

pop ax
ret 


;_______
prnt: 
mov cx, 8

prnt2: cmp di, 1786
je skip

cmp di, 1624
je red
mov ah, 07h
rr:
mov [es:di], ax

add di, 2
back:
loop prnt2
ret
skip: add di, 2
jmp back
red: mov ah, 04h
jmp rr
;___________________delay
dl1:
push cx
mov cx, 0xffff
l111:
push bx
mov bx, 0
pop bx
loop l111
pop cx
ret
;___________________________


start:
call clrscr
call BuildGrid						;start with a predefined grid
call dl1							;delay just to keep track of moves


;keyboard hooking 
xor ax, ax
mov es, ax ; point es to IVT base
mov ax, [es:9*4]
mov [oldisr1], ax ; save offset of old routine
mov ax, [es:9*4+2]
mov [oldisr1+2], ax ; save segment of old routine
cli ; disable interrupts
mov word [es:9*4], kbisr ; store offset at n*4
mov [es:9*4+2], cs ; store segment at n*4+2
sti ; enable interrupts
l1: mov ah, 0 ; service 0 – get keystroke
int 0x16 ; call BIOS keyboard service
cmp al, 27 ; is the Esc key pressed
jne l1 ; if no, check for next key
mov ax, [oldisr1] ; read old offset in ax
mov bx, [oldisr1+2] ; read old segment in bx
cli ; disable interrupts
mov [es:9*4], ax ; restore old offset from ax
mov [es:9*4+2], bx ; restore old segment from bx
sti


mov ax, 0x0003					 ; go back to 80x25 text mode

;termination
mov ax, 0x4c00
int 0x21
