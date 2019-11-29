; multi-segment executable file template.

data segment
;.....temporary variables......;
    date dw 46, 33, 18, 18, 11, 2019, 244,0 
    sec db ?
    min db ?
    hour db ? 
    secx2 dw ?
    A dw ?
    filesize dw ?
    result dw ?
    axtest dw ?
    cxtest dw ?
;..............................;    
ends

stack segment
    dw   128  dup(0)
ends

code segment
start:
; set segment registers:
    mov ax, data
    mov ds, ax
    mov es, ax

;.......GET SISTEM TIME........;
;OUTPUT:    CH = hour
;           CL = minute
;           DH = second
;           DL = 1/100 second            
    mov ah, 2Ch
    int 21h
    call random        
;..............................;    
    
    mov ax, 4c00h ; exit to operating system.
    int 21h    
ends
    
random proc
;.....for testes-temporary.....;
mov filesize, 30
mov sec, dh
mov min, cl
mov hour, ch
;..............................;
push di
mov di, offset date 
mov dh, byte ptr [di]
add di, 2 
mov cl, byte ptr [di]
add di, 2 
mov ch, byte ptr [di]
pop di
;..............................;


mov dl, 02h
mov al, dh       ;AX = segundo*2
mul dl 


mov secx2, ax


mov ch, 00h      ;CX = 00h:(min)
                 ;as horas nao nos interessam
 
;checks if 'sec*2' is less or greater than 'min' valeu
cmp ax, cx

ja sub1 
                     
sub cx, ax       ;
mov cxtest, cx
mov bx, cx
mov A, bx
jmp endsub1
 
sub1:
sub ax, cx       ;
mov axtest, ax
mov bx, ax
mov A, bx


endsub1:
;checks if filesize is less or greater than last result (BX) 
cmp bx, filesize
ja sub2         
mov ax, 0000h    ;clear registo AX
mov ax, filesize
sub ax, bx         ;filesize > BX: AX = AX-BX (AX=filesize-sem o alterar)
mov result, ax
jmp endrandom

sub2: 
sub bx, filesize ;BX > filesize: BX = BX-filesize 
mov result, bx

endrandom: ret
   
endp
    
end start 
