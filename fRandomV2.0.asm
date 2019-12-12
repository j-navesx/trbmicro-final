; multi-segment executable file template.

data segment
;.....temporary variables......;
    date dw 6 dup(?),244,0  
    filesize dw ?
    randomNum dw ?
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
    ;mov ah, 2Ch
    ;int 21h
    call GetDate
    call random        
;..............................;    
    
    mov ax, 4c00h ; exit to operating system.
    int 21h    
ends
    
    random proc
      ;.....for testes-temporary.....;
      mov filesize, 30
      ;mov sec, dh
      ;mov min, cl
      ;mov hour, ch
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
      
      
      ;mov secx2, ax
      
      
      mov ch, 00h      ;CX = 00h:(min)
                       ;as horas nao nos interessam
       
      ;checks if 'sec*2' is less or greater than 'min' valeu
      cmp ax, cx
      
      ja sub1 
                           
      sub cx, ax       ;
      ;mov cxtest, cx
      mov bx, cx
      ;mov A, bx
      jmp endsub1
       
      sub1:
      sub ax, cx       ;
      ;mov axtest, ax
      mov bx, ax
      ;mov A, bx
      
      
      endsub1:
      ;checks if filesize is less or greater than last result (BX) 
      cmp bx, filesize
      ja sub2         
      mov ax, 0000h    ;clear registo AX
      mov ax, filesize
      sub ax, bx         ;filesize > BX: AX = AX-BX (AX=filesize-sem o alterar)
      mov randomNum, ax
      jmp endrandom
      
      sub2: 
      sub bx, filesize ;BX > filesize: BX = BX-filesize 
      mov randomNum, bx
      
      endrandom:
       
      ret   
    endp

    GetDate proc
      push ax
      push cx
      push dx
      
      mov ah, 2Ah
      int 21h
      
      mov date[10], cx ;Ano
      
      xor cx, cx
      mov cl, dh 
      mov date[8], cx ;Mes
      mov cl, dl 
      mov date[6], cx ;Dia
      
      mov ah, 2Ch
      int 21h
      
      xor ax, ax
      mov al, ch
      mov date[4], ax ;Hora
      mov al, cl
      mov date[2], ax ;Minutos
      mov al, dh
      mov date[0], ax ;Segundos
      
      pop dx
      pop cx
      pop ax
      ret
    GetDate endp 
    
end start 
