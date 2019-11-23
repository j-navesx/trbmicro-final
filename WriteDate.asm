; multi-segment executable file template.

data segment 
    date dw 46, 33, 18, 18, 11, 2019, 244 
    file db "D:\emu8086\MyBuild\caveriaTeste.txt",0  
    fileHandle dw ?,0 
    buffer db 80 dup(?)  
    sizeDateString db ?
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
    
    call writeDate
    
    mov ax, 4c00h ; exit to operating system.
    int 21h
    
    writeDate proc 
      
      mov di, offset date
      call dateToBuffer
           
      mov al, 2
      mov dx, offset file
      call fopen
      
      mov fileHandle, ax
      
      xor cx, cx
      mov al, 2 
      mov bx, fileHandle
      mov dx, 0
      call fseek
      
      mov bx, fileHandle
      mov cx, 0
      mov dx, offset buffer
      xor ch, ch
      mov cl, sizeDateString
      call fwrite 
      
      mov bx, fileHandle
      call fclose
      ret
    writeDate endp  
    
    fopen proc
      push dx
      mov ah, 3Dh
      int 21h
      pop dx 
      ret
    fopen endp
    
    fseek proc
      push dx
      mov ah, 42h
      int 21h
      pop dx
      ret         
    fseek endp
    
    fwrite proc
      push dx
      mov ah, 40h
      int 21h
      pop dx
      ret
    fwrite endp
    
    fclose proc
      mov ah, 3Eh
      int 21h 
      ret
    fclose endp
    
    dateToBuffer proc
      push ax 
      push dx 
      push bx 
      push si
      push di
      mov si, offset buffer 
      add si, 79
      mov bx, 10
     Loop1:
      mov ax, word ptr [di]
     Loop2: 
      div bl
      add ah, 48
      mov [si], ah
      or al, al
      jz End_Loop2
      mov ah, 0
      dec si
      jmp Loop2
     End_Loop2:
      add di, 2
      cmp word ptr [di], 244
      jz End_Loop1
      dec si
      mov [si], '-'
      dec si
      jmp Loop1 
     End_Loop1:
      call moveToBeginnigOfString
      pop di
      pop si
      pop bx
      pop dx
      pop ax
      ret
    dateToBuffer endp 
    
    moveToBeginnigOfString proc
      push di 
      push cx
      mov di, offset buffer
      mov [di], 0ah
      inc di
      mov cx, si
      sub cx, offset buffer
      sub cx, 80
      neg cx
      cld
      rep movsb
      mov [di], 0ah
      inc di
      mov cx, di
      sub cx, offset buffer
      mov sizeDateString, cl
      pop cx
      pop di
      ret
    moveToBeginnigOfString endp
ends

end start ; set entry point and stop the assembler.
