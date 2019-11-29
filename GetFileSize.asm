; multi-segment executable file template.

data segment
    filename db "D:\emu8086\MyBuild\caveriaTeste.txt",0 
    fileSize dd ?,255
    filehandle dw ?
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
    
    call getFileSize
    
    mov ax, 4c00h ; exit to operating system.
    int 21h
    
    fopen proc
      mov ah, 3Dh
      int 21h
      ret
    fopen endp
    
    fseek proc
      mov ah, 42h
      int 21h
      ret         
    fseek endp
    
    fclose proc
      mov ah, 3Eh
      int 21h 
      ret
    fclose endp
    
    GetFileSize proc
      push di
      mov al, 2
      mov dx, offset filename
      call fopen
      
      mov filehandle, ax
      
      mov al, 2
      mov bx, filehandle
      xor cx, cx
      xor dx, dx
      call fseek 
      
      mov di, offset filesize
      
      mov [di], ax 
      add di, 2
      mov [di], dx 
   
      call fclose 
      pop di
      ret
    GetFileSize endp    
ends

end start ; set entry point and stop the assembler.
