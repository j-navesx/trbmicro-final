; multi-segment executable file template.

data segment
    File1 db "D:\emu8086\MyBuild\Hello World!",0
    Buffer db 80 dup(?),0
    File1Write db "Boas!!!!!!",0  
    filehandle dw ?,0
    number dw 13
    NBWord dw ?
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

    mov dx, offset File1                
    mov al, 0
    call fopen
    mov filehandle, ax
    
    mov dx, number
    mov bx, filehandle
    mov al, 0
    call fGetWord 
    
    mov NBWord, ax
     
    call fclose
    
    mov ax, 4c00h ; exit to operating system.
    int 21h
    
    ;*******************************************************
    ;Function: fopen()
    ;entry:
    ;
    ;AL = acess and sharing modes:
    ;  mov al, 0          ;  read 
    ;  mov al, 1          ;  write 
    ;  mov al, 2          ;  real/write 
    ;DS:DX -> ASCII filename.
    ;
    ;returns:
    ;CF clear if successful, AX = file handle.
    ;CF set on error,        AX = error code.
    ;
    ;Note1: File pointer is set to start of file.
    ;Note2: File must exist.
    ;*******************************************************
    
    fopen proc
      push dx
      mov ah, 3Dh
      int 21h
      pop dx 
      ret
    fopen endp
    
    ;*******************************************************
    ;Function: fclose()
    ;
    ;entry:
    ;BX = file handle
    ;
    ;returns:
    ;CF clear if successful, AX = destroyed.
    ;CF set on error,        AX = error code (06h).
    ;
    ;*******************************************************
    
    fclose proc
      mov ah, 3Eh
      int 21h 
      ret
    fclose endp
    
    ;*******************************************************
    ;Function: fread()
    ;
    ;entry:
    ;BX = file handle.
    ;CX = number of bytes to read.
    ;DS:DX -> buffer for data.
    ;
    ;returns:
    ;CF clear if successful, AX = number of bytes actually read;
    ;                             0 if at EOF (end of file) before call.
    ;CF set on error,        AX = error code.
    ;
    ;Note: data is read beginning at current file position,
    ;      and the file position is updated after a successful read 
    ;      the returned AX may be smaller than the request in CX if a partial read occurred.
    ;*******************************************************
    
    fread proc
      push dx
      mov ah, 3Fh
      int 21h
      pop dx
      ret
    fread endp
    
    ;*******************************************************
    ;Function: fseek()
    ;entry:
    ;
    ;AL = origin of move:
    ;  mov al, 0          ;  start of file. 
    ;  mov al, 1          ;  current file position. 
    ;  mov al, 2          ;  end of file.
    ;BX = file handle. 
    ;CX:DX = offset from origin of new file position.
    ;
    ;returns:
    ;CF clear if successful, DX:AX = new file position in bytes from start of file.
    ;CF set on error,        AX = error code.
    ;*******************************************************
    
    fseek proc
      push dx
      mov ah, 42h
      int 21h
      pop dx
      ret         
    fseek endp 
    
    ;*******************************************************
    ;Function: fGetWord()
    ;
    ;entry:
    ;BX = file handle
    ;AL = origin of move:
    ;  mov al, 0          ;  start of file. 
    ;  mov al, 1          ;  current file position. 
    ;  mov al, 2          ;  end of file.
    ;CX:DX = offset from origin of new file position.
    ;
    ;returns:
    ;CF clear if successful, AX = Number of characters in word.
    ;CF set on error,        AX = error code.
    ;
    ;*******************************************************
      
 ;-------------------------------------------------------------------------   
    
    fGetWord proc
      push cx
      push dx
      mov di, offset buffer
     loopGetWord:
      mov cx, 0 
      call readCharPos
      cmp byte ptr [di], 0
      je ErrorEOF 
     ;if != A-Z ou a-z 
     ;-----------------------
      push ax 
      call ifword
      cmp al, 0
      pop ax
      jne ifEqualsWord1   
     ;-----------------------
     ;then 
        inc dx 
        jmp loopGetWord
     ;else
     ifEqualsWord1:
       ;if anterior != A-Z ou a-z 
       ;---------------------------------
        push ax
        push dx
        dec dx
        mov cx, 0
        call readCharPos
        call ifword
        cmp al, 0
        pop dx
        pop ax
        jne ifEqualsWord2
       ;---------------------------------
       ;then 
         ;--> Read until 0dh or space
         ;--------------------------------
         mov ax, 0
         Loop1:
           mov cx, 0
           inc ax
           call readCharPos
           inc dx
           cmp byte ptr [di], 0dh
           je Fim_Loop1
           cmp byte ptr [di], ' '
           je Fim_Loop1
           inc di 
           jmp Loop1
         Fim_Loop1:
           mov byte ptr [di], 0
           dec ax
         ;--------------------------------
         jmp loopGetWord_end
       ;else
       ifEqualsWord2:
         ;--> jump until 0dh or space 
         ;--------------------------------
         Loop2:
           mov cx, 0
           call readCharPos
           inc dx
           cmp byte ptr [di], 0dh
           je Fim_Loop2
           cmp byte ptr [di], ' '
           je Fim_Loop2 
           jmp Loop2
         Fim_Loop2:
         ;--------------------------------
         ;--> jump until A-Z ou a-z 
         ;--------------------------------
         Loop3:
           mov cx, 0
           call readCharPos
           inc dx
           cmp byte ptr [di], 0
           je ErrorEOF
           call ifword
           cmp al, 1
           je Fim_Loop3 
           jmp Loop3
         Fim_Loop3:
           inc di
         ;--------------------------------
         ;--> Read until 0dh or space
         ;--------------------------------
         mov ax, 1
         Loop4:
           mov cx, 0 
           inc ax
           call readCharPos
           inc dx
           cmp byte ptr [di], 0dh
           je Fim_Loop4
           cmp byte ptr [di], ' '
           je Fim_Loop4
           inc di 
           jmp Loop4
         Fim_Loop4:
           mov byte ptr [di], 0
           dec ax
         ;-------------------------------- 
         jmp loopGetWord_end
         ErrorEOF:
           stc
           mov ax, -1
     loopGetWord_end:
      pop dx
      pop cx
      ret 
    fGetWord endp 
    
    ;-------------------------------------------------------------------------------
    
    readCharPos proc 
      push ax
      mov al, 0 
      call fseek
      mov cx, 1 
      push dx
      mov dx, di
      call fread 
      pop dx
      pop ax
      ret
    readCharPos endp
    
    ifword proc
      mov al, 0
      cmp byte ptr [di], 'A'
      jb notword 
      cmp byte ptr [di], 'Z'
      jbe wordset
      cmp byte ptr [di], 'a'
      jb notword
      cmp byte ptr [di], 'z'
      ja notword
     wordset: 
      mov al, 1
     notword:  
      ret
    ifword endp 
   
ends

end start ; set entry point and stop the assembler.
