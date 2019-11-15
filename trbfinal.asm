; multi-segment executable file template.

data segment
    cdrive db "\",0
    dir db "Files\",0
    masterfile db "mastertext.txt",0
    handlers dw 8 dup(?),0
    nhandler dw ?,0
    masterh dw ?,0
    buffer db 300 dup(?),0
    msg1 db "Hi!",0 
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

    ;CODE
      
      call cdir
      call loadfiles
      
      mov dx, handlers[0]
      mov bx, handlers[1]
      
      mov ax, 4c00h
      int 21h
      
    ;END CODE
    
    ;PROCS
      
      cdir proc
        push ax
        push dx
        mov dx, offset cdrive
        mov ah, 3Bh
        int 21h
        pop dx
        pop ax
        ret
      cdir endp
      
      filesdir proc
        push ax
        push dx
        mov dx, offset dir
        mov ah, 3Bh
        int 21h
        pop dx
        pop ax
        ret
      filesdir endp
      
      loadfiles proc
        push ax
        push dx
        push di
        push si
        
        call filesdir
        call loadmaster
        
        mov bx, masterh
        mov cx, 1
        mov dx, offset buffer
        call fread
        mov di, offset buffer[0]
        call ifword
        or al,al
        jz endloadf  
        
        inc di
        mov cx, 199
        mov dx, di
        call fread
        mov di, offset buffer
        
        loopfiles:
          mov si, di
          mov ax, 0dh
          repne scasb
          dec di
          mov byte ptr di, 0
          mov dx, si
          mov al, 00h
          call fopen
          push di
          mov di, nhandler
          mov handlers[di], ax
          pop di
          inc nhandler
          add di, 2
          cmp byte ptr di, 0
          jnz loopfiles 
        endloadf:
        
        pop si
        pop di
        pop dx
        pop ax
        ret
      loadfiles endp
      
      fopen proc
        mov ah, 3Dh
        int 21h       
        ret
      fopen endp
       
      fread proc
        push ax
        mov ah, 3Fh
        int 21h
        pop ax
        ret
      fread endp
      
      loadmaster proc
        mov dx, offset masterfile
        mov ax, 3D02h
        int 21h
        jc errorfne:
        mov masterh, ax
        jmp endloadmaster
        errorfne:
          call masterinit
        endloadmaster:
        ret
      loadmaster endp
      
      masterinit proc
        push cx
        mov cx,0
        mov ax, 3C00h
        int 21h
        mov masterh, ax
        pop cx
        ret  
      masterinit endp
      
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
      
    ;END PROCS
    
        
ends

end start ; set entry point and stop the assembler.
