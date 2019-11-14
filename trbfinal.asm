; multi-segment executable file template.

data segment
    cdrive db "\",0
    dir db "Files\",0
    masterfile db "mastertext.txt",0
    handlers dw 8 dup(?),0
    nhandler db ?,0
    buffer db 3000 dup(?),0 
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
      
      call loadcdrive
      call loadfiles
      
      mov ax, 4c00h
      int 21h
      
    ;END CODE
    
    ;PROCS
      
      loadcdrive proc
        mov dx, offset cdrive
        mov ah, 3Bh
        int 21h
        ret
      loadcdrive endp
      
      loadfiles proc
        mov dx, offset dir
        mov ah, 3Bh
        int 21h
        mov si, offset buffer
        mov ah, 47h
        int 21h
        mov dx, offset masterfile
        mov al, 0
        mov ah, 3Dh
        int 21h
        ret
      loadfiles endp
    
    ;END PROCS
    
        
ends

end start ; set entry point and stop the assembler.
