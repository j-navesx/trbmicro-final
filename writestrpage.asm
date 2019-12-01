; multi-segment executable file template.

data segment
    masterh dw ?,0
    mastertxt db "mastertext.txt",0
    buffer db 300 dup(?),0
    currentpage db 0,0
    var db ?,0    
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
     
     mov si, 0
     mov dx, 0319h
     call printtxtnames
     
;    mov al, 0
;    mov si, 28
;    mov dx, 0000h
;    call writestrpagens
;    mov al, 1
;    mov si, 0
;    mov dx, 0000h
;    call writestrpagens
;    mov si, 0
;    mov bl, 0000_1010b
;    mov dx, 0000h
;    call writestrpagews
;    add si, cx
;    add dx, cx
;    mov bl, 1010_0000b 
;    call writestrpagews
    
    mov ax, 4c00h ; exit to operating system.
    int 21h
    
    writestrpagews proc
      mov al, 1
      mov bh, currentpage
      mov bp, offset buffer
      add bp, si
      call sizebuffer
      mov ah, 13h
      int 10h
      inc cx
      ret
    writestrpagews endp
    
    sizebuffer proc
      push ax
      push di
      mov cx, 300
      mov di, offset buffer
      add di, si
      mov ax, "$"
      repne scasb
      mov ax, cx
      mov cx, 299
      sub cx, ax
      pop di
      pop ax
      ret
    sizebuffer endp
    
    writestrpagens proc
      cmp al,0
      jnz noselpos
      call selcursorpos
      noselpos:
        mov dx, offset buffer
        add dx, si
        mov ah, 09h
        int 21h
      ret
    writestrpagens endp  
    
    selcursorpos proc
      mov bh, currentpage
      mov ah, 02h
      int 10h
      ret
    selcursorpos endp
    
    printtxtnames proc 
      call selcursorpos
      mov al, 0
      mov dx, offset mastertxt
      call fopen
      mov masterh, ax
      mov bx, masterh
      mov cx, 199
      mov dx, offset buffer
      call fread
      looptxts:
        call writestrpagens
        call space
        add si, 25
        cmp buffer[si], 0
        jnz looptxts  
      ret
    printtxtnames endp
    
    
    
    space proc
      mov ah, 2
    	mov dl, 32
    	int 21h
      ret
    space endp
    
    ;
      ; fopen - opens a file
      ; inputs:
      ;   - Al: {0 - read; 1 - write; 2 - read/write;} 
      ;   - [Dx]: Filename
      ; outputs:
      ;   - Ax: File handler
      ; 
      
      fopen proc
        mov ah, 3Dh
        int 21h       
        ret
      fopen endp
      
      ;
      ; fread - reads from a file
      ; inputs:
      ;   -Bx: File handler
      ;   -Cx: Bytes to read
      ;   -[Dx]: Output
      ; outputs:
      ;   -Ax: Number of bytes read
      ;
       
      fread proc
        mov ah, 3Fh
        int 21h
        ret
      fread endp
        
ends

end start ; set entry point and stop the assembler.
