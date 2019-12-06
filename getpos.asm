; multi-segment executable file template.

data segment
    startstr db 5,"START",0
    addfilestr db 8,"ADD FILE",0
    loadfilestr db 10,"LIST FILES",0
    exitstr db 4,"EXIT",0
    currentpage db 0,0
    buffer db 300 dup(?),0
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
    
    mov al, 1
    mov bl, 0000_1010b
    
    mov bp, offset startstr
    mov dh, 15
    call getpos
    call writestrpagews
    
    mov bp, offset addfilestr
    mov dh, 16
    call getpos
    call writestrpagews
    
    mov bp, offset loadfilestr
    mov dh, 17
    call getpos
    call writestrpagews
    
    mov bp, offset exitstr
    mov dh, 18
    call getpos
    call writestrpagews
    
    ;Função click
    mov bx, 17 ;Linha a verificar
    call cmpbutton
    cmp dx, 1  ;Success
    jz Tag
    
    mov ax, 4c00h ; exit to operating system.
    int 21h
    
    getpos proc
      push ax
      push bx
      push cx
      push di
      push si
      mov buffer[300], dh
      mov si, w.buffer[300]
      mov di, bp
      xor ax,ax
      mov cx, 40
      mov al, byte ptr di
      xor bx, bx
      mov bl, 2
      div bl
      add al, ah
      sub cl, al
      mov buffer[si], cl 
      mov dl, cl
      add si, 25
      add cl, byte ptr di
      mov buffer[si], cl
      pop si
      pop di
      pop cx
      pop bx
      pop ax 
      ret
    getpos endp
    
    cmpbutton proc
      push di
      mov di, bx
      cmp ah, bl
      jnz notin
      cmp al, buffer[di]
      jl notin
      cmp al, buffer[di+25]
      ja notin
      mov dx, 1
      jmp exitcmp
      notin:
        mov dx, 0
      exitcmp: 
      pop di 
      ret
    cmpbutton endp
    
    writestrpagews proc
      cmp al, 1
      jz nobuffer
      cmp al, 0
      jnz nocursorchange
      call selcursorpos
      nocursorchange:
        mov bh, currentpage
        mov bp, offset buffer
        add bp, si
        call sizebuffer
        jmp writews
      nobuffer:
        call selcursorpos
        ;LAYOUT DA VAR var db 5,"START",0
        xor cx, cx
        mov di, bp
        mov cl, byte ptr di
        inc bp
      writews:
        mov ah, 13h
        int 10h
        inc cx
      ret
    writestrpagews endp
    
    selcursorpos proc
      mov bh, currentpage
      mov ah, 02h
      int 10h
      ret
    selcursorpos endp
    
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
        
ends

end start ; set entry point and stop the assembler.
