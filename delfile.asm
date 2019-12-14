; multi-segment executable file template.

data segment
    cdrive db "\",0
    dir db "Files\",0
    masterfile db "mastertext.txt",0
    filealternator db "$$$$$$$$$$$$$$$$$$$$$$$$$",0
    startstr db 5,"START",0
    addfilestr db 8,"ADD FILE",0
    loadfilestr db 10,"LIST FILES",0
    backstr db 4,"BACK",0
    exitstr db 4,"EXIT",0
    separator db ":","$",0
    handlers dw 8 dup(?),0
    nhandler db ?,0
    masterh dw ?,0
    currentpage db ?,0
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
    
    call cdir 
    call loadfiles
    
    mov ah, 2
    call delfile
    
    mov ax, 4c00h ; exit to operating system.
    int 21h
    
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
    
    ;
    ; filesdir - Navigate to the C:\Files\ directory
    ; NOTE: Necessary to navigate to C:\ directory first
    ;
         
    filesdir proc
      push ax
      push dx
      dirchecker:
      mov dx, offset dir
      mov ah, 3Bh
      int 21h
      jc makedir
      jmp exitfilesdir
      makedir:
        mov dx, offset dir
        mov ah, 39h
        int 21h
        jmp dirchecker
      exitfilesdir:
      pop dx
      pop ax
      ret
    filesdir endp
    
    ;
    ; loadfiles - loads all files located in mastertext.txt (if exists)
    ;             if it doesn't exist create it  
    ;
    
    loadfiles proc
        push ax
        push dx
        push di
        push si
        
        call filesdir
        
        call loadmaster
        
        ;Moves to buffer all file names
        call dumpfilesbuffer
        ;Verification to see if there is any file in the mastertext.txt
        call filesdir
        mov di, offset buffer[0]
        call ifword
        or al,al
        jz endloadf
        mov di, offset buffer
        
        ;Loop to open the handlers for the files registred in mastertext
        loopfiles:
          mov si, di
          mov ax, "$"
          repne scasb
          dec di
          mov byte ptr di, 0
          
          mov dx, si
          mov al, 00h
          call fopen
          
          mov di, si
          add di, 25
          
          push di
          push ax
          push bx
          mov bl, 2
          mov al, nhandler
          mul bl
          pop bx
          mov di, ax
          pop ax
          mov handlers[di], ax
          pop di
          inc nhandler

          cmp byte ptr di, 0
          jnz loopfiles 
        endloadf:
        
        pop si
        pop di
        pop dx
        pop ax
        call cdir
        ret
      loadfiles endp
      
    
    addfile proc 
      call filesdir
      mov dx, offset buffer
      add dx, 2
      mov al, 0
      call fopen
      jc filenotexist
        
        push di
        push bx
        mov bl, 2
        mov al, nhandler
        mul bl
        pop bx
        mov di, ax
        mov handlers[di], ax
        pop di
        inc nhandler
        
        mov bx, masterh
        call fwrite
        mov dx, offset filealternator
        mov cx, 25
        sub cx, ax
        call fwrite
      
      jmp endaddfile
      filenotexist:
        mov bx, ax
        call fclose
        jmp endaddfile
      endaddfile:
      call cdir        
      ret
    addfile endp
    
    
    fclose proc
      mov ah, 3Eh
      int 21h
      ret
    fclose endp
    
    ;
    ; fwrite - writes to a file
    ; Inputs:
    ;   -Bx: File handler
    ;   -Cx: Number of bytes to write
    ;   -[Dx]: Buffer
    ;
    
    fwrite proc
      mov ah, 40h
      int 21h
      ret
    fwrite endp
    
    ;
    ; sizebuffer - gives you the size of a word in the buffer
    ;   Inputs:
    ;     -Si: Offset of the word
    ;   Outputs:
    ;     -Cx: Size of the word + $
    ;
    
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
    
    ;
    ; loadmaster - loads mastertext.txt file or creates it
    ; output:
    ;   -Masterh(DATA): mastertext.txt file handler
    ;
    
    loadmaster proc
      mov dx, offset masterfile
      mov al, 2
      call fopen
      jc errorfne:
      mov masterh, ax
      jmp endloadmaster
      errorfne:
        ;mastertext.txt file does not exist
        call masterinit
      endloadmaster:
      ret
    loadmaster endp
    
    ;
    ; masterinit - creates mastertext.txt file
    ; output:
    ;   -Masterh(DATA): mastertext.txt file handler
    ;
    
    masterinit proc
      push cx
      mov cx, 2
      mov ax, 3C00h
      int 21h
      mov masterh, ax
      pop cx
      ret  
    masterinit endp
    
    ;
    ;
    ;

    delfile proc    
      push bx
      push cx
      push di
      push si
      dec ah
      cmp ah, nhandler
      jae nofile
      push ax
      mov bl, 2
      mul bl
      mov cl, ah
      mov di, cx
      mov bx, handlers[di]
      call fclose
      xor ax,ax
      mov al, nhandler
      dec ax
      mov bl, 2
      mul bl
      mov si, ax
      mov ax, handlers[si]
      mov handlers[di], ax
      mov handlers[si], 0000h
      
      call dumpfilesbuffer 
       
      pop ax
      push ax
      mov cl, 25
      mul cl
      mov dx, ax
      mov di, offset buffer ;File to delete
      add di, dx
      xor ax, ax
      mov al, nhandler
      dec al
      mul cl
      mov dx, ax
      mov si, offset buffer ;Final file
      add si, dx
      pop ax
      cmp al,nhandler
      je remlastfile
      remfile:
        mov cx, 25
        push si
        repne movsb
        pop si
        mov di,si
      remlastfile:
        mov cx, 25
        mov al, 0
        repne stosb
      jmp enddelfile
      enddelfile:
      dec nhandler
      mov bx, masterh
      mov ax,0
      call fseek
      mov dx, offset buffer
      mov al, nhandler
      mov bl, 25
      mul bl
      mov cx, ax
      call fwrite
      nofile:
      pop si
      pop di
      pop cx
      pop bx
      ret 
    delfile endp
    ;
    ; ifword - 
    ;
    
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
    
    dumpfilesbuffer proc
      push bx
      push cx
      push dx
      call filesdir
      mov bx, masterh
      mov cx, 200
      mov dx, offset buffer
      call fread
      call cdir
      pop dx
      pop cx
      pop bx
      ret
    dumpfilesbuffer endp
    
    fseek proc 
      push dx
      mov ah, 42h
      int 21h
      pop dx
      ret         
    fseek endp
          
ends

end start ; set entry point and stop the assembler.
