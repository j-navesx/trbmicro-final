; multi-segment executable file template.

data segment
    ;DATA
      cdrive db "\",0
      dir db "Files\",0
      masterfile db "mastertext.txt",0
      handlers dw 8 dup(?),0
      nhandler dw ?,0
      masterh dw ?,0
      currentpage db ?,0
      buffer db 300 dup(?),0
    ;END DATA 
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
      
      mov cx, 0
      mov bx, 1
      call readtobuffer
      
      ;call cdir
      ;call loadfiles
      
      
      ;mov dx, handlers[0]
      ;mov bx, handlers[1]
      
      mov ax, 4c00h
      int 21h
      
    ;END CODE
    
    ;PROCS
      
      ;
      ; cdir - Navigate to the C:\ directory
      ;
      
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
        
        ;Verification to see if there is any file in the mastertext.txt
        mov bx, masterh
        mov cx, 1
        mov dx, offset buffer
        call fread
        mov di, offset buffer[0]
        call ifword
        or al,al
        jz endloadf  
        
        ;Moves to buffer all file names
        inc di
        mov cx, 199
        mov dx, di
        call fread
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
          push di
          mov di, nhandler
          mov handlers[di], ax
          pop di
          inc nhandler
          add di, 3
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
      
      ;
      ; addfile - add a file to mastertext.txt and open it
      ; Inputs:
      ;   -Buffer: name of the file you want to write (has to terminate in 0)
      ;
      
      addfile proc
        call filesdir
        mov dx, offset buffer
        mov al, 0
        call fopen
        jc filenotexist
          
          push di
          mov di, nhandler
          mov handlers[di], ax
          pop di
          inc nhandler
          
          mov bx, masterh
          call sizebuffer
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
      ; sizebuffer - gives you the size of the first word in the buffer
      ; Outputs:
      ;   -Cx: Size of first word + $ + 0dH + 0aH
      ;
      
      sizebuffer proc
        push ax
        push di
        mov cx, 26
        mov di, offset buffer
        mov ax, 0
        repne scasb
        mov ax, cx
        mov cx, 25
        sub cx, ax
        add cx, 3
        inc di
        mov byte ptr di, 0Ah
        dec di
        mov byte ptr di, 0Dh
        dec di
        mov byte ptr di, "$"
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
        mov cx,0
        mov ax, 3C00h
        int 21h
        mov masterh, ax
        pop cx
        ret  
      masterinit endp
      
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
      
      ;
      ; readtobuffer - reads from the screen to buffer
      ; Inputs:
      ;   Cx: 0-for free reading(buffer size) ; Other number-Cx number of characters
      ;   Bx: 0-terminates in 0 ; 1-terminates in $
      ; Outputs:
      ;   Buffer[DATA]: string
      ;       
      
      readtobuffer proc
        push ax
        push di
        or cx,cx
        jz freereading
        mov di, 0002h
        readingloop:
          or cx,cx
          jz terminate
          dec cx
          mov ah, 00h
          int 16h
          cmp ah, 4Bh
          jz inesquerda
          cmp ah, 4Dh
          jz indireita
          mov buffer[di], al
          inc di
          ;ADICIONAR COMPARES DE SETAS
          ;inesquerda: 4Bh
          ;indireita: 4Dh
          ;outesquerda: 1Bh
          ;outdireita: 1Ah
          jmp readingloop
          inesquerda:
            mov buffer[di], 1Bh
            inc di
            jmp readingloop
          indireita:
            mov buffer[di], 1Ah
            inc di
            jmp readingloop
        freereading:
          mov buffer[0], 0FFh
          mov buffer[1], 00h
          mov dx, offset buffer
		      mov ah, 0ah
		      int 21h
		      mov dx, 0
		      mov dl, buffer[1]
		      add di, dx
        terminate:
          mov cx, 4
          cmp bx, 0
          jz end0
          endd:
            mov buffer[cx+i+2], "$"
            jmp endreading
          end0:
            mov buffer[cx+di+2], 0   
        endreading:
        mov buffer[0], 07h
        mov buffer[1], 07h
        pop di
        pop ax
        ret
      readtobuffer endp
      
      writestrpage proc
        ret
      writestrpage endp
      
    ;END PROCS
    
        
ends

end start ; set entry point and stop the assembler.
