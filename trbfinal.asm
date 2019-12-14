; multi-segment executable file template.

data segment
    ;DATA
      cdrive db "\",0
      dir db "Files\",0
      masterfile db "mastertext.txt",0
      filealternator db "$$$$$$$$$$$$$$$$$$$$$$$$$",0
      startstr db 5,"START",0
      addfilestr db 8,"ADD FILE",0
      listfilestr db 10,"LIST FILES",0
      backstr db 4,"BACK",0
      exitstr db 4,"EXIT",0
      separator db ":","$",0
      delbuttonstr db 1,"X",0
      FileSymbols db 2,"F1",2,"F2",2,"F3",2,"F4",2,"F5",2,"F6",2,"F7",2,"F8",0
      errorfe db 19,"FILE ALREADY EXISTS",0
      errorfnv db 14,"FILE NOT VALID",0
      errornef db 17,"CANT HAVE 0 FILES",0
      handlers dw 8 dup(?),0
      nhandler db ?,0
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
      
      mov currentpage, 0
      call cdir
      call loadfiles
      call cdir
      call Menu
      
      ;mov currentpage, 1
      ;call changepage
      ;call Game
            
      mov ax, 4c00h
      int 21h
      
    ;END CODE
    
    ;PROCS
      
      Menu proc
        MenuInicial:
        mov currentpage, 0
        call changepage
        mov dh, 15
        mov cx, 10
        call clearLines
        cmp nhandler, 3
        jl nostart
        mov bl, 0000_1010b
        jmp inicialprint
        nostart:
        mov bl, 0000_0010b
        inicialprint:
          mov al, 1
          mov dh, 15
          mov bp, offset startstr
          call getpos          
          call writestrpagews
          
          mov bl, 0000_1010b
          
          mov dh, 16
          mov bp, offset addfilestr
          call getpos
          call writestrpagews
          
          mov dh, 17
          mov bp, offset listfilestr
          call getpos
          call writestrpagews
          
          mov dh, 18
          mov bp, offset exitstr
          call getpos
          call writestrpagews
       
         call mouseinit
         
          mov currentpage, 0
          call changepage
         
         MouseLoop:
          call click
          mov bx, 15
          call cmpbutton
          cmp dx, 1
          jz cStart
          mov bx, 16
          call cmpbutton
          cmp dx, 1
          jz cAddFile
          mov bx, 17
          call cmpbutton
          cmp dx, 1
          jz cListFile
          mov bx, 18
          call cmpbutton
          cmp dx, 1
          jz cExit
          jmp MouseLoop
         
         cExit:
          jmp EndMenu
         
         cListFile:
          call MenuListFiles
          jmp MenuInicial
         
         cAddFile:
          call MenuAddFile
          jmp MenuInicial
         
         cStart:
          cmp nhandler, 3
          ja continueStart
          jmp mouseloop
          continueStart:
            mov currentpage, 1
            call changepage
            call Game
            mov currentpage, 0
            call changepage
            jmp MouseLoop
            
        EndMenu:      
        ret
      Menu endp
      
      Game proc
        ret
      Game endp
      
      ;
      ; changepage - changes visible video page
      ;
      
      changepage proc
        mov al, currentpage
        mov ah, 05h
        int 10h
        ret
      changepage endp
      
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
      
      ;
      ; addfile - add a file to mastertext.txt and open it
      ; Inputs:
      ;   -Buffer: name of the file you want to write (has to terminate in 0)
      ;
      
      addfile proc 
        push cx
        call filesdir
        cmp buffer[2], 0
        jz errorfiledontexist
        mov dx, offset buffer
        add dx, 2
        mov al, 0
        call fopen
        jc errorfiledontexist
        call clonecheck
        cmp dx, 1
        jz endaddfile  
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
          mov dx, offset buffer
          add dx, 2
          mov bx, masterh
          pop cx
          call fwrite
          mov dx, offset filealternator
          mov cx, 25
          sub cx, ax
          call fwrite
          jmp endaddfile
        
        errorfiledontexist:
          mov al, 1
          mov bl, 0000_1100b 
          mov dh, 25
          mov dl, 0
          mov bp, offset errorfnv
          call writestrpagews  
         
        endaddfile:
        call cdir        
        ret
      addfile endp
      
      ;
      ; fclose - closes a file
      ;
      
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
      ;   cx: number of bytes writen
      ;       
      
      readtobuffer proc
        push ax
        push di
        push cx
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
		      mov dh, 0 
		      mov dl, buffer[1]
		      mov di, dx
        terminate:
          pop cx
          cmp bx, 0
          jz end0
          endd:
            mov buffer[cx+di+2], "$"
            jmp endreading
          end0:
            mov buffer[cx+di+2], 0   
        endreading:
        xor cx, cx
        mov cl, buffer[1]
        mov buffer[0], 07h
        mov buffer[1], 07h
        pop di
        pop ax
        ret
      readtobuffer endp
      
      ;
      ;
      ;   Inputs:
      ;     -Al - 1: Var selection; 0: Buffer without cursor selection; else: Buffer with cursor selection
      
      writestrpagews proc
        mov bh, currentpage
        push bp
        cmp al, 1
        jz nobuffer
        cmp al, 0
        jnz nocursorchange
        call selcursorpos
        nocursorchange:
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
          pop bp
        ret
      writestrpagews endp
      
      ;
      ;
      ;
      
      writestrpagens proc
        push ax
        cmp al,0
        jz noselpos
        call selcursorpos
        noselpos:
          mov dx, offset buffer
          add dx, si
          mov ah, 09h
          int 21h
        pop ax
        ret
      writestrpagens endp
      
      ;
      ;
      ;
      
      selcursorpos proc
        push ax
        push bx
        mov bh, currentpage
        mov ah, 02h
        int 10h
        pop bx
        pop ax
        ret
      selcursorpos endp
      
      ;
      ;
      ;
      
      printtxtnames proc 
        call dumpfilesbuffer
        mov al, 1
        looptxts:
          call selcursorpos
          push dx
          call writestrpagens
          add si, 25
          pop dx
          inc dh
          cmp buffer[si], 0
          jnz looptxts
        ret
      printtxtnames endp
      
      ;
      ;
      ;
      
      enter proc
        push ax
        push dx
        mov ah, 2
        mov dl, 0ah
        int 21h
        mov dl, 0dh
        int 21h
        pop dx
        pop ax
        ret
      enter endp
      
      click proc 
        mLoop:
        mov ax, 03h
        int 33h
        cmp bx, 00h     ;;;;MUDAR AQUIIII para qualquer mouse button
        jnz lClick
        jmp mLoop
        lClick:
        
        ;converte posicao em pixeis para nlinha
        mov ax, dx
        mov bl, 8       ;pixel number for character's height
        div bl
        mov dl, al
        mov ax, cx
        div bl
        mov ah, dl          ;AH - n de linhas AL - n de colunas
        ret 
      click endp
      
      clicknoloop proc 
        mov ax, 03h
        int 33h
        cmp bx, 00h     ;;;;MUDAR AQUIIII para qualquer mouse button
        jnz lClick2
        mov ax, 0
        jmp mEnd
        lClick2:
        
        ;converte posicao em pixeis para nlinha
        mov ax, dx
        mov bl, 8       ;pixel number for character's height
        div bl
        mov dl, al
        mov ax, cx
        div bl
        mov ah, dl
        mEnd:          ;AH - n de linhas AL - n de colunas
        ret 
      clicknoloop endp
      
      mouseinit proc
        push ax
        mov ax, 0
        int 33h
        pop ax
        ret
      mouseinit endp
      
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
      
      MenuAddFile proc
        mov dh, 15
        mov cx, 4
        call clearLines
        mov al, 1
        mov dh, 15
        mov bl, 0000_1010b
        mov bp, offset addfilestr
        call getpos
        call writestrpagews
        
        mov al, separator[0]
        mov buffer[0], al
        mov al, separator[1]
        mov buffer[1], al
        
        mov al, 0
        mov si, 0
        call writestrpagens
        mov al, 1
        mov dh, 16
        mov dl, 27
        call writestrpagens 
        mov dh, 18
        mov bp, offset backstr
        call getpos
        call writestrpagews
        mov dh, 16
        mov dl, 28
        call selcursorpos
        
        verificationloop:
          mov ah, 01h
          int 16h
          jnz inserttext
          call clicknoloop
          mov bx, 18
          call cmpbutton
          cmp dx, 1
          jz backbutton
          jmp verificationloop 
          
        ;Loop em que intercala verificação do buffer do teclado com o cursor
        inserttext:
        mov bx, 0
        mov cx, 0
        call readtobuffer
        call addfile
        mov ah, 00h
        int 16h
        backbutton:
        ret
      MenuAddFile endp

      ;
      ; clearLines - clears a certain number of lines
      ;   Inputs:
      ;     - Dh - first line
      ;     - Cx - number of lines to be deleted
      ;
      
      clearLines proc
        loopCTRLZ:
        mov dl, 00h
        mov bh, currentpage ;first page
        mov ah, 02h
        int 10h
    ;loop clearLine
        cmp cx, 0
        jz endCTRLZ
        push cx 
        mov cx, 80  ;numero de vezes que escreve no ecra-nao mexer  
        mov ah, 0Ah
        mov al, 00h ;caracter to display
        int 10h
        inc dh
        pop cx
        dec cx
        jmp loopCTRLZ 
        endCTRLZ: 
        ret
      clearLines endp
     
     MenuListFiles proc
      call cdir
      mov currentpage, 2
      call changepage
      Beginlist:
      mov dh, 1
      ;xor cx,cx
;      mov cl, nhandler
;      inc cx
      mov cx, 10
      call clearLines
      
      mov si, 0
      mov al, 1
      mov dx, 0
      mov bl, 0000_1010b
      mov bp, offset listfilestr
      call writestrpagews
      
      mov dh, 1
      mov dl, 28
      call printtxtnames
      
      mov al, 1
      mov dh, 1
      mov dl, 54
      mov bl, 0000_1100b
      mov bp, offset delbuttonstr
      loopdelbuttons:
        call writestrpagews
        cmp dh, nhandler
        jz loopdelbend
        inc dh
        jmp loopdelbuttons
      loopdelbend:
      
      mov al, 1
      mov dh, 1
      mov dl, 24
      mov bl, 0000_1010b
      mov bp, offset FileSymbols
      
      filenumbersloop:
        call writestrpagews
        cmp dh, nhandler
        jz numbersloopend
        inc dh
        add bp, 3
        jmp filenumbersloop
      numbersloopend:
      
      mov dh, 18
      mov bl, 0000_1010b
      mov bp, offset backstr
      call getpos          
      call writestrpagews 
      
      listfilesloop:
        call click
        cmp al, 26
        jz cmpdelbuttons
        mov bx, 18
        call cmpbutton
        cmp dx, 1
        jz endlistfiles
        cmpdelbuttons:
          cmp ah, 0
          jz listfilesloop
          cmp ah, nhandler
          ja listfilesloop
          call delfile
          jmp Beginlist 
      jmp listfilesloop 
      
        
        
      ;ACABAR
      
      mov ah, 1
      int 21h 
      endlistfiles:
      ret
     MenuListFiles endp
     
     dumpfilesbuffer proc
      push bx
      push cx
      push dx
      call cdir
      call filesdir
      mov bx, masterh
      mov ax, 0
      mov cx, 0
      mov dx, 0
      call fseek
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
     
    delfile proc    
      push bx
      push cx
      push di
      push si
      mov bl, nhandler
      dec bl
      cmp bl, 0
      jz error0files
      dec ah
      cmp ah, nhandler
      jae nofile
      xor al,al
      mov al, ah
      push ax
      mov bl, 2
      mul bl
      mov di, ax
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
      mov di, offset buffer ;File to delete
      add di, ax
      xor ax, ax
      mov al, nhandler
      dec al
      mov cl, 25
      mul cl
      mov si, offset buffer ;Final file
      add si, ax
      pop ax
      cmp al,nhandler
      je remlastfile
      remfile:
        mov cx, 25
        push si
        repne movsb
        dec di
        mov byte ptr di, "$"
        inc di
        pop si
        mov di,si
      remlastfile:
        mov cx, 25
        mov al, 0
        repne stosb
      jmp enddelfile
      enddelfile:
      mov bx, masterh
      mov ax, 0
      mov cx, 0
      call fseek
      mov dx, offset buffer
      mov al, nhandler
      mov bl, 25
      mul bl
      mov cx, ax
      mov bx, masterh
      call fwrite
      dec nhandler
      jmp nofile
      
      error0files:
        mov al, 1
        mov bl, 0000_1100b 
        mov dh, 24
        mov dl, 0
        mov bp, offset errornef
        call writestrpagews
      
      nofile:
      pop si
      pop di
      pop cx
      pop bx
      ret 
    delfile endp
    
    fseek proc 
      push dx
      mov ah, 42h
      int 21h
      pop dx
      ret         
    fseek endp
    
    clonecheck proc
      ;AX - handler
      xor cx,cx
      mov cl, nhandler
      mov di, offset handlers
      repne scasw
      jz copia
      jmp notcopia
      copia:
          mov al, 1
          mov bl, 0000_1100b 
          mov dh, 25
          mov dl, 0
          mov bp, offset errorfe
          call writestrpagews
        mov dx, 1
        jmp endclone
      notcopia:
        mov dx, 0
      endclone:
      ret 
    clonecheck endp
      
    ;END PROCS
    
        
ends

end start ; set entry point and stop the assembler.
