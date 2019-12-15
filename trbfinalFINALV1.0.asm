; multi-segment executable file template.

data segment
    ;DATA
      cdrive db "\",0
      dir db "Files\",0
      masterfile db "mastertext.txt",0
      filealternator db "$$$$$$$$$$$$$$$$$$$$$$$$$",0
      separacoes db ':',':',' ','-','-',0
      startstr db 5,"START",0
      addfilestr db 8,"ADD FILE",0
      listfilestr db 10,"LIST FILES",0
      menugamestraddfile db 19,"1. ADDFILE     EXIT"
      menugamestrback db 7,"2. BACK"
      menuStr db 7,"1. Menu",0
      filedisplay db 3,"2. ",0
      zonadetexto db 3,"3. ",0
      backstr db 4,"BACK",0
      exitstr db 4,"EXIT",0
      separator db ":","$",0
      delbuttonstr db 1,"X",0
      caveiraFile db "caveira.txt",0
      filesymbols db 2,"F1",2,"F2",2,"F3",2,"F4",2,"F5",2,"F6",2,"F7",2,"F8",0
      errorfe db 19,"FILE ALREADY EXISTS",0
      errorfnv db 14,"FILE NOT VALID",0
      errornef db 17,"CANT HAVE 0 FILES",0
      date dw 6 dup(?),244,0
      mouseposgameY db ?
      mouseposgameX db ?
      gameini db ?
      wordsize dw ?,0
      sizedatestring db ?,0  
      filesize dw ?,244
      randomNum dw ?
      handlers dw 8 dup(?),0
      nhandler db ?,0
      masterh dw ?,0
      caveirah dw ?,0
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
      
            
      mov ax, 4c00h
      int 21h
      
    ;END CODE
    
    ;PROCS
      
      ;
      ; Menu - Dislpays the menu screen
      ;
      
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
          
          cmp nhandler, 0
          jz listnotactivated
          
          list:
          mov dh, 17
          mov bp, offset listfilestr
          call getpos
          call writestrpagews
          mov bl, 0000_1010b
          
          mov dh, 18
          mov bp, offset exitstr
          call getpos
          call writestrpagews
         
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
         
          listnotactivated:
          mov bl, 0000_0010b
          jmp list
         
         cExit:
          jmp EndMenu
         
         cListFile:
          cmp nhandler, 0
          jz mouseloop
          call MenuListFiles
          jmp MenuInicial
         
         cAddFile:
          call MenuAddFile
          jmp MenuInicial
         
         cStart:
          cmp nhandler, 3
          jae continueStart
          jmp mouseloop
          continueStart:
            mov currentpage, 1
            call changepage
            call Game
            mov currentpage, 0
            call changepage
            jmp MenuInicial
            
        EndMenu:      
        ret
      Menu endp
      
      ;
      ;Game - vai para o menu de jogo e executa todos os procs necessários para o jogo correr
      ;
      
      Game proc 
        push di 
        push ax
        push dx
        push cx
        push bx
        
        call cdir 
        call loadcaveira 
        
        mov al, 2                    ;
        mov bx, caveirah             ;
        xor cx, cx                   ;
        xor dx, dx                   ;
        call fseek                   ;
        
        pop bx
        pop cx
        pop dx
        pop ax
        
        jmp Game_Cont
        
       Game_inic:
        mov gameINI, 1
       Game_Cont: 
        
        ;-----------Escreve as coisas do menu do jogo------------
        mov bl, 0000_1010b 
        mov al, 1
        mov dh, 0 ;Y
        mov dl, 0 ;X
        mov bp, offset menuStr
        call writestrpagews
        
        mov dh, 1 ;Y
        mov dl, 0 ;X
        mov bp, offset filedisplay
        call writestrpagews  
        
        call EscritaSimbolos
        
        mov dh, 2 ;Y
        mov dl, 0 ;X
        mov bp, offset zonadetexto
        call writestrpagews 
        
        cmp GameINI, 1
        jnz LoopRandomWord
        mov dh, mouseposgamey
        mov dl, mouseposgamex 
        call selcursorpos
        ;---------------------------------------------------------          
       LoopRandomWord:  
        
        call mouseMenuOrFile ;(devolve 0 a 7 para di dependendo da localização do click)
                             ;(Ou vai para o menu)
        
        cmp di, -2
        jz Game_inic
        cmp di, -3
        jz EndLoopRandomWord
        mov bx, di
        inc bx
        cmp bl, nhandler
        ja LoopRandomWord
        cmp bl, 1
        jl LoopRandomWord 
        
        shl di, 1  ;converter para um numero par
        
        call GetDate
        mov bx, handlers[di]
        call GetFileSize 
        call random
        
        mov dx, randomNum
        dec dx 
        mov bx, handlers[di]
        call fGetWord
        
        mov si, 0
        call sizebuffer 
        
        mov wordSize, cx
        
        inc cx
        
        push cx
        mov bh, currentpage
        mov ah, 03H
        int 10h
        pop cx
        
        call EscritaSimbolos 
        call selcursorpos  
         
        add dl, cl
        cmp dl, 80
        jl NotPageOverflow
        
        inc dh    ;Y
        cmp dh, 24
        jz EndLoopRandomWord
        
        mov al, 1 
        mov dl, 0 ;X 
        mov si, 0
        call writestrpagens
        
        call space
        ;-----------Escreve a palavra com espaço e enter----------
        push di 
        mov di, wordsize 
        mov buffer[di], ' '
        inc di
        mov buffer[di], 0ah
        mov cx, di 
        inc cx
        mov bx, caveirah
        mov dx, offset buffer
        call fwrite
        pop di
        ;--------------------------------------------------------- 
        jmp LoopRandomWord 
       NotPageOverflow:
       
        mov al, 0
        mov si, 0
        call writestrpagens
        
        call space
        ;-----------Escreve a palavra com espaço------------------
        push di 
        mov di, wordsize 
        mov buffer[di], ' '
        mov cx, di 
        inc cx
        mov bx, caveirah
        mov dx, offset buffer
        call fwrite
        pop di
        ;---------------------------------------------------------
        jmp LoopRandomWord
        
       EndLoopRandomWord:
       
        call writeDate
       
        mov bx, caveirah
        call fclose 
        
        pop di
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
      ; addfile - add a file to mastertext.txt and the handlers vector and open it
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
          push ax
          mov bl, 2
          mov al, nhandler
          mul bl
          mov di, ax
          pop ax
          pop bx
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
          mov dh, 24
          mov dl, 0
          mov bp, offset errorfnv
          call writestrpagews
          pop cx  
         
        endaddfile:
        call cdir        
        ret
      addfile endp
      
      ;
      ; fclose - closes a file
      ;   Input:
      ;     -Bx - handler to close
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
        mov cx, 0
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
          mov ah, 07h
          int 21h
          mov buffer[di], al
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
      ; writestrpagews - Writes a string in the current page with the ability to change string atribute
      ;   Inputs:
      ;     -Al - 1: Var selection (with cursor selection); 
      ;           0: Buffer without cursor selection; 
      ;           else: Buffer with cursor selection
      
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
      ; writestrpagens - Writes string from buffer to the current page without the ability 
      ;                  to select atributte (More efficient)
      ;   Input:
      ;     -Al - 0: no cursor selection
      ;           else: cursor selection
      ;     -Si - offset to the buffer
      ;   Output:
      ;     Buffer [DATA]: string
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
      ; selcursorpos - Selects cursor position on screen
      ;   Input:
      ;     -Dh - Line
      ;     -Dl - Column
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
      ; printtxtnames - prints the names of the files in mastertext.txt
      ;   Output:
      ;     -[Screen]: names of the files 
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
      ; enter - prints a enter character (CR + NL)
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
      
      ;
      ; click - loop for mouse verification
      ;   Output:
      ;     -Ah - number of the line
      ;     -Al - number of the collumn
      ;
      
      click proc 
        mLoop:
        mov ax, 03h
        int 33h
        cmp bx, 00h
        jnz lClick
        jmp mLoop
        lClick:
        
        ;Converts the pixels in lines and collumns
        mov ax, dx
        mov bl, 8       ;pixel number for character's height
        div bl
        mov dl, al
        mov ax, cx
        div bl
        mov ah, dl
        ret 
      click endp
      
      ;
      ; clicknoloop - mouse verification without loop (to use in bigger loops)
      ;   Output:
      ;     -Ah - number of the line
      ;     -Al - number of the collumn  
      ;
      
      clicknoloop proc 
        mov ax, 03h
        int 33h
        cmp bx, 00h     
        jnz lClick2
        mov ax, 0
        jmp mEnd
        lClick2:
        
        ;Converts the pixels in lines and collumns
        mov ax, dx
        mov bl, 8       ;pixel number for character's height
        div bl
        mov dl, al
        mov ax, cx
        div bl
        mov ah, dl
        mEnd:
        ret 
      clicknoloop endp
      
      ;
      ; getpos - a function used to center a word in the screen and save the coordinates for the columns in the buffer
      ;          meant to be used with cmpbutton()
      ;   Input:
      ;     -Dh - number of the line
      ;     -[Bp] - variable to be written
      ;   Output:
      ;     -Dl - Column to write
      ;     -Buffer [DATA] - position [line(stored in Dh)] (first coordinate : start of button)
      ;     -Buffer [DATA] - position [line+25] (second coordinate : end of button) 
      ;
      
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
      
      ;
      ; cmpbutton - compares the positions of memory with the coordinates stored and evaluates if the button was clicked
      ;   Input:
      ;     -Ah - number of the line
      ;   Output:
      ;     -Dx - 0: Button wasnt clicked
      ;           1: Button was clicked
      ;
      
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
      
      ;
      ; MenuAddFile - "add file" menu option
      ;
      
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
        
                 
        ;Loop that checks cursor and buffer interchangeably
        
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
        mov bl, 0000_1010b  ;atributo
        mov dl, 00h
        mov bh, currentpage 
        mov ah, 02h
        int 10h
        cmp cx, 0
        jz endCTRLZ
        push cx 
        mov cx, 80  ;Number of characters in a line  
        mov ah, 09h ;imprime null com atributo
        mov al, 00h ;Character to display
        int 10h
        inc dh
        pop cx
        dec cx
        jmp loopCTRLZ 
        endCTRLZ: 
        ret
      clearLines endp
     
     ;
     ; MenuListFiles - "list files" menu option
     ;
     
     MenuListFiles proc
      call cdir
      mov currentpage, 2
      call changepage
      Beginlist:
      mov dh, 1
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
       
      endlistfiles:
      ret
     MenuListFiles endp
     
     ;
     ; dumpfilesbuffer - dumps all text in mastertext (filenames) in the buffer
     ;    Output:
     ;      -Buffer [DATA] - All text file names
     ;
     
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
    
    ;
    ; delfile - delete file function (from the mastertext.txt and the handlers vector)
    ;
     
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
    
    ;
    ; fseek - seeks a postion in a file
    ;   Input:
    ;     -Al - 0: starts from the beginning
    ;           1: starts from the curent file position
    ;           2: starts from the end of file
    ;     -Cx:Dx - offset from the origin
    ;
    
    fseek proc 
      push dx
      mov ah, 42h
      int 21h
      pop dx
      ret         
    fseek endp
    
    ;
    ; clonecheck - function to verify if the handler already exists
    ;   Input:
    ;     -Ax - handler
    ;   Output:
    ;     -Dx - 1:already exists
    ;           0:else
    ;
    
    clonecheck proc
      push bp
      mov bp, 0
      cmp nhandler, 0
      jz notcopia
      mov si, 0
      cloneloop:
        xor dx,dx
        mov dl, nhandler
        cmp bp, dx
        jz notcopia
        
        push cx
        push dx
        
        
        mov ax, 0
        mov dx, 0
        mov bx, masterh
        add dx, si
        call fseek
        
        pop dx
        pop cx
        
        mov dx, offset buffer[3]
        add dx, cx
        mov bx, masterh
        call fread
        
        push si
        mov si, offset buffer[3]
        add si, cx
        
        mov di, offset buffer
        add di, 2
        
        mov ax, cx
        repe cmpsb
        jz copia
        pop si
        mov cx, ax
        add si, 25
        inc bp
      jmp cloneloop
      
      copia:
          pop si
          mov al, 1
          mov bl, 0000_1100b 
          mov dh, 24
          mov dl, 0
          mov bp, offset errorfe
          call writestrpagews
        mov dx, 1
        jmp endclone
      notcopia:
        mov ax, 0
        mov bl, 25
        mov al, nhandler
        mul bl
        mov dx, ax
        mov ax, 0
        mov cx, 0
        mov bx, masterh
        call fseek
        mov dx, 0
      endclone:
      pop bp
      ret 
    clonecheck endp 
    
    ;
    ;
    ;
    
    space proc 
      push dx
      mov ah, 2
    	mov dl, 32
    	int 21h
    	pop dx
      ret
    space endp
    
    ;
    ;
    ;
    
    fGetWord proc 
      push di
      push cx
      push dx
      mov di, offset buffer
      mov cx, 0
     loopGetWord: 
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
        cmp dx, -1
        je inicioDeFicheiro
        call readCharPos 
        call ifword
        cmp al, 0 
        inicioDeFicheiro:
        pop dx
        pop ax
        jne ifEqualsWord2
       ;---------------------------------
       ;then 
         ;--> Read until 0dh or space
         ;--------------------------------
         mov ax, 0
         call ReadUntilNnWord
         ;--------------------------------
         jmp loopGetWord_end
       ;else
       ifEqualsWord2:
         ;--> jump until 0dh or space 
         ;--------------------------------
         call JumpUntilEndWord
         cmp byte ptr [di], 0
         je ErrorEOF
         ;--------------------------------
         ;--> jump until A-Z ou a-z 
         ;--------------------------------
         call JumpUntilWord
         cmp byte ptr [di], 0
         je ErrorEOF
         ;--------------------------------
         ;--> Read until 0dh or space
         ;--------------------------------
         mov ax, 1
         call ReadUntilNnWord
         ;-------------------------------- 
         jmp loopGetWord_end
         ErrorEOF:
           mov dx, 0
           mov di, offset buffer
           mov cx, 0
           jmp loopGetWord
     loopGetWord_end:
      pop dx
      pop cx
      pop di
      ret 
    fGetWord endp 
    
    ;
    ;
    ;
    
    readCharPos proc
      push cx
      push ax 
      mov al, 0 
      call fseek
      mov cx, 1 
      push dx
      mov dx, di
      call fread
      cmp ax, 0
      jne NotEOF
      mov byte ptr [di], 0
     NotEOF: 
      pop dx
      pop ax
      pop cx
      ret
    readCharPos endp
    
    ;
    ;
    ;
    
    ReadUntilNnWord proc
     Loop1:
      inc ax
      call readCharPos
      inc dx 
      cmp byte ptr [di], 64
      jl Fim_Loop1
      inc di 
      jmp Loop1
     Fim_Loop1:
      mov byte ptr [di], '$'
      dec ax
      ret
    ReadUntilNnWord endp
    
    ;
    ;
    ;
    
    JumpUntilWord proc
     Loop2:
      call readCharPos 
      inc dx
      cmp byte ptr [di], 0
      je Fim_Loop2
      call ifword
      cmp al, 1
      je Fim_Loop2 
      jmp Loop2
     Fim_Loop2:
      inc di      
      ret
    JumpUntilWord endp 
     
    ;
    ;
    ;
    
    JumpUntilEndWord proc 
     Loop3:
      call readCharPos 
      inc dx 
      cmp byte ptr [di], 64
      jl Fim_Loop3
      jmp Loop3
     Fim_Loop3:
      ret
    JumpUntilEndWord endp 
    
    ;
    ; random - generates a random number (from 0 to filesize) 
    ; Outputs:
    ;   randomNum[DATA]: random number
    ;
    
    random proc
      mov ah, 00h         
      int 1AH
      
      cmp filesize, 255
      ja WordRand
      
     ByteRand:
      mov bx, filesize   
      mov al, dl
     RandBack1:  
      cmp al, bl         
      jna NoSUBFileSize
      sub ax, filesize
      jmp RandBack1 
                  
     WordRand:            
      mov bx, filesize   
      mov ax, dx
     RandBack2:  
      cmp ax, bx         
      jna NoSUBFileSize
      sub ax, filesize
      jmp RandBack2
      
     NoSUBFileSize:
      mov randomNum, ax 
      ret   
    random endp
     
    ;
    ;
    ;
     
    GetDate proc
      push ax
      push cx
      push dx
      
      mov ah, 2Ah
      int 21h
      
      mov date[10], cx ;Ano
      
      xor cx, cx
      mov cl, dh 
      mov date[8], cx ;Mes
      mov cl, dl 
      mov date[6], cx ;Dia
      
      mov ah, 2Ch
      int 21h
      
      xor ax, ax
      mov al, ch
      mov date[4], ax ;Hora
      mov al, cl
      mov date[2], ax ;Minutos
      mov al, dh
      mov date[0], ax ;Segundos
      
      pop dx
      pop cx
      pop ax
      ret
    GetDate endp 
    
    ;
    ;
    ;
    
    GetFileSize proc 
      push di
      mov al, 2
      xor cx, cx
      xor dx, dx
      call fseek 
      
      mov di, offset filesize
      
      mov [di], ax 
      ;add di, 2
      ;mov [di], dx
      pop di
      ret 
    GetFileSize endp 
    
    ;
    ;
    ;
    
    writeDate proc 
      call GetDate
      mov di, offset date
      call dateToBuffer
      
      xor cx, cx
      mov al, 2 
      mov bx, caveirah
      mov dx, 0
      call fseek
      
      mov bx, caveirah
      mov cx, 0
      mov dx, offset buffer
      xor ch, ch
      mov cl, sizedatestring
      call fwrite 
      ret
    writeDate endp
    
    ;
    ;
    ;
    
    dateToBuffer proc
      push ax 
      push dx 
      push bx
      push cx 
      push si
      push di
      mov cx, offset separacoes
      mov si, offset buffer 
      add si, 79
      mov bx, 10
     dTB_Loop1:
      mov ax, word ptr [di]
     dTB_Loop2: 
      div bl
      add ah, 48 
      mov [si], ah 
      or al, al
      jz dTB_End_Loop2
      mov ah, 0
      dec si
      jmp dTB_Loop2
     dTB_End_Loop2:
      add di, 2
      cmp word ptr [di], 244
      jz dTB_End_Loop1
      dec si 
      push di
      push ax
      mov di, cx  
      mov al, byte ptr [di]
      mov [si], al 
      inc cx
      pop ax 
      pop di
      dec si
      jmp dTB_Loop1 
     dTB_End_Loop1:
      call moveToBeginnigOfString
      pop di
      pop si
      pop cx
      pop bx
      pop dx
      pop ax
      ret
    dateToBuffer endp 
    
    ;
    ;
    ;
    
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
      mov sizedatestring, cl
      pop cx
      pop di
      ret
    moveToBeginnigOfString endp 
    
    ;
    ;
    ;
    
    EscritaSimbolos proc
      push bx
      push cx
      push ax
      push dx 
      mov cx, 0
      mov ax, 0
      mov dx, 0
      mov bl, 0000_1010b
      mov dh, 1 ;Y

     LoopEscritaSimbolos: 
     
      push cx
      shl cx, 1
      cmp cx, di
      jnz notRecolor
      mov bl, 1010_0000b
     notRecolor:
      pop cx
     
      push cx
      push ax
      push bx 
      mov bp, offset filesymbols  ;mov pointer para inicio da string de simbolos
      
      mov ax, cx        ;al= numero do handler a escolher           
      mov bl, 3         ;bl= 3
      mul bl            ;al= 3*al 
      
      add bp, ax        ;posição inicial do pointer= 3*al posição inicial do simbolo do handler escolhido         
      pop bx 
      pop ax
       
      mov al, 1 
      add dl, 3 ;Atualizar X
      push di            
      call writestrpagews  ;escrita no ecrã  de um simbolo
      call space
      pop di
      pop cx 
      
      mov bl, 0000_1010b  ;por na cor original
      inc cl            ;incremento de contagem do loop
      cmp cl, nhandler  ;Verifica se já passamos por todos os handlers 
      jz Fim_LoopEscritaSimbolos  ;No caso de ser igual é porque já passamos por todos
      
      jmp LoopEscritaSimbolos  ;No caso de ser diferente é porque não passamos por isso continua o loop
      
     Fim_LoopEscritaSimbolos:
      pop dx
      pop ax
      pop cx
      pop bx
      ret  
    EscritaSimbolos endp 
    
    ;
    ;
    ;
    
    mouseMenuOrFile proc
      push cx
      mov bh, currentpage
      mov ah, 03H
      int 10h
      pop cx
      
      mov mouseposgamey, dh
      mov mouseposgamex, dl
      
      push bx
      push ax 
      push cx
      push dx
     WrongPlace:
      
      xor ax, ax
      call click
      cmp ah, 01h
      ja WrongPlace
      call FileOrMenuClick 
      cmp ax, -1
      jz WrongPlace
      
      cmp ax, -2
      jnz OtherAX
      mov di, ax
      jmp mouseMenuOrFile_end
     OtherAX:
      cmp ax, -3
      jnz mouseMenuOrFile_end
      mov di, ax
     mouseMenuOrFile_end:  
     
      pop dx
      pop cx 
      pop ax
      pop bx
      ret
    mouseMenuOrFile endp 
    
    ;
    ;
    ;
    
    MenuGame proc
      
      mov dh, 0
      mov cx, 2 
      call clearLines
      mov bl, 0000_1010b 
      mov al, 1
      mov dh, 0 ;Y
      mov dl, 0 ;X
      mov bp, offset menugamestraddfile
      call writestrpagews
      mov dh, 1 ;Y
      mov dl, 0 ;X
      mov bp, offset menugamestrback
      call writestrpagews
     WrongPlaceGM:
     
      xor ax, ax
      call click 
      cmp ah, 01h
      ja WrongPlaceGM
      call GameMenuClick
      cmp ax, -1
      jz WrongPlaceGM 

      ret
    MenuGame endp
    
    ;
    ;
    ;
    
    FileOrMenuClick proc 
      cmp ah, 00h
      jz clkline1
      
      xor cx, cx 
      mov dl, 3
     clkLine2:
      cmp al, dl
      jz clkFileGame
      inc dl
      cmp al, dl
      jz clkFileGame
      add dl, 2
      inc cx
      cmp cx, 7
      jna clkLine2
      jmp WrongCol
      
     clkline1:
      cmp al, 7
      ja WrongCol
      jmp clkMenuGame
      
     WrongCol:
      mov ax, -1 
     
     jmp FileOrMenuClick_end  
      
     clkMenuGame:
      call MenuGame
      jmp FileOrMenuClick_end
      
     clkFileGame: 
      mov di, cx
      
     FileOrMenuClick_end: 
      ret  
    FileOrMenuClick endp
    
    ;
    ;
    ;
    
    GameMenuClick proc 
      cmp ah, 00h
      jz clkline1GM
      
     clkLine2GM:
      cmp al, 3
      jl WrongColGM
      cmp al, 7
      jl clkBackGame
      jmp WrongColGM
      
     clkline1GM:
      cmp al, 3
      jl WrongColGM
      cmp al, 11
      jl clkADDFileGame
      cmp al, 16
      jl WrongColGM
      cmp al, 19
      ja WrongColGM
      jmp clkExitGame
      
     WrongColGM:
      mov ax, -1 
     
     jmp GameMenuClick_end  
      
     clkADDFileGame:
      call addFileGameMenu
      mov dh, 0
      mov cx, 2
      call clearLines
      mov ax, -2
      jmp GameMenuClick_end
      
     clkExitGame:
      mov ax, -3
      jmp GameMenuClick_end
      
     clkBackGame:
      mov dh, 0
      mov cx, 2 
      call clearLines
      mov ax, -2
      jmp GameMenuClick_end
      
     GameMenuClick_end: 
      ret  
    GameMenuClick endp
    
    ;
    ;
    ;
    
    addFileGameMenu proc 
      push bx

     ReadScreenFileGameMenu:
      
      mov dh, 0
      mov dl, 15 
      call selcursorpos 
      
      mov bl, 0000_1010b 
      mov bh, currentpage
      mov cx, 60  ;numero de vezes que escreve no ecra   
      mov ah, 09h
      mov al, 00h ;caracter to display
      int 10h 
      
      verificationloopGM:
          mov ah, 01h
          int 16h
          jnz inserttextGM
          call clicknoloop
          cmp ah, 01h
          jnz verificationloopGM
          cmp al, 3
          jl verificationloopGM
          cmp al, 7
          jl backbuttonGM
          jmp verificationloopGM
          
     inserttextGM: 
      
      mov cx, 0
    	mov bx, 0
    	call readtobuffer
    	cmp cx, 25
    	ja ReadScreenFileGameMenu    
      call addfile
    	
     backbuttonGM:
      
      pop bx
      ret
    addFileGameMenu endp
    
      ;
      ; loadcaveira - loads caveira.txt file or creates it
      ; output:
      ;   -caveirah(DATA): mastertext.txt file handler
      ;
      
      loadcaveira proc
        mov dx, offset caveiraFile
        mov al, 2
        call fopen
        jc errorfneLC:
        mov masterh, ax
        jmp endloadcaveira
        errorfneLC:
          ;caveira.txt file does not exist
          call caveirainit
        endloadcaveira:
        ret
      loadcaveira endp
      
      ;
      ; caveirainit - creates caveira.txt file
      ; output:
      ;   -caveirarh(DATA): caveiratext.txt file handler
      ;
      
      caveirainit proc
        push cx
        mov cx, 0
        mov ax, 3C00h
        int 21h
        mov caveirah, ax
        pop cx
        ret  
      caveirainit endp

  
    ;END PROCS
    
        
ends

end start ; set entry point and stop the assembler.
