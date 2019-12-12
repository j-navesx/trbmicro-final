  ; multi-segment executable file template.
  
  data segment
      ;DATA
        cdrive db "\",0
        dir db "Files\",0
        masterfile db "mastertext.txt",0
        filealternator db "$$$$$$$$$$$$$$$$$$$$$$$$$",0
        startstr db 5,"START",0 
        handlers dw 8 dup(?),0
        nhandler db ?,0
        masterh dw ?,0
        currentpage db ?,0
        buffer db 300 dup(?),0
        
        menuStr db 7,"1. Menu",0
        FileDisplay db 3,"2. ",0
        ZonaDeTexto db 3,"3. ",0
        number1 dw 2
        date dw 6 dup(?),244,0  
        filesize dw ?,244
        randomNum dw ? 
        FileSimbols db 0,"F1 F2 F3 F4 F5 F6 F7 F8 " 
        caveiraFile db "D:\emu8086\vdrive\C\Files\caveira.txt",0
        caveirah dw ?,0  
        wordSize dw ?,0
        sizeDateString db ?,0
        separacoes db ':',':',' ','-','-',0
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
        
        mov currentpage, 1
        call changepage
        call Game
              
        mov ax, 4c00h
        int 21h
        
      ;END CODE
      
      ;PROCS
        
        Menu proc
          Menuinicial:
            cmp nhandler, 3
            jl nostart
            mov bl, 0000_1010b
            jmp inicialprint
            nostart:
            mov bl, 0000_0010b
            inicialprint:
              mov al, 1
              
              mov dh, 15
              mov dl, 38
              mov bp, offset startstr
              call writestrpagews
                         
  ;            mov dh,
  ;            mov dl,
  ;            mov bp,
  ;            call writestrpagens
                
          ret
        Menu endp 
        
;--------------------------------------------------------------------------------------------------------------        

        Game proc 
          push di
          
          push ax
          push dx
          push cx
          push bx
          mov al, 1
          mov dx, offset caveiraFile
          call fopen
          
          mov caveirah, ax 
          
          mov al, 2
          mov bx, caveirah
          xor cx, cx
          xor dx, dx
          call fseek
          pop bx
          pop cx
          pop dx
          pop ax
          
          mov dh, 0 ;Y
          mov dl, 0 ;X
          mov bp, offset menuStr
          call writestrpagews
          
          mov dh, 1 ;Y
          mov dl, 0 ;X
          mov bp, offset FileDisplay
          call writestrpagews 
          
          push ax
          push bx
          mov al, nhandler
          mov bl, 3
          mul bl
          mov FileSimbols[0], al
          pop bx 
          pop ax
          
          mov dh, 1 ;Y
          mov dl, 3 ;X
          mov bp, offset FileSimbols
          call writestrpagews
          
          mov dh, 2 ;Y
          mov dl, 0 ;X
          mov bp, offset ZonaDeTexto
          call writestrpagews
          
         LoopRandomWord:  
          
          mov ah, 00h
          INT 16h
          xor ah, ah 
          sub ax, 49  
          mov di, ax
          shl di, 1
          
          call GetDate
          mov bx, handlers[di]
          call GetFileSize 
          call random
          
          mov dx, randomNum
          dec dx 
          mov bx, handlers[di]
          call fGetWord
          
          call sizebuffer 
          
          mov wordSize, cx
          
          inc cx
          
          push cx
          mov bh, currentpage
          mov ah, 03H
          int 10h
          pop cx 
           
          add dl, cl
          cmp dl, 80
          jl NotPageOverflow
          
          inc dh    ;Y
          cmp dh, 24
          jz EndLoopRandomWord
          
          mov al, 1 
          mov dl, 0 ;X
          call writestrpagens
          
          call space
          
          push di 
          mov di, wordSize 
          mov buffer[di], ' '
          inc di
          mov buffer[di], 0ah
          mov cx, di 
          inc cx
          mov bx, caveirah
          mov dx, offset buffer
          call fwrite
          pop di
           
          jmp LoopRandomWord 
         NotPageOverflow:
         
          mov al, 0
          call writestrpagens
          
          call space
          
          push di 
          mov di, wordSize 
          mov buffer[di], ' '
          mov cx, di 
          inc cx
          mov bx, caveirah
          mov dx, offset buffer
          call fwrite
          pop di
          
          jmp LoopRandomWord
          
         EndLoopRandomWord:
         
          call writeDate
         
          mov bx, caveirah
          call fclose 
          
          pop di
          ret
        Game endp

;--------------------------------------------------------------------------------------------------------------        
        
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
          mov cx, 200
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
        ;     -Al - 1: Var selection; 0: Buffer with cursor selection; else: Buffer without cursor selection
        
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
         
        ;--------------------------------------------------------------------------------------------------------------         
         
        ;
        ;
        ; 
        
        fseek proc 
          push dx
          mov ah, 42h
          int 21h
          pop dx
          ret         
        fseek endp
        
        ;
        ;
        ; 
        
        writestrpagens proc
          cmp al,0
          jz noselpos
          call selcursorpos
          noselpos:
            mov dx, offset buffer
            add dx, si
            mov ah, 09h
            int 21h
          ret
        writestrpagens endp
        
        ;
        ;
        ;
        
        selcursorpos proc
          mov bh, currentpage
          mov ah, 02h
          int 10h
          ret
        selcursorpos endp
        
        ;
        ;
        ;
        
        printtxtnames proc 
          call selcursorpos
          mov bx, masterh
          mov cx, 200
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
        
        ;
        ;
        ;
        
        space proc
          mov ah, 2
        	mov dl, 32
        	int 21h
          ret
        space endp
        
        ;
        ;
        ;
        
        fGetWord proc
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
          mov cl, sizeDateString
          call fwrite 
          ret
        writeDate endp
        
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
          mov sizeDateString, cl
          pop cx
          pop di
          ret
        moveToBeginnigOfString endp

;--------------------------------------------------------------------------------------------------------------        
        
      ;END PROCS
      
          
  ends
  
  end start ; set entry point and stop the assembler.
