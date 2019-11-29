; multi-segment executable file template.


data segment
    startMSG db "START$"
    newFileMSG db "INSERT NEW FILE$" 
    yourFilesMSG db "YOUR FILES$"
    exitMSG db "EXIT$"
    
;.....temporary variables.....;
    op1 db "option 1$"
    op2 db "option 2$"
    op3 db "option 3$"
    op4 db "option 4$"
;.............................;    
    
data ends    

stack segment
    dw   128  dup(0)
ends

code segment
start:    

; set segment registers:
    mov ax, data
    mov ds, ax
    mov es, ax
  
;..........NAVES' CODE........;
;
;LOOP THAT PRINTS ICON DIRECTLY FROM BUFFER    
    loopIcon:
    mov al, 1
    cmp al, 4
    jz endLoopIcon
    mov dx, offset buffer
    mov cx, 299
    mov bx, [handler do skull.txt]
    call fread
    mov buffer[300], $
    
    endLoopIcon:
;SET CURSOR POSITION
;INPUTS: DH: row
;        DL: column
;        BH: page number (0-7)    
    
    mov ah, 02h
    mov bh, 0h
       
    mov dh, 0Fh  
    mov dl, 25h 
    int 10h
    mov si, offset startMSG 
    call printf
    
    mov dh, 11h  
    mov dl, 20h 
    int 10h
    mov si, offset newFileMSG
    call printf
    
    mov dh, 13h  
    mov dl, 23h 
    int 10h
    mov si, offset yourFilesMSG
    call printf
    
    mov dh, 15h  
    mov dl, 26h 
    int 10h
    mov si, offset exitMSG
    call printf
      
    call mouseLoop  
  
    mov ah,4Ch  ;stop program
    int 21h
code ends

;FUNTIONS TO PRINT A STRING ON SCREEN
printf proc
    loop1:
    mov al,byte ptr [si]
    cmp al, "$"
    jz fimprtstr
    call co
    inc si
    jmp loop1
    fimprtstr: ret
endp 
      
co proc
    push ax
    push dx
    mov ah,02H
    mov dl,al
    int 21H
    pop dx
    pop ax
    ret
endp 


;FUNTION CHECKS THE OPTION PRESSED ON SCREEN
;VARIABLES: BX: button pressed (01h-left button, 02h-right button, 03h-both buttons)
;           CX: column
;           DX: row

mouseLoop proc 
    
;.....returns current mouse position & button pressed.....;
    mouselp:
    mov ax, 03h
    int 33h
    cmp bx, 01h
    jz clicked
    jmp mouselp
;.........................................................;
    
    clicked:
    cmp dx, 0078h
    jb NOTstart
    cmp dx, 007Eh
    ja NOTstart
    jmp startChecked
    
    NOTstart:
    cmp dx, 0088h
    jb NOTnewFile
    cmp dx, 008Eh
    ja NOTnewFile 
    jmp newFileChecked
    
    NOTnewFile:  
    cmp dx, 0098h
    jb NOTyourFiles
    cmp dx, 009Eh
    ja NOTyourFiles
    jmp yourFilesChecked
    
    NOTyourFiles:
    cmp dx, 00A8h
    jb mouselp  
    cmp dx, 00AEh
    ja mouselp
    jmp exitChecked
    
    startChecked:
    cmp cx, 0127h
    jb mouselp
    cmp cx, 014Eh
    ja mouselp
    call STARTbt  
    jmp endMouselp 
    
    newFileChecked:
    cmp cx, 0102h
    jb mouselp
    cmp cx, 0177h
    ja mouselp
    call NEWFILE
    jmp endMouselp
    
    yourFilesChecked:
    cmp cx, 0107h
    jb mouselp
    cmp cx, 015Dh
    ja mouselp
    call YOURFILES
    jmp endMouselp
    
    exitChecked:
    cmp cx, 0128h
    jb mouselp
    cmp cx, 0145h
    ja mouselp
    call EXIT
    jmp endMouselp
    
    endMouselp: 
    ret

endp


;FUNTIONS OF EACH OPTION BUTTON
;
;.........edit here..........;
;
;PRINTS NUM. OPTION (1,2,3 or 4)
; 
STARTbt proc
    mov ah, 02h   
    mov dh, 05h  
    mov dl, 18h 
    int 10h
    
    mov si, offset op1 
    call printf
    ret
endp

NEWFILE proc
    mov ah, 02h   
    mov dh, 09h  
    mov dl, 18h 
    int 10h
    
    mov si, offset op2
    call printf
    ret
endp

YOURFILES proc
    mov ah, 02h   
    mov dh, 09h  
    mov dl, 18h 
    int 10h
    
    mov si, offset op3
    call printf
    ret
endp

EXIT proc
    mov ah, 02h   
    mov dh, 09h  
    mov dl, 18h 
    int 10h
    
    mov si, offset op4
    call printf
    ret
endp
;..............................;


end start

