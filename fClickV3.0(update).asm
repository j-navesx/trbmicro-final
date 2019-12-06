; multi-segment executable file template.


data segment
    startMSG db "START$"
    newFileMSG db "INSERT NEW FILE$" 
    yourFilesMSG db "YOUR FILES$"
    exitMSG db "EXIT$"
    
    nLine db ?
    Xmouse dw ?
    Ymouse dw ?
    dif db ?
    
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

;por acabar...    
;    loopIcon:
;    mov al, 1
;    cmp al, 4
;    jz endLoopIcon
;    mov dx, offset buffer
;    mov cx, 299
;    mov bx, [handler do skull.txt]
;    call fread
;    mov buffer[300], $
    
;    endLoopIcon:
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
      
    call click  
  
    mov ah,4Ch  ;stop program
    int 21h
code ends

;FUNTIONS TO PRINT A STRING ON SCREEN

;a ser alterado para fprint do Naves...
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



;FUNTION gets mouse position and verifies pressed button of menu 
;input: BX: mouse button pressed (01h-left button, 02h-right button, 03h-both buttons)
;       CX: column
;       DX: row
;       nLine: line number pressed

click proc 
    mLoop:
    mov ax, 03h
    int 33h
    cmp bx, 00h     ;;;;MUDAR AQUIIII para qualquer mouse button
    jnz mClick    ;
    jmp mLoop
    mClick:
    mov Xmouse, cx
    mov Ymouse, dx
    
    ;converte posicao em pixeis para nlinha
    mov ax, Ymouse
    mov bl, 8       ;pixel number for character's height
    div bl          ;AL = nLine(AX)/8(BL)
    mov nLine, al
    call clickOption;AQUI COMECA AS COMPARACOES NLINHA/FUNCAO BOTAO
    ret 
endp 


clickOption proc
    cmp nLine, 0Fh
    jz clkstart
    
    notStart:
    cmp nLine, 11h
    jz newFile
    
    notNewFile:         
    cmp nLine, 13h
    jz yourFiles
    
    notYourFiles:
    cmp nLine, 15h
    jz exit
    
    ret
    
    clkstart:
    call STARTbt
    ret
    newFile:
    call NEWFILEbt
    ret
    yourFiles:
    call YOURFILESbt
    ret
    exit:
    call EXITbt
    ret
     
endp 
 
;FUNTIONS OF EACH OPTION BUTTON
;
;.........edit here..........;
;
;PRINTS NUM. OPTION (1,2,3 or 4)
; 
STARTbt proc
   
    mov si, offset op1 
    call printf
    ret
endp

NEWFILEbt proc
    
    mov si, offset op2
    call printf
    ret
endp

YOURFILESbt proc
    
    mov si, offset op3
    call printf
    ret
endp

EXITbt proc
 
    mov si, offset op4
    call printf
    ret
endp
;..............................;


end start
