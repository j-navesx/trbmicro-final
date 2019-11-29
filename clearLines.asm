; multi-segment executable file template.


data segment
    MSG1 db "hello Rebeca, u good?", 0dh, 0dh, 0ah, "$"
    MSG2 db "what u up to, coding?", 0dh, 0dh, 0ah, "$" 
    MSG3 db "seems like it works... at least that what it's look like", 0dh, 0dh, 0ah, 0ah, "$"
    MSG4 db "good job", 0dh, 0dh, 0ah, "$"
    
    Line1 db ?
    
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
    
    mov si, offset MSG1 
    call printf
    
    mov si, offset MSG2
    call printf
   
;    mov si, offset MSG3
;    call printf
   
    mov si, offset MSG4
    call printf
      
    call clearLines  
  
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



;FUNTION gets mouse position and verifies button used if LEFT  
;input: BX: button pressed (01h-left button, 02h-right button, 03h-both buttons)
;       CX: column
;       DX: row



;FUNCTION RETURNS LINE NUMBER SELECTED 
;input: Xmouse
;       Ymouse  



;FUNCTION CLEARS NUMBER OF LINES (numLines) WANTED
;input: Line1: first line to clear
;       numLine: number of lines to clear

clearLines proc
;dh primeria linha a ser apagado, tem o y cursor
;cx depois tem numero de linhas que quero apagar (fora loop)
;cx numero de vezes que imprime o caracter (dentro loop)

;............................
    
    mov Line1, 2    ;number of rows: 0 a 25   
;set cursor position
    mov dh, Line1
    mov cx, 1  ;numero de linhas que quero apagar
    
    loopCTRLZ:
    mov dl, 00h
    mov bh, 00h ;first page
    mov ah, 02h
    int 10h
;loop clearLine
  
    cmp cx, 0
    jz endCTRLZ
    push cx 
    mov cx, 80  ;numero de vezes que escreve no ecra-nao mexer  
    mov bh, 00h 
    mov ah, 0Ah
    mov al, 00h ;caracter to display
    int 10h
    inc dh
    pop cx
    dec cx
    jmp loopCTRLZ 
    endCTRLZ: 
    ret
   
endp

end start
