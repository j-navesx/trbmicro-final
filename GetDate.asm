; multi-segment executable file template.

data segment
    date dw 6 dup(?),244
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
                
    call GetDate 
    
    mov ax, 4c00h ; exit to operating system.
    int 21h 
    
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
ends 

end start ; set entry point and stop the assembler.
