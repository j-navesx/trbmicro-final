; multi-segment executable file template.

data segment
    ano dw ?,0
    mes db ?,0
    dia db ?,0
    hora db ?,0
    min db ?,0 
    sec db ?,0
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
                
     
    
    mov ax, 4c00h ; exit to operating system.
    int 21h    
ends 

    GetDate proc
      push ax
      push cx
      push dx
      
      mov ah, 2Ah
      int 21h
      
      mov ano, cx 
      mov mes, dh 
      mov dia, dl
      
      mov ah, 2Ch
      int 21h
      
      mov hora, ch
      mov min, cl
      mov sec, dh
      
      pop dx
      pop cx
      pop ax
      ret
    GetDate endp

end start ; set entry point and stop the assembler.
