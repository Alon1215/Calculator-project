
; ebx hold's string adress
%macro calc_update_limit 0
    pushad
    xor         eax, eax                        ; eax = counter for string iterating
    xor         ecx, ecx                        ; ecx = temporary holds char & char's value
    xor         edx, edx                        ; accumulator for st_limit
   
    mov     cl, byte[ebx + eax]                 ; cl holds current char
    cmp     cl, 10                              ; check if it is the end of string
    jle     %%skip                              
    
    char_to_value cl                            ; parse char's value
    mov     dl, cl                              ; edx = left most digit
    inc     eax                                 ; update eax to check next char

    mov     cl, byte[ebx + eax]                 ; cl holds current char
    cmp     cl, 10                              ; check if it is the end of string
    jle     %%skip
    char_to_value cl                            ; parse char's value
    shl     dl, 4                               ; multiply dl by 16                   
    add     dl, cl                              ; add current digit
    
    %%skip:
        mov dword[st_limit], edx                ; update st_limit
        popad
%endmacro


; 1st = number's adress, 2nd = 0/1 flag, is number recieved from user, or push to stack
; 0 = read from user, 1 = result pushed to stack
%macro check_handle_debug 2
    pushad
    cmp     dword[debug_flag] ,0                    ; check if its the last digit   
    je      %%skip                                  ; not activated
    mov     ebx, %2
    cmp     ebx, 1                                  ; check type of case
    je      %%debug_pushed                          ; handle pushed on stack
    
    push    debug_read_msg                          ; push string agrument on stack
    push    format_string                           ; format_string
    push    dword[stderr]                           ; STDERR
    call    fprintf                                 ; fprintf(...) 
    add     esp, 12                                 ; remove args from stack
    calc_fprintf input_buffer, format_string, dword[stderr]   ; print number to stderr
    jmp     %%skip                                  ; finish stderr print
    
    
    %%debug_pushed:
        push    debug_result_msg                    ; push string agrument on stack
        push    format_string                       ; format_string
        push    dword[stderr]                       ; STDERR
        call    fprintf                             ; fprintf(...) 
        add     esp, 12                             ; remove args from stack
        print_number %1, dword[stderr]              ; print number to stderr

    %%skip:
        popad
%endmacro


; counts chars in input_buffer (until null char)
; return counting value to '1st argument' i.e: selected register
%macro char_counter_proc 1
    pushad
    xor ecx, ecx
    xor ebx,ebx
    xor edi,edi
    mov edx, input_buffer

    %%skip_leading_zeros:
        mov bl, byte[edx]                       ; bl = next char
        cmp bl,'0'
        jne %%increase_counter_loop
        inc edx
        inc ecx
        jmp %%skip_leading_zeros

    %%increase_counter_loop:
        
        mov bl, byte[edx]                       ; bl = next char
        cmp bl,10                               ; check if its the last digit   
        jle %%skip                              ; last digit, continue with process
        inc ecx                                 ; increase counter
        inc edx                                 ; point to next char
        inc edi
        mov bl, byte[edx]                       ; bl = next char
        jmp %%increase_counter_loop             ; repeat
        
    %%skip:
        mov dword[char_counter_var],ecx         ; save return value in tmp variable 
        mov dword[char_counter_var2],edi
        popad                                   ; restore registers condition
        mov %1, dword[char_counter_var]         ; return string size value to selected register (ecx)
        mov edi, dword[char_counter_var2]       ; return number of chars not including leading zeroes
%endmacro


; 1st argument = register with adress of number, 2nd = file pointer
%macro print_number 2
    pushad
    xor ecx, ecx                                        ; set length counter to zero
    xor ebx, ebx                                        ; set ebx zero

    %%push_link_on_stack:
        movzx ebx, byte[%1]                             ; ebx = link's value
        push ebx                                        ; put link's value on stack
        inc ecx                                         ; increase counter
        mov %1 , dword[%1 + 1]                          ; eax = link.next adress
        cmp %1, 0                                       ; link.next == NULL
        jne %%push_link_on_stack                        ; push value on stack

    %%skip_zeros:
        cmp ecx, 0                                          ; check length of number
        je %%skip  
        pop ebx  
        dec ecx                                            ; handle case
        cmp ecx,0
        je %%print_first                                                      ; update counter
        cmp ebx,0                                           ; pop from stack to ebx
        je %%skip_zeros

    %%print_first:
        calc_fprintf ebx, format_hexa_first_digit, %2       ; print link's value, starting fro, MSB

    %%print_link_value:
        cmp ecx, 0                                      ; check if number has ended
        je %%skip                                       ; jump to end of macro
        pop ebx                                         ; pop next link's value to ebx
        calc_fprintf ebx, format_hexa, %2               ; fprintf() 
        loop %%print_link_value, ecx                    ; repeat process

    %%skip:
        calc_fprintf empty_string, format_string, %2
    popad
%endmacro


; allocating memory to new link
%macro calc_allocate_link_memory 1
    pushad
    push link_size                  ; 1st agrument = memory size
    push 1                          ; 2nd agrument = number of elements to allocate
    call calloc                     ; calloc(), eax stores adress to allocated memory 
    add esp, 8                      ; clean stack of calloc() arguments
    mov [new_link_adress] , eax
    popad
    mov %1, [new_link_adress]
%endmacro


%macro startFunc 0
    push ebp
    mov ebp, esp 
%endmacro

%macro endFunc 0
    mov esp, ebp	
	pop ebp
	ret
%endmacro

%macro print_calc_promt 0
    calc_printf msg,format_string_without_new_line
%endmacro

; 1st = argument to print, 2nd = format
%macro calc_printf 2
    pushad
    push    %1                         ; push string agrument on stack
    push    %2                         ; format_string
    call    printf                     ; printf(%1, %2)
    add      esp ,8                    ; remove fun arguments of stack
    popad
%endmacro

; 1st = argument to print, 2nd = format, 3rd = file pointer
%macro calc_fprintf 3
    pushad
    push    %1                          ; push string agrument on stack
    push    %2                          ; format_string
    push    %3                          ; file pointer
    call    fprintf                     ; fprintf(...)
    add      esp ,12                    ; remove fun arguments of stack
    popad
%endmacro

%macro calc_fgets 0
    pushad
    push    dword [stdin]              ; push stdin on stack
    push    max_input                  ; size of input_buffer (assumption: max_input = 80)
    push    input_buffer               ; input_buffer as agrument fo fgets
    call    fgets                      ; call fgets( stdin ,input_buffer, 80 )
    add     esp, 12                    ; remove args of stack
    popad
%endmacro

; check if enough opernds are in stack:
; arg = number of operands required
%macro calc_check_enough_operands 1
    mov     eax, dword[st_size]                             ; eax = number of operands
    cmp     eax, %1                                         ; check if args num required is larger then stack size
    jb      error_insufficient_number_of_arguments_on_stack ; handle error case
%endmacro

%macro calc_check_stack_full 0
    pushad
    mov     eax, dword[st_limit]            ; eax = limit of op_stack
    cmp     eax,dword[st_size]              ; check if args num required is larger then stack size
    je      error_calc_stack_overflow       ; handle error
    popad
%endmacro

;recieves as arg a pointer to num linked list, and inserts it to calc_stack
%macro calc_push 1  
            mov edx, dword [st_size]
            shl edx, 2
            add edx, dword[op_stack]
            mov dword [edx], %1                                 ; insert the new number to operands stack        

            inc dword [st_size]                                 ; update st_size 
%endmacro


%macro char_to_value 1
        cmp     %1, 'A'                 ; compare char to 'A'
        jge     %%if_digit_A_to_F       ; handle digit between A-F 
        sub     %1, '0'			        ; ebx-'0'
        jmp     %%skip

    %%if_digit_A_to_F:
            sub     %1, 55              ; char between A-F 
    %%skip:
%endmacro

; pop a number from stack, store it on the register %1 and free the 
%macro calc_pop 1
    cmp dword[st_size], 0                              ; check if op_stack is empty
    je error_insufficient_number_of_arguments_on_stack  ; handle error

    mov %1, dword[st_size]                              ; %1 holds index op st_size.top
    dec %1                                              ; correct %1 to hold top's index 
    shl %1, 2                                           ; multiply index by 4 (since op_stack is adresses stack)
    add %1, dword[op_stack]                             ; %1 holds the ADRESS IN OP_STACK, which holds the top
    mov %1, dword[%1]                                   ; %1 holds the top number's adress, this is the return value
    dec dword[st_size]                                  ; set st_size to point at the current stack top    
%endmacro

%macro calc_peek 1 
    cmp dword[st_size], 0                               ; check if op_stack is empty
    je error_insufficient_number_of_arguments_on_stack  ; handle error

    mov %1, dword[st_size]                              ; %1 holds index op st_size.top
    dec %1                                              ; correct %1 to hold top's index
    shl %1, 2                                           ; multiply index by 4 (since op_stack is adresses stack)
    add %1, dword[op_stack]                             ; %1 holds the ADRESS IN OP_STACK, which holds the top
    mov %1, dword[%1]                                   ; %1 holds the top number's adress, this is the return value    
%endmacro

%define max_input   80              ; max number of chars in input
%define link_size   5               ; number of bytes in each link 
%define stack_size  0xFF            ; max size of stack

section .data
    debug_flag:     dd 0                    ; flag for debug mode
    st_limit:       dd  5                   ; a var for current opreand-stack size (5 by defult)
    st_size:        dd 0                    ; size of current stack-top
    carry_var:      dd 0                    ; a var to hold it's value
    op_counter:     dd 0                    ; a counter for number of successfull calc operations

     
section	.rodata
    msg:                db "calc: " ,0                                                          ; calc_loop message
    empty_string:       db "" ,0                                                                ; empty string
    error_STF_string:   db "Error: Operand Stack Overflow" ,0                                   ; error_over_flow message
    error_IO_string:    db "Error: Insufficient Number of Arguments on Stack" ,10, 0            ; error_insufficient message
    debug_read_msg:     db "Debug -> number inserted: " ,0                                      ; debugger message
    debug_result_msg:   db "Debug -> operation result: " ,0                                     ; debugger message
    format_string:      db "%s", 10, 0	                                                        ; format string_string
    format_string_without_new_line: db "%s", 0
    format_decimal:     db "%d", 10, 0                                                          ; format string_int
    format_hexa:        db "%02X",0                                                             ; format string_hexa
    format_hexa_first_digit: db "%X",0                                                          ; format string_hexa for first digit
    format_hexa_new_line: db "%02X",10,0 

section .bss
    new_link_adress:    resd 1                      ; link adress (used for creating numbers)
    char_counter_var:   resd 1                      ; counting chars in input (for insert_number proc)
    char_counter_var2:  resd 1                      ; counting chars without zero's
    op_stack:           resd 1                      ; operands stack for assignment 
    input_buffer:       resb max_input              ; input_buffer limited to 80 chars
    temp_num1:          resd 1                      ; temp pointer for holding a number linked list address
    temp_num2:          resd 1                      ; temp pointer for holding a number linked list address


section .text
  align 16
  global main
  extern stdout
  extern stdin
  extern stderr
  extern printf
  extern fprintf 
  extern fflush
  extern malloc 
  extern calloc 
  extern free 
  extern getchar 
  extern fgets 

main:
    startFunc
    pushad	

    .check_argc:
        xor eax, eax                                    ; set eax to zero
        mov     eax, dword[ebp + 8]                     ; eax = argc 
        cmp     eax, 1                                  ; argc == 1
        je      .skip_setup_main                        ; if -d and st_limit are'nt arguments

    .check_argv1:
        mov     esi, dword[ebp + 12]                    ; esi = argv 
        mov     ebx, dword[esi + 4]                     ; ebx = argument[1] (ebx = string's adress)
        cmp     byte[ebx], '-'                          ; compare 1st byte of argv[1], check argv[1] == -d
        je     .update_debug_flag                       ; handle argv[1] = debug flag
        
        
        calc_update_limit                               ; else, ebx is the new limit
        jmp .check_argv2
    
    .update_debug_flag:
        mov     dword[debug_flag], 1                    ; update flag for debug mode
    
    .check_argv2:
        cmp     eax, 3                                  ; argc == 3
        jl      .skip_setup_main                        ; if -d and st_limit are'nt arguments            
        mov     ebx, dword[esi + 8]                     ; eax = argv[2] 
        cmp     byte[ebx], '-'                          ; compare first byte of argv[1], check argv[0] == -d
        je     .update_debug_second                     ; handle argv[2] = debug flag
        calc_update_limit                               ; update limit
        jmp     .skip_setup_main                        ; initiate main function
    
    .update_debug_second:
        mov     dword[debug_flag], 1                    ; update flag for debug mode
    
    .skip_setup_main:
        mov eax, dword[st_limit]        ; eax = updated stack size
        shl eax, 2                      ; multiply eax by 4 (since stack holdt at most st_limit adresses)
        push eax                        ; 1st agrument = operand stack size
        push 1                          ; 2nd agrument = number of elements to allocate
        call calloc                     ; calloc(), eax stores adress to allocated memory
        add esp,8                       ; clean arguments of stack
        mov  dword[op_stack], eax              ; set op_stack to store the allocated operand stack

        call    myCalc                                  ; call the calculator with the given stack size (to implement)
        .calc_end_program:
            calc_printf eax,format_hexa_first_digit         ; print number of operations executed
            calc_printf empty_string,format_string          ; print number of operations executed
            popad
            endFunc

myCalc:
    startFunc
    pushad
    .menu_loop:
        print_calc_promt                            ; print "calc: "
        calc_fgets                                  ; fgets(stdin, input_buffer, 80)
        movzx   eax, byte[input_buffer]             ; check first byte in input_buffer

        ; check eax, and jump to check validation of the operand
        
        cmp     eax, 'q'
        je      .handle_exit
        cmp     eax, '+'
        je      .handle_add
        cmp     eax, 'p'
        je      .handle_pop_print
        cmp     eax, 'd'
        je      .handle_duplicate
        cmp     eax, '&'
        je      .handle_and
        cmp     eax, '|'
        je      .handle_or
        cmp     eax, 'n'
        je      .handle_n_digits

        jmp .handle_insert                       ; ELSE - NUMBER IS INSERTED


    .handle_exit:
        jmp .quit        ; manage to free memory and exit gracefullly

    
    .handle_add:
        inc dword[op_counter]                   ; updating operations counter
        calc_check_enough_operands 2            ; is there at least 2 numbers in op_stack
        call .addition                          ; execute function
        jmp .menu_loop                          ; jump back to main loop

    
    .handle_pop_print:
        inc dword[op_counter]                   ; updating operations counter
        calc_check_enough_operands 1
        call .pop_and_print                          ; execute function
        jmp .menu_loop                          ; jump back to main loop

    .handle_duplicate:
        inc dword[op_counter]                   ; updating operations counter
        calc_check_stack_full 
        calc_check_enough_operands 1
        
        call .duplicate
        jmp .menu_loop
    
    .handle_and:
        inc dword[op_counter]                   ; updating operations counter
        calc_check_enough_operands 2
        call .bitwise_and
        jmp .menu_loop
    
    .handle_or:
        inc dword[op_counter]                   ; updating operations counter
        calc_check_enough_operands 2            ; is there at least 2 numbers in op_stack
        call .bitwise_or                        ; execute function
        jmp .menu_loop                          ; jump back to main loop

    .handle_n_digits:
        inc dword[op_counter]                   ; updating operations counter
        calc_check_enough_operands 1
        call .number_of_hexadecimal_digits      ; execute function
        jmp .menu_loop                          ; jump back to main loop

    .handle_insert:
        inc dword[op_counter]                   ; updating operations counter
        mov eax, input_buffer
        check_handle_debug eax, 0                       ; handle insert 
        .st_over_flow_check:
            mov eax, dword[st_limit]                    ; eax = limit
            cmp eax,dword[st_size]                      ; stack.size == stack.max ?  
            je  .stack_over_flow                        ; raise error
        call    .insert_number                          ; execute function
        jmp     .menu_loop                              ; jump back to main loop



;                                       //------------------- FUNCTIONS: -------------------//
    .insert_number:
    
        ; convert a string of number to linked-list
        ; each link bulit and point next link

        .setup_for_insert:
            startFunc
            pushad
            xor     ecx, ecx                                    ; set next 4 registers to zero
            xor     ebx, ebx
            xor     eax, eax
            xor     edx, edx
            calc_allocate_link_memory eax                       ; eax holds adress of 5 byte memory                               ; MALLOC OR CALLOC?
            mov     esi, eax                                    ; pointer to first link
            
            char_counter_proc ecx                               ; store in ecx input number's length
            dec     ecx                                         ; set ecx with last digit's index

        ; parse value of link value:
        .link_loop:
            .parse_link_value:

                xor ebx, ebx                            ; clean ebx (double check)
                mov edx, ecx                            ; set edx with char-counter value
                add edx, input_buffer                   ; set edx to store next char adress

                mov bl,         byte[edx]                           ; set bh to point on the next digit
                char_to_value   bl                                  ; convert char in bh to it's value                   
                dec             ecx                                 ; update ecx to point on next char in input_buffer (reverse wise)
                dec             edi
                cmp             edi,0                               ; check if its the last digit   
                jle             .finish_link                         ; last digit, continue with process
                
                mov             bh ,byte[input_buffer + ecx]        ; bh = left most digit in the link's value
                char_to_value   bh                                  ; convert second char to the value
                shl             bh, 4                               ; multiply by 16 the left digit in the link's value
                add             bl, bh                              ; bh = link's value
                
                xor             bh, bh                              ; ebx = bl
                dec             ecx                                 ; counter's value = next char
                dec             edi

            .finish_link:
                mov byte[eax], bl                   ; set link's value                                

                cmp edi,0                           ; check if there is a next digit   
                jle .push_number_to_stack           ; finished building number, upload to stack
                
                xor edx, edx                        ; clean edx, in order to store link's adress                        
                mov edx, eax                        ; edx store the recent-link adress 
                
                calc_allocate_link_memory eax       ; allocate memory for next link
                                                    ; eax points to 5 byte memory
                .check_nodes:
                                                    ; eax holds the adress of "next link" adress, of the new node
                mov dword[edx + 1], eax             ; new link points at the "to-be-created" link (in next loop)
                jmp .link_loop                      ; build next link    
        
        .push_number_to_stack:
        
            mov     edx, dword [st_size]
            shl     edx, 2
            add     edx, dword[op_stack]
            mov     dword [edx], esi                ; insert the new number to operands stack    
            inc     dword [st_size]                 ; update st_size
            

        popad
        endFunc                                             ; END OF INSERT NUMBER
        
        .stack_over_flow:
            calc_printf error_STF_string,format_string  ; print to screen error message
            jmp .menu_loop



    ;other operations to implement: ___________________________________________________________________________

    .quit:
        cmp     dword[st_size],0               ; if op_stack.empty(), continue
        je      .end_free_stack                ; no need to clean stack
        mov     ecx, dword[st_size]            ; set ecx as op_stack's size

        .clean_stack_loop:
            calc_pop    eax                         ; eax = op_stack.pop()
            push        eax                         ; push it's adress on stack
            call        free_number                 ; use assistant functions to free number
            add         esp, 4                      ; clean arguments from stack
            loop        .clean_stack_loop, ecx        ; repeat until op_stack is empty

        .end_free_stack:
            mov     eax, dword[op_stack]                ; eax = adress of op_stack
            push    eax                                 ; push op_stack adress on stack
            call    free                                ; free allocated memory
            add     esp,4                               ; clean argument from stack

        .end_calc_finish:
            popad
            mov eax, dword[op_counter]                  ; return the number of operations performed
            endFunc
   
        
    .addition:
        .setup_addition:
            startFunc
            pushad
            mov dword[carry_var], 0                             ; set carry to zero
            xor ecx, ecx                                        ; set ecx as zero            
            xor edx, edx                                        ; set edx as zero
            calc_allocate_link_memory esi                       ; esi holds the memory of the first link of the new number
            mov edi, esi                                        ; edi = esi. to be used as the pointer of the first link of number

            calc_pop eax                                        ; eax hold adress of 1st operand
            calc_pop ebx                                        ; eax hold adress of 2nd operand
            mov dword[temp_num1], eax                           ;keep pointer for this num (for calling free in the end)
            mov dword[temp_num2], ebx                           ;keep pointer for this num
    


        .addition_link_loop:
            movzx ecx, byte[eax]                            ; add to link's value 1st argument's value
            movzx edx, byte[ebx]                            ; edx cointains 2nd argument's value
            add ecx, edx                                    ; add to link's value 2nd argument's value
            add ecx, dword[carry_var]                       ; add to link's value carry
            mov dword[carry_var], 0                         ; reset carry_var

            .check_if_carry_needed:
                cmp ecx, 0x100                               ; compare value to 16
                jl .addition_finish_link                    ; fix value to be aproppriate, and carry status

            .update_value_n_carry:
                mov dword[carry_var], 1                     ; update carry to be 1
                sub ecx, 0x100                                 ; update ecx to correct value
            
            .addition_finish_link:
                ; at this point, cl = link's value
                mov     byte[esi], cl                           ; set link's value
                cmp     dword[eax + 1], 0                       ; eax.next == NULL?
                je      .addition_eax_case                      ; case |1st| <= |2nd|
                cmp     dword[ebx + 1], 0                       ; ebx.next == NULL?
                je      .addition_ebx_case                      ; case |2nd| < |1st|
                                                                ; else, set eax, ebx to point on next links
                mov     eax, dword[eax + 1]                         ; 1st = 1st.next
                mov     ebx, dword[ebx + 1]                         ; 2nd = 2nd.next
                calc_allocate_link_memory edx                   ; edx holds next link's adress
                mov     dword[esi + 1], edx                        ; created link points on the next link
                mov     esi, edx                                    ; update esi to last link's adress
                jmp     .addition_link_loop                         ; repeat


        .addition_eax_case:
            
            mov     edx,dword[ebx+1]                                ; edx holds 2nd.next adress
            mov     dword[esi+1], edx                               ; esi.next = 2nd.next
            mov     dword[ebx + 1], 0                               ; 2nd.next = NULL
            cmp     dword[carry_var], 1                             ; check if case ned to be handled
            je     .addition_case_carry                             ; MSB carry case
            jmp     .addition_insert_stack                           ; insert number to op_stack

        .addition_ebx_case:
            mov     edx,dword[eax+1]                                ; edx holds 1st.next adress
            mov     dword[esi+1], edx                               ; esi.next = 1st.next        
            mov dword[eax + 1], 0                               ; 1st.next = NULL
            cmp dword[carry_var], 1                             ; check if case ned to be handled
            jl .addition_insert_stack                             ; MSB carry case

        .addition_case_carry:
            cmp dword[esi+1], 0                                 ; check if carry is MSB 
            je .create_new_msb                                  ; handle carry = MSB
            mov esi, dword[esi + 1]                             ; set esi to next link's adress
            xor ecx,ecx
            add cl,byte[esi]
            cmp ecx,0xFF                                     ; check the edge case, in which process repeats
            jl  .addition_carry_skip
            xor ecx,ecx
            mov byte[esi],0                                
            jmp .addition_case_carry
            
            .addition_carry_skip:
                xor ecx,ecx
                add byte[esi], 1                                    ; add carry to the current link
                jmp .addition_insert_stack                          ; repeat process

        
        .create_new_msb:
            calc_allocate_link_memory edx                       ; edx holds memory of new link
            mov byte[edx], 1                                    ; set link's value to carry
            mov dword[esi + 1], edx                             ; set the new MSB

        .addition_insert_stack:
            calc_push edi                   ;push result number to calc_stack
            push    dword[temp_num1]       
            call    free_number             ;free first number          
            add     esp, 4                  ;clean args from stack
            push    dword[temp_num2]       
            call    free_number             ;free second number
            add     esp, 4                  ;clean args from stack

            check_handle_debug edi, 1       ; handle debug error
            popad
           
            endFunc

    ; pop_and_print = DONE (delete before submission)
    .pop_and_print:
        startFunc
        pushad
        ;calc_pop eax                           ; replaced with code, just for debug
        mov     eax, dword[st_size]             ; eax = number of operands in stack
        dec     eax                             ; correct eax to hold top's index
        shl     eax, 2                          ; multiply eax by 4
        add     eax, dword[op_stack]            ; eax holds the adress which contains number's address
        dec     dword[st_size]                  ; update st_size
        
        mov     eax, dword[eax]                 ; eax = number's adress
        print_number eax, dword [stdout]
        push    eax 
        call    free_number
        add     esp,4
        popad
        endFunc     

            
    .duplicate:
        startFunc
        pushad   
        calc_peek   eax                   ;eax now points to top number in stack
        calc_allocate_link_memory esi   ;esi will point to the current link of new dup number
        mov dword[temp_num1], esi                           ;keep the pointer to the beginning of new num
        
        .loop_old_num:
             movzx      ecx, byte[eax]                              ; ecx holds curr link value of old num
             mov        byte[esi], cl                               ; new number's curr link get this value
             mov        eax,dword[eax+1]
             cmp        eax,0                                       ; check if we finished copying old num
             je         .push_dup
             calc_allocate_link_memory edx                          ; create next link for new num
             mov        dword[esi+1],edx                            ; old link.next = new link
             mov        esi, edx                                    ; curr = new link
             jmp        .loop_old_num

        .push_dup:
            mov         eax,dword[temp_num1]                        ; edx now points to beginning of new num (maybe possible to just send temp_num1 instead?)
            calc_push   eax                                         ; push the number on stack
            mov         edi, eax                                    ; set edi as the new number for debug option
        check_handle_debug edi, 1                                   ; handle debug error
        popad
        endFunc
        
    .bitwise_or:
        .setup_or:
            startFunc
            pushad
            xor     ecx, ecx                                        ; set ecx as zero            
            xor     edx, edx                                        ; set edx as zero
            calc_allocate_link_memory esi                           ; esi holds the memory of the first link of the new number
            mov     edi, esi                                        ; edi = esi. to be used as the pointer of the first link of number

            calc_pop eax                                            ; eax hold adress of 1st operand
            calc_pop ebx                                            ; eax hold adress of 2nd operand
            mov     dword[temp_num1], eax                           ;keep pointer for this num (for calling free in the end)
            mov     dword[temp_num2], ebx                           ;keep pointer for this num
    


        .or_link_loop:
            movzx   ecx, byte[eax]                              ; add to link's value 1st argument's value
            movzx   edx, byte[ebx]                              ; edx cointains 2nd argument's value
            or      cl, dl                                      ; add to link's value 2nd argument's value
            .or_finish_link:
                ; at this point, cl = link's value
                mov     byte[esi], cl                           ; set link's value
                cmp     dword[eax + 1], 0                       ; eax.next == NULL?
                je      .or_eax_case                            ; case |1st| <= |2nd|
                cmp     dword[ebx + 1], 0                       ; ebx.next == NULL?
                je      .or_ebx_case                            ; case |2nd| < |1st|
                                                                ; else, set eax, ebx to point on next links
                mov     eax, dword[eax + 1]                     ; 1st = 1st.next
                mov     ebx, dword[ebx + 1]                     ; 2nd = 2nd.next
                calc_allocate_link_memory edx                   ; edx holds next link's adress
                mov     dword[esi + 1], edx                     ; created link points on the next link
                mov     esi, edx                                ; update esi to last link's adress
                jmp     .or_link_loop                           ; repeat


        .or_eax_case:

            
            mov     edx,dword[ebx+1]                                ; edx holds 2nd.next adress
            mov     dword[esi+1], edx                               ; esi.next = 2nd.next
            mov     dword[ebx + 1], 0                               ; 2nd.next = NULL
            jmp     .or_insert_stack                                ; insert number to op_stack

        .or_ebx_case:
            mov     edx,dword[eax+1]                                ; edx holds 1st.next adress
            mov     dword[esi+1], edx                               ; esi.next = 1st.next        
            mov dword[eax + 1], 0                                   ; 1st.next = NULL

        

        .or_insert_stack:
            calc_push   edi                     ; push result number to calc_stack
            push        dword[temp_num1]        ; push number stored in the tmp variable          
            call        free_number             ; free first number          
            add         esp, 4                  ; clean args from stack
            push        dword[temp_num2]        ; push number stored in the tmp variable
            call        free_number             ; free second number
            add         esp, 4                  ; clean args from stack
        
            check_handle_debug edi, 1   ; handle debug error
            popad
            endFunc
        
    .bitwise_and:
     .setup_and:
            startFunc
            pushad
            xor     ecx, ecx                                        ; set ecx as zero            
            xor     edx, edx                                        ; set edx as zero
            calc_allocate_link_memory esi                           ; esi holds the memory of the first link of the new number
            mov     edi, esi                                        ; edi = esi. to be used as the pointer of the first link of number

            calc_pop    eax                                             ; eax hold adress of 1st operand
            calc_pop    ebx                                             ; eax hold adress of 2nd operand
            mov         dword[temp_num1], eax                           ;keep pointer for this num (for calling free in the end)
            mov         dword[temp_num2], ebx                           ;keep pointer for this num
    


        .and_link_loop:
            movzx       ecx, byte[eax]                            ; add to link's value 1st argument's value
            movzx       edx, byte[ebx]                            ; edx cointains 2nd argument's value
            and         cl, dl                                    ; add to link's value 2nd argument's value
            
                 
            
            .and_finish_link:
                ; at this point, cl = link's value
                mov     byte[esi], cl                           ; set link's value
                cmp     dword[eax + 1], 0                       ; eax.next == NULL?
                je      .and_insert_stack                       ; case |1st| <= |2nd|
                cmp     dword[ebx + 1], 0                       ; ebx.next == NULL?
                je      .and_insert_stack                       ; case |2nd| < |1st|
                                                                ; else, set eax, ebx to point on next links
                mov     eax, dword[eax + 1]                     ; 1st = 1st.next
                mov     ebx, dword[ebx + 1]                     ; 2nd = 2nd.next
                calc_allocate_link_memory edx                   ; edx holds next link's adress
                mov     dword[esi + 1], edx                     ; created link points on the next link
                mov     esi, edx                                ; update esi to last link's adress
                jmp     .and_link_loop                          ; repeat


        .and_insert_stack:
            calc_push   edi                     ; push result number to calc_stack
            push        dword[temp_num1]        ; push number stored in the tmp variable          
            call        free_number             ; free first number          
            add         esp, 4                  ; clean args from stack
            push        dword[temp_num2]        ; push number stored in the tmp variable
            call        free_number             ; free second number
            add         esp, 4                  ; clean args from stack
        
            check_handle_debug edi, 1           ; handle debug error
            popad
            endFunc

.number_of_hexadecimal_digits:
    startFunc  
    pushad
    calc_pop    eax                         ; pop top number and store it on eax
    mov         edi, eax                    ; edi hold's pointer to number, to be free
    xor         ecx,ecx                     ; counter ecx = 0;
    xor         edx,edx                     ; keep as boolean stating if we saw a number different than 0
    .loop_count_digits:
        add     ecx,2                       ; add 2 digits to counter
        cmp     byte[eax],0
        je      .skip_found_non_zero
        mov     edx,1
        .skip_found_non_zero:
        cmp     dword[eax+1],0              ; check if this is the last link
        je      .push_res            
        mov     eax,dword[eax+1]            ; curr = curr->next
        jmp     .loop_count_digits

        .push_res:                          ; push our result to calc_stack
            cmp     edx,0
            je      .push_one                ;if we only found zeros than char number is 1
            movzx   ebx,byte[eax]           ; ebx hold's last link val (MSB)
            cmp     bl,16                   ; check if final link number has 1 or 2 digits
            jae     .push_now               ; if MSB<16 then it only has 1 digit , so we will decrease 1 from counter (ecx) 
            dec     ecx                     ; if only 1 digit then we decrease ecx by 1
        
        .push_now:
            calc_allocate_link_memory eax               ; eax holds new link memory address

            mov                 byte[eax],cl            ; set link's value
            calc_push           eax                     ; push new num to top of the stack
            push edi                                    ; push original number's adress to stack as argument for free_number()
            call free_number                            ; free number
            add esp, 4                                  ; clean stack from argument 
            mov                 edi, eax                ; set edi as the new number
            check_handle_debug  edi, 1                  ; handle debug error

            popad
            endFunc
    .push_one:
        mov     ecx,1
        jmp     .push_now
    
    ; assistant functions:

error_insufficient_number_of_arguments_on_stack:                            ; print error message and jmp back to menu
    calc_printf     error_IO_string ,format_string                          ; print error message
    jmp             myCalc.menu_loop                                        ; back to menu
  
error_calc_stack_overflow:                                                  ; print error message and jmp back to menu
    calc_printf     error_STF_string ,format_string                         ; print error message
    jmp             myCalc.menu_loop                                        ; back to menu

free_number:
    startFunc
    pushad
    mov     edx, dword [ebp+8]                                      ; get arg (pointer to first link in number)
    movzx   ecx, byte[edx]                                          ; get number's value
   
    .iterate_free:
        mov     eax ,dword[edx+1]                                   ; eax temporary store next link's adress
        mov     dword[temp_num1],eax                                ; save in a variable the adress of next link
        push    edx                                                 ; put number's adress on stack
        call    free                                                ; free link
        add     esp,4                                               ; clear stack

        mov     edx,dword[temp_num1]                                ; edx = next link's adress
        cmp     edx,0                                               ; next == NULL ?
        jne     .iterate_free                                       ; free to next link
    mov     dword[temp_num1],0
    popad
    endFunc    







    