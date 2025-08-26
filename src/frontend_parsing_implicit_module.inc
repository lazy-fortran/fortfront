    ! Check if there are declarations before an explicit program
    function has_declarations_before_program(tokens) result(has_decls)
        type(token_t), intent(in) :: tokens(:)
        logical :: has_decls
        integer :: i
        logical :: found_declarations, found_explicit_program
        
        has_decls = .false.
        found_declarations = .false.
        found_explicit_program = .false.
        
        ! Debug output
        ! print *, "DEBUG: Checking for declarations before program, total tokens:", size(tokens)
        
        ! Scan tokens to find declarations before explicit program
        do i = 1, size(tokens)
            if (tokens(i)%kind == TK_KEYWORD) then
                ! Debug each keyword  
                ! print *, "DEBUG: Token", i, "keyword:", trim(tokens(i)%text)
                
                ! Check for explicit program statement
                if (tokens(i)%text == "program") then
                    ! Check that it's not "end program"
                    if (i == 1) then
                        found_explicit_program = .true.
                        ! print *, "DEBUG: Found explicit program at position 1"
                        exit
                    else if (i > 1) then
                        if (tokens(i-1)%kind /= TK_KEYWORD .or. tokens(i-1)%text /= "end") then
                            found_explicit_program = .true.
                            ! print *, "DEBUG: Found explicit program at position", i
                            exit
                        end if
                    end if
                    
                ! Check for declaration keywords that appear before program
                else if (.not. found_explicit_program) then
                    select case (tokens(i)%text)
                    case ("type", "interface", "integer", "real", "logical", &
                          "character", "double", "complex")
                        ! For type, check if it's a type definition (type ::)
                        if (tokens(i)%text == "type") then
                            if (i + 1 <= size(tokens)) then
                                if (tokens(i+1)%kind == TK_OPERATOR .and. &
                                    tokens(i+1)%text == "::") then
                                    found_declarations = .true.
                                else if (i + 2 <= size(tokens) .and. &
                                         tokens(i+1)%kind == TK_IDENTIFIER .and. &
                                         tokens(i+2)%kind == TK_OPERATOR .and. &
                                         tokens(i+2)%text == "::") then
                                    ! type name :: case
                                    found_declarations = .true.
                                end if
                            end if
                        else
                            ! Other declaration keywords
                            found_declarations = .true.
                            ! print *, "DEBUG: Found declaration keyword:", trim(tokens(i)%text)
                        end if
                    end select
                end if
            end if
        end do
        
        ! We have declarations before program if both conditions are met
        has_decls = found_declarations .and. found_explicit_program
        ! print *, "DEBUG: has_declarations_before_program result:", has_decls
        ! print *, "DEBUG: found_declarations:", found_declarations, "found_explicit_program:", found_explicit_program
    end function has_declarations_before_program
    
    ! Find the boundary between declarations and explicit program
    subroutine find_declaration_program_boundary(tokens, decl_end, prog_start)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(out) :: decl_end, prog_start
        integer :: i
        
        decl_end = 0
        prog_start = 0
        
        ! Find where the explicit program starts
        do i = 1, size(tokens)
            if (tokens(i)%kind == TK_KEYWORD .and. tokens(i)%text == "program") then
                ! Check that it's not "end program"
                if (i == 1) then
                    prog_start = i
                    decl_end = i - 1
                    exit
                else if (tokens(i-1)%kind /= TK_KEYWORD .or. tokens(i-1)%text /= "end") then
                    prog_start = i
                    ! Find the actual end of declarations (skip whitespace/comments)
                    decl_end = i - 1
                    do while (decl_end > 0)
                        if (tokens(decl_end)%kind /= TK_EOF .and. &
                            tokens(decl_end)%kind /= TK_NEWLINE .and. &
                            tokens(decl_end)%kind /= TK_COMMENT) then
                            exit
                        end if
                        decl_end = decl_end - 1
                    end do
                    exit
                end if
            end if
        end do
    end subroutine find_declaration_program_boundary
    
    ! Create tokens for implicit module wrapping declarations
    function wrap_declarations_in_module(tokens, decl_start, decl_end) result(module_tokens)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: decl_start, decl_end
        type(token_t), allocatable :: module_tokens(:)
        integer :: i, idx, num_tokens
        
        ! print *, "DEBUG: wrap_declarations_in_module: decl_start =", decl_start, "decl_end =", decl_end
        
        ! Validate bounds
        if (decl_start < 1 .or. decl_end > size(tokens) .or. decl_start > decl_end) then
            ! print *, "ERROR: Invalid bounds in wrap_declarations_in_module"
            allocate(module_tokens(0))
            return
        end if
        
        ! Calculate number of tokens needed
        ! module implicit_module + implicit none + declarations + end module + extra space
        num_tokens = 10 + (decl_end - decl_start + 1) + 5
        allocate(module_tokens(num_tokens))
        
        idx = 1
        
        ! Add "module implicit_module"
        module_tokens(idx)%kind = TK_KEYWORD
        module_tokens(idx)%text = "module"
        module_tokens(idx)%line = 1
        module_tokens(idx)%column = 1
        idx = idx + 1
        
        module_tokens(idx)%kind = TK_IDENTIFIER
        module_tokens(idx)%text = "implicit_module"
        module_tokens(idx)%line = 1
        module_tokens(idx)%column = 8
        idx = idx + 1
        
        module_tokens(idx)%kind = TK_NEWLINE
        module_tokens(idx)%text = ""
        module_tokens(idx)%line = 1
        module_tokens(idx)%column = 23
        idx = idx + 1
        
        ! Add "implicit none"
        module_tokens(idx)%kind = TK_KEYWORD
        module_tokens(idx)%text = "implicit"
        module_tokens(idx)%line = 2
        module_tokens(idx)%column = 5
        idx = idx + 1
        
        module_tokens(idx)%kind = TK_KEYWORD
        module_tokens(idx)%text = "none"
        module_tokens(idx)%line = 2
        module_tokens(idx)%column = 14
        idx = idx + 1
        
        module_tokens(idx)%kind = TK_NEWLINE
        module_tokens(idx)%text = ""
        module_tokens(idx)%line = 2
        module_tokens(idx)%column = 18
        idx = idx + 1
        
        ! Copy declaration tokens (preserving original tokens exactly)
        do i = decl_start, decl_end
            module_tokens(idx) = tokens(i)
            ! Adjust line numbers to account for module header
            module_tokens(idx)%line = module_tokens(idx)%line + 2
            idx = idx + 1
        end do
        
        ! Add newline if needed
        if (module_tokens(idx-1)%kind /= TK_NEWLINE) then
            module_tokens(idx)%kind = TK_NEWLINE
            module_tokens(idx)%text = ""
            module_tokens(idx)%line = module_tokens(idx-1)%line
            module_tokens(idx)%column = module_tokens(idx-1)%column + &
                                       len(module_tokens(idx-1)%text)
            idx = idx + 1
        end if
        
        ! Add "end module implicit_module"
        module_tokens(idx)%kind = TK_KEYWORD
        module_tokens(idx)%text = "end"
        module_tokens(idx)%line = module_tokens(idx-1)%line + 1
        module_tokens(idx)%column = 1
        idx = idx + 1
        
        module_tokens(idx)%kind = TK_KEYWORD
        module_tokens(idx)%text = "module"
        module_tokens(idx)%line = module_tokens(idx-1)%line
        module_tokens(idx)%column = 5
        idx = idx + 1
        
        module_tokens(idx)%kind = TK_IDENTIFIER
        module_tokens(idx)%text = "implicit_module"
        module_tokens(idx)%line = module_tokens(idx-1)%line
        module_tokens(idx)%column = 12
        
        ! Resize if we allocated too much
        if (idx < num_tokens) then
            module_tokens = module_tokens(1:idx)
        end if
    end function wrap_declarations_in_module
    
    ! Add use statement to program tokens
    function add_use_statement_to_program(tokens, prog_start) result(new_tokens)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: prog_start
        type(token_t), allocatable :: new_tokens(:)
        integer :: i, idx, prog_name_end
        integer :: num_new_tokens
        
        ! Find end of program statement (program name)
        prog_name_end = prog_start + 1
        do while (prog_name_end <= size(tokens))
            if (tokens(prog_name_end)%kind == TK_NEWLINE .or. &
                tokens(prog_name_end)%kind == TK_EOF) then
                exit
            end if
            prog_name_end = prog_name_end + 1
        end do
        
        ! Allocate space for new tokens (original + use statement tokens)
        num_new_tokens = size(tokens) + 4  ! use implicit_module newline
        allocate(new_tokens(num_new_tokens))
        
        idx = 1
        
        ! Copy up to end of program statement
        do i = prog_start, prog_name_end - 1
            new_tokens(idx) = tokens(i)
            idx = idx + 1
        end do
        
        ! Add newline after program statement if needed
        if (tokens(prog_name_end)%kind == TK_NEWLINE) then
            new_tokens(idx) = tokens(prog_name_end)
            idx = idx + 1
        else
            new_tokens(idx)%kind = TK_NEWLINE
            new_tokens(idx)%text = ""
            new_tokens(idx)%line = tokens(prog_name_end-1)%line
            new_tokens(idx)%column = tokens(prog_name_end-1)%column + &
                                     len(tokens(prog_name_end-1)%text)
            idx = idx + 1
        end if
        
        ! Add "use implicit_module"
        new_tokens(idx)%kind = TK_KEYWORD
        new_tokens(idx)%text = "use"
        new_tokens(idx)%line = new_tokens(idx-1)%line + 1
        new_tokens(idx)%column = 5
        idx = idx + 1
        
        new_tokens(idx)%kind = TK_IDENTIFIER
        new_tokens(idx)%text = "implicit_module"
        new_tokens(idx)%line = new_tokens(idx-1)%line
        new_tokens(idx)%column = 9
        idx = idx + 1
        
        new_tokens(idx)%kind = TK_NEWLINE
        new_tokens(idx)%text = ""
        new_tokens(idx)%line = new_tokens(idx-1)%line
        new_tokens(idx)%column = 24
        idx = idx + 1
        
        ! Copy remaining tokens (skip the newline we already handled)
        do i = prog_name_end + 1, size(tokens)
            new_tokens(idx) = tokens(i)
            new_tokens(idx)%line = new_tokens(idx)%line + 1
            idx = idx + 1
        end do
        
        ! Resize if needed
        if (idx - 1 < num_new_tokens) then
            new_tokens = new_tokens(1:idx-1)
        end if
    end function add_use_statement_to_program