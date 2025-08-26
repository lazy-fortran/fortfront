    ! Find program unit boundary (function/subroutine/module spans multiple lines)
    subroutine find_program_unit_boundary(tokens, start_pos, unit_start, unit_end, &
                                         has_explicit_program)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_pos
        integer, intent(out) :: unit_start, unit_end
        logical, intent(in) :: has_explicit_program

        integer :: i, current_line, nesting_level
        logical :: in_function, in_subroutine, in_module, in_do_loop, &
                   in_select_case, in_if_block

        unit_start = start_pos
        unit_end = start_pos
        call initialize_construct_flags(in_function, in_subroutine, in_module, &
                                      in_do_loop, in_select_case, in_if_block)
        nesting_level = 0

        ! Check if starting token indicates a multi-line construct
        if (start_pos <= size(tokens)) then
            call detect_construct_start(tokens, start_pos, in_function, in_subroutine, &
                                      in_module, in_do_loop, in_select_case, &
                                      in_if_block, nesting_level)
        end if

        ! If this is a multi-line construct, find the end
        if (in_function .or. in_subroutine .or. in_module .or. in_do_loop .or. &
            in_select_case .or. in_if_block) then
            call find_multiline_construct_end(tokens, start_pos, unit_end, &
                                            in_function, in_subroutine, in_module, &
                                            in_do_loop, in_select_case, in_if_block, &
                                            nesting_level)
        else
            ! Not a module/function/subroutine/etc - handle as single line or implicit main
            call handle_single_line_construct(tokens, start_pos, unit_start, unit_end, &
                                            has_explicit_program, current_line)
        end if
    end subroutine find_program_unit_boundary

    ! Initialize construct flags
    subroutine initialize_construct_flags(in_function, in_subroutine, in_module, &
                                        in_do_loop, in_select_case, in_if_block)
        logical, intent(out) :: in_function, in_subroutine, in_module, in_do_loop, &
                               in_select_case, in_if_block

        in_function = .false.
        in_subroutine = .false.
        in_module = .false.
        in_do_loop = .false.
        in_select_case = .false.
        in_if_block = .false.
    end subroutine initialize_construct_flags

    ! Detect construct start
    subroutine detect_construct_start(tokens, start_pos, in_function, in_subroutine, &
                                    in_module, in_do_loop, in_select_case, &
                                    in_if_block, nesting_level)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_pos
        logical, intent(out) :: in_function, in_subroutine, in_module, in_do_loop, &
                               in_select_case, in_if_block
        integer, intent(out) :: nesting_level

        ! Look for function definition patterns
        if (is_function_start(tokens, start_pos)) then
            in_function = .true.
            nesting_level = 1
        else if (is_subroutine_start(tokens, start_pos)) then
            in_subroutine = .true.
            nesting_level = 1
        else if (is_module_start(tokens, start_pos)) then
            in_module = .true.
            nesting_level = 1
        else if (is_program_start(tokens, start_pos)) then
            ! Handle explicit program blocks
            in_module = .true.  ! Reuse module logic for program blocks
            nesting_level = 1
        else if (is_do_while_start(tokens, start_pos)) then
            in_do_loop = .true.
            nesting_level = 1
        else if (is_do_loop_start(tokens, start_pos)) then
            in_do_loop = .true.
            nesting_level = 1
        else if (is_select_case_start(tokens, start_pos)) then
            in_select_case = .true.
            nesting_level = 1
        else if (is_if_then_start(tokens, start_pos)) then
            in_if_block = .true.
            nesting_level = 1
        end if
    end subroutine detect_construct_start

    ! Find multiline construct end
    subroutine find_multiline_construct_end(tokens, start_pos, unit_end, &
                                          in_function, in_subroutine, in_module, &
                                          in_do_loop, in_select_case, in_if_block, &
                                          nesting_level)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_pos
        integer, intent(inout) :: unit_end
        logical, intent(in) :: in_function, in_subroutine, in_module, in_do_loop, &
                              in_select_case, in_if_block
        integer, intent(inout) :: nesting_level
        integer :: i, i_before

        i = start_pos
        do while (i <= size(tokens) .and. nesting_level > 0)
            if (tokens(i)%kind == TK_EOF) exit

            ! Check for nested constructs (but skip the first one at start_pos)
            if (i /= start_pos) then
                call check_nested_constructs(tokens, i, in_function, in_subroutine, &
                                           in_module, in_do_loop, in_select_case, &
                                           in_if_block, nesting_level)
            end if

            ! Store i before check_end_constructs (which may modify it)
            i_before = i
            
            ! Check for end constructs
            call check_end_constructs(tokens, i, unit_end, in_function, &
                                    in_subroutine, in_module, in_do_loop, &
                                    in_select_case, in_if_block, nesting_level)

            ! Stop when we've closed all nested constructs
            if (nesting_level == 0) exit
            
            ! If check_end_constructs didn't advance i, do it ourselves
            if (i == i_before) then
                unit_end = i
                i = i + 1
            end if
            ! Otherwise check_end_constructs already advanced i properly
        end do
    end subroutine find_multiline_construct_end

    ! Check nested constructs
    subroutine check_nested_constructs(tokens, i, in_function, in_subroutine, &
                                     in_module, in_do_loop, in_select_case, &
                                     in_if_block, nesting_level)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: i
        logical, intent(in) :: in_function, in_subroutine, in_module, in_do_loop, &
                              in_select_case, in_if_block
        integer, intent(inout) :: nesting_level

        if (in_function .and. is_function_start(tokens, i)) then
            nesting_level = nesting_level + 1
        else if (in_subroutine .and. is_subroutine_start(tokens, i)) then
            nesting_level = nesting_level + 1
        else if (in_module .and. is_module_start(tokens, i)) then
            nesting_level = nesting_level + 1
        else if (in_do_loop .and. (is_do_loop_start(tokens, i) .or. &
                                    is_do_while_start(tokens, i))) then
            nesting_level = nesting_level + 1
        else if (in_select_case .and. is_select_case_start(tokens, i)) then
            nesting_level = nesting_level + 1
        else if (in_if_block .and. is_if_then_start(tokens, i)) then
            nesting_level = nesting_level + 1
        end if
    end subroutine check_nested_constructs

    ! Check end constructs
    subroutine check_end_constructs(tokens, i, unit_end, in_function, &
                                  in_subroutine, in_module, in_do_loop, &
                                  in_select_case, in_if_block, nesting_level)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(inout) :: i
        integer, intent(inout) :: unit_end
        logical, intent(in) :: in_function, in_subroutine, in_module, in_do_loop, &
                              in_select_case, in_if_block
        integer, intent(inout) :: nesting_level

        if (in_function .and. is_end_function(tokens, i)) then
            nesting_level = nesting_level - 1
            ! Check if there's a function name after "end function"
            if (i + 2 <= size(tokens) .and. tokens(i + 2)%kind == TK_IDENTIFIER) then
                unit_end = i + 2  ! Include "end", "function", and function name
                i = i + 3  ! Skip all three tokens
            else
                unit_end = i + 1  ! Include both "end" and "function" tokens
                i = i + 2  ! Skip both "end" and "function" tokens
            end if
        else if (in_subroutine .and. is_end_subroutine(tokens, i)) then
            nesting_level = nesting_level - 1
            ! Check if there's a subroutine name after "end subroutine"
            if (i + 2 <= size(tokens) .and. tokens(i + 2)%kind == TK_IDENTIFIER) then
                unit_end = i + 2  ! Include "end", "subroutine", and subroutine name
                i = i + 3  ! Skip all three tokens
            else
                unit_end = i + 1  ! Include both "end" and "subroutine" tokens
                i = i + 2  ! Skip both "end" and "subroutine" tokens
            end if
        else if (in_module .and. is_end_module(tokens, i)) then
            nesting_level = nesting_level - 1
            ! Check if there's a module name after "end module"
            if (i + 2 <= size(tokens) .and. tokens(i + 2)%kind == TK_IDENTIFIER) then
                unit_end = i + 2  ! Include "end", "module", and module name
                i = i + 3  ! Skip all three tokens
            else
                unit_end = i + 1  ! Include both "end" and "module" tokens
                i = i + 2  ! Skip both "end" and "module" tokens
            end if
        else if (in_do_loop .and. is_end_do(tokens, i)) then
            nesting_level = nesting_level - 1
            unit_end = i + 1  ! Include both "end" and "do" tokens
            i = i + 2  ! Skip both "end" and "do" tokens
        else if (in_select_case .and. is_end_select(tokens, i)) then
            nesting_level = nesting_level - 1
            unit_end = i + 1  ! Include both "end" and "select" tokens
            i = i + 2  ! Skip both "end" and "select" tokens
        else if (in_if_block .and. is_end_if(tokens, i)) then
            nesting_level = nesting_level - 1
            ! Check if it's "endif" (single token) or "end if" (two tokens)
            if (tokens(i)%text == "endif") then
                unit_end = i  ! Include just the "endif" token
                i = i + 1  ! Skip one token
            else
                unit_end = i + 1  ! Include both "end" and "if" tokens
                i = i + 2  ! Skip both tokens
            end if
        else
            unit_end = i
            i = i + 1
        end if
    end subroutine check_end_constructs

    ! Handle single line construct
    subroutine handle_single_line_construct(tokens, start_pos, unit_start, unit_end, &
                                          has_explicit_program, current_line)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_pos
        integer, intent(inout) :: unit_start, unit_end
        logical, intent(in) :: has_explicit_program
        integer, intent(out) :: current_line

        ! Check if we're after a module end - if so, group as implicit main
        if (is_after_module_end(tokens, start_pos)) then
            call group_as_main_program(tokens, start_pos, unit_end)
        else if (.not. has_explicit_program) then
            ! For pure lazy fortran without any program blocks
            call handle_lazy_fortran_unit(tokens, start_pos, unit_end)
        else
            ! Explicit program units
            call handle_explicit_program_unit(tokens, start_pos, unit_start, unit_end, current_line)
        end if

        call handle_unit_edge_cases(tokens, start_pos, unit_start, unit_end)
    end subroutine handle_single_line_construct
    
    ! Check if position is after module end
    function is_after_module_end(tokens, pos) result(after_module)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        logical :: after_module
        integer :: i
        logical :: seen_module_end
        
        after_module = .false.
        seen_module_end = .false.
        
        ! Scan from beginning to current position to check if we've passed a module
        do i = 1, min(pos - 1, size(tokens) - 1)
            if (tokens(i)%kind == TK_KEYWORD .and. tokens(i)%text == "end") then
                if (i + 1 <= size(tokens) .and. tokens(i+1)%kind == TK_KEYWORD .and. &
                    tokens(i+1)%text == "module") then
                    seen_module_end = .true.
                end if
            end if
        end do
        
        ! We're after module if we've seen "end module" and current position is not a module/program/etc
        if (seen_module_end .and. pos <= size(tokens)) then
            if (.not. is_module_start(tokens, pos) .and. &
                .not. is_program_start(tokens, pos) .and. &
                .not. is_function_start(tokens, pos) .and. &
                .not. is_subroutine_start(tokens, pos)) then
                after_module = .true.
            end if
        end if
    end function is_after_module_end

    ! Handle mixed module and implicit main constructs
    subroutine handle_mixed_constructs(tokens, start_pos, unit_start, unit_end, current_line)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_pos
        integer, intent(inout) :: unit_start, unit_end
        integer, intent(out) :: current_line
        integer :: i
        logical :: found_module_end_before

        ! Check if we've seen "end module" before this position
        ! This indicates we're after a module and should group as implicit main
        found_module_end_before = .false.
        do i = 1, start_pos - 1
            if (tokens(i)%kind == TK_KEYWORD .and. tokens(i)%text == "end") then
                if (i + 1 <= size(tokens) .and. tokens(i+1)%kind == TK_KEYWORD .and. &
                    tokens(i+1)%text == "module") then
                    found_module_end_before = .true.
                    exit
                end if
            end if
        end do

        if (found_module_end_before) then
            ! After a module, group all remaining statements as implicit main
            call group_as_main_program(tokens, start_pos, unit_end)
        else
            call handle_explicit_program_unit(tokens, start_pos, unit_start, unit_end, current_line)
        end if
    end subroutine handle_mixed_constructs

    ! Handle lazy fortran unit
    subroutine handle_lazy_fortran_unit(tokens, start_pos, unit_end)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_pos
        integer, intent(inout) :: unit_end

        ! Find end of all tokens (excluding final EOF)
        unit_end = size(tokens)
        do while (unit_end > start_pos .and. tokens(unit_end)%kind == TK_EOF)
            unit_end = unit_end - 1
        end do
    end subroutine handle_lazy_fortran_unit

    ! Handle explicit program unit
    subroutine handle_explicit_program_unit(tokens, start_pos, unit_start, unit_end, &
                                          current_line)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_pos
        integer, intent(inout) :: unit_start, unit_end
        integer, intent(out) :: current_line
        integer :: i

        ! Single line construct - find end of current line
        current_line = tokens(start_pos)%line
        i = start_pos
        do while (i <= size(tokens))
            if (tokens(i)%line == current_line) then
                unit_end = i
                i = i + 1
            else
                exit
            end if
        end do
    end subroutine handle_explicit_program_unit

    ! Check if should group as main program
    function should_group_as_main_program(tokens, start_pos) result(should_group)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_pos
        logical :: should_group

        should_group = (start_pos <= size(tokens) .and. &
                       .not. is_function_start(tokens, start_pos) .and. &
                       .not. is_subroutine_start(tokens, start_pos) .and. &
                       .not. is_module_start(tokens, start_pos) .and. &
                       .not. is_program_start(tokens, start_pos))
    end function should_group_as_main_program

    ! Group as main program
    subroutine group_as_main_program(tokens, start_pos, unit_end)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_pos
        integer, intent(inout) :: unit_end

        ! After a module ends, group ALL remaining statements as implicit main
        ! Don't check for "meaningful" content - any statements should be grouped
        unit_end = size(tokens)
        do while (unit_end > start_pos .and. tokens(unit_end)%kind == TK_EOF)
            unit_end = unit_end - 1
        end do
        
        ! Only skip if there's truly nothing left
        if (unit_end < start_pos) then
            unit_end = start_pos - 1
        end if
    end subroutine group_as_main_program

    ! Check for meaningful statements
    function check_for_meaningful_statements(tokens, start_pos) result(has_meaningful_content)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_pos
        logical :: has_meaningful_content
        integer :: check_pos

        has_meaningful_content = .false.
        do check_pos = start_pos, size(tokens)
            ! Skip EOF, newlines, comments
            if (tokens(check_pos)%kind == TK_EOF .or. &
                tokens(check_pos)%kind == TK_NEWLINE .or. &
                tokens(check_pos)%kind == TK_COMMENT) then
                cycle
            end if
            
            ! Look for executable statement patterns that warrant main program wrapping
            if (is_executable_statement_pattern(tokens, check_pos)) then
                has_meaningful_content = .true.
                exit
            end if
        end do
    end function check_for_meaningful_statements

    ! Check if token is executable statement pattern
    function is_executable_statement_pattern(tokens, pos) result(is_executable)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        logical :: is_executable

        is_executable = .false.
        
        ! Keywords that typically start executable statements
        if (tokens(pos)%kind == TK_KEYWORD) then
            select case (trim(tokens(pos)%text))
            case ("use", "call", "print", "write", "read", "stop", "end", &
                  "if", "do", "select", "where", "forall", "goto", &
                  "allocate", "deallocate", "assign", "pause", "return")
                is_executable = .true.
            case default
                ! Variable declarations and other non-executable keywords
                ! should not trigger main program wrapping by themselves
            end select
        ! Identifier could be start of assignment or procedure call
        else if (tokens(pos)%kind == TK_IDENTIFIER) then
            is_executable = .true.
        ! Numbers, strings, operators in unexpected context
        else if (tokens(pos)%kind == TK_NUMBER .or. &
                 tokens(pos)%kind == TK_STRING .or. &
                 tokens(pos)%kind == TK_OPERATOR) then
            is_executable = .true.
        end if
    end function is_executable_statement_pattern

    ! Handle unit edge cases
    subroutine handle_unit_edge_cases(tokens, start_pos, unit_start, unit_end)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_pos
        integer, intent(inout) :: unit_start, unit_end

        ! Skip empty lines (single EOF token on its own line)
        if (unit_end == unit_start .and. tokens(unit_start)%kind == TK_EOF) then
            unit_end = unit_start - 1  ! Signal to skip this unit
        end if

        ! Skip single "real", "integer", etc. that are part of function definitions
        if (should_skip_type_keyword(tokens, start_pos, unit_start, unit_end)) then
            unit_end = unit_start - 1  ! Signal to skip this unit - it's part of a function def
        end if
    end subroutine handle_unit_edge_cases

    ! Check if should skip type keyword
    function should_skip_type_keyword(tokens, start_pos, unit_start, unit_end) result(should_skip)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_pos, unit_start, unit_end
        logical :: should_skip

        should_skip = .false.
        
        if (unit_end == unit_start .and. start_pos < size(tokens) .and. &
            tokens(start_pos)%kind == TK_KEYWORD .and. &
       (tokens(start_pos)%text == "real" .or. tokens(start_pos)%text == "integer" .or. &
   tokens(start_pos)%text == "logical" .or. tokens(start_pos)%text == "character")) then
            ! Check if next token is "function"
            if (start_pos + 1 <= size(tokens) .and. &
                tokens(start_pos + 1)%kind == TK_KEYWORD .and. &
                tokens(start_pos + 1)%text == "function") then
                should_skip = .true.
            end if
        end if
    end function should_skip_type_keyword

    ! Find statement boundary (handles multi-line constructs)
    subroutine find_statement_boundary(tokens, start_pos, stmt_start, stmt_end)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_pos
        integer, intent(out) :: stmt_start, stmt_end
        integer :: i, nesting_level
        logical :: in_if_block, in_do_loop, in_select_case, in_function

        stmt_start = start_pos
        stmt_end = start_pos
        call initialize_statement_flags(in_if_block, in_do_loop, in_select_case, in_function)
        nesting_level = 0

        ! Check for multi-line constructs
        call detect_statement_constructs(tokens, start_pos, in_if_block, in_do_loop, &
                                       in_select_case, in_function, nesting_level)

        if (nesting_level > 0) then
            call find_multiline_statement_end(tokens, start_pos, stmt_end, &
                                            in_if_block, in_do_loop, in_select_case, &
                                            in_function, nesting_level)
        else
            call find_single_line_statement_end(tokens, start_pos, stmt_start, stmt_end)
        end if
    end subroutine find_statement_boundary

    ! Initialize statement flags
    subroutine initialize_statement_flags(in_if_block, in_do_loop, in_select_case, in_function)
        logical, intent(out) :: in_if_block, in_do_loop, in_select_case, in_function

        in_if_block = .false.
        in_do_loop = .false.
        in_select_case = .false.
        in_function = .false.
    end subroutine initialize_statement_flags

    ! Detect statement constructs
    subroutine detect_statement_constructs(tokens, start_pos, in_if_block, in_do_loop, &
                                         in_select_case, in_function, nesting_level)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_pos
        logical, intent(out) :: in_if_block, in_do_loop, in_select_case, in_function
        integer, intent(out) :: nesting_level

        if (is_if_then_start(tokens, start_pos)) then
            in_if_block = .true.
            nesting_level = 1
        else if (is_do_loop_start(tokens, start_pos) .or. &
                 is_do_while_start(tokens, start_pos)) then
            in_do_loop = .true.
            nesting_level = 1
        else if (is_select_case_start(tokens, start_pos)) then
            in_select_case = .true.
            nesting_level = 1
        else if (is_function_start(tokens, start_pos)) then
            in_function = .true.
            nesting_level = 1
        end if
    end subroutine detect_statement_constructs

    ! Find multiline statement end
    subroutine find_multiline_statement_end(tokens, start_pos, stmt_end, &
                                          in_if_block, in_do_loop, in_select_case, &
                                          in_function, nesting_level)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_pos
        integer, intent(inout) :: stmt_end
        logical, intent(in) :: in_if_block, in_do_loop, in_select_case, in_function
        integer, intent(inout) :: nesting_level
        integer :: i

        i = start_pos
        do while (i <= size(tokens) .and. nesting_level > 0)
            if (tokens(i)%kind == TK_EOF) exit

            ! Check for nested constructs
            if (i /= start_pos) then
                call check_nested_statement_constructs(tokens, i, in_if_block, in_do_loop, &
                                                     in_select_case, in_function, nesting_level)
            end if

            ! Check for end constructs
            call check_statement_end_constructs(tokens, i, stmt_end, in_if_block, &
                                              in_do_loop, in_select_case, in_function, &
                                              nesting_level)

            i = i + 1
        end do

        if (stmt_end == start_pos) then
            stmt_end = i - 1  ! Couldn't find matching end
        end if
    end subroutine find_multiline_statement_end

    ! Check nested statement constructs
    subroutine check_nested_statement_constructs(tokens, i, in_if_block, in_do_loop, &
                                                in_select_case, in_function, nesting_level)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: i
        logical, intent(in) :: in_if_block, in_do_loop, in_select_case, in_function
        integer, intent(inout) :: nesting_level

        if (in_if_block .and. is_if_then_start(tokens, i)) then
            nesting_level = nesting_level + 1
        else if (in_do_loop .and. (is_do_loop_start(tokens, i) .or. &
                                    is_do_while_start(tokens, i))) then
            nesting_level = nesting_level + 1
        else if (in_select_case .and. is_select_case_start(tokens, i)) then
            nesting_level = nesting_level + 1
        else if (in_function .and. is_function_start(tokens, i)) then
            nesting_level = nesting_level + 1
        end if
    end subroutine check_nested_statement_constructs

    ! Check statement end constructs
    subroutine check_statement_end_constructs(tokens, i, stmt_end, in_if_block, &
                                            in_do_loop, in_select_case, in_function, &
                                            nesting_level)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: i
        integer, intent(inout) :: stmt_end
        logical, intent(in) :: in_if_block, in_do_loop, in_select_case, in_function
        integer, intent(inout) :: nesting_level

        if (in_if_block .and. is_end_if(tokens, i)) then
            nesting_level = nesting_level - 1
            if (nesting_level == 0) then
                ! Check if it's "end if" (two tokens) or "endif" (one token)
                if (i + 1 <= size(tokens) .and. tokens(i)%text == "end" .and. &
         tokens(i + 1)%kind == TK_KEYWORD .and. tokens(i + 1)%text == "if") then
                    stmt_end = i + 1  ! Include both "end" and "if"
                else
                    stmt_end = i  ! Just "endif"
                end if
            end if
        else if (in_do_loop .and. is_end_do(tokens, i)) then
            nesting_level = nesting_level - 1
            if (nesting_level == 0) then
                stmt_end = i + 1  ! Include both "end" and "do"
            end if
        else if (in_select_case .and. is_end_select(tokens, i)) then
            nesting_level = nesting_level - 1
            if (nesting_level == 0) then
                stmt_end = i + 1  ! Include both "end" and "select"
            end if
        else if (in_function .and. is_end_function(tokens, i)) then
            nesting_level = nesting_level - 1
            if (nesting_level == 0) then
                stmt_end = i + 1  ! Include both "end" and "function"
            end if
        end if
    end subroutine check_statement_end_constructs

    ! Find single line statement end
    subroutine find_single_line_statement_end(tokens, start_pos, stmt_start, stmt_end)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_pos
        integer, intent(inout) :: stmt_start, stmt_end
        integer :: i, paren_depth

        ! Single-line statement - find end of line or comment
        ! Track parentheses to handle array literals (/ ... /)
        paren_depth = 0
        i = start_pos
        do while (i <= size(tokens))
            ! Track parentheses depth
            if (tokens(i)%kind == TK_OPERATOR) then
                if (tokens(i)%text == "(") then
                    paren_depth = paren_depth + 1
                else if (tokens(i)%text == ")") then
                    paren_depth = paren_depth - 1
                end if
            end if

            if (tokens(i)%kind == TK_EOF) then
                stmt_end = i - 1
                exit
            else if (tokens(i)%kind == TK_COMMENT .and. i > start_pos) then
                ! Stop before comment - let comment be parsed separately
                stmt_end = i - 1
                exit
            else if (i < size(tokens) .and. tokens(i)%line < tokens(i + 1)%line .and. &
                     paren_depth == 0) then
                ! Only end statement at line break if parentheses are balanced
                stmt_end = i
                exit
            else
                stmt_end = i
                i = i + 1
            end if
        end do
    end subroutine find_single_line_statement_end