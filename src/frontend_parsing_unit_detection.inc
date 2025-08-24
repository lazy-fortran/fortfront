    ! Helper functions to detect program unit types
    logical function is_function_start(tokens, pos)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos

        is_function_start = .false.
        if (pos < 1 .or. pos > size(tokens)) return

        ! Only detect function start at the beginning of a line/statement
        ! Check for "type function" pattern first
        if (tokens(pos)%kind == TK_KEYWORD .and. &
            (tokens(pos)%text == "real" .or. tokens(pos)%text == "integer" .or. &
             tokens(pos)%text == "logical" .or. tokens(pos)%text == "character")) then
            if (pos + 1 <= size(tokens) .and. &
                tokens(pos + 1)%kind == TK_KEYWORD .and. &
                tokens(pos + 1)%text == "function") then
                if (is_function_at_line_start(tokens, pos)) then
                    is_function_start = .true.
                end if
            end if
        else if (tokens(pos)%kind == TK_KEYWORD .and. tokens(pos)%text == "function") then
            if (is_standalone_function(tokens, pos)) then
                is_function_start = .true.
            end if
        end if
    end function is_function_start

    ! Check if function is at line start
    function is_function_at_line_start(tokens, pos) result(at_start)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        logical :: at_start

        at_start = .false.
        ! Check if this is at the start of a line or after a statement boundary
        if (pos == 1) then
            at_start = .true.
        else if (pos > 1 .and. tokens(pos - 1)%line < tokens(pos)%line) then
            at_start = .true.  ! New line
        else if (pos > 2 .and. tokens(pos - 2)%text == "end" .and. &
                 tokens(pos - 1)%text == "function") then
            at_start = .true.  ! After "end function"
        end if
    end function is_function_at_line_start

    ! Check if standalone function keyword
    function is_standalone_function(tokens, pos) result(is_standalone)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        logical :: is_standalone

        is_standalone = .false.
        ! Make sure this isn't the second part of "type function" or "end function"
        if (pos > 1) then
            if (tokens(pos - 1)%kind == TK_KEYWORD .and. &
       (tokens(pos - 1)%text == "real" .or. tokens(pos - 1)%text == "integer" .or. &
   tokens(pos - 1)%text == "logical" .or. tokens(pos - 1)%text == "character" .or. &
                 tokens(pos - 1)%text == "end")) then
                is_standalone = .false.  ! Already counted with the type or it's "end function"
            else
                is_standalone = .true.
            end if
        else
            is_standalone = .true.
        end if
    end function is_standalone_function

    logical function is_subroutine_start(tokens, pos)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos

        is_subroutine_start = .false.
        if (pos < 1 .or. pos > size(tokens)) return

        if (tokens(pos)%kind == TK_KEYWORD .and. tokens(pos)%text == "subroutine") then
            is_subroutine_start = .true.
        end if
    end function is_subroutine_start

    logical function is_module_start(tokens, pos)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos

        is_module_start = .false.
        if (pos < 1 .or. pos > size(tokens)) return

        if (tokens(pos)%kind == TK_KEYWORD .and. tokens(pos)%text == "module") then
            is_module_start = .true.
        end if
    end function is_module_start

    logical function is_program_start(tokens, pos)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos

        is_program_start = .false.
        if (pos < 1 .or. pos > size(tokens)) return

        if (tokens(pos)%kind == TK_KEYWORD .and. tokens(pos)%text == "program") then
            is_program_start = .true.
        end if
    end function is_program_start

    logical function is_end_function(tokens, pos)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos

        is_end_function = .false.
        if (pos + 1 > size(tokens)) return

        if (tokens(pos)%kind == TK_KEYWORD .and. tokens(pos)%text == "end" .and. &
       tokens(pos + 1)%kind == TK_KEYWORD .and. tokens(pos + 1)%text == "function") then
            is_end_function = .true.
        end if
    end function is_end_function

    logical function is_end_subroutine(tokens, pos)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos

        is_end_subroutine = .false.
        if (pos + 1 > size(tokens)) return

        if (tokens(pos)%kind == TK_KEYWORD .and. tokens(pos)%text == "end" .and. &
     tokens(pos + 1)%kind == TK_KEYWORD .and. tokens(pos + 1)%text == "subroutine") then
            is_end_subroutine = .true.
        end if
    end function is_end_subroutine

    logical function is_end_module(tokens, pos)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos

        is_end_module = .false.
        if (pos + 1 > size(tokens)) return

        if (tokens(pos)%kind == TK_KEYWORD .and. tokens(pos)%text == "end" .and. &
         tokens(pos + 1)%kind == TK_KEYWORD .and. tokens(pos + 1)%text == "module") then
            is_end_module = .true.
        end if
    end function is_end_module

    ! Check if token sequence starts a do loop
    logical function is_do_loop_start(tokens, pos)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos

        is_do_loop_start = .false.
        if (pos >= 1 .and. pos <= size(tokens)) then
            if (tokens(pos)%kind == TK_KEYWORD .and. tokens(pos)%text == "do") then
                ! Regular do loop (not do while)
                if (pos + 1 <= size(tokens)) then
      if (tokens(pos + 1)%kind == TK_KEYWORD .and. tokens(pos + 1)%text == "while") then
                        is_do_loop_start = .false.  ! It's a do while, not a regular do loop
                    else
                        is_do_loop_start = .true.
                    end if
                else
                    is_do_loop_start = .true.
                end if
            end if
        end if
    end function is_do_loop_start

    ! Check if token sequence starts a do while loop
    logical function is_do_while_start(tokens, pos)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos

        is_do_while_start = .false.
        if (pos >= 1 .and. pos <= size(tokens) - 1) then
            if (tokens(pos)%kind == TK_KEYWORD .and. tokens(pos)%text == "do" .and. &
          tokens(pos + 1)%kind == TK_KEYWORD .and. tokens(pos + 1)%text == "while") then
                is_do_while_start = .true.
            end if
        end if
    end function is_do_while_start

    ! Check if token sequence starts a select case
    logical function is_select_case_start(tokens, pos)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos

        is_select_case_start = .false.
        if (pos >= 1 .and. pos <= size(tokens) - 1) then
           if (tokens(pos)%kind == TK_KEYWORD .and. tokens(pos)%text == "select" .and. &
           tokens(pos + 1)%kind == TK_KEYWORD .and. tokens(pos + 1)%text == "case") then
                is_select_case_start = .true.
            end if
        end if
    end function is_select_case_start

    ! Check if token sequence ends a do loop
    logical function is_end_do(tokens, pos)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos

        is_end_do = .false.
        if (pos <= size(tokens) - 1) then
            if (tokens(pos)%kind == TK_KEYWORD .and. tokens(pos)%text == "end" .and. &
             tokens(pos + 1)%kind == TK_KEYWORD .and. tokens(pos + 1)%text == "do") then
                is_end_do = .true.
            end if
        end if
    end function is_end_do

    ! Check if token sequence ends a select case
    logical function is_end_select(tokens, pos)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos

        is_end_select = .false.
        if (pos <= size(tokens) - 1) then
            if (tokens(pos)%kind == TK_KEYWORD .and. tokens(pos)%text == "end" .and. &
         tokens(pos + 1)%kind == TK_KEYWORD .and. tokens(pos + 1)%text == "select") then
                is_end_select = .true.
            end if
        end if
    end function is_end_select

    logical function is_if_then_start(tokens, pos)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        integer :: i

        is_if_then_start = .false.
        if (pos < 1 .or. pos > size(tokens)) return

        ! Check if current token is "if"
        if (tokens(pos)%kind == TK_KEYWORD .and. tokens(pos)%text == "if") then
            ! Check if this is "else if" - if so, it's not a new if block for nesting purposes
            if (is_else_if_construct(tokens, pos)) then
                is_if_then_start = .false.
                return
            end if

            ! Look for "then" on the same line
            is_if_then_start = has_then_keyword(tokens, pos)
        end if
    end function is_if_then_start

    ! Check if this is else if construct
    function is_else_if_construct(tokens, pos) result(is_else_if)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        logical :: is_else_if

        is_else_if = .false.
        if (pos > 1 .and. pos <= size(tokens)) then
            if (tokens(pos - 1)%kind == TK_KEYWORD .and. &
                tokens(pos - 1)%text == "else" .and. &
                tokens(pos - 1)%line == tokens(pos)%line) then
                ! This is "else if", not a new if block
                is_else_if = .true.
            end if
        end if
    end function is_else_if_construct

    ! Check for then keyword
    function has_then_keyword(tokens, pos) result(has_then)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        logical :: has_then
        integer :: i

        has_then = .false.
        i = pos + 1
        do while (i <= size(tokens))
            if (tokens(i)%line /= tokens(pos)%line) exit
            if (tokens(i)%kind == TK_KEYWORD .and. tokens(i)%text == "then") then
                has_then = .true.
                exit
            end if
            i = i + 1
        end do
    end function has_then_keyword

    logical function is_end_if(tokens, pos)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos

        is_end_if = .false.
        if (pos > size(tokens)) return

        ! Check for "endif" (single keyword)
        if (tokens(pos)%kind == TK_KEYWORD .and. tokens(pos)%text == "endif") then
            is_end_if = .true.
            return
        end if

        ! Check for "end if" (two keywords)
        if (pos + 1 <= size(tokens)) then
            if (tokens(pos)%kind == TK_KEYWORD .and. tokens(pos)%text == "end" .and. &
             tokens(pos + 1)%kind == TK_KEYWORD .and. tokens(pos + 1)%text == "if") then
                is_end_if = .true.
            end if
        end if
    end function is_end_if