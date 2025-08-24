module frontend_parsing
    ! fortfront - Parsing functions module
    ! Contains all parsing-related functionality

    use lexer_core, only: token_t, TK_EOF, TK_KEYWORD, TK_COMMENT, TK_NEWLINE, &
                           TK_OPERATOR, TK_IDENTIFIER, TK_NUMBER, TK_STRING, TK_UNKNOWN
    use parser_state_module, only: parser_state_t, create_parser_state
    use parser_core, only: parse_function_definition
    use parser_dispatcher_module, only: parse_statement_dispatcher, &
                                           get_additional_indices, clear_additional_indices
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: program_node
    use ast_nodes_misc, only: comment_node
    use ast_factory, only: push_program
    use frontend_utilities, only: int_to_str

    implicit none
    private

    public :: parse_tokens
    ! Debug functions for unit testing
    public :: find_program_unit_boundary, is_function_start, is_end_function, &
              parse_program_unit
    public :: is_do_loop_start, is_do_while_start, is_select_case_start, &
              is_end_do, is_end_select
    public :: is_if_then_start, is_end_if

contains

    ! Create a container for multiple top-level program units
    function create_multi_unit_container(arena, unit_indices) result(container_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: unit_indices(:)
        integer :: container_index

        ! For now, use a program node with a special flag to indicate multiple units
        ! The code generator will handle this specially
        container_index = push_program(arena, "__MULTI_UNIT__", unit_indices, 1, 1)
    end function create_multi_unit_container

    ! Phase 2: Parsing
    subroutine parse_tokens(tokens, arena, prog_index, error_msg)
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(out) :: prog_index
        character(len=*), intent(out) :: error_msg

        ! Local variables for arena-based parsing
        integer, allocatable :: body_indices(:)
        integer :: stmt_index
        integer :: i, unit_start, unit_end, stmt_count
        type(token_t), allocatable :: unit_tokens(:)
        logical :: has_explicit_program_unit

        error_msg = ""
        stmt_count = 0
        allocate (body_indices(0))
        
        has_explicit_program_unit = detect_explicit_program_unit(tokens)

        ! Parse program units, not individual lines
        i = 1
        do while (i <= size(tokens))
            if (tokens(i)%kind == TK_EOF) exit

            ! Skip empty lines (just EOF tokens)
            if (i < size(tokens) .and. tokens(i)%kind == TK_EOF) then
                i = i + 1
                cycle
            end if

            ! Find program unit boundary
            call find_program_unit_boundary(tokens, i, unit_start, unit_end, &
                                           has_explicit_program_unit)

            ! Check if this unit has any meaningful content
            if (.not. unit_has_meaningful_content(tokens, unit_start, unit_end)) then
                i = unit_end + 1
                cycle
            end if

            ! Process unit if it has meaningful content
            if (should_process_unit(tokens, unit_start, unit_end)) then
                call process_program_unit(tokens, unit_start, unit_end, arena, &
                                        has_explicit_program_unit, stmt_index)

                if (stmt_index > 0) then
                    ! Add to body indices
                    body_indices = [body_indices, stmt_index]
                    stmt_count = stmt_count + 1
                end if
            end if

            i = unit_end + 1

            ! For lazy fortran without explicit program units,
            ! parse_all_statements has already processed everything
            if (.not. has_explicit_program_unit .and. unit_end >= size(tokens) - 1) then
                exit  ! We've processed all tokens
            end if
        end do

        ! Create final program structure
        call create_final_program_structure(arena, body_indices, stmt_count, &
                                          has_explicit_program_unit, prog_index, error_msg)
    end subroutine parse_tokens

    ! Detect if file has explicit program unit
    function detect_explicit_program_unit(tokens) result(has_explicit_program_unit)
        type(token_t), intent(in) :: tokens(:)
        logical :: has_explicit_program_unit
        integer :: i

        has_explicit_program_unit = .false.

        ! Check if file starts with explicit 'program', 'module', &
        ! 'function', or 'subroutine' statement
        do i = 1, size(tokens)
            if (tokens(i)%kind == TK_KEYWORD) then
                if (tokens(i)%text == "program" .or. tokens(i)%text == "module") then
                    has_explicit_program_unit = .true.
                    exit  ! Found explicit program unit
                else if (tokens(i)%text == "function" .or. tokens(i)%text == "subroutine") then
                    if (is_program_unit_start(tokens, i)) then
                        has_explicit_program_unit = .true.
                        exit
                    end if
                else
                    ! Found other keyword, not a program unit
                    exit
                end if
            else if (tokens(i)%kind /= TK_EOF) then
                exit  ! Stop at first non-EOF token
            end if
        end do
    end function detect_explicit_program_unit

    ! Check if function/subroutine is at program unit start
    function is_program_unit_start(tokens, i) result(is_start)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: i
        logical :: is_start

        is_start = .false.
        
        if (i == 1) then
            is_start = .true.
        else if (i > 1 .and. tokens(i - 1)%line < tokens(i)%line) then
            ! At start of file or start of new line
            is_start = .true.
        else if (i > 1 .and. tokens(i - 1)%kind == TK_KEYWORD .and. &
               (tokens(i - 1)%text == "real" .or. tokens(i - 1)%text == "integer" .or. &
           tokens(i - 1)%text == "logical" .or. tokens(i - 1)%text == "character")) then
            ! Type prefixed function/subroutine
            is_start = .true.
        end if
    end function is_program_unit_start

    ! Check if unit has meaningful content
    function unit_has_meaningful_content(tokens, unit_start, unit_end) result(has_content)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: unit_start, unit_end
        logical :: has_content
        integer :: j

        has_content = .false.
        do j = unit_start, unit_end
            if (tokens(j)%kind /= TK_EOF .and. &
                tokens(j)%kind /= TK_NEWLINE) then
                has_content = .true.
                exit
            end if
        end do
    end function unit_has_meaningful_content

    ! Check if unit should be processed
    function should_process_unit(tokens, unit_start, unit_end) result(should_process)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: unit_start, unit_end
        logical :: should_process

        ! Skip empty units, units with just EOF, or single-token keywords
        ! that are part of larger constructs
        should_process = (unit_end >= unit_start .and. &
          .not. (unit_end == unit_start .and. tokens(unit_start)%kind == TK_EOF) .and. &
       .not. (unit_end == unit_start .and. tokens(unit_start)%kind == TK_KEYWORD .and. &
     (tokens(unit_start)%text == "real" .or. tokens(unit_start)%text == "integer" .or. &
 tokens(unit_start)%text == "logical" .or. tokens(unit_start)%text == "character" .or. &
                            tokens(unit_start)%text == "function" .or. &
                            tokens(unit_start)%text == "subroutine" .or. &
        tokens(unit_start)%text == "module" .or. tokens(unit_start)%text == "end" .or. &
      tokens(unit_start)%text == "else" .or. tokens(unit_start)%text == "elseif")))
    end function should_process_unit

    ! Process a program unit
    subroutine process_program_unit(tokens, unit_start, unit_end, arena, &
                                  has_explicit_program_unit, stmt_index)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: unit_start, unit_end
        type(ast_arena_t), intent(inout) :: arena
        logical, intent(in) :: has_explicit_program_unit
        integer, intent(out) :: stmt_index

        type(token_t), allocatable :: unit_tokens(:)

        ! Extract unit tokens and add EOF
        allocate (unit_tokens(unit_end - unit_start + 2))
        unit_tokens(1:unit_end - unit_start + 1) = tokens(unit_start:unit_end)
        ! Add EOF token
        unit_tokens(unit_end - unit_start + 2)%kind = TK_EOF
        unit_tokens(unit_end - unit_start + 2)%text = ""
        unit_tokens(unit_end - unit_start + 2)%line = tokens(unit_end)%line
        unit_tokens(unit_end - unit_start + 2)%column = tokens(unit_end)%column + 1

        ! Parse the program unit
        stmt_index = parse_program_unit(unit_tokens, arena, &
                                       has_explicit_program_unit)

        deallocate (unit_tokens)
    end subroutine process_program_unit

    ! Create final program structure
    subroutine create_final_program_structure(arena, body_indices, stmt_count, &
                                            has_explicit_program_unit, prog_index, error_msg)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: body_indices(:)
        integer, intent(in) :: stmt_count
        logical, intent(in) :: has_explicit_program_unit
        integer, intent(out) :: prog_index
        character(len=*), intent(out) :: error_msg

        ! Create program node with collected body indices
        ! Only wrap in implicit program if there's no explicit program unit
        ! (program/module/function/subroutine)
        if (.not. has_explicit_program_unit) then
            ! For lazy fortran, parse_all_statements already created the program node
            if (stmt_count > 0) then
                prog_index = body_indices(1)  ! This is the program node from &
                                             ! parse_all_statements
            else
                error_msg = "No statements found in file"
                prog_index = 0
            end if
        else if (stmt_count > 0) then
            ! Handle multiple top-level program units (modules, programs, etc.)
            call handle_multiple_program_units(arena, body_indices, prog_index, error_msg)
        else
            ! No program unit found
            error_msg = "No program unit found in file"
            prog_index = 0
        end if
    end subroutine create_final_program_structure

    ! Handle multiple program units
    subroutine handle_multiple_program_units(arena, body_indices, prog_index, error_msg)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: body_indices(:)
        integer, intent(out) :: prog_index
        character(len=*), intent(out) :: error_msg

        ! Filter out empty implicit main programs that might have been created
        ! due to Issue #321 mixed construct handling
        block
            integer, allocatable :: filtered_indices(:)
            integer :: i, filtered_count
            
            allocate(filtered_indices(0))
            filtered_count = 0
            
            do i = 1, size(body_indices)
                if (body_indices(i) > 0 .and. body_indices(i) <= arena%size) then
                    if (should_include_program_unit(arena, body_indices(i))) then
                        ! Add this unit to filtered list
                        filtered_indices = [filtered_indices, body_indices(i)]
                        filtered_count = filtered_count + 1
                    end if
                end if
            end do
            
            ! Use filtered indices
            if (filtered_count == 1) then
                ! Single program unit - use it directly
                prog_index = filtered_indices(1)
            else if (filtered_count > 1) then
                ! Multiple program units - create a special multi-unit container
                prog_index = create_multi_unit_container(arena, filtered_indices)
            else
                ! No meaningful units left after filtering
                error_msg = "No meaningful program units found after filtering"
                prog_index = 0
            end if
        end block
    end subroutine handle_multiple_program_units

    ! Check if program unit should be included
    function should_include_program_unit(arena, unit_index) result(should_include)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: unit_index
        logical :: should_include

        should_include = .true.

        ! Check if this is an empty main program
        select type (node => arena%entries(unit_index)%node)
        type is (program_node)
            if (node%name == "main") then
                if (is_empty_main_program(node, arena)) then
                    should_include = .false.
                end if
            end if
        end select
    end function should_include_program_unit

    ! Check if main program is empty
    function is_empty_main_program(node, arena) result(is_empty)
        type(program_node), intent(in) :: node
        type(ast_arena_t), intent(in) :: arena
        logical :: is_empty

        is_empty = .false.

        ! Check if program has no meaningful content
        if (.not. allocated(node%body_indices) .or. &
            size(node%body_indices) == 0) then
            ! Skip empty main program
            is_empty = .true.
        else
            ! Check if all statements are meaningless (just EOF/whitespace)
            block
                integer :: j
                logical :: has_meaningful_content
                has_meaningful_content = .false.
                
                do j = 1, size(node%body_indices)
                    if (node%body_indices(j) > 0 .and. &
                        node%body_indices(j) <= arena%size) then
                        select type (stmt_node => arena%entries(node%body_indices(j))%node)
                        type is (comment_node)
                            ! Comments don't count
                        class default
                            ! Any non-comment node counts as meaningful
                            has_meaningful_content = .true.
                            exit
                        end select
                    end if
                end do
                
                if (.not. has_meaningful_content) then
                    ! Skip empty main program
                    is_empty = .true.
                end if
            end block
        end if
    end function is_empty_main_program

    ! Parse a program unit (function, subroutine, module, or statements)
    function parse_program_unit(tokens, arena, has_explicit_program) result(unit_index)
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        logical, intent(in) :: has_explicit_program
        integer :: unit_index

        ! Check for meaningful content first
        if (not_meaningful_program_unit(tokens)) then
            unit_index = 0
            return
        end if

        ! Determine unit type and parse accordingly
        if (is_function_start(tokens, 1)) then
            unit_index = parse_function_unit(tokens, arena)
        else if (is_subroutine_start(tokens, 1)) then
            unit_index = parse_subroutine_unit(tokens, arena)
        else if (is_module_start(tokens, 1)) then
            unit_index = parse_statement_dispatcher(tokens, arena)
        else if (is_program_start(tokens, 1)) then
            unit_index = parse_statement_dispatcher(tokens, arena)
        else
            unit_index = parse_implicit_main_program(tokens, arena, has_explicit_program)
        end if
    end function parse_program_unit

    ! Check if program unit has meaningful content
    function not_meaningful_program_unit(tokens) result(not_meaningful)
        type(token_t), intent(in) :: tokens(:)
        logical :: not_meaningful
        logical :: has_content
        integer :: i

        ! Check if tokens contain any real content (not just comments/EOF/newlines)
        has_content = .false.
        do i = 1, size(tokens)
            if (tokens(i)%kind /= TK_COMMENT .and. tokens(i)%kind /= TK_EOF .and. &
                tokens(i)%kind /= TK_NEWLINE) then
                has_content = .true.
                exit
            end if
        end do

        ! For lazy Fortran, comments-only input should still create a program unit
        ! Only skip if there are no tokens at all (completely empty)
        not_meaningful = (.not. has_content .and. size(tokens) <= 1)
    end function not_meaningful_program_unit

    ! Parse function unit
    function parse_function_unit(tokens, arena) result(unit_index)
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        integer :: unit_index

        ! Multi-line function definition
        block
            type(parser_state_t) :: parser
            parser = create_parser_state(tokens)
            unit_index = parse_function_definition(parser, arena)
        end block
    end function parse_function_unit

    ! Parse subroutine unit
    function parse_subroutine_unit(tokens, arena) result(unit_index)
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        integer :: unit_index

        ! Subroutine definition - wrap in program like functions
        block
            integer :: subroutine_index, prog_index
            subroutine_index = parse_statement_dispatcher(tokens, arena)
            
            ! Wrap standalone subroutine in program structure for standard compliance
            if (subroutine_index > 0) then
                prog_index = push_program(arena, "main", [subroutine_index])
                unit_index = prog_index
            else
                unit_index = subroutine_index
            end if
        end block
    end function parse_subroutine_unit

    ! Parse implicit main program
    function parse_implicit_main_program(tokens, arena, has_explicit_program) &
            result(unit_index)
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        logical, intent(in) :: has_explicit_program
        integer :: unit_index

        ! For mixed module/main program files, we need to parse implicit main programs
        ! even when explicit program units exist elsewhere in the file.
        if (has_executable_statements(tokens)) then
            unit_index = parse_all_statements(tokens, arena)
        else
            unit_index = 0
        end if
    end function parse_implicit_main_program

    ! Check if tokens have executable statements
    function has_executable_statements(tokens) result(has_executable_content)
        type(token_t), intent(in) :: tokens(:)
        logical :: has_executable_content
        integer :: k

        has_executable_content = .false.
        do k = 1, size(tokens)
            ! Skip EOF, newlines, comments
            if (tokens(k)%kind == TK_EOF .or. &
                tokens(k)%kind == TK_NEWLINE .or. &
                tokens(k)%kind == TK_COMMENT) then
                cycle
            end if
            
            ! Look for executable statement patterns
            if (tokens(k)%kind == TK_KEYWORD) then
                select case (trim(tokens(k)%text))
                case ("use", "call", "print", "write", "read", "stop", "end", &
                      "if", "do", "select", "where", "forall", "goto", &
                      "allocate", "deallocate", "assign", "pause", "return")
                    has_executable_content = .true.
                    exit
                case default
                    ! Declaration keywords and other non-executable patterns
                    ! should not trigger main program creation by themselves
                end select
            ! Identifier could be start of assignment or procedure call
            else if (tokens(k)%kind == TK_IDENTIFIER) then
                has_executable_content = .true.
                exit
            ! Numbers, strings, operators in unexpected context
            else if (tokens(k)%kind == TK_NUMBER .or. &
                     tokens(k)%kind == TK_STRING .or. &
                     tokens(k)%kind == TK_OPERATOR) then
                has_executable_content = .true.
                exit
            end if
        end do
    end function has_executable_statements

    ! Parse all statements in a token array (for lazy fortran)
    function parse_all_statements(tokens, arena) result(prog_index)
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        integer :: prog_index
        integer, allocatable :: body_indices(:)
        integer :: i, stmt_start, stmt_end, stmt_index
        type(token_t), allocatable :: stmt_tokens(:)

        allocate (body_indices(0))

        ! Parse each statement
        i = 1
        do while (i <= size(tokens))
            if (tokens(i)%kind == TK_EOF) then
                exit
            end if

            ! Special handling for comments - treat as standalone statements
            if (tokens(i)%kind == TK_COMMENT) then
                call process_comment_statement(tokens, i, arena, stmt_index, body_indices)
                i = i + 1
                cycle
            end if

            ! Find statement boundary
            call find_statement_boundary(tokens, i, stmt_start, stmt_end)

            if (stmt_end >= stmt_start) then
                call process_regular_statement(tokens, stmt_start, stmt_end, &
                                             arena, stmt_index, body_indices)
            end if

            i = stmt_end + 1

            ! Check bounds
            if (i > size(tokens)) then
                exit
            end if
        end do

        ! Create program node with all statements
        ! Only create program node if we have actual statements
        if (size(body_indices) > 0) then
            prog_index = push_program(arena, "main", body_indices, 1, 1)
        else
            prog_index = 0
        end if
    end function parse_all_statements

    ! Process comment statement
    subroutine process_comment_statement(tokens, i, arena, stmt_index, body_indices)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: i
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(out) :: stmt_index
        integer, allocatable, intent(inout) :: body_indices(:)
        type(token_t), allocatable :: stmt_tokens(:)

        ! Create a token array with just the comment and EOF
        allocate (stmt_tokens(2))
        stmt_tokens(1) = tokens(i)
        stmt_tokens(2)%kind = TK_EOF
        stmt_tokens(2)%text = ""
        stmt_tokens(2)%line = tokens(i)%line
        stmt_tokens(2)%column = tokens(i)%column + len(tokens(i)%text)

        ! Parse the comment
        stmt_index = parse_statement_dispatcher(stmt_tokens, arena)
        if (stmt_index > 0) then
            body_indices = [body_indices, stmt_index]
        end if

        deallocate (stmt_tokens)
    end subroutine process_comment_statement

    ! Process regular statement
    subroutine process_regular_statement(tokens, stmt_start, stmt_end, arena, &
                                       stmt_index, body_indices)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: stmt_start, stmt_end
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(out) :: stmt_index
        integer, allocatable, intent(inout) :: body_indices(:)
        type(token_t), allocatable :: stmt_tokens(:)

        ! Extract statement tokens
        allocate (stmt_tokens(stmt_end - stmt_start + 2))
        stmt_tokens(1:stmt_end - stmt_start + 1) = tokens(stmt_start:stmt_end)
        ! Add EOF token
        stmt_tokens(stmt_end - stmt_start + 2)%kind = TK_EOF
        stmt_tokens(stmt_end - stmt_start + 2)%text = ""
        stmt_tokens(stmt_end - stmt_start + 2)%line = tokens(stmt_end)%line
        stmt_tokens(stmt_end - stmt_start + 2)%column = tokens(stmt_end)%column + 1

        ! Parse the statement
        stmt_index = parse_statement_dispatcher(stmt_tokens, arena)
        if (stmt_index > 0) then
            body_indices = [body_indices, stmt_index]
            
            ! Handle additional indices from multi-declaration parsing
            block
                integer, allocatable :: extra_indices(:)
                extra_indices = get_additional_indices()
                if (size(extra_indices) > 0) then
                    body_indices = [body_indices, extra_indices]
                end if
                call clear_additional_indices()
            end block
        end if

        deallocate (stmt_tokens)
    end subroutine process_regular_statement

    include 'frontend_parsing_unit_detection.inc'
    include 'frontend_parsing_boundary_detection.inc'

end module frontend_parsing