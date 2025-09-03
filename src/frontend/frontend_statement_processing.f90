module frontend_statement_processing
    ! Statement-level parsing and processing functionality
    ! Handles parsing of all statements into a program structure

    use lexer_core, only: token_t, TK_EOF, TK_KEYWORD, TK_COMMENT, TK_NEWLINE, &
                           TK_OPERATOR, TK_IDENTIFIER, TK_NUMBER, TK_STRING, TK_UNKNOWN
    use parser_dispatcher_module, only: parse_statement_dispatcher, &
                                        get_additional_indices, clear_additional_indices
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: program_node
    use ast_nodes_misc, only: comment_node
    use ast_factory, only: push_program
    use frontend_utilities, only: int_to_str

    implicit none
    private

    ! Public statement processing interface  
    public :: parse_all_statements, parse_explicit_program_unit
    public :: process_comment_statement, process_regular_statement
    public :: create_final_program_structure, handle_multiple_program_units
    public :: should_include_program_unit, is_empty_main_program

    ! Statement boundary detection
    public :: find_statement_boundary

contains

    ! Parse all statements into a program block
    function parse_all_statements(tokens, arena) result(prog_index)
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        integer :: prog_index
        
        integer, allocatable :: body_indices(:)
        integer :: i, stmt_start, stmt_end, stmt_index, stmt_count
        
        allocate(body_indices(0))
        stmt_count = 0
        i = 1
        
        ! Process all statements
        do while (i <= size(tokens))
            if (tokens(i)%kind == TK_EOF) exit
            
            call find_statement_boundary(tokens, i, stmt_start, stmt_end)
            
            if (tokens(stmt_start)%kind == TK_COMMENT) then
                call process_comment_statement(tokens, stmt_start, arena, stmt_index, body_indices)
            else
                call process_regular_statement(tokens, stmt_start, stmt_end, arena, &
                                             stmt_index, body_indices)
            end if
            
            if (stmt_index > 0) stmt_count = stmt_count + 1
            i = stmt_end + 1
        end do
        
        ! Create final program structure
        call create_final_program_structure(arena, body_indices, stmt_count, prog_index)
    end function parse_all_statements

    ! Process comment statement
    subroutine process_comment_statement(tokens, i, arena, stmt_index, body_indices)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: i
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(out) :: stmt_index
        integer, allocatable, intent(inout) :: body_indices(:)
        type(token_t), allocatable :: stmt_tokens(:)

        ! Create tokens for comment parsing
        allocate (stmt_tokens(2))
        stmt_tokens(1) = tokens(i)
        ! Add EOF token
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

        ! Skip empty statements (can happen with consecutive semicolons)
        if (stmt_end < stmt_start) then
            stmt_index = 0
            return
        end if

        ! Heuristic: skip non-Fortran prefixes like "Simple test:" before real code (fixes #843)
        block
            integer :: i, colon_pos, eq_pos, local_start, paren_depth
            logical :: saw_keyword
            colon_pos = 0
            eq_pos = 0
            saw_keyword = .false.
            ! Local adjustable start to avoid modifying INTENT(IN) argument
            local_start = stmt_start
            paren_depth = 0

            ! Locate first '=' in the statement (assignment anchor)
            do i = local_start, stmt_end
                if (tokens(i)%kind == TK_KEYWORD) then
                    saw_keyword = .true.
                end if
                if (tokens(i)%kind == TK_OPERATOR .and. tokens(i)%text == "=") then
                    eq_pos = i
                    exit
                end if
            end do

            if (eq_pos > 0) then
                ! If there's a ':' before '=', and the prefix contains no Fortran
                ! keywords, treat everything up to and including ':' as a non-Fortran
                ! label and skip it. Do NOT do this when ':' is inside parentheses
                ! (e.g., array slices/substrings like a(1:3) or s(2:4)).
                do i = local_start, eq_pos - 1
                    if (tokens(i)%kind == TK_OPERATOR) then
                        select case (tokens(i)%text)
                        case ("(")
                            paren_depth = paren_depth + 1
                        case (")")
                            if (paren_depth > 0) paren_depth = paren_depth - 1
                        case (":")
                            if (paren_depth == 0) then
                                colon_pos = i
                                exit
                            end if
                        end select
                    end if
                end do
                if (colon_pos > 0 .and. .not. saw_keyword) then
                    local_start = colon_pos + 1
                end if
            end if
        
        ! Extract statement tokens (after any prefix adjustment)
        allocate (stmt_tokens(stmt_end - local_start + 2))
        stmt_tokens(1:stmt_end - local_start + 1) = tokens(local_start:stmt_end)
        ! Add EOF token
        stmt_tokens(stmt_end - local_start + 2)%kind = TK_EOF
        stmt_tokens(stmt_end - local_start + 2)%text = ""
        stmt_tokens(stmt_end - local_start + 2)%line = tokens(stmt_end)%line
        stmt_tokens(stmt_end - local_start + 2)%column = tokens(stmt_end)%column + 1
        end block

        ! Note: stmt_tokens already allocated and filled in the block above

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

    ! Create final program structure from parsed statements
    subroutine create_final_program_structure(arena, body_indices, stmt_count, &
                                            prog_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable, intent(in) :: body_indices(:)
        integer, intent(in) :: stmt_count
        integer, intent(out) :: prog_index
        
        character(len=:), allocatable :: prog_name

        if (size(body_indices) == 0) then
            ! Empty program
            prog_index = push_program(arena, "main", [integer::], 1, 1)
        else if (stmt_count == 1) then
            ! Single statement program
            prog_name = "main"
            prog_index = push_program(arena, prog_name, body_indices, 1, 1)
        else
            ! Multi-statement program
            prog_name = "main"
            prog_index = push_program(arena, prog_name, body_indices, 1, 1)
        end if
    end subroutine create_final_program_structure

    ! Handle multiple program units
    subroutine handle_multiple_program_units(arena, body_indices, prog_index, error_msg)
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable, intent(in) :: body_indices(:)
        integer, intent(out) :: prog_index
        character(len=*), intent(out) :: error_msg
        
        integer, allocatable :: valid_units(:)
        integer :: i, valid_count
        
        error_msg = ""
        valid_count = 0
        allocate(valid_units(size(body_indices)))
        
        ! Filter out empty or invalid units
        do i = 1, size(body_indices)
            if (should_include_program_unit(arena, body_indices(i))) then
                valid_count = valid_count + 1
                valid_units(valid_count) = body_indices(i)
            end if
        end do
        
        if (valid_count == 0) then
            ! No valid units - create empty main program
            prog_index = push_program(arena, "main", [integer::], 1, 1)
        else if (valid_count == 1) then
            ! Single unit - check if it's already a program node
            if (allocated(arena%entries(valid_units(1))%node)) then
                select type (node => arena%entries(valid_units(1))%node)
                type is (program_node)
                    ! Already a program node
                    prog_index = valid_units(1)
                class default
                    ! Wrap in program node for consistent API
                    prog_index = push_program(arena, "main", valid_units(1:1), 1, 1)
                end select
            else
                ! Safety fallback  
                prog_index = push_program(arena, "main", [integer::], 1, 1)
            end if
        else
            ! Multiple units - create container
            prog_index = push_program(arena, "__MULTI_UNIT__", valid_units(1:valid_count), 1, 1)
        end if
        
        deallocate(valid_units)
    end subroutine handle_multiple_program_units

    ! Check if program unit should be included
    function should_include_program_unit(arena, unit_index) result(should_include)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: unit_index
        logical :: should_include
        
        should_include = .true.
        
        if (unit_index <= 0 .or. unit_index > size(arena%entries)) then
            should_include = .false.
            return
        end if
        
        ! Check for empty main programs
        if (is_empty_main_program(arena%entries(unit_index)%node, arena)) then
            should_include = .false.
        end if
    end function should_include_program_unit

    ! Check if node is an empty main program
    function is_empty_main_program(node, arena) result(is_empty)
        class(*), intent(in) :: node
        type(ast_arena_t), intent(in) :: arena
        logical :: is_empty
        
        is_empty = .false.
        
        select type (prog_node => node)
        type is (program_node)
            if ((prog_node%name == "main" .or. prog_node%name == "__IMPLICIT_MAIN__") .and. &
                size(prog_node%body_indices) == 0) then
                is_empty = .true.
            end if
        end select
    end function is_empty_main_program

    ! Parse explicit program unit
    function parse_explicit_program_unit(tokens, arena) result(prog_index)
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        integer :: prog_index

        ! Parse explicit program statement
        prog_index = parse_statement_dispatcher(tokens, arena)
    end function parse_explicit_program_unit

    ! Find statement boundary (control-flow aware)
    subroutine find_statement_boundary(tokens, start_pos, stmt_start, stmt_end)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_pos
        integer, intent(out) :: stmt_start, stmt_end
        
        integer :: i, nesting_level
        logical :: is_multiline_construct
        
        stmt_start = start_pos
        stmt_end = start_pos
        is_multiline_construct = .false.
        nesting_level = 0
        
        ! Skip leading newlines
        stmt_start = start_pos
        do while (stmt_start <= size(tokens) .and. tokens(stmt_start)%kind == TK_NEWLINE)
            stmt_start = stmt_start + 1
        end do
        
        if (stmt_start > size(tokens)) then
            stmt_end = size(tokens)
            return
        end if
        
        ! Check if this starts a multi-line control flow construct
        if (tokens(stmt_start)%kind == TK_KEYWORD) then
            select case (tokens(stmt_start)%text)
            case ("if")
                ! Check if it's if/then (multi-line) by looking ahead
                do i = stmt_start + 1, min(stmt_start + 20, size(tokens))
                    if (tokens(i)%kind == TK_KEYWORD .and. tokens(i)%text == "then") then
                        is_multiline_construct = .true.
                        nesting_level = 1
                        exit
                    else if (tokens(i)%kind == TK_NEWLINE) then
                        exit  ! Single-line if
                    end if
                end do
            case ("do")
                is_multiline_construct = .true.
                nesting_level = 1
            case ("select")
                is_multiline_construct = .true.
                nesting_level = 1
            case ("where")
                ! Check if it's where construct (has newline before end where)
                do i = stmt_start + 1, min(stmt_start + 20, size(tokens))
                    if (tokens(i)%kind == TK_NEWLINE) then
                        ! Might be multi-line where construct
                        is_multiline_construct = .true.
                        nesting_level = 1
                        exit
                    else if (tokens(i)%kind == TK_KEYWORD .and. &
                            (tokens(i)%text == "end" .or. tokens(i)%text == "elsewhere")) then
                        is_multiline_construct = .true.
                        nesting_level = 1
                        exit
                    end if
                end do
            end select
        end if
        
        if (is_multiline_construct) then
            ! Find the matching end construct
            stmt_end = stmt_start
            do i = stmt_start, size(tokens)
                if (tokens(i)%kind == TK_EOF) then
                    stmt_end = i - 1
                    exit
                end if
                
                if (tokens(i)%kind == TK_KEYWORD) then
                    select case (tokens(i)%text)
                    ! Handle nested constructs
                    case ("if")
                        if (i > stmt_start) then
                            ! Check if it's if/then (nested)
                            block
                                integer :: j
                                do j = i + 1, min(i + 20, size(tokens))
                                    if (tokens(j)%kind == TK_KEYWORD .and. tokens(j)%text == "then") then
                                        nesting_level = nesting_level + 1
                                        exit
                                    else if (tokens(j)%kind == TK_NEWLINE) then
                                        exit
                                    end if
                                end do
                            end block
                        end if
                    case ("do")
                        if (i > stmt_start) then
                            nesting_level = nesting_level + 1
                        end if
                    case ("select")
                        if (i > stmt_start) then
                            nesting_level = nesting_level + 1
                        end if
                    
                    ! Handle end constructs
                    case ("endif", "end")
                        if (tokens(stmt_start)%text == "if") then
                            if (tokens(i)%text == "endif") then
                                nesting_level = nesting_level - 1
                                if (nesting_level == 0) then
                                    stmt_end = i
                                    exit
                                end if
                            else if (tokens(i)%text == "end" .and. i + 1 <= size(tokens) .and. &
                                    tokens(i + 1)%kind == TK_KEYWORD .and. tokens(i + 1)%text == "if") then
                                nesting_level = nesting_level - 1
                                if (nesting_level == 0) then
                                    stmt_end = i + 1
                                    exit
                                end if
                            end if
                        end if
                    case ("enddo")
                        if (tokens(stmt_start)%text == "do") then
                            nesting_level = nesting_level - 1
                            if (nesting_level == 0) then
                                stmt_end = i
                                exit
                            end if
                        end if
                    end select
                    
                    ! Check for two-word end constructs
                    if (tokens(i)%text == "end") then
                        if (i + 1 <= size(tokens) .and. tokens(i + 1)%kind == TK_KEYWORD) then
                            if (tokens(i + 1)%text == "do" .and. tokens(stmt_start)%text == "do") then
                                nesting_level = nesting_level - 1
                                if (nesting_level == 0) then
                                    stmt_end = i + 1
                                    exit
                                end if
                            else if (tokens(i + 1)%text == "select" .and. tokens(stmt_start)%text == "select") then
                                nesting_level = nesting_level - 1
                                if (nesting_level == 0) then
                                    stmt_end = i + 1
                                    exit
                                end if
                            else if (tokens(i + 1)%text == "where" .and. tokens(stmt_start)%text == "where") then
                                nesting_level = nesting_level - 1
                                if (nesting_level == 0) then
                                    stmt_end = i + 1
                                    exit
                                end if
                            end if
                        end if
                    end if
                end if
                
                stmt_end = i
            end do
        else
            ! Single-line statement - find end at newline or semicolon
            do i = stmt_start, size(tokens)
                if (tokens(i)%kind == TK_EOF) then
                    stmt_end = i - 1
                    exit
                else if (tokens(i)%kind == TK_NEWLINE .or. &
                        (tokens(i)%kind == TK_OPERATOR .and. tokens(i)%text == ";")) then
                    stmt_end = i - 1
                    exit
                else if (tokens(i)%kind /= TK_COMMENT) then
                    stmt_end = i
                end if
            end do
        end if
        
        ! Ensure we don't go beyond bounds
        if (stmt_end > size(tokens)) stmt_end = size(tokens)
        if (stmt_end < stmt_start) stmt_end = stmt_start
    end subroutine find_statement_boundary

end module frontend_statement_processing
