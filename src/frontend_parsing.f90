module frontend_parsing
    ! fortfront - Parsing functions module (refactored for SRP compliance)
    ! Now serves as a compatibility layer over split modules

    use lexer_core, only: token_t, TK_EOF, TK_KEYWORD, TK_COMMENT, TK_NEWLINE, &
                          TK_OPERATOR, TK_IDENTIFIER, TK_NUMBER, TK_STRING, &
                          TK_UNKNOWN, &
                          TK_WHITESPACE, to_lower
    use parser_state_module, only: parser_state_t, create_parser_state
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: program_node
    use ast_nodes_data, only: module_node, block_data_node
    use ast_nodes_misc, only: interface_block_node
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node
    use ast_nodes_transfer, only: entry_node
    use ast_factory, only: push_program
    use iso_fortran_env, only: error_unit
    use frontend_utilities, only: int_to_str, is_type_start
    use parser_do_constructs_module, only: ensure_if_do_registration
    use mixed_construct_detector, only: detect_mixed_constructs, &
                                        mixed_construct_result_t
    use error_handling, only: result_t
    use parser_dispatcher_module, only: clear_parser_errors

    ! Import from split modules
    use frontend_program_units, only: parse_program_unit, parse_module_unit, &
                                      parse_function_unit, parse_subroutine_unit, &
                                      parse_type_unit, parse_explicit_program_unit, &
                                      parse_implicit_main_program, &
                                      not_meaningful_program_unit, &
                                      has_any_non_comment_content, &
                                      has_executable_statements
    use frontend_statement_processing, only: parse_all_statements, &
                                             process_comment_statement, &
                                             process_regular_statement, &
                                             create_final_program_structure, &
                                             handle_multiple_program_units, &
                                             should_include_program_unit, &
                                             is_empty_main_program, &
                                             find_statement_boundary
    use frontend_mixed_constructs, only: parse_mixed_constructs, &
                                         create_mixed_construct_container_arena, &
                                         parse_declaration_range, parse_program_range
    use parser_procedure_definition_bodies_module, only: &
        reset_nested_internal_procedure_error, &
        has_nested_internal_procedure_error, &
        get_nested_internal_procedure_message

    implicit none
    private

    ! Parse result type combining result_t with program index
    type, public :: parse_result_with_index_t
        type(result_t) :: result
        integer :: prog_index = 0
    end type parse_result_with_index_t

    ! Main public interface (preserved for compatibility)
    public :: parse_tokens, parse_tokens_safe

    ! Re-export functions needed by other modules
    public :: find_program_unit_boundary, is_function_start, is_end_function, &
              parse_program_unit
    public :: is_do_loop_start, is_do_while_start, is_select_case_start, &
              is_end_do, is_end_select
    public :: is_if_then_start, is_end_if
    public :: is_type_start, is_end_type, find_statement_boundary

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

    ! Main parsing entry point
    subroutine parse_tokens(tokens, arena, prog_index, error_msg)
        use iso_fortran_env, only: error_unit
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(out) :: prog_index
        character(len=*), intent(out) :: error_msg

        type(mixed_construct_result_t) :: mixed_result
        logical :: has_explicit_program
        logical :: declaration_only_file
        integer, allocatable :: unit_indices(:)
        integer :: i, unit_start, unit_end, unit_index, unit_count
        character(len=8) :: debug_flag
        integer :: debug_status
        logical :: debug_units
        character(len=:), allocatable :: start_text, end_text
        type(token_t), allocatable :: tokens_local(:)
        character(len=:), allocatable :: nested_error

        error_msg = ""
        prog_index = 0
        call reset_nested_internal_procedure_error()

        ! Clear any stale parser errors from previous transformations
        call clear_parser_errors()

        call ensure_if_do_registration()

        tokens_local = tokens
        call normalize_keyword_identifiers(tokens_local)

        call get_environment_variable( &
            'FORTFRONT_DEBUG_DUMP_AST', debug_flag, status=debug_status)
        debug_units = (debug_status == 0 .and. len_trim(debug_flag) > 0)

        ! Check for mixed constructs first (Issue #511)
        call detect_mixed_constructs(tokens_local, mixed_result)
        declaration_only_file = (mixed_result%num_implicit_ranges > 0 .and. &
                                 mixed_result%num_explicit_ranges == 0)
        if (mixed_result%has_mixed_constructs .or. declaration_only_file) then
            block
                character(len=500) :: mixed_error
                call parse_mixed_constructs(tokens_local, arena, mixed_result, &
                                            prog_index, &
                                            mixed_error)
                if (len_trim(mixed_error) > 0) then
                    error_msg = trim(mixed_error)
                end if
            end block
            return
        end if

        ! Detect explicit program units
        has_explicit_program = detect_explicit_program_unit(tokens_local)

        ! Find and parse program units
        allocate (unit_indices(0))
        unit_count = 0
        i = 1

        do while (i <= size(tokens_local))
            if (tokens_local(i)%kind == TK_EOF) exit

            ! Find program unit boundary
            call find_program_unit_boundary(tokens_local, i, unit_start, unit_end)

            if (unit_end >= unit_start) then
                block
                    use iso_fortran_env, only: error_unit
                    character(len=:), allocatable :: unit_error
                    call process_program_unit(tokens_local, unit_start, &
                                              unit_end, arena, &
                                              unit_index, has_explicit_program, &
                                              unit_error)

                    ! Check for parser errors and propagate
                    if (allocated(unit_error) .and. len_trim(unit_error) > 0) then
                        error_msg = trim(unit_error)
                        prog_index = 0
                        return
                    end if
                end block

                if (debug_units) then
                    start_text = ''
                    end_text = ''
                    if (unit_start >= 1 .and. unit_start <= size(tokens_local)) then
                        start_text = tokens_local(unit_start)%text
                    end if
                    if (unit_end >= 1 .and. unit_end <= size(tokens_local)) then
                        end_text = tokens_local(unit_end)%text
                    end if
                    write (error_unit, '(A,3I6,2X,A,1X,A)') &
                        'DEBUG parse unit bounds:', unit_start, unit_end, unit_index, &
                        trim(start_text), trim(end_text)
                end if
                if (unit_index > 0) then
                    unit_count = unit_count + 1
                    unit_indices = [unit_indices, unit_index]
                end if

                i = unit_end + 1
            else
                i = i + 1
            end if
        end do

        if (has_nested_internal_procedure_error()) then
            nested_error = get_nested_internal_procedure_message()
            if (allocated(nested_error)) then
                error_msg = trim(nested_error)
            else
                error_msg = 'Nested internal procedures are not supported.'
            end if
            prog_index = 0
            return
        end if

        ! Handle final program structure
        if (unit_count == 0) then
            ! No units found - create empty main program
            prog_index = push_program(arena, "main", [integer ::], 1, 1)
        else if (unit_count == 1) then
            ! Single unit - pass through programs and modules directly
            ! Wrap other constructs in a program for consistent API
            if (allocated(arena%entries(unit_indices(1))%node)) then
                select type (node => arena%entries(unit_indices(1))%node)
                type is (program_node)
                    prog_index = unit_indices(1)
                type is (module_node)
                    ! Do not wrap a lone module in a synthetic program
                    prog_index = unit_indices(1)
                type is (block_data_node)
                    ! Do not wrap a lone BLOCK DATA in a synthetic program
                    prog_index = unit_indices(1)
                type is (function_def_node)
                    if (procedure_has_entry(arena, unit_indices(1))) then
                        prog_index = unit_indices(1)
                    else
                        prog_index = create_multi_unit_container(arena, &
                                                                 unit_indices(1:1))
                    end if
                type is (subroutine_def_node)
                    if (procedure_has_entry(arena, unit_indices(1))) then
                        prog_index = unit_indices(1)
                    else
                        prog_index = create_multi_unit_container(arena, &
                                                                 unit_indices(1:1))
                    end if
                type is (interface_block_node)
                    prog_index = push_program(arena, "__MULTI_UNIT__", &
                                              unit_indices(1:1), 1, 1)
                class default
                    prog_index = push_program(arena, "main", unit_indices(1:1), 1, 1)
                end select
            else
                ! Safety fallback
                prog_index = push_program(arena, "main", [integer ::], 1, 1)
            end if
        else
            ! Multiple units
            call handle_multiple_program_units(arena, unit_indices, &
                                               prog_index, error_msg)
        end if
    end subroutine parse_tokens

    ! Safe parsing wrapper
    function parse_tokens_safe(tokens, arena) result(parse_result)
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        type(parse_result_with_index_t) :: parse_result

        character(len=500) :: error_msg

        call parse_tokens(tokens, arena, parse_result%prog_index, error_msg)

        if (len_trim(error_msg) > 0) then
            parse_result%result%success = .false.
            parse_result%result%error_message = trim(error_msg)
        else
            parse_result%result%success = .true.
            parse_result%result%error_message = ""
        end if
    end function parse_tokens_safe

    subroutine normalize_keyword_identifiers(tokens)
        type(token_t), intent(inout) :: tokens(:)
        integer :: i
        type(token_t) :: prev_token
        type(token_t) :: next_token
        character(len=:), allocatable :: lowered

        do i = 1, size(tokens)
            if (tokens(i)%kind /= TK_KEYWORD) cycle

            lowered = to_lower(trim(tokens(i)%text))
            if (lowered == "end") then
                prev_token = find_previous_nontrivial_token(tokens, i)
                if (.not. token_precedes_identifier(prev_token)) cycle

                next_token = find_next_nontrivial_token(tokens, i)
                if (token_is_block_keyword(next_token)) cycle

                tokens(i)%kind = TK_IDENTIFIER
                cycle
            end if

            if (.not. keyword_can_be_identifier(lowered)) cycle

            if (lowered == "goto") then
                if (is_computed_goto_context(tokens, i)) cycle
            end if

            prev_token = find_previous_nontrivial_token(tokens, i)
            next_token = find_next_nontrivial_token(tokens, i)

            if (token_precedes_identifier(prev_token) .or. &
                token_requires_identifier_after(prev_token) .or. &
                token_follows_identifier_context(next_token)) then
                tokens(i)%kind = TK_IDENTIFIER
            end if
        end do
    end subroutine normalize_keyword_identifiers

    function find_previous_nontrivial_token(tokens, pos) result(prev_token)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        type(token_t) :: prev_token
        integer :: i

        prev_token%kind = TK_EOF
        prev_token%text = ""

        if (pos <= 1) return

        do i = pos - 1, 1, -1
            select case (tokens(i)%kind)
            case (TK_WHITESPACE, TK_NEWLINE, TK_COMMENT)
                cycle
            case default
                prev_token = tokens(i)
                return
            end select
        end do
    end function find_previous_nontrivial_token

    function find_next_nontrivial_token(tokens, pos) result(next_token)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        type(token_t) :: next_token
        integer :: i

        next_token%kind = TK_EOF
        next_token%text = ""

        if (pos >= size(tokens)) return

        do i = pos + 1, size(tokens)
            select case (tokens(i)%kind)
            case (TK_WHITESPACE, TK_NEWLINE, TK_COMMENT)
                cycle
            case default
                next_token = tokens(i)
                return
            end select
        end do
    end function find_next_nontrivial_token

    integer function find_next_nontrivial_index(tokens, pos) result(idx)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        integer :: i

        idx = 0
        if (pos >= size(tokens)) return

        do i = pos + 1, size(tokens)
            select case (tokens(i)%kind)
            case (TK_WHITESPACE, TK_NEWLINE, TK_COMMENT)
                cycle
            case default
                idx = i
                return
            end select
        end do
    end function find_next_nontrivial_index

    logical function token_precedes_identifier(token) result(is_valid)
        type(token_t), intent(in) :: token
        character(len=:), allocatable :: lowered

        is_valid = .false.

        if (token%kind /= TK_OPERATOR) then
            return
        end if

        lowered = trim(token%text)
        select case (lowered)
        case ("%", "::", ",", "=", "=>", "(")
            is_valid = .true.
        end select
    end function token_precedes_identifier

    logical function token_requires_identifier_after(token) result(requires)
        type(token_t), intent(in) :: token
        character(len=:), allocatable :: lowered

        requires = .false.
        if (token%kind /= TK_KEYWORD) return

        lowered = to_lower(trim(token%text))
        select case (lowered)
        case ("call")
            requires = .true.
        end select
    end function token_requires_identifier_after

    logical function is_computed_goto_context(tokens, pos) result(is_computed)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        integer :: next_idx

        is_computed = .false.
        next_idx = find_next_nontrivial_index(tokens, pos)
        if (next_idx <= 0) return
        if (tokens(next_idx)%kind /= TK_OPERATOR) return
        if (trim(tokens(next_idx)%text) /= "(") return

        next_idx = find_next_nontrivial_index(tokens, next_idx)
        if (next_idx <= 0) return

        if (tokens(next_idx)%kind == TK_NUMBER) then
            is_computed = .true.
        end if
    end function is_computed_goto_context

    logical function token_follows_identifier_context(token) result(is_valid)
        type(token_t), intent(in) :: token
        character(len=:), allocatable :: lowered

        is_valid = .false.
        if (token%kind /= TK_OPERATOR) return

        lowered = trim(token%text)
        select case (lowered)
        case ("=", "=>", "(", "%")
            is_valid = .true.
        end select
    end function token_follows_identifier_context

    logical function keyword_can_be_identifier(keyword) result(can_be_id)
        character(len=*), intent(in) :: keyword

        select case (keyword)
        case ("call", "cycle", "exit", "entry", "select", "goto", "go", &
              "common", "dimension", "program", "module", "contains", &
              "stop", "pause", "return", "continue")
            can_be_id = .true.
        case default
            can_be_id = .false.
        end select
    end function keyword_can_be_identifier

    logical function token_is_block_keyword(token) result(is_block)
        type(token_t), intent(in) :: token
        character(len=:), allocatable :: lowered

        is_block = .false.
        if (token%kind /= TK_KEYWORD) then
            return
        end if

        lowered = to_lower(trim(token%text))
        select case (lowered)
        case ("type", "module", "subroutine", "function", "program", "interface", &
              "procedure", "select", "if", "do", "forall", "where", "associate", &
              "block", "team", "critical", "blockdata")
            is_block = .true.
        end select
    end function token_is_block_keyword

    ! Detect explicit program unit
    function detect_explicit_program_unit(tokens) result(has_explicit_program_unit)
        type(token_t), intent(in) :: tokens(:)
        logical :: has_explicit_program_unit
        integer :: i
        integer :: next_idx
        character(len=:), allocatable :: lowered
        character(len=:), allocatable :: next_lower

        has_explicit_program_unit = .false.
        do i = 1, size(tokens)
            if (tokens(i)%kind /= TK_KEYWORD) cycle

            lowered = to_lower(trim(tokens(i)%text))
            select case (lowered)
            case ("program", "module", "function", "subroutine", "interface")
                has_explicit_program_unit = .true.
                exit
            case ("abstract")
                next_idx = find_next_nontrivial_index(tokens, i)
                if (next_idx > 0 .and. next_idx <= size(tokens)) then
                    if (tokens(next_idx)%kind == TK_KEYWORD) then
                        next_lower = to_lower(trim(tokens(next_idx)%text))
                        if (next_lower == "interface") then
                            has_explicit_program_unit = .true.
                            exit
                        end if
                    end if
                end if
            end select
        end do
    end function detect_explicit_program_unit

    ! Check if inside module context
    function is_inside_module(tokens, pos) result(inside_module)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        logical :: inside_module
        integer :: i
        logical :: in_module

        inside_module = .false.
        in_module = .false.

        do i = 1, min(pos, size(tokens))
            if (tokens(i)%kind == TK_KEYWORD) then
                if (tokens(i)%text == "module") then
                    in_module = .true.
                else if (tokens(i)%text == "end") then
                    if (i < size(tokens) .and. tokens(i + 1)%kind == TK_KEYWORD .and. &
                        tokens(i + 1)%text == "module") then
                        in_module = .false.
                    end if
                end if
            end if
        end do

        inside_module = in_module
    end function is_inside_module

    ! Check if position is start of program unit
    function is_program_unit_start(tokens, i) result(is_start)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: i
        logical :: is_start
        integer :: lookahead
        integer :: keyword_idx
        integer :: prev_idx
        logical :: label_allowed

        is_start = .false.
        if (i > size(tokens)) return
        keyword_idx = i

        if (tokens(keyword_idx)%kind == TK_NUMBER) then
            label_allowed = .true.
            prev_idx = keyword_idx - 1
            do while (prev_idx >= 1)
                select case (tokens(prev_idx)%kind)
                case (TK_WHITESPACE)
                    prev_idx = prev_idx - 1
                    cycle
                case default
                    exit
                end select
            end do

            if (prev_idx >= 1) then
                select case (tokens(prev_idx)%kind)
                case (TK_NEWLINE, TK_COMMENT)
                    label_allowed = .true.
                case default
                    label_allowed = .false.
                end select
            end if

            if (.not. label_allowed) return
            keyword_idx = keyword_idx + 1
        end if

        do while (keyword_idx <= size(tokens))
            select case (tokens(keyword_idx)%kind)
            case (TK_WHITESPACE, TK_NEWLINE, TK_COMMENT)
                keyword_idx = keyword_idx + 1
                cycle
            case default
                exit
            end select
        end do

        if (keyword_idx > size(tokens)) return
        if (tokens(keyword_idx)%kind /= TK_KEYWORD) return

        select case (to_lower(trim(tokens(keyword_idx)%text)))
        case ("program", "module", "function", "subroutine", "type", "interface")
            is_start = .true.
        case ("abstract")
            lookahead = keyword_idx + 1
            do while (lookahead <= size(tokens))
                select case (tokens(lookahead)%kind)
                case (TK_WHITESPACE, TK_NEWLINE, TK_COMMENT)
                    lookahead = lookahead + 1
                    cycle
                case (TK_KEYWORD)
                    if (to_lower(trim(tokens(lookahead)%text)) == "interface") then
                        is_start = .true.
                    end if
                    exit
                case default
                    exit
                end select
            end do
        case ("block")
            lookahead = keyword_idx + 1
            do while (lookahead <= size(tokens))
                select case (tokens(lookahead)%kind)
                case (TK_WHITESPACE, TK_NEWLINE, TK_COMMENT)
                    lookahead = lookahead + 1
                    cycle
                case (TK_KEYWORD, TK_IDENTIFIER)
                    if (to_lower(trim(tokens(lookahead)%text)) == "data") then
                        is_start = .true.
                    end if
                    exit
                case default
                    exit
                end select
            end do
        case default
            lookahead = find_procedure_keyword_after_prefix(tokens, keyword_idx)
            if (lookahead > 0) is_start = .true.
        end select
    end function is_program_unit_start

    integer function find_procedure_keyword_after_prefix(tokens, start_idx) &
        result(proc_idx)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_idx
        integer :: idx
        character(len=:), allocatable :: lowered

        proc_idx = 0
        idx = start_idx

        do while (idx <= size(tokens))
            select case (tokens(idx)%kind)
            case (TK_WHITESPACE, TK_NEWLINE, TK_COMMENT, TK_OPERATOR, TK_IDENTIFIER, TK_NUMBER)
                idx = idx + 1
                cycle
            case (TK_KEYWORD)
                lowered = to_lower(trim(tokens(idx)%text))
                if (lowered == "function" .or. lowered == "subroutine") then
                    proc_idx = idx
                    return
                else if (is_procedure_attribute_keyword(lowered)) then
                    idx = idx + 1
                    cycle
                else if (is_intrinsic_type_keyword(lowered)) then
                    idx = idx + 1
                    cycle
                else if ((lowered == "type" .or. lowered == "class") .and. &
                         is_type_spec_prefix(tokens, idx)) then
                    idx = idx + 1
                    cycle
                else
                    return
                end if
            case default
                return
            end select
        end do
    end function find_procedure_keyword_after_prefix

    logical function is_intrinsic_type_keyword(word) result(is_type_kw)
        character(len=*), intent(in) :: word

        select case (trim(word))
        case ("integer", "real", "double", "precision", "logical", "character", &
              "complex")
            is_type_kw = .true.
        case default
            is_type_kw = .false.
        end select
    end function is_intrinsic_type_keyword

    logical function is_procedure_attribute_keyword(word) result(is_attr)
        character(len=*), intent(in) :: word

        select case (trim(word))
        case ("pure", "impure", "elemental", "recursive", "nonrecursive", &
              "non_recursive", "module")
            is_attr = .true.
        case default
            is_attr = .false.
        end select
    end function is_procedure_attribute_keyword

    logical function is_type_spec_prefix(tokens, idx) result(is_spec)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: idx
        integer :: next_idx

        is_spec = .false.
        next_idx = find_next_nontrivial_index(tokens, idx)
        if (next_idx <= 0) return
        if (tokens(next_idx)%kind == TK_OPERATOR) then
            if (trim(tokens(next_idx)%text) == "(") is_spec = .true.
        end if
    end function is_type_spec_prefix

    ! Check if unit has meaningful content
    function unit_has_meaningful_content(tokens, unit_start, unit_end) &
        result(has_content)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: unit_start, unit_end
        logical :: has_content
        integer :: i

        has_content = .false.
        do i = unit_start, min(unit_end, size(tokens))
            if (tokens(i)%kind /= TK_COMMENT .and. tokens(i)%kind /= TK_EOF .and. &
                tokens(i)%kind /= TK_NEWLINE) then
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

        should_process = unit_has_meaningful_content(tokens, unit_start, unit_end)
    end function should_process_unit

    ! Process program unit
    subroutine process_program_unit(tokens, unit_start, unit_end, arena, &
                                    unit_index, has_explicit_program, error_msg)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: unit_start, unit_end
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(out) :: unit_index
        logical, intent(in) :: has_explicit_program
        character(len=:), allocatable, intent(out), optional :: error_msg

        type(token_t), allocatable, target :: unit_tokens(:)
        character(len=:), allocatable :: parse_error

        unit_index = 0

        ! Initialize error message
        if (present(error_msg)) error_msg = ""

        if (.not. should_process_unit(tokens, unit_start, unit_end)) then
            return
        end if

        ! Extract unit tokens
        allocate (unit_tokens(unit_end - unit_start + 2))
        unit_tokens(1:unit_end - unit_start + 1) = tokens(unit_start:unit_end)
        ! Add EOF token
        unit_tokens(unit_end - unit_start + 2)%kind = TK_EOF
        unit_tokens(unit_end - unit_start + 2)%text = ""
        unit_tokens(unit_end - unit_start + 2)%line = tokens(unit_end)%line
        unit_tokens(unit_end - unit_start + 2)%column = tokens(unit_end)%column + 1

        ! Parse the unit and capture any errors
        unit_index = parse_program_unit(unit_tokens, arena, has_explicit_program, &
                                        parse_error)

        ! Propagate error message if requested
        block
            use iso_fortran_env, only: error_unit
            if (allocated(parse_error)) then
            end if

            if (present(error_msg) .and. allocated(parse_error)) then
                if (len_trim(parse_error) > 0) then
                    error_msg = parse_error
                end if
            end if
        end block

        deallocate (unit_tokens)
    end subroutine process_program_unit

    logical function procedure_has_entry(arena, proc_index) result(has_entry)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: proc_index

        has_entry = .false.
        if (proc_index <= 0) return
        if (proc_index > arena%size) return
        if (.not. allocated(arena%entries(proc_index)%node)) return

        select type (proc => arena%entries(proc_index)%node)
        type is (function_def_node)
            if (.not. allocated(proc%body_indices)) return
            has_entry = body_indices_have_entry(arena, proc%body_indices)
        type is (subroutine_def_node)
            if (.not. allocated(proc%body_indices)) return
            has_entry = body_indices_have_entry(arena, proc%body_indices)
        class default
            has_entry = .false.
        end select
    end function procedure_has_entry

    logical function body_indices_have_entry(arena, body_indices) result(found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        integer :: i, idx

        found = .false.
        if (size(body_indices) == 0) return

        do i = 1, size(body_indices)
            idx = body_indices(i)
            if (idx <= 0) cycle
            if (idx > arena%size) cycle
            if (.not. allocated(arena%entries(idx)%node)) cycle
            select type (stmt => arena%entries(idx)%node)
            type is (entry_node)
                found = .true.
                return
            end select
        end do
    end function body_indices_have_entry

    ! Find program unit boundary
    subroutine find_program_unit_boundary(tokens, start_pos, unit_start, unit_end)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_pos
        integer, intent(out) :: unit_start, unit_end

        integer :: i, nesting_level
        integer :: next_pos
        integer :: name_pos
        integer :: block_keyword_pos
        character(len=:), allocatable :: unit_type
        character(len=:), allocatable :: keyword_text
        character(len=:), allocatable :: next_keyword
        logical :: in_module_contains
        logical :: preceded_by_end
        logical :: is_interface_unit

        integer :: effective_start

        unit_start = start_pos
        unit_end = start_pos
        nesting_level = 0
        in_module_contains = .false.
        unit_type = ""
        block_keyword_pos = start_pos

        is_interface_unit = .false.

        ! Skip leading trivia to locate the first meaningful token
        effective_start = start_pos
        do while (effective_start <= size(tokens))
            select case (tokens(effective_start)%kind)
            case (TK_WHITESPACE, TK_NEWLINE, TK_COMMENT)
                effective_start = effective_start + 1
                cycle
            case default
                exit
            end select
        end do

        if (effective_start > size(tokens)) then
            unit_start = start_pos
            unit_end = start_pos
            return
        end if

        unit_start = effective_start

        ! Determine the unit type from the first keyword
        if (unit_start <= size(tokens)) then
            select case (tokens(unit_start)%kind)
            case (TK_KEYWORD)
                unit_type = to_lower(trim(tokens(unit_start)%text))
                if (unit_type == "block") block_keyword_pos = unit_start
            case (TK_NUMBER)
                next_pos = unit_start + 1
                do while (next_pos <= size(tokens))
                    select case (tokens(next_pos)%kind)
                    case (TK_WHITESPACE, TK_NEWLINE, TK_COMMENT)
                        next_pos = next_pos + 1
                        cycle
                    case (TK_KEYWORD)
                        unit_type = to_lower(trim(tokens(next_pos)%text))
                        if (unit_type == "block") block_keyword_pos = next_pos
                        exit
                    case default
                        exit
                    end select
                end do
            end select
        end if

        if (unit_type == "block") then
            next_pos = block_keyword_pos + 1
            do while (next_pos <= size(tokens))
                select case (tokens(next_pos)%kind)
                case (TK_WHITESPACE, TK_NEWLINE, TK_COMMENT)
                    next_pos = next_pos + 1
                    cycle
                case (TK_KEYWORD, TK_IDENTIFIER)
                    if (to_lower(trim(tokens(next_pos)%text)) == "data") then
                        unit_type = "blockdata"
                    end if
                    exit
                case default
                    exit
                end select
            end do
        else if (unit_type == "abstract") then
            next_pos = unit_start + 1
            do while (next_pos <= size(tokens))
                select case (tokens(next_pos)%kind)
                case (TK_WHITESPACE, TK_NEWLINE, TK_COMMENT)
                    next_pos = next_pos + 1
                    cycle
                case (TK_KEYWORD)
                    if (to_lower(trim(tokens(next_pos)%text)) == "interface") then
                        unit_type = "interface"
                        is_interface_unit = .true.
                    end if
                    exit
                case default
                    exit
                end select
            end do
        else if (unit_type == "interface") then
            is_interface_unit = .true.
        end if

        ! For interface blocks, find the matching END INTERFACE
        if (is_interface_unit) then
            nesting_level = 1
            do i = unit_start + 1, size(tokens)
                if (tokens(i)%kind == TK_EOF) then
                    unit_end = i - 1
                    exit
                else if (tokens(i)%kind == TK_KEYWORD) then
                    keyword_text = to_lower(trim(tokens(i)%text))
                    select case (keyword_text)
                    case ("interface")
                        nesting_level = nesting_level + 1
                    case ("end")
                        next_pos = i + 1
                        do while (next_pos <= size(tokens))
                            select case (tokens(next_pos)%kind)
                            case (TK_WHITESPACE, TK_NEWLINE, TK_COMMENT)
                                next_pos = next_pos + 1
                                cycle
                            case default
                                exit
                            end select
                        end do
                        if (next_pos <= size(tokens)) then
                            if (tokens(next_pos)%kind == TK_KEYWORD) then
                                next_keyword = to_lower(trim(tokens(next_pos)%text))
                                if (next_keyword == "interface") then
                                    nesting_level = nesting_level - 1
                                    unit_end = next_pos
                                    name_pos = next_pos + 1
                                    if (name_pos <= size(tokens)) then
                                        if (tokens(name_pos)%kind == TK_IDENTIFIER) then
                                            unit_end = name_pos
                                        end if
                                    end if
                                    if (nesting_level == 0) exit
                                end if
                            end if
                        end if
                    end select
                end if
                unit_end = i
            end do
            ! For modules, we need to find the matching "end module"
        else if (unit_type == "module") then
            nesting_level = 1
            do i = unit_start + 1, size(tokens)
                if (tokens(i)%kind == TK_EOF) then
                    unit_end = i - 1
                    exit
                else if (tokens(i)%kind == TK_KEYWORD) then
                    if (to_lower(trim(tokens(i)%text)) == "contains") then
                        in_module_contains = .true.
                    else if (to_lower(trim(tokens(i)%text)) == "end") then
                        ! Check if this is "end module"
                        if (i + 1 <= size(tokens)) then
                            if (tokens(i + 1)%kind == TK_KEYWORD .and. &
                                to_lower(trim(tokens(i + 1)%text)) == "module") then
                                nesting_level = nesting_level - 1
                                if (nesting_level == 0) then
                                    unit_end = i + 1  ! Include "end module"
                                    ! Check if there's a module name after "end module"
                                    if (i + 2 <= size(tokens)) then
                                        if (tokens(i + 2)%kind == TK_IDENTIFIER) then
                                            ! Include module name
                                            unit_end = i + 2
                                        end if
                                    end if
                                    exit
                                end if
                            end if
                        end if
                    else if (to_lower(trim(tokens(i)%text)) == "module" .and. &
                             .not. in_module_contains) then
                        ! Nested module (rare but possible) - ensure this isn't part of
                        ! "module procedure" or similar interface syntax (issue #1411)
                        if (i + 1 <= size(tokens)) then
                            if (tokens(i + 1)%kind == TK_IDENTIFIER) then
                                nesting_level = nesting_level + 1
                            end if
                        end if
                    end if
                end if
                unit_end = i
            end do
        else if (unit_type == "submodule") then
            nesting_level = 1
            do i = unit_start + 1, size(tokens)
                if (tokens(i)%kind == TK_EOF) then
                    unit_end = i - 1
                    exit
                else if (tokens(i)%kind == TK_KEYWORD) then
                    keyword_text = to_lower(trim(tokens(i)%text))
                    select case (keyword_text)
                    case ("submodule")
                        nesting_level = nesting_level + 1
                    case ("endsubmodule")
                        nesting_level = nesting_level - 1
                        if (nesting_level == 0) then
                            unit_end = i
                            if (i + 1 <= size(tokens)) then
                                if (tokens(i + 1)%kind == TK_IDENTIFIER) then
                                    unit_end = i + 1
                                end if
                            end if
                            exit
                        end if
                    case ("end")
                        if (i + 1 <= size(tokens)) then
                            if (tokens(i + 1)%kind == TK_KEYWORD) then
                                next_keyword = to_lower(trim(tokens(i + 1)%text))
                                if (next_keyword == "submodule") then
                                    nesting_level = nesting_level - 1
                                    if (nesting_level == 0) then
                                        unit_end = i + 1
                                        if (i + 2 <= size(tokens)) then
                                            if (tokens(i + 2)%kind == &
                                                TK_IDENTIFIER) then
                                                unit_end = i + 2
                                            end if
                                        end if
                                        exit
                                    end if
                                end if
                            end if
                        end if
                    end select
                end if
                unit_end = i
            end do
        else if (unit_type == "subroutine" .or. unit_type == "function") then
            ! For standalone subroutines and functions, find the matching "end"
            ! Note: We only handle standalone procedures here.
            ! Internal procedures are handled by the default logic below
            do i = unit_start + 1, size(tokens)
                if (tokens(i)%kind == TK_EOF) then
                    unit_end = i - 1
                    exit
                else if (tokens(i)%kind == TK_KEYWORD) then
                    if (to_lower(trim(tokens(i)%text)) == "end") then
                        ! Check if this is "end subroutine" or "end function"
                        if (i + 1 <= size(tokens)) then
                            if (tokens(i + 1)%kind == TK_KEYWORD .and. &
                                to_lower(trim(tokens(i + 1)%text)) == unit_type) then
                                unit_end = i + 1  ! Include "end <unit_type>"
                                ! Check if there's a name after "end <unit_type>"
                                if (i + 2 <= size(tokens)) then
                                    if (tokens(i + 2)%kind == TK_IDENTIFIER) then
                                        unit_end = i + 2  ! Include the name too
                                    end if
                                end if
                                exit
                            end if
                        end if
                        ! Also handle standalone "end" for simple procedures
                        if (i == size(tokens) .or. (i + 1 <= size(tokens) .and. &
                                                    tokens(i + 1)%kind /= &
                                                    TK_KEYWORD)) then
                            unit_end = i
                            exit
                        end if
                    end if
                end if
                unit_end = i
            end do
        else if (unit_type == "program") then
            ! For explicit programs, locate the matching end program and include body
            nesting_level = 1
            do i = unit_start + 1, size(tokens)
                if (tokens(i)%kind == TK_EOF) then
                    unit_end = i - 1
                    exit
                else if (tokens(i)%kind == TK_KEYWORD) then
                    keyword_text = to_lower(trim(tokens(i)%text))
                    select case (keyword_text)
                    case ("program")
                        nesting_level = nesting_level + 1
                    case ("end")
                        next_pos = i + 1
                        do while (next_pos <= size(tokens))
                            select case (tokens(next_pos)%kind)
                            case (TK_WHITESPACE)
                                next_pos = next_pos + 1
                            case (TK_NEWLINE, TK_COMMENT)
                                exit
                            case default
                                exit
                            end select
                        end do

                        if (next_pos > size(tokens)) then
                            nesting_level = nesting_level - 1
                            if (nesting_level == 0) then
                                unit_end = i
                                exit
                            end if
                        else if (tokens(next_pos)%kind == TK_KEYWORD) then
                            next_keyword = to_lower(trim(tokens(next_pos)%text))
                            if (next_keyword == "program") then
                                nesting_level = nesting_level - 1
                                if (nesting_level == 0) then
                                    unit_end = next_pos
                                    name_pos = next_pos + 1
                                    do while (name_pos <= size(tokens))
                                        select case (tokens(name_pos)%kind)
                                        case (TK_WHITESPACE)
                                            name_pos = name_pos + 1
                                        case (TK_IDENTIFIER)
                                            unit_end = name_pos
                                            exit
                                        case default
                                            exit
                                        end select
                                    end do
                                    exit
                                end if
                            end if
                        else
                            select case (tokens(next_pos)%kind)
                            case (TK_NEWLINE, TK_COMMENT, TK_EOF)
                                nesting_level = nesting_level - 1
                                if (nesting_level == 0) then
                                    unit_end = i
                                    exit
                                end if
                            end select
                        end if
                    end select
                end if
                unit_end = i
            end do
        else if (unit_type == "blockdata") then
            ! For BLOCK DATA units, find "end block data"
            do i = block_keyword_pos + 2, size(tokens)
                if (tokens(i)%kind == TK_EOF) then
                    unit_end = i - 1
                    exit
                else if (tokens(i)%kind == TK_KEYWORD .and. &
                         to_lower(trim(tokens(i)%text)) == "end") then
                    if (i + 2 <= size(tokens)) then
                        if (tokens(i + 1)%kind == TK_KEYWORD .and. &
                            to_lower(trim(tokens(i + 1)%text)) == "block" .and. &
                            tokens(i + 2)%kind == TK_KEYWORD .and. &
                            to_lower(trim(tokens(i + 2)%text)) == "data") then
                            unit_end = i + 2
                            if (i + 3 <= size(tokens) .and. &
                                tokens(i + 3)%kind == TK_IDENTIFIER) then
                                unit_end = i + 3
                            end if
                            exit
                        end if
                    end if
                end if
                unit_end = i
            end do
        else
            ! Original logic for other units
            do i = unit_start, size(tokens)
                if (tokens(i)%kind == TK_EOF) then
                    unit_end = i - 1
                    exit
                else
                    preceded_by_end = .false.
                    if (i > 1) then
                        if (tokens(i - 1)%kind == TK_KEYWORD .and. &
                            tokens(i - 1)%text == "end") then
                            preceded_by_end = .true.
                        end if
                    end if
                    if (i > unit_start .and. is_program_unit_start(tokens, i) .and. &
                        .not. preceded_by_end) then
                        unit_end = i - 1
                        exit
                    else
                        unit_end = i
                    end if
                end if
            end do
        end if

        if (unit_end > size(tokens)) unit_end = size(tokens)
    end subroutine find_program_unit_boundary

    ! Remaining helper functions (preserved for compatibility)
    function is_function_start(tokens, pos) result(is_start)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        logical :: is_start

        is_start = .false.
        if (pos <= size(tokens)) then
            if (tokens(pos)%kind == TK_KEYWORD .and. tokens(pos)%text == &
                "function") then
                is_start = .true.
            end if
        end if
    end function is_function_start

    function is_end_construct(tokens, pos, construct_name) result(is_end)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        character(len=*), intent(in) :: construct_name
        logical :: is_end

        is_end = .false.
        if (pos < size(tokens)) then
            if (tokens(pos)%kind == TK_KEYWORD .and. &
                tokens(pos)%text == "end" .and. &
                tokens(pos + 1)%kind == TK_KEYWORD .and. &
                tokens(pos + 1)%text == construct_name) then
                is_end = .true.
            end if
        end if
    end function is_end_construct

    function is_end_function(tokens, pos) result(is_end)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        logical :: is_end

        is_end = is_end_construct(tokens, pos, "function")
    end function is_end_function

    function is_do_loop_start(tokens, pos) result(is_start)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        logical :: is_start

        is_start = .false.
        if (pos <= size(tokens)) then
            if (tokens(pos)%kind == TK_KEYWORD .and. tokens(pos)%text == "do") then
                is_start = .true.
            end if
        end if
    end function is_do_loop_start

    function is_do_while_start(tokens, pos) result(is_start)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        logical :: is_start

        is_start = .false.
        if (pos < size(tokens)) then
            if (tokens(pos)%kind == TK_KEYWORD .and. &
                tokens(pos)%text == "do" .and. &
                tokens(pos + 1)%kind == TK_KEYWORD .and. &
                tokens(pos + 1)%text == "while") then
                is_start = .true.
            end if
        end if
    end function is_do_while_start

    function is_select_case_start(tokens, pos) result(is_start)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        logical :: is_start

        is_start = .false.
        if (pos < size(tokens)) then
            if (tokens(pos)%kind == TK_KEYWORD .and. &
                tokens(pos)%text == "select" .and. &
                tokens(pos + 1)%kind == TK_KEYWORD .and. &
                tokens(pos + 1)%text == "case") then
                is_start = .true.
            end if
        end if
    end function is_select_case_start

    function is_end_do(tokens, pos) result(is_end)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        logical :: is_end

        is_end = is_end_construct(tokens, pos, "do")
    end function is_end_do

    function is_end_select(tokens, pos) result(is_end)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        logical :: is_end

        is_end = is_end_construct(tokens, pos, "select")
    end function is_end_select

    function is_if_then_start(tokens, pos) result(is_start)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        logical :: is_start

        is_start = .false.
        if (pos <= size(tokens)) then
            if (tokens(pos)%kind == TK_KEYWORD .and. tokens(pos)%text == "if") then
                is_start = .true.
            end if
        end if
    end function is_if_then_start

    function is_end_if(tokens, pos) result(is_end)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        logical :: is_end

        is_end = is_end_construct(tokens, pos, "if")
    end function is_end_if

    function is_end_type(tokens, pos) result(is_end)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        logical :: is_end

        is_end = is_end_construct(tokens, pos, "type")
    end function is_end_type

end module frontend_parsing
