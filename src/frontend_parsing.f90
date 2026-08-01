module frontend_parsing
    ! fortfront - Parsing functions module (refactored for SRP compliance)
    ! Now serves as a compatibility layer over split modules

    use lexer_core, only: token_t, TK_EOF, TK_KEYWORD, &
        TK_OPERATOR, TK_IDENTIFIER, TK_NUMBER, TK_STRING, &
        TK_UNKNOWN, &
        TK_WHITESPACE, to_lower
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: program_node
    use ast_nodes_data, only: module_node, block_data_node, submodule_node
    use ast_nodes_misc, only: interface_block_node
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node
    use ast_factory, only: push_program, push_multi_unit_container
    use frontend_utilities, only: is_type_start
    use parser_do_constructs_module, only: ensure_if_do_registration
    use mixed_construct_detector, only: detect_mixed_constructs, &
        mixed_construct_result_t, &
        is_top_level_declaration
    use error_handling, only: result_t
    use error_reporting, only: error_collection_t, error_record_t
    use parser_dispatcher_module, only: clear_parser_errors
    use parser_construct_terminators_module, only: validate_construct_terminators
    use parser_statement_placement_module, only: validate_statement_placement

    ! Import from split modules
    use frontend_program_units, only: parse_program_unit, &
        parse_function_unit, parse_subroutine_unit, &
        parse_type_unit, parse_explicit_program_unit, &
        parse_implicit_main_program, &
        not_meaningful_program_unit, &
        has_any_non_comment_content, &
        has_executable_statements, &
        is_function_start
    use frontend_statement_processing, only: &
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
    use parser_label_validation_module, only: reset_statement_label_error, &
        has_statement_label_error, get_statement_label_message
    use frontend_program_unit_scanner, only: detect_explicit_program_unit, &
        is_inside_module, is_program_unit_start, &
        unit_has_meaningful_content, &
        should_process_unit, &
        process_program_unit, &
        find_program_unit_boundary, &
        procedure_has_entry
    use frontend_token_normalization, only: normalize_keyword_identifiers

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

        container_index = push_multi_unit_container(arena, unit_indices, 1, 1)
    end function create_multi_unit_container

    ! Main parsing entry point
    subroutine parse_tokens(tokens, arena, prog_index, error_msg, parser_errors)
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(out) :: prog_index
        character(len=*), intent(out) :: error_msg
        type(error_record_t), allocatable, intent(out), optional :: parser_errors(:)

        type(mixed_construct_result_t) :: mixed_result
        logical :: has_explicit_program
        logical :: debug_units
        integer, allocatable :: unit_indices(:)
        type(token_t), allocatable :: tokens_local(:)
        character(len=:), allocatable :: nested_error
        character(len=:), allocatable :: unit_error
        character(len=:), allocatable :: terminator_error
        character(len=:), allocatable :: placement_error
        type(error_collection_t), target :: diagnostics

        error_msg = ""
        prog_index = 0
        call reset_nested_internal_procedure_error()
        call reset_statement_label_error()
        call clear_parser_errors()
        call ensure_if_do_registration()

        tokens_local = tokens
        call normalize_keyword_identifiers(tokens_local)
        debug_units = debug_dump_enabled()

        call validate_construct_terminators(tokens_local, terminator_error)
        if (len_trim(terminator_error) > 0) then
            error_msg = terminator_error
            prog_index = 0
            call export_parser_errors(diagnostics, parser_errors)
            return
        end if

        call validate_statement_placement(tokens_local, placement_error)
        if (len_trim(placement_error) > 0) then
            error_msg = placement_error
            prog_index = 0
            call export_parser_errors(diagnostics, parser_errors)
            return
        end if

        call detect_mixed_constructs(tokens_local, mixed_result)
        if (should_use_mixed_constructs(tokens_local, mixed_result)) then
            call parse_mixed_construct_file(tokens_local, arena, mixed_result, &
                prog_index, error_msg, diagnostics)
            if (has_statement_label_error()) then
                error_msg = get_statement_label_message()
                prog_index = 0
            end if
            call export_parser_errors(diagnostics, parser_errors)
            return
        end if

        has_explicit_program = detect_explicit_program_unit(tokens_local)
        call collect_program_units(tokens_local, arena, has_explicit_program, &
            debug_units, unit_indices, unit_error, diagnostics)

        if (allocated(unit_error)) then
            if (len_trim(unit_error) > 0) then
                error_msg = trim(unit_error)
                prog_index = 0
                call export_parser_errors(diagnostics, parser_errors)
                return
            end if
        end if

        if (has_statement_label_error()) then
            error_msg = get_statement_label_message()
            prog_index = 0
            call export_parser_errors(diagnostics, parser_errors)
            return
        end if

        if (has_nested_internal_procedure_error()) then
            nested_error = get_nested_internal_procedure_message()
            if (allocated(nested_error)) then
                error_msg = trim(nested_error)
            else
                error_msg = 'Nested internal procedures are not supported.'
            end if
            prog_index = 0
            call export_parser_errors(diagnostics, parser_errors)
            return
        end if

        if (.not. allocated(unit_indices)) allocate (unit_indices(0))
        call finalize_program_result(arena, unit_indices, prog_index, error_msg)
        call export_parser_errors(diagnostics, parser_errors)
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

    logical function debug_dump_enabled() result(is_enabled)
        character(len=8) :: debug_flag
        integer :: debug_status

        call get_environment_variable('FORTFRONT_DEBUG_DUMP_AST', debug_flag, &
            status=debug_status)
        is_enabled = (debug_status == 0 .and. len_trim(debug_flag) > 0)
    end function debug_dump_enabled

    logical function should_use_mixed_constructs(tokens, mixed_result) &
            result(require_mixed)
        type(token_t), intent(in) :: tokens(:)
        type(mixed_construct_result_t), intent(in) :: mixed_result
        integer :: token_idx

        require_mixed = mixed_result%has_mixed_constructs
        if (require_mixed) return

        if (mixed_result%num_explicit_ranges /= 0) return
        if (has_executable_statements(tokens)) return

        do token_idx = 1, size(tokens)
            if (is_top_level_declaration(tokens, token_idx)) then
                require_mixed = .true.
                exit
            end if
        end do
    end function should_use_mixed_constructs

    subroutine parse_mixed_construct_file(tokens, arena, mixed_result, prog_index, &
            error_msg, diagnostics)
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        type(mixed_construct_result_t), intent(in) :: mixed_result
        integer, intent(out) :: prog_index
        character(len=*), intent(inout) :: error_msg
        type(error_collection_t), target, intent(inout) :: diagnostics

        character(len=500) :: mixed_error

        mixed_error = ""
        call parse_mixed_constructs(tokens, arena, mixed_result, prog_index, &
            mixed_error, diagnostics)
        if (len_trim(mixed_error) > 0) then
            error_msg = trim(mixed_error)
        end if
    end subroutine parse_mixed_construct_file

    subroutine collect_program_units(tokens, arena, has_explicit_program, &
            debug_units, unit_indices, unit_error, diagnostics)
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        logical, intent(in) :: has_explicit_program
        logical, intent(in) :: debug_units
        integer, allocatable, intent(out) :: unit_indices(:)
        character(len=:), allocatable, intent(out) :: unit_error
        type(error_collection_t), target, intent(inout) :: diagnostics

        integer :: i
        integer :: unit_start
        integer :: unit_end
        integer :: unit_index
        character(len=:), allocatable :: local_error

        allocate (unit_indices(0))
        unit_error = ""
        i = 1
        do while (i <= size(tokens))
            if (tokens(i)%kind == TK_EOF) exit

            call find_program_unit_boundary(tokens, i, unit_start, unit_end)
            if (unit_end < unit_start) then
                i = i + 1
                cycle
            end if

            local_error = ""
            call process_program_unit(tokens, unit_start, unit_end, arena, &
                unit_index, has_explicit_program, local_error, diagnostics)

            if (debug_units) then
                call log_unit_bounds(tokens, unit_start, unit_end, unit_index)
            end if

            if (allocated(local_error)) then
                if (len_trim(local_error) > 0) then
                    unit_error = local_error
                    return
                end if
            end if

            if (unit_index > 0) then
                unit_indices = [unit_indices, unit_index]
            end if
            i = unit_end + 1
        end do
    end subroutine collect_program_units

    subroutine export_parser_errors(diagnostics, parser_errors)
        type(error_collection_t), intent(in) :: diagnostics
        type(error_record_t), allocatable, intent(out), optional :: parser_errors(:)

        if (.not. present(parser_errors)) return
        allocate (parser_errors(diagnostics%count))
        if (diagnostics%count > 0) then
            parser_errors = diagnostics%errors(1:diagnostics%count)
        end if
    end subroutine export_parser_errors

    subroutine log_unit_bounds(tokens, unit_start, unit_end, unit_index)
        use, intrinsic :: iso_fortran_env, only: error_unit
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: unit_start
        integer, intent(in) :: unit_end
        integer, intent(in) :: unit_index
        character(len=:), allocatable :: start_text
        character(len=:), allocatable :: end_text

        start_text = ""
        end_text = ""
        if (unit_start >= 1 .and. unit_start <= size(tokens)) then
            start_text = tokens(unit_start)%text
        end if
        if (unit_end >= 1 .and. unit_end <= size(tokens)) then
            end_text = tokens(unit_end)%text
        end if

        write (error_unit, '(A,3I6,2X,A,1X,A)') 'DEBUG parse unit bounds:', &
            unit_start, unit_end, unit_index, trim(start_text), trim(end_text)
    end subroutine log_unit_bounds

    subroutine finalize_program_result(arena, unit_indices, prog_index, error_msg)
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable, intent(in) :: unit_indices(:)
        integer, intent(out) :: prog_index
        character(len=*), intent(inout) :: error_msg
        integer :: unit_count

        unit_count = size(unit_indices)
        if (unit_count == 0) then
            prog_index = push_program(arena, "main", [integer ::], 1, 1)
            return
        end if

        if (unit_count == 1) then
            call finalize_single_unit(arena, unit_indices(1), prog_index)
        else
            call handle_multiple_program_units(arena, unit_indices, prog_index, &
                error_msg)
        end if
    end subroutine finalize_program_result

    subroutine finalize_single_unit(arena, unit_index, prog_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: unit_index
        integer, intent(out) :: prog_index

        if (.not. allocated(arena%entries(unit_index)%node)) then
            prog_index = push_program(arena, "main", [integer ::], 1, 1)
            return
        end if

        select type (node => arena%entries(unit_index)%node)
            type is (program_node)
            prog_index = unit_index
            type is (module_node)
            prog_index = unit_index
            type is (submodule_node)
            prog_index = unit_index
            type is (block_data_node)
            prog_index = unit_index
            type is (function_def_node)
            if (procedure_has_entry(arena, unit_index)) then
                prog_index = unit_index
            else
                prog_index = push_program(arena, "main", [unit_index], 1, 1)
            end if
            type is (subroutine_def_node)
            if (procedure_has_entry(arena, unit_index)) then
                prog_index = unit_index
            else
                prog_index = push_program(arena, "main", [unit_index], 1, 1)
            end if
            type is (interface_block_node)
            prog_index = push_multi_unit_container(arena, [unit_index], 1, 1)
        class default
            prog_index = push_program(arena, "main", [unit_index], 1, 1)
        end select
    end subroutine finalize_single_unit

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
