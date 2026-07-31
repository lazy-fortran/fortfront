module codegen_core
    use ast_arena_modern, only: ast_arena_t
    use codegen_expressions
    use codegen_statements
    use codegen_control_flow
    use codegen_declarations
    use codegen_type_utils, only: set_type_standardization, &
        get_type_standardization
    use codegen_basic_utils, only: add_line_continuations
    use codegen_arena_interface, only: set_arena_generator
    use ast_nodes_data, only: mixed_construct_container_node, declaration_node, &
        multi_unit_container_node, &
        parameter_declaration_node, module_node, &
        derived_type_node, block_data_node, submodule_node
    use ast_nodes_bounds, only: range_expression_node, array_bounds_node, &
        array_slice_node, &
        array_operation_node
    use ast_nodes_core, only: range_subscript_node, literal_node, identifier_node, &
        binary_op_node, call_or_subscript_node, &
        array_literal_node, &
        assignment_node, pointer_assignment_node, program_node, &
        component_access_node
    use ast_nodes_procedure
    use ast_nodes_associate, only: associate_node, block_construct_node
    use ast_nodes_misc, only: complex_literal_node, comment_node, &
        blank_line_node, &
        implicit_statement_node, &
        allocate_statement_node, &
        deallocate_statement_node, use_statement_node, &
        intrinsic_statement_node, import_statement_node, &
        include_statement_node, statement_function_node, &
        visibility_statement_node, &
        namelist_statement_node, &
        data_statement_node, contains_node, &
        directive_node, &
        end_statement_node, &
        interface_block_node, module_procedure_node
    use ast_nodes_legacy, only: common_block_node, enum_node
    use ast_nodes_generics, only: template_block_node, &
        instantiate_statement_node, trait_block_node, &
        requirement_block_node, implements_block_node
    use ast_error_nodes, only: error_node_t
    use ast_nodes_control
    use ast_nodes_loops
    use ast_nodes_io
    use codegen_generics, only: generate_code_template_block, &
        generate_code_instantiate_statement, &
        generate_code_trait_block, &
        generate_code_requirement_block, &
        generate_code_implements_block
    implicit none
    private

    public :: codegen_core_generate_arena
    public :: generate_code_polymorphic
    public :: safe_codegen_core_generate_arena
    public :: set_type_standardization, get_type_standardization
    public :: initialize_codegen

contains

    ! Main entry point — delegates to category sub-dispatchers so that
    ! no single stack frame carries all 60+ select-type branches.
    recursive function codegen_core_generate_arena(arena, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        logical :: handled

        code = ""
        if (.not. arena%has_node_at(node_index)) return

        call dispatch_expressions(arena, node_index, code, handled)
        if (.not. handled) call dispatch_statements(arena, node_index, code, handled)
        if (.not. handled) call dispatch_control_flow(arena, node_index, code, handled)
        if (.not. handled) call dispatch_declarations(arena, node_index, code, handled)
        if (.not. handled) call dispatch_special(arena, node_index, code, handled)

        if (len(code) > 0) then
            code = normalize_line_spacing(code)
        end if

        if (handled .and. len(code) > 0) then
            code = append_trailing_comment(code, arena, node_index)
        end if
    end function codegen_core_generate_arena

    recursive subroutine dispatch_expressions(arena, node_index, code, handled)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: code
        logical, intent(out) :: handled

        handled = .true.
        select type (node => arena%entries(node_index)%node)
            type is (literal_node)
            code = generate_code_literal(node)
            type is (identifier_node)
            code = generate_code_identifier(node)
            type is (binary_op_node)
            code = generate_code_binary_op(arena, node, node_index)
            type is (call_or_subscript_node)
            code = generate_code_call_or_subscript(arena, node, node_index)
            type is (component_access_node)
            code = generate_code_component_access(arena, node, node_index)
            type is (array_literal_node)
            code = generate_code_array_literal(arena, node, node_index)
            type is (io_implied_do_node)
            code = generate_code_io_implied_do(arena, node, node_index)
            type is (complex_literal_node)
            code = generate_code_complex_literal(arena, node, node_index)
            type is (range_expression_node)
            code = generate_code_range_expression(arena, node, node_index)
            type is (array_bounds_node)
            code = generate_code_array_bounds(arena, node, node_index)
            type is (array_slice_node)
            code = generate_code_array_slice(arena, node, node_index)
            type is (range_subscript_node)
            code = generate_code_range_subscript(arena, node, node_index)
            type is (array_operation_node)
            code = generate_code_array_operation(arena, node, node_index)
        class default
            code = ""
            handled = .false.
        end select
    end subroutine dispatch_expressions

    recursive subroutine dispatch_statements(arena, node_index, code, handled)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: code
        logical, intent(out) :: handled

        handled = .true.
        select type (node => arena%entries(node_index)%node)
            type is (assignment_node)
            code = generate_code_assignment(arena, node, node_index)
            type is (pointer_assignment_node)
            code = generate_code_pointer_assignment(arena, node, node_index)
            type is (statement_function_node)
            code = generate_code_statement_function(arena, node, node_index)
            type is (subroutine_call_node)
            code = generate_code_subroutine_call(arena, node, node_index)
            type is (print_statement_node)
            code = generate_code_print_statement(arena, node, node_index)
            type is (write_statement_node)
            code = generate_code_write_statement(arena, node, node_index)
            type is (read_statement_node)
            code = generate_code_read_statement(arena, node, node_index)
            type is (open_statement_node)
            code = generate_code_open_statement(arena, node, node_index)
            type is (close_statement_node)
            code = generate_code_close_statement(arena, node, node_index)
            type is (inquire_statement_node)
            code = generate_code_inquire_statement(arena, node, node_index)
            type is (backspace_statement_node)
            code = generate_code_backspace_statement(arena, node, node_index)
            type is (rewind_statement_node)
            code = generate_code_rewind_statement(arena, node, node_index)
            type is (endfile_statement_node)
            code = generate_code_endfile_statement(arena, node, node_index)
            type is (format_statement_node)
            code = generate_code_format_statement(arena, node, node_index)
            type is (stop_node)
            code = generate_code_termination(arena, node, node_index)
            type is (return_node)
            code = generate_code_return(arena, node, node_index)
            type is (alt_return_spec_node)
            code = generate_code_alt_return_spec(arena, node, node_index)
            type is (entry_node)
            code = generate_code_entry(arena, node, node_index)
            type is (continue_node)
            code = generate_code_continue(arena, node, node_index)
            type is (goto_node)
            code = generate_code_goto(arena, node, node_index)
            type is (error_stop_node)
            code = generate_code_error_termination(arena, node, node_index)
            type is (pause_node)
            code = generate_code_pause_statement(arena, node, node_index)
            type is (nullify_node)
            code = generate_code_nullify_statement(arena, node, node_index)
            type is (cycle_node)
            code = generate_code_cycle(arena, node, node_index)
            type is (exit_node)
            code = generate_code_exit(arena, node, node_index)
            type is (visibility_statement_node)
            code = generate_code_visibility_statement(node)
            type is (namelist_statement_node)
            code = generate_code_namelist_statement(node)
            type is (data_statement_node)
            code = generate_code_data_statement(arena, node)
            type is (use_statement_node)
            code = generate_code_use_statement(node)
            type is (intrinsic_statement_node)
            code = generate_code_intrinsic_statement(node)
            type is (import_statement_node)
            code = generate_code_import_statement(node)
            type is (include_statement_node)
            code = generate_code_include_statement(node)
            type is (implicit_statement_node)
            code = generate_code_implicit_statement(node)
            type is (comment_node)
            code = generate_code_comment(node)
            type is (directive_node)
            code = generate_code_directive(node)
            type is (blank_line_node)
            code = generate_code_blank_line(node)
            type is (allocate_statement_node)
            code = generate_code_allocate_statement(arena, node, node_index)
            type is (deallocate_statement_node)
            code = generate_code_deallocate_statement(arena, node, node_index)
            type is (common_block_node)
            code = generate_code_common_block(node)
        class default
            code = ""
            handled = .false.
        end select
    end subroutine dispatch_statements

    recursive subroutine dispatch_control_flow(arena, node_index, code, handled)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: code
        logical, intent(out) :: handled

        handled = .true.
        select type (node => arena%entries(node_index)%node)
            type is (if_node)
            code = generate_code_if(arena, node, node_index)
            type is (do_loop_node)
            code = generate_code_do_loop(arena, node, node_index)
            type is (do_while_node)
            code = generate_code_do_while(arena, node, node_index)
            type is (select_case_node)
            code = generate_code_select_case(arena, node, node_index)
            type is (select_type_node)
            code = generate_code_select_type(arena, node, node_index)
            type is (select_rank_node)
            code = generate_code_select_rank(arena, node, node_index)
            type is (where_node)
            code = generate_code_where(arena, node, node_index)
            type is (forall_node)
            code = generate_code_forall(arena, node, node_index)
            type is (associate_node)
            code = generate_code_associate(arena, node, node_index)
            type is (block_construct_node)
            code = generate_code_block_construct(arena, node, node_index)
        class default
            code = ""
            handled = .false.
        end select
    end subroutine dispatch_control_flow

    recursive subroutine dispatch_declarations(arena, node_index, code, handled)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: code
        logical, intent(out) :: handled

        handled = .true.
        select type (node => arena%entries(node_index)%node)
            type is (declaration_node)
            code = generate_code_declaration(arena, node, node_index)
            type is (parameter_declaration_node)
            code = generate_code_parameter_declaration(arena, node, node_index)
            type is (interface_block_node)
            code = generate_code_interface_block(arena, node, node_index)
            type is (module_procedure_node)
            code = generate_code_module_procedure(arena, node, node_index)
            type is (function_def_node)
            code = generate_code_function_def(arena, node, node_index)
            type is (subroutine_def_node)
            code = generate_code_subroutine_def(arena, node, node_index)
            type is (module_node)
            code = generate_code_module(arena, node, node_index)
            type is (submodule_node)
            code = generate_code_submodule(arena, node, node_index)
            type is (block_data_node)
            code = generate_code_block_data(arena, node, node_index)
            type is (program_node)
            code = generate_code_program(arena, node, node_index)
            type is (derived_type_node)
            code = generate_code_derived_type(arena, node, node_index)
            type is (mixed_construct_container_node)
            code = generate_code_mixed_construct_container(arena, node, &
                node_index)
            type is (multi_unit_container_node)
            code = generate_multi_unit_program(arena, node)
            type is (template_block_node)
            code = generate_code_template_block(arena, node)
            type is (instantiate_statement_node)
            code = generate_code_instantiate_statement(node)
            type is (trait_block_node)
            code = generate_code_trait_block(arena, node)
            type is (requirement_block_node)
            code = generate_code_requirement_block(arena, node)
            type is (implements_block_node)
            code = generate_code_implements_block(arena, node)
            type is (enum_node)
            code = generate_code_enum(node)
        class default
            code = ""
            handled = .false.
        end select
    end subroutine dispatch_declarations

    recursive subroutine dispatch_special(arena, node_index, code, handled)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: code
        logical, intent(out) :: handled

        handled = .true.
        select type (node => arena%entries(node_index)%node)
            type is (contains_node)
            code = "contains"
            type is (end_statement_node)
            code = "end"
            type is (error_node_t)
            code = "! ERROR: "//node%error_message
        class default
            code = "! Unknown node type"
            handled = .true.
        end select
    end subroutine dispatch_special

    function normalize_line_spacing(text) result(clean)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: clean
        character(len=:), allocatable :: buffer
        integer :: len_text
        integer :: i
        character :: ch
        logical :: leading
        logical :: in_string
        character :: string_delim
        integer :: space_run

        len_text = len(text)
        if (len_text == 0) then
            clean = text
            return
        end if

        buffer = ''
        leading = .true.
        in_string = .false.
        string_delim = ' '
        space_run = 0

        i = 1
        do while (i <= len_text)
            ch = text(i:i)

            if (in_string) then
                buffer = buffer//ch
                if (ch == string_delim) then
                    if (i < len_text) then
                        if (text(i + 1:i + 1) == string_delim) then
                            buffer = buffer//string_delim
                            i = i + 1
                        else
                            in_string = .false.
                        end if
                    else
                        in_string = .false.
                    end if
                end if
                if (ch == new_line('A')) then
                    leading = .true.
                    space_run = 0
                    in_string = .false.
                end if
            else if (ch == ' ') then
                if (leading) then
                    buffer = buffer//' '
                else
                    space_run = space_run + 1
                end if
            else if (ch == '''' .or. ch == '"') then
                if (space_run > 0) then
                    buffer = buffer//' '
                    space_run = 0
                end if
                buffer = buffer//ch
                in_string = .true.
                string_delim = ch
                leading = .false.
            else
                if (space_run > 0) then
                    buffer = buffer//' '
                    space_run = 0
                end if
                buffer = buffer//ch
                if (ch == new_line('A')) then
                    leading = .true.
                else
                    leading = .false.
                end if
            end if

            i = i + 1
        end do

        if (space_run > 0 .and. .not. leading) buffer = buffer//' '
        clean = buffer
        clean = replace_all(clean, ' * ', '*')
        clean = replace_all(clean, ' ** ', '**')
        clean = replace_all(clean, '* ', '*')
        ! Apply namelist spacing for:
        ! 1. NAMELIST I/O: nml=/group/
        ! 2. NAMELIST declarations: namelist/group/vars
        ! Exclude slashes inside string literals, e.g. file=/tmp/test.dat
        if (index(clean, 'nml=') > 0 .or. index(clean, 'nml =') > 0 .or. &
            index(adjustl(clean), 'namelist') == 1) then
            clean = adjust_namelist_spacing(clean)
        else if (index(clean, 'namelist') == 0) then
            ! Only remove spaces around slashes when namelist is absent
            clean = replace_all(clean, ' / ', '/')
            clean = replace_all(clean, '/ ', '/')
        end if
        clean = replace_all(clean, ' ,', ',')
        clean = replace_all(clean, ' )', ')')
    end function normalize_line_spacing

    function adjust_namelist_spacing(text) result(out)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: out
        character(len=:), allocatable :: trimmed, prefix, rest, group, remainder
        integer :: slash1, slash2, i
        logical :: in_string
        character :: quote_char

        trimmed = adjustl(text)

        ! Find first slash not inside a string literal
        in_string = .false.
        quote_char = ' '
        slash1 = 0
        do i = 1, len(trimmed)
            if (.not. in_string) then
                if (trimmed(i:i) == "'" .or. trimmed(i:i) == '"') then
                    in_string = .true.
                    quote_char = trimmed(i:i)
                else if (trimmed(i:i) == '/') then
                    slash1 = i
                    exit
                end if
            else
                if (trimmed(i:i) == quote_char) then
                    in_string = .false.
                end if
            end if
        end do

        if (slash1 <= 0) then
            out = trimmed
            return
        end if

        prefix = trim(trimmed(1:slash1 - 1))
        rest = trimmed(slash1 + 1:)

        ! Find second slash not inside a string literal
        in_string = .false.
        quote_char = ' '
        slash2 = 0
        do i = 1, len(rest)
            if (.not. in_string) then
                if (rest(i:i) == "'" .or. rest(i:i) == '"') then
                    in_string = .true.
                    quote_char = rest(i:i)
                else if (rest(i:i) == '/') then
                    slash2 = i
                    exit
                end if
            else
                if (rest(i:i) == quote_char) then
                    in_string = .false.
                end if
            end if
        end do

        if (slash2 <= 0) then
            out = trimmed
            return
        end if

        group = trim(adjustl(rest(1:slash2 - 1)))
        remainder = rest(slash2 + 1:) ! Keep original spacing, do not adjustl
        ! Add space before first slash, and optionally before remainder if present
        out = trim(prefix)//' /'//group//'/'
        if (len_trim(remainder) > 0) then
            ! Preserve original spacing of remainder (may have leading space)
            out = out//' '//trim(adjustl(remainder))
        end if
    end function adjust_namelist_spacing

    function replace_all(text, pattern, replacement) result(out)
        character(len=*), intent(in) :: text
        character(len=*), intent(in) :: pattern
        character(len=*), intent(in) :: replacement
        character(len=:), allocatable :: out
        character(len=:), allocatable :: buffer
        integer :: start, pos, pat_len

        if (len(pattern) == 0) then
            out = text
            return
        end if

        buffer = ''
        start = 1
        pat_len = len(pattern)

        do
            pos = index(text(start:), pattern)
            if (pos == 0) exit
            pos = pos + start - 1
            if (pos > start) buffer = buffer//text(start:pos - 1)
            buffer = buffer//replacement
            start = pos + pat_len
        end do

        if (start <= len(text)) buffer = buffer//text(start:)
        out = buffer
    end function replace_all

    ! Polymorphic code generator (same as codegen_core_generate_arena)
    function generate_code_polymorphic(arena, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code

        code = codegen_core_generate_arena(arena, node_index)
    end function generate_code_polymorphic

    subroutine safe_codegen_core_generate_arena(arena, node_index, code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: code

        if (node_index <= 0 .or. node_index > arena%size) then
            code = ""
            return
        end if

        if (.not. allocated(arena%entries(node_index)%node)) then
            code = ""
            return
        end if

        code = codegen_core_generate_arena(arena, node_index)

        ! Add line continuations for overly long lines
        code = add_line_continuations(code)
    end subroutine safe_codegen_core_generate_arena

    ! Initialize the codegen system by setting up dependency injection
    subroutine initialize_codegen()
        call set_arena_generator(codegen_core_generate_arena)
    end subroutine initialize_codegen

    ! Generate code for mixed construct containers
    function generate_code_mixed_construct_container(arena, node, node_index) &
            result(code)
        type(ast_arena_t), intent(in) :: arena
        type(mixed_construct_container_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        integer :: i

        code = ""

        ! Generate code for explicit program units
        if (allocated(node%explicit_program_indices)) then
            do i = 1, size(node%explicit_program_indices)
                if (node%explicit_program_indices(i) > 0 .and. &
                    node%explicit_program_indices(i) <= arena%size) then

                    if (i > 1) then
                        code = code//new_line('A')//new_line('A')
                    end if

                    code = code// &
                        codegen_core_generate_arena(arena, &
                        node%explicit_program_indices(i))
                end if
            end do
        end if

        ! Generate code for implicit declarations (if any)
        if (allocated(node%implicit_declaration_indices)) then
            do i = 1, size(node%implicit_declaration_indices)
                if (node%implicit_declaration_indices(i) > 0 .and. &
                    node%implicit_declaration_indices(i) <= arena%size) then

                    if (len(code) > 0) then
                        code = code//new_line('A')//new_line('A')
                    end if

                    code = code//codegen_core_generate_arena( &
                        arena, node%implicit_declaration_indices(i))
                end if
            end do
        end if
    end function generate_code_mixed_construct_container

    function append_trailing_comment(code, arena, node_index) result(result_code)
        character(len=:), allocatable, intent(in) :: code
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable :: result_code

        result_code = code
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. arena%has_node_at(node_index)) return

        ! Control-flow constructs emit their header trailing comment inline
        ! during their own codegen, so skip the generic end-of-node append.
        select type (node => arena%entries(node_index)%node)
            type is (do_loop_node)
            return
            type is (do_while_node)
            return
            type is (if_node)
            return
        end select

        if (.not. allocated(arena%entries(node_index)%node%trailing_comment)) return
        associate (comment => arena%entries(node_index)%node%trailing_comment)
            if (len_trim(comment) > 0) result_code = code//'  '//trim(comment)
        end associate
    end function append_trailing_comment

end module codegen_core
