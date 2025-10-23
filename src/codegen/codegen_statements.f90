module codegen_statements
    use iso_fortran_env, only: error_unit
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core
    use ast_nodes_io
    use ast_nodes_misc
    use ast_nodes_procedure
    use ast_nodes_control
    use type_system_unified
    use string_types, only: string_t
    use codegen_indent
    use codegen_arena_interface, only: generate_code_from_arena
    use lexer_core, only: to_lower
    implicit none
    private

    public :: generate_code_assignment
    public :: generate_code_pointer_assignment
    public :: generate_code_subroutine_call
    public :: generate_code_print_statement
    public :: generate_code_write_statement
    public :: generate_code_read_statement
    public :: generate_code_format_statement
    public :: generate_code_termination
    public :: generate_code_return
    public :: generate_code_continue
    public :: generate_code_goto
    public :: generate_code_error_termination
    public :: generate_code_cycle
    public :: generate_code_exit
    public :: generate_code_use_statement
    public :: generate_code_import_statement
    public :: generate_code_visibility_statement
    public :: generate_code_namelist_statement
    public :: generate_code_implicit_statement
    public :: generate_code_comment
    public :: generate_code_blank_line
    public :: generate_code_allocate_statement
    public :: generate_code_deallocate_statement
    public :: generate_code_open_statement
    public :: generate_code_close_statement
    public :: generate_code_inquire_statement
    public :: generate_code_backspace_statement
    public :: generate_code_rewind_statement
    public :: generate_code_endfile_statement
    public :: generate_code_pause_statement
    public :: generate_code_nullify_statement

contains
    pure subroutine prepend_stmt_label(code, label)
        character(len=:), allocatable, intent(inout) :: code
        character(len=:), allocatable, intent(in) :: label

        if (.not. allocated(code)) code = ""
        if (allocated(label)) then
            if (len(code) > 0) then
                code = label // " " // code
            else
                code = label
            end if
        end if
    end subroutine prepend_stmt_label

    ! Generate code for assignment statements
    function generate_code_assignment(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(assignment_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: left_code, right_code

        ! Generate left-hand side
        if (node%target_index > 0 .and. node%target_index <= arena%size) then
            left_code = generate_code_from_arena(arena, node%target_index)
        else
            left_code = ""
        end if

        ! Generate right-hand side
        if (node%value_index > 0 .and. node%value_index <= arena%size) then
            right_code = generate_code_from_arena(arena, node%value_index)
        else
            right_code = ""
        end if

        ! Build assignment
        if (allocated(node%operator) .and. node%operator == "=>") then
            code = left_code // " => " // right_code
        else
            code = left_code // " = " // right_code
        end if

        call prepend_stmt_label(code, node%stmt_label)
    end function generate_code_assignment

    ! Generate code for pointer assignment statements (ptr => target)
    function generate_code_pointer_assignment(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(pointer_assignment_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: pointer_code, target_code

        ! Generate pointer (left-hand side)
        if (node%pointer_index > 0 .and. node%pointer_index <= arena%size) then
            pointer_code = generate_code_from_arena(arena, node%pointer_index)
        else
            pointer_code = ""
        end if

        ! Generate target (right-hand side)
        if (node%target_index > 0 .and. node%target_index <= arena%size) then
            target_code = generate_code_from_arena(arena, node%target_index)
        else
            target_code = ""
        end if

        ! Build pointer assignment with => operator
        code = pointer_code // " => " // target_code

        call prepend_stmt_label(code, node%stmt_label)
    end function generate_code_pointer_assignment

    ! Generate code for subroutine calls
    function generate_code_subroutine_call(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(subroutine_call_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: args_code
        integer :: i

        code = "call " // node%name

        ! Generate arguments
        if (allocated(node%arg_indices)) then
            args_code = ""
            do i = 1, size(node%arg_indices)
                if (i > 1) args_code = args_code // ", "
                if (node%arg_indices(i) > 0 .and. &
                    node%arg_indices(i) <= arena%size) then
                    args_code = args_code // &
                        generate_code_from_arena(arena, node%arg_indices(i))
                end if
            end do
            code = code // "(" // args_code // ")"
        else
            code = code // "()"
        end if

        call prepend_stmt_label(code, node%stmt_label)
    end function generate_code_subroutine_call

    ! Generate code for print statements
    function generate_code_print_statement(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(print_statement_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: format_code, args_code
        integer :: i

        ! Generate format specifier
        if (allocated(node%format_spec)) then
            format_code = node%format_spec
        else
            format_code = "*"
        end if

        ! Generate output list using recursive code generation for each expression
        args_code = ""
        if (allocated(node%expression_indices)) then
            do i = 1, size(node%expression_indices)
                if (i > 1) args_code = args_code // ", "
                if (node%expression_indices(i) > 0 .and. &
                    node%expression_indices(i) <= arena%size) then
                    args_code = args_code // &
                        generate_code_from_arena(arena, node%expression_indices(i))
                end if
            end do
        end if

        code = "print " // format_code
        if (len(args_code) > 0) then
            code = code // ", " // args_code
        end if

        call prepend_stmt_label(code, node%stmt_label)
    end function generate_code_print_statement

    ! Generate code for write statements
    function generate_code_write_statement(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(write_statement_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: unit_code, format_code, args_code
        integer :: i

        ! Generate unit specifier
        if (allocated(node%unit_spec)) then
            unit_code = node%unit_spec
        else
            unit_code = "*"
        end if

        ! Generate format specifier or namelist
        if (allocated(node%namelist_group)) then
            ! Namelist write
            format_code = "nml=" // node%namelist_group
        else if (allocated(node%format_spec)) then
            format_code = node%format_spec
        else
            format_code = "*"
        end if

        ! Generate argument list using recursive code generation
        args_code = ""
        if (allocated(node%arg_indices)) then
            do i = 1, size(node%arg_indices)
                if (i > 1) args_code = args_code // ", "
                if (node%arg_indices(i) > 0 .and. &
                    node%arg_indices(i) <= arena%size) then
                    ! Use recursive code generation for proper expression handling
                    args_code = args_code // &
                        generate_code_from_arena(arena, node%arg_indices(i))
                end if
            end do
        end if

        ! Assemble write statement: write(unit, format) args
        code = "write(" // unit_code // ", " // format_code // ")"
        if (len(args_code) > 0) then
            code = code // " " // args_code
        end if

        call prepend_stmt_label(code, node%stmt_label)
    end function generate_code_write_statement

    ! Generate code for read statements
    function generate_code_read_statement(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(read_statement_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: unit_code, format_code, vars_code
        integer :: i

        ! Generate unit specifier
        if (allocated(node%unit_spec)) then
            unit_code = node%unit_spec
        else
            unit_code = "*"
        end if

        ! Generate format specifier
        if (allocated(node%format_spec)) then
            format_code = node%format_spec
        else
            format_code = "*"
        end if

        ! Generate variable list using recursive code generation
        vars_code = ""
        if (allocated(node%var_indices)) then
            do i = 1, size(node%var_indices)
                if (i > 1) vars_code = vars_code // ", "
                if (node%var_indices(i) > 0 .and. &
                    node%var_indices(i) <= arena%size) then
                    vars_code = vars_code // &
                        generate_code_from_arena(arena, node%var_indices(i))
                end if
            end do
        end if

        ! Assemble read statement: read(unit, format) vars
        code = "read(" // unit_code // ", " // format_code // ")"
        if (len(vars_code) > 0) then
            code = code // " " // vars_code
        end if

        call prepend_stmt_label(code, node%stmt_label)
    end function generate_code_read_statement

    ! Generate code for format statements
    function generate_code_format_statement(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(format_statement_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code

        if (allocated(node%format_spec)) then
            code = "format " // trim(node%format_spec)
        else
            code = "format (*)"
        end if

        call prepend_stmt_label(code, node%stmt_label)
    end function generate_code_format_statement

    ! Generate code for termination statements
    function generate_code_termination(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(stop_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: code_expr

        ! Generate proper STOP statement preserving original syntax
        if (allocated(node%stop_message)) then
            code = "stop " // node%stop_message
        else if (node%stop_code_index > 0 .and. &
                 node%stop_code_index <= arena%size) then
            code_expr = generate_code_from_arena(arena, node%stop_code_index)
            code = "stop " // code_expr
        else
            code = "stop"
        end if

        call prepend_stmt_label(code, node%stmt_label)
    end function generate_code_termination

    ! Generate code for return statements
    function generate_code_return(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(return_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code

        code = "return"

        call prepend_stmt_label(code, node%stmt_label)
    end function generate_code_return

    function generate_code_continue(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(continue_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code

        code = "continue"

        call prepend_stmt_label(code, node%stmt_label)
    end function generate_code_continue

    ! Generate code for goto statements
    function generate_code_goto(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(goto_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code, selector_code

        ! Check if computed GOTO
        if (allocated(node%label_list) .and. node%selector_index > 0) then
            ! Computed GOTO: go to (label_list), selector
            selector_code = generate_code_from_arena(arena, node%selector_index)
            code = "go to (" // trim(node%label_list) // "), " // trim(selector_code)
        else if (allocated(node%label)) then
            ! Simple GOTO
            code = "go to " // trim(node%label)
        else
            code = "go to 999"  ! Fallback for invalid goto
        end if

        call prepend_stmt_label(code, node%stmt_label)
    end function generate_code_goto

    ! Generate code for error termination statements
    function generate_code_error_termination(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(error_stop_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code

        ! Generate proper error termination with exit call
        if (allocated(node%error_message)) then
            code = "call exit(2) ! " // node%error_message
        else if (node%error_code_index > 0) then
            code = "call exit(2) ! Error termination with code"
        else
            code = "call exit(2) ! Error termination"
        end if

        call prepend_stmt_label(code, node%stmt_label)
    end function generate_code_error_termination

    ! Generate code for cycle statements
    function generate_code_cycle(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(cycle_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code

        if (allocated(node%label)) then
            code = "cycle " // trim(adjustl(node%label))
        else
            code = "cycle"
        end if

        call prepend_stmt_label(code, node%stmt_label)
    end function generate_code_cycle

    ! Generate code for exit statements
    function generate_code_exit(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(exit_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code

        if (allocated(node%label)) then
            code = "exit " // trim(adjustl(node%label))
        else
            code = "exit"
        end if

        call prepend_stmt_label(code, node%stmt_label)
    end function generate_code_exit

    subroutine append_rename_list_to_clause(rename_list, only_clause)
        type(string_t), allocatable, intent(in) :: rename_list(:)
        character(len=:), allocatable, intent(inout) :: only_clause
        character(len=:), allocatable :: rename_entry
        integer :: i

        do i = 1, size(rename_list), 2
            if (i + 1 > size(rename_list)) exit
            if (.not. allocated(rename_list(i)%s)) cycle
            if (.not. allocated(rename_list(i + 1)%s)) cycle
            rename_entry = rename_list(i)%s // " => " // rename_list(i + 1)%s
            if (len_trim(only_clause) > 0) only_clause = only_clause // ", "
            only_clause = only_clause // rename_entry
        end do
    end subroutine append_rename_list_to_clause

    ! Generate code for use statements
    function generate_code_use_statement(node) result(code)
        type(use_statement_node), intent(in) :: node
        character(len=:), allocatable :: code
        character(len=:), allocatable :: only_clause
        character(len=:), allocatable :: rename_entry
        integer :: i
        logical :: needs_double_colon

        ! Build proper use statement with all components
        if (.not. allocated(node%module_name)) then
            code = "use ! ERROR: no module name"
            return
        end if

        code = "use"

        needs_double_colon = node%has_double_colon .or. node%is_intrinsic .or. &
                             node%is_non_intrinsic

        ! Add URL spec for Go-style imports if present
        if (allocated(node%url_spec)) then
            code = code // " " // node%url_spec
            if (needs_double_colon) code = code // " ::"
        else
            if (node%is_intrinsic) then
                code = code // ", intrinsic"
            else if (node%is_non_intrinsic) then
                code = code // ", non_intrinsic"
            end if
            if (needs_double_colon) code = code // " ::"
        end if

        ! Add module name
        code = code // " " // node%module_name

        ! Add only clause if present
        only_clause = ""
        if (node%has_only) then
            if (allocated(node%only_list)) then
                do i = 1, size(node%only_list)
                    if (.not. allocated(node%only_list(i)%s)) cycle
                    if (len_trim(only_clause) > 0) only_clause = only_clause // ", "
                    only_clause = only_clause // node%only_list(i)%s
                end do
            end if
            if (allocated(node%rename_list)) then
                call append_rename_list_to_clause(node%rename_list, only_clause)
            end if
            if (len_trim(only_clause) > 0) then
                code = code // ", only: " // trim(only_clause)
            else
                code = code // ", only:"
            end if
        else if (allocated(node%rename_list)) then
            ! Fallback: emit rename list under only clause even if flag missing
            call append_rename_list_to_clause(node%rename_list, only_clause)
            if (len_trim(only_clause) > 0) then
                code = code // ", only: " // trim(only_clause)
            end if
        end if

        call prepend_stmt_label(code, node%stmt_label)
    end function generate_code_use_statement

    function generate_code_import_statement(node) result(code)
        type(import_statement_node), intent(in) :: node
        character(len=:), allocatable :: code
        integer :: i

        code = "import"

        if (node%is_all) then
            code = code // ", all"
        else if (node%is_none) then
            code = code // ", none"
        else
            if (node%has_double_colon) then
                code = code // " ::"
            end if

            if (node%has_list .and. allocated(node%import_list)) then
                if (.not. node%has_double_colon .and. size(node%import_list) > 0) then
                    code = code // " "
                end if
                do i = 1, size(node%import_list)
                    if (.not. allocated(node%import_list(i)%s)) cycle
                    if (i > 1) code = code // ", "
                    if (i == 1 .and. node%has_double_colon) code = code // " "
                    code = code // node%import_list(i)%s
                end do
            end if
        end if
    end function generate_code_import_statement

    function generate_code_visibility_statement(node) result(code)
        type(visibility_statement_node), intent(in) :: node
        character(len=:), allocatable :: code
        integer :: i

        if (node%is_private) then
            code = "private"
        else
            code = "public"
        end if

        if (node%has_list .and. allocated(node%names)) then
            code = code // " :: "
            do i = 1, size(node%names)
                if (i > 1) code = code // ", "
                if (allocated(node%names(i)%s)) then
                    code = code // trim(node%names(i)%s)
                end if
            end do
        else if (node%has_double_colon) then
            code = code // " ::"
        end if

        call prepend_stmt_label(code, node%stmt_label)
    end function generate_code_visibility_statement

    function generate_code_namelist_statement(node) result(code)
        type(namelist_statement_node), intent(in) :: node
        character(len=:), allocatable :: code
        character(len=:), allocatable :: names
        integer :: i

        if (.not. allocated(node%group_name)) then
            code = "namelist"
            return
        end if

        names = ""
        if (allocated(node%variable_names)) then
            do i = 1, size(node%variable_names)
                if (.not. allocated(node%variable_names(i)%s)) cycle
                if (len_trim(names) > 0) names = names // ", "
                names = names // trim(node%variable_names(i)%s)
            end do
        end if

        if (len_trim(names) > 0) then
            code = "namelist /" // trim(node%group_name) // "/ " // trim(names)
        else
            code = "namelist /" // trim(node%group_name) // "/"
        end if

        call prepend_stmt_label(code, node%stmt_label)
    end function generate_code_namelist_statement

    ! Generate code for implicit statements
    function generate_code_implicit_statement(node) result(code)
        type(implicit_statement_node), intent(in) :: node
        character(len=:), allocatable :: code, type_part, letter_part
        integer :: i

        if (node%is_none) then
            code = "implicit none"
        else
            ! Build type specification
            if (allocated(node%type_spec%type_name)) then
                type_part = trim(node%type_spec%type_name)
            else
                type_part = "real"
            end if

            ! Build letter range specification
            letter_part = ""
            if (allocated(node%letter_specs)) then
                do i = 1, size(node%letter_specs)
                    if (i > 1) letter_part = letter_part // ", "
                    if (node%letter_specs(i)%start_letter == &
                        node%letter_specs(i)%end_letter) then
                        letter_part = letter_part // node%letter_specs(i)%start_letter
                    else
                        letter_part = letter_part // node%letter_specs(i)%start_letter &
                            // "-" // node%letter_specs(i)%end_letter
                    end if
                end do
            end if

            if (len_trim(letter_part) > 0) then
                code = "implicit " // type_part // " (" // letter_part // ")"
            else
                code = "implicit " // type_part
            end if
        end if

        call prepend_stmt_label(code, node%stmt_label)
    end function generate_code_implicit_statement

    ! Generate code for comment nodes
    function generate_code_comment(node) result(code)
        type(comment_node), intent(in) :: node
        character(len=:), allocatable :: code
        character(len=:), allocatable :: lowered_text
        logical :: is_legacy_statement

        is_legacy_statement = .false.
        if (allocated(node%text)) then
            lowered_text = to_lower(adjustl(trim(node%text)))
            if (len_trim(lowered_text) >= 11) then
                if (index(lowered_text, "equivalence") == 1) then
                    is_legacy_statement = .true.
                end if
            end if
            if (len_trim(lowered_text) >= 6 .and. .not. is_legacy_statement) then
                if (index(lowered_text, "common") == 1) then
                    is_legacy_statement = .true.
                end if
            end if
            if (len_trim(lowered_text) >= 5 .and. .not. is_legacy_statement) then
                if (index(lowered_text, "block") == 1) then
                    is_legacy_statement = .true.
                end if
            end if
        end if

        if (is_legacy_statement) then
            code = trim(node%text)
        else if (allocated(node%text)) then
            code = "!" // node%text
        else
            code = "!"
        end if

        call prepend_stmt_label(code, node%stmt_label)
    end function generate_code_comment

    ! Generate code for blank line nodes
    function generate_code_blank_line(node) result(code)
        type(blank_line_node), intent(in) :: node
        character(len=:), allocatable :: code
        integer :: i

        ! Generate the appropriate number of blank lines
        code = ""
        do i = 1, node%count
            code = code // new_line('A')
        end do
    end function generate_code_blank_line

    ! generate_code_from_arena is provided as an interface at the module level

    ! Generate code for allocate statements
    function generate_code_allocate_statement(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(allocate_statement_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: args, var_code, shape_code
        integer :: i, j

        args = ""

        ! Add type-spec if present
        if (allocated(node%type_spec)) then
            args = node%type_spec // " :: "
        end if

        if (allocated(node%var_indices)) then
            do i = 1, size(node%var_indices)
                if (i > 1) args = args // ", "
                if (node%var_indices(i) > 0 .and. &
                    node%var_indices(i) <= arena%size) then
                    var_code = generate_code_from_arena(arena, node%var_indices(i))
                else
                    var_code = ""
                end if
                if (i == 1) then
                    ! Attach shape to the first variable if present
                    if (allocated(node%shape_indices)) then
                        if (size(node%shape_indices) > 0) then
                            shape_code = ""
                            do j = 1, size(node%shape_indices)
                                if (j > 1) shape_code = shape_code // ", "
                                if (node%shape_indices(j) > 0 .and. &
                                    node%shape_indices(j) <= arena%size) then
                                    shape_code = shape_code // &
                                                 generate_code_from_arena( &
                                                 arena, node%shape_indices(j))
                                end if
                            end do
                            if (len(shape_code) > 0) then
                                var_code = var_code // "(" // shape_code // ")"
                            end if
                        end if
                    end if
                end if
                args = args // var_code
            end do
        end if

        ! Optional keyword arguments
        if (node%stat_var_index > 0 .and. &
            node%stat_var_index <= arena%size) then
            args = args // ", stat=" // &
                generate_code_from_arena(arena, node%stat_var_index)
        end if
        if (node%errmsg_var_index > 0 .and. &
            node%errmsg_var_index <= arena%size) then
            args = args // ", errmsg=" // &
                generate_code_from_arena(arena, node%errmsg_var_index)
        end if
        if (node%source_expr_index > 0 .and. &
            node%source_expr_index <= arena%size) then
            args = args // ", source=" // &
                generate_code_from_arena(arena, node%source_expr_index)
        end if
        if (node%mold_expr_index > 0 .and. &
            node%mold_expr_index <= arena%size) then
            args = args // ", mold=" // &
                generate_code_from_arena(arena, node%mold_expr_index)
        end if

        code = "allocate(" // args // ")"

        call prepend_stmt_label(code, node%stmt_label)
    end function generate_code_allocate_statement

    ! Generate code for deallocate statements
    function generate_code_deallocate_statement(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(deallocate_statement_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: args
        integer :: i

        args = ""

        if (allocated(node%var_indices)) then
            do i = 1, size(node%var_indices)
                if (i > 1) args = args // ", "
        if (node%var_indices(i) > 0 .and. &
            node%var_indices(i) <= arena%size) then
            args = args // generate_code_from_arena(arena, node%var_indices(i))
        end if
            end do
        end if

        if (node%stat_var_index > 0 .and. &
            node%stat_var_index <= arena%size) then
            args = args // ", stat=" // &
                generate_code_from_arena(arena, node%stat_var_index)
        end if
        if (node%errmsg_var_index > 0 .and. &
            node%errmsg_var_index <= arena%size) then
            args = args // ", errmsg=" // &
                generate_code_from_arena(arena, node%errmsg_var_index)
        end if

        code = "deallocate(" // args // ")"

        call prepend_stmt_label(code, node%stmt_label)
    end function generate_code_deallocate_statement

    function generate_code_open_statement(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(open_statement_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code

        if (allocated(node%unit_spec)) then
            code = "open(" // node%unit_spec // ")"
        else
            code = "open()"
        end if
        call prepend_stmt_label(code, node%stmt_label)
    end function generate_code_open_statement

    function generate_code_close_statement(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(close_statement_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code

        if (allocated(node%unit_spec)) then
            code = "close(" // node%unit_spec // ")"
        else
            code = "close()"
        end if
        call prepend_stmt_label(code, node%stmt_label)
    end function generate_code_close_statement

    function generate_code_inquire_statement(arena, node, node_index) &
        result(code)
        type(ast_arena_t), intent(in) :: arena
        type(inquire_statement_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code

        if (allocated(node%spec_list)) then
            code = "inquire(" // node%spec_list // ")"
        else
            code = "inquire()"
        end if
        call prepend_stmt_label(code, node%stmt_label)
    end function generate_code_inquire_statement

    function generate_code_backspace_statement(arena, node, node_index) &
        result(code)
        type(ast_arena_t), intent(in) :: arena
        type(backspace_statement_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code

        if (allocated(node%unit_spec)) then
            code = "backspace " // node%unit_spec
        else
            code = "backspace"
        end if
        call prepend_stmt_label(code, node%stmt_label)
    end function generate_code_backspace_statement

    function generate_code_rewind_statement(arena, node, node_index) &
        result(code)
        type(ast_arena_t), intent(in) :: arena
        type(rewind_statement_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code

        if (allocated(node%unit_spec)) then
            code = "rewind " // node%unit_spec
        else
            code = "rewind"
        end if
        call prepend_stmt_label(code, node%stmt_label)
    end function generate_code_rewind_statement

    function generate_code_endfile_statement(arena, node, node_index) &
        result(code)
        type(ast_arena_t), intent(in) :: arena
        type(endfile_statement_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code

        if (allocated(node%unit_spec)) then
            code = "endfile " // node%unit_spec
        else
            code = "endfile"
        end if
        call prepend_stmt_label(code, node%stmt_label)
    end function generate_code_endfile_statement

    function generate_code_pause_statement(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(pause_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: code_expr

        if (allocated(node%pause_message)) then
            code = "pause " // node%pause_message
        else if (node%pause_code_index > 0 .and. &
                 node%pause_code_index <= arena%size) then
            code_expr = generate_code_from_arena(arena, node%pause_code_index)
            code = "pause " // code_expr
        else
            code = "pause"
        end if

        call prepend_stmt_label(code, node%stmt_label)
    end function generate_code_pause_statement

    function generate_code_nullify_statement(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(nullify_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: ptr_code
        integer :: i

        code = "nullify("

        if (allocated(node%pointer_indices)) then
            do i = 1, size(node%pointer_indices)
                if (node%pointer_indices(i) > 0 .and. &
                    node%pointer_indices(i) <= arena%size) then
                    ptr_code = generate_code_from_arena(arena, node%pointer_indices(i))
                    if (i > 1) code = code // ", "
                    code = code // ptr_code
                end if
            end do
        end if

        code = code // ")"

        call prepend_stmt_label(code, node%stmt_label)
    end function generate_code_nullify_statement

end module codegen_statements
