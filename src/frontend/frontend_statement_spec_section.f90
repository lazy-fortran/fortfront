module frontend_statement_spec_section
    use lexer_core, only: to_lower
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: assignment_node, call_or_subscript_node, &
                              identifier_node
    use ast_nodes_data, only: declaration_node
    use ast_nodes_misc, only: blank_line_node, comment_node, contains_node, &
                              data_statement_node, directive_node, &
                              implicit_statement_node, import_statement_node, &
                              include_statement_node, intrinsic_statement_node, &
                              namelist_statement_node, statement_function_node, &
                              use_statement_node
    use ast_nodes_io, only: format_statement_node

    implicit none
    private

    integer, parameter :: STMT_FN_MAX_NAME_LEN = 128

    public :: convert_statement_function_if_needed
    public :: update_spec_section_state

contains

    subroutine convert_statement_function_if_needed(arena, stmt_index, &
                                                    declaration_indices)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: stmt_index
        integer, intent(in) :: declaration_indices(:)
        type(statement_function_node) :: stmt_fn
        logical :: success

        if (.not. arena_index_has_node(arena, stmt_index)) return

        select type (assign_node => arena%entries(stmt_index)%node)
        type is (assignment_node)
            success = build_statement_function_from_assignment(arena, assign_node, &
                                                               declaration_indices, &
                                                               stmt_fn)
            if (.not. success) return
            call replace_statement_with_stmt_fn(arena, stmt_index, stmt_fn)
        end select
    end subroutine convert_statement_function_if_needed

    pure logical function arena_index_has_node(arena, idx) result(valid)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: idx

        valid = .false.
        if (idx <= 0 .or. idx > arena%size) return
        valid = allocated(arena%entries(idx)%node)
    end function arena_index_has_node

    logical function build_statement_function_from_assignment(arena, assign_node, &
                                                              declaration_indices, &
                                                              stmt_fn) result(success)
        type(ast_arena_t), intent(inout) :: arena
        type(assignment_node), intent(in) :: assign_node
        integer, intent(in) :: declaration_indices(:)
        type(statement_function_node), intent(out) :: stmt_fn

        integer :: num_args
        character(len=:), allocatable :: base_name

        success = .false.
        if (assign_node%value_index <= 0) return
        if (.not. arena_index_has_node(arena, assign_node%target_index)) return

        select type (call_node => arena%entries(assign_node%target_index)%node)
        type is (call_or_subscript_node)
            if (.not. statement_function_target_name(call_node, base_name)) return
            if (has_array_declaration(arena, declaration_indices, base_name)) return
            if (.not. allocated(call_node%arg_indices)) return
            num_args = size(call_node%arg_indices)
            if (num_args <= 0) return
            if (.not. gather_identifier_arg_names(arena, call_node%arg_indices, &
                                                  stmt_fn%arg_names)) return

            stmt_fn%uid = assign_node%uid
            stmt_fn%line = assign_node%line
            stmt_fn%column = assign_node%column
            if (allocated(assign_node%stmt_label)) stmt_fn%stmt_label = &
                assign_node%stmt_label
            stmt_fn%name = base_name
            stmt_fn%body_expr_index = assign_node%value_index
            success = .true.
        end select
    end function build_statement_function_from_assignment

    logical function statement_function_target_name(call_node, base_name) &
        result(success)
        type(call_or_subscript_node), intent(in) :: call_node
        character(len=:), allocatable, intent(out) :: base_name

        success = .false.
        if (.not. allocated(call_node%name)) return
        if (call_node%base_expr_index /= 0) return
        base_name = trim(call_node%name)
        success = (len_trim(base_name) > 0)
    end function statement_function_target_name

    logical function gather_identifier_arg_names(arena, arg_indices, arg_names) &
        result(success)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: arg_indices(:)
        character(len=:), allocatable, intent(out) :: arg_names(:)

        integer :: i, arg_idx

        success = .false.
        if (size(arg_indices) <= 0) return
        allocate (character(len=STMT_FN_MAX_NAME_LEN) :: arg_names(size(arg_indices)))

        do i = 1, size(arg_indices)
            arg_idx = arg_indices(i)
            if (.not. arena_index_has_node(arena, arg_idx)) then
                deallocate (arg_names)
                return
            end if
            select type (arg_node => arena%entries(arg_idx)%node)
            type is (identifier_node)
                if (.not. allocated(arg_node%name)) then
                    deallocate (arg_names)
                    return
                end if
                arg_names(i) = trim(arg_node%name)
            class default
                deallocate (arg_names)
                return
            end select
        end do

        success = .true.
    end function gather_identifier_arg_names

    subroutine replace_statement_with_stmt_fn(arena, stmt_index, stmt_fn)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: stmt_index
        type(statement_function_node), intent(in) :: stmt_fn

        if (allocated(arena%entries(stmt_index)%node)) then
            deallocate (arena%entries(stmt_index)%node)
        end if
        allocate (arena%entries(stmt_index)%node, source=stmt_fn)
    end subroutine replace_statement_with_stmt_fn

    subroutine update_spec_section_state(arena, stmt_index, in_spec_section, &
                                         declaration_indices)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: stmt_index
        logical, intent(inout) :: in_spec_section
        integer, allocatable, intent(inout) :: declaration_indices(:)

        if (stmt_index <= 0 .or. stmt_index > arena%size) return
        if (.not. allocated(arena%entries(stmt_index)%node)) return

        select type (node => arena%entries(stmt_index)%node)
        type is (comment_node)
            return
        type is (blank_line_node)
            return
        type is (directive_node)
            return
        type is (declaration_node)
            declaration_indices = [declaration_indices, stmt_index]
            return
        type is (use_statement_node)
            return
        type is (implicit_statement_node)
            return
        type is (intrinsic_statement_node)
            return
        type is (import_statement_node)
            return
        type is (include_statement_node)
            return
        type is (namelist_statement_node)
            return
        type is (data_statement_node)
            return
        type is (format_statement_node)
            return
        type is (statement_function_node)
            return
        type is (contains_node)
            in_spec_section = .false.
            return
        end select

        in_spec_section = .false.
    end subroutine update_spec_section_state

    logical function has_array_declaration(arena, declaration_indices, name) &
        result(found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: declaration_indices(:)
        character(len=*), intent(in) :: name
        integer :: i
        character(len=:), allocatable :: target

        found = .false.
        target = to_lower(trim(name))
        if (len_trim(target) == 0) return

        do i = 1, size(declaration_indices)
            if (declaration_indices(i) <= 0 .or. &
                declaration_indices(i) > arena%size) cycle
            if (.not. allocated(arena%entries(declaration_indices(i))%node)) cycle
            select type (decl => arena%entries(declaration_indices(i))%node)
            type is (declaration_node)
                if (.not. declaration_includes_name(decl, target)) cycle
                if (decl%is_array) then
                    found = .true.
                    return
                end if
            end select
        end do
    end function has_array_declaration

    logical function declaration_includes_name(decl, target) result(matches)
        type(declaration_node), intent(in) :: decl
        character(len=*), intent(in) :: target
        integer :: j
        character(len=:), allocatable :: normalized

        matches = .false.
        if (len_trim(target) == 0) return

        if (decl%is_multi_declaration) then
            if (.not. allocated(decl%var_names)) return
            do j = 1, size(decl%var_names)
                normalized = to_lower(trim(decl%var_names(j)))
                if (normalized == target) then
                    matches = .true.
                    return
                end if
            end do
        else
            if (.not. allocated(decl%var_name)) return
            normalized = to_lower(trim(decl%var_name))
            matches = (normalized == target)
        end if
    end function declaration_includes_name

end module frontend_statement_spec_section
