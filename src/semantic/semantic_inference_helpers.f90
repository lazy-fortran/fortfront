module semantic_inference_helpers
    ! Helper module for type inference of control flow and declarations
    ! This avoids circular dependencies with semantic_analyzer
    use type_system_unified, only: mono_type_t, poly_type_t, type_var_t, &
                                   create_mono_type, create_type_var, &
                                   create_poly_type, TVAR, TINT, TREAL, TCHAR, &
                                   TLOGICAL, TCOMPLEX, TDOUBLE, TDERIVED
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: program_node
    use ast_nodes_control, only: if_node, do_loop_node, do_while_node, where_node, &
                                 where_stmt_node, forall_node, select_case_node, &
                                 associate_node, stop_node, pause_node, nullify_node
    use ast_nodes_data, only: declaration_node
    use ast_nodes_misc, only: implicit_statement_node
    use scope_manager, only: scope_stack_t
    implicit none
    private

    public :: process_if_node_branches
    public :: process_do_loop_body
    public :: process_do_while_node_body
    public :: process_where_node_clauses
    public :: process_where_stmt_node
    public :: process_forall_node_body
    public :: process_select_case_blocks
    public :: process_associate_node_body
    public :: process_stop_node_code
    public :: process_pause_node_code
    public :: process_nullify_node_code
    public :: process_declaration_variables
    public :: check_implicit_none

contains

    ! Process if node branches and return control type
    subroutine process_if_node_branches(node, control_type)
        type(if_node), intent(in) :: node
        type(mono_type_t), intent(out) :: control_type

        ! If statements don't have a type - return control marker
        control_type = create_mono_type(TVAR, var=create_type_var(0, "control"))
    end subroutine process_if_node_branches

    ! Process do loop body
    subroutine process_do_loop_body(node, int_scheme, control_type)
        type(do_loop_node), intent(in) :: node
        type(poly_type_t), intent(out) :: int_scheme
        type(mono_type_t), intent(out) :: control_type

        ! Create integer type scheme for loop variable
        int_scheme = create_poly_type(forall_vars=[type_var_t ::], &
                                      mono=create_mono_type(TINT))

        ! Do loops don't have a type
        control_type = create_mono_type(TVAR, var=create_type_var(0, "control"))
    end subroutine process_do_loop_body

    ! Process do while body
    subroutine process_do_while_node_body(node, control_type)
        type(do_while_node), intent(in) :: node
        type(mono_type_t), intent(out) :: control_type

        ! Do while loops don't have a type
        control_type = create_mono_type(TVAR, var=create_type_var(0, "control"))
    end subroutine process_do_while_node_body

    ! Process where construct clauses
    subroutine process_where_node_clauses(node, control_type)
        type(where_node), intent(in) :: node
        type(mono_type_t), intent(out) :: control_type

        ! Where constructs don't have a type
        control_type = create_mono_type(TVAR, var=create_type_var(0, "control"))
    end subroutine process_where_node_clauses

    ! Process where statement
    subroutine process_where_stmt_node(node, control_type)
        type(where_stmt_node), intent(in) :: node
        type(mono_type_t), intent(out) :: control_type

        ! Where statements use assignment type or control marker
        control_type = create_mono_type(TVAR, var=create_type_var(0, "control"))
    end subroutine process_where_stmt_node

    ! Process forall construct body
    subroutine process_forall_node_body(node, int_scheme, control_type)
        type(forall_node), intent(in) :: node
        type(poly_type_t), intent(out) :: int_scheme
        type(mono_type_t), intent(out) :: control_type

        ! Create integer type scheme for index variables
        int_scheme = create_poly_type(forall_vars=[type_var_t ::], &
                                      mono=create_mono_type(TINT))

        ! Forall constructs don't have a type
        control_type = create_mono_type(TVAR, var=create_type_var(0, "control"))
    end subroutine process_forall_node_body

    ! Process select case blocks
    subroutine process_select_case_blocks(node, control_type)
        type(select_case_node), intent(in) :: node
        type(mono_type_t), intent(out) :: control_type

        ! Select case constructs don't have a type
        control_type = create_mono_type(TVAR, var=create_type_var(0, "control"))
    end subroutine process_select_case_blocks

    ! Process associate construct body
    subroutine process_associate_node_body(node, control_type)
        type(associate_node), intent(in) :: node
        type(mono_type_t), intent(out) :: control_type

        ! Associate constructs don't have a type
        control_type = create_mono_type(TVAR, var=create_type_var(0, "control"))
    end subroutine process_associate_node_body

    ! Process stop node code
    subroutine process_stop_node_code(node, control_type)
        type(stop_node), intent(in) :: node
        type(mono_type_t), intent(out) :: control_type

        ! Stop statements don't have a type
        control_type = create_mono_type(TVAR, var=create_type_var(0, "control"))
    end subroutine process_stop_node_code

    subroutine process_pause_node_code(node, control_type)
        type(pause_node), intent(in) :: node
        type(mono_type_t), intent(out) :: control_type

        ! Pause statements don't have a type
        control_type = create_mono_type(TVAR, var=create_type_var(0, "control"))
    end subroutine process_pause_node_code

    subroutine process_nullify_node_code(node, control_type)
        type(nullify_node), intent(in) :: node
        type(mono_type_t), intent(out) :: control_type

        ! Nullify statements don't have a type
        control_type = create_mono_type(TVAR, var=create_type_var(0, "control"))
    end subroutine process_nullify_node_code

    ! Process declaration and return type
    subroutine process_declaration_variables(decl, var_type)
        use type_system_unified, only: TARRAY
        type(declaration_node), intent(in) :: decl
        type(mono_type_t), intent(out) :: var_type
        type(mono_type_t) :: element_type
        type(mono_type_t), allocatable :: args(:)
        integer :: var_type_kind
        integer :: rank, i

        ! Map type name to type kind
        select case (trim(decl%type_name))
        case ("integer")
            var_type_kind = TINT
        case ("real")
            var_type_kind = TREAL
        case ("character")
            var_type_kind = TCHAR
        case ("logical")
            var_type_kind = TLOGICAL
        case ("complex")
            var_type_kind = TCOMPLEX
        case ("double precision")
            var_type_kind = TDOUBLE
        case default
            ! Check for derived type (starts with type() or user-defined name)
            if (index(trim(decl%type_name), "type(") == 1) then
                var_type_kind = TDERIVED
            else
                ! Default to real for unknown types
                var_type_kind = TREAL
            end if
        end select

        ! Create element type
        if (var_type_kind == TINT) then
            element_type = create_mono_type(var_type_kind, &
                                            is_unsigned=decl%is_unsigned)
        else
            element_type = create_mono_type(var_type_kind)
        end if

        ! Handle kind parameter if present
        if (decl%has_kind) then
            element_type%kind = var_type_kind
            if (var_type_kind == TCHAR) then
                if (decl%kind_value > 0) then
                    element_type%size = decl%kind_value
                else if (decl%kind_value == -1) then
                    element_type%size = -1
                end if
            end if
        end if

        ! Handle array declarations - build nested array type for each dimension
        if (decl%is_array .and. allocated(decl%dimension_indices)) then
            rank = size(decl%dimension_indices)
            if (rank > 0) then
                var_type = element_type
                do i = rank, 1, -1
                    allocate (args(1))
                    args(1) = var_type
                    var_type = create_mono_type(TARRAY, args=args)
                    deallocate (args)
                end do
                return
            end if
        end if

        var_type = element_type
    end subroutine process_declaration_variables

    ! Check if program has implicit none statement
    function check_implicit_none(arena, prog) result(has_implicit)
        use, intrinsic :: iso_fortran_env, only: error_unit
        use debug_trace, only: trace_is_enabled
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: prog
        logical :: has_implicit
        integer :: i
        integer :: implicit_count

        has_implicit = .false.
        implicit_count = 0

        ! Scan through program body for implicit none statement
        if (allocated(prog%body_indices)) then
            do i = 1, size(prog%body_indices)
                if (prog%body_indices(i) > 0 .and. &
                    prog%body_indices(i) <= arena%size) then
                    if (allocated(arena%entries(prog%body_indices(i))%node)) then
                        select type (stmt => arena%entries(prog%body_indices(i))%node)
                        type is (implicit_statement_node)
                            implicit_count = implicit_count + 1
                            ! True when implicit none statement sets is_none flag
                            if (stmt%is_none) then
                                has_implicit = .true.
                                if (trace_is_enabled()) then
                                    write (error_unit, '(A)') &
                                        'TRACE: Found implicit none!'
                                end if
                                return
                            end if
                        end select
                    end if
                end if
            end do
        end if

        if (trace_is_enabled()) then
            write (error_unit, '(A,I0,A,I0)') &
                'TRACE: check_implicit_none scanned ', &
                size(prog%body_indices), ' nodes, found ', implicit_count, &
                ' implicit statements'
        end if
    end function check_implicit_none

end module semantic_inference_helpers
