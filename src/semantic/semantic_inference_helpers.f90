module semantic_inference_helpers
    ! Helper module for type inference of control flow and declarations
    ! This avoids circular dependencies with semantic_analyzer
    use type_system_unified, only: mono_type_t, poly_type_t, type_var_t, &
                                   create_mono_type, create_type_var, create_poly_type, &
                                   TVAR, TINT, TREAL, TCHAR, TLOGICAL, TCOMPLEX, TDOUBLE, TDERIVED
    use ast_core
    use ast_nodes_core, only: program_node
    use ast_nodes_control, only: if_node, do_while_node, where_node, where_stmt_node, &
                                  forall_node, select_case_node, associate_node, &
                                  stop_node
    use ast_nodes_data, only: declaration_node
    use scope_manager, only: scope_stack_t
    implicit none
    private

    public :: process_if_node_branches
    public :: process_do_while_node_body
    public :: process_where_node_clauses
    public :: process_where_stmt_node
    public :: process_forall_node_body
    public :: process_select_case_blocks
    public :: process_associate_node_body
    public :: process_stop_node_code
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
        int_scheme = create_poly_type(forall_vars=[type_var_t::], &
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

    ! Process declaration and return type
    subroutine process_declaration_variables(decl, var_type)
        type(declaration_node), intent(in) :: decl
        type(mono_type_t), intent(out) :: var_type
        integer :: var_type_kind
        
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
            ! Check if it's a derived type (starts with "type(" or contains user-defined name)
            if (index(trim(decl%type_name), "type(") == 1) then
                var_type_kind = TDERIVED
            else
                ! Default to real for unknown types
                var_type_kind = TREAL
            end if
        end select
        
        ! Create mono type
        var_type = create_mono_type(var_type_kind)
        
        ! Handle kind parameter if present
        if (decl%has_kind) then
            var_type%kind = var_type_kind
        end if
    end subroutine process_declaration_variables

    ! Check if program has implicit none statement
    function check_implicit_none(arena, prog) result(has_implicit)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: prog
        logical :: has_implicit
        integer :: i

        has_implicit = .false.
        
        ! Scan through program body for implicit none statement
        if (allocated(prog%body_indices)) then
            do i = 1, size(prog%body_indices)
                if (prog%body_indices(i) > 0 .and. &
                    prog%body_indices(i) <= arena%size) then
                    if (allocated(arena%entries(prog%body_indices(i))%node)) then
                        select type (stmt => arena%entries(prog%body_indices(i))%node)
                        type is (declaration_node)
                            ! Check if it's an implicit none declaration
                            if (allocated(stmt%type_name)) then
                                if (index(stmt%type_name, "implicit") > 0) then
                                    has_implicit = .true.
                                    return
                                end if
                            end if
                        end select
                    end if
                end if
            end do
        end if
    end function check_implicit_none

end module semantic_inference_helpers