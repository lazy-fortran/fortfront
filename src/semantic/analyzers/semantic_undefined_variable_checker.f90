module semantic_undefined_variable_checker
    ! Undefined variable detection and checking utilities
    ! Extracted from semantic_analyzer.f90 for architectural compliance (Issue #1067)
    use ast_core, only: ast_arena_t
    use ast_nodes_core, only: identifier_node, binary_op_node, assignment_node, &
                               call_or_subscript_node, array_literal_node, program_node
    use ast_nodes_control, only: if_node
    use ast_nodes_data, only: declaration_node
    use type_system_unified, only: poly_type_t, mono_type_t, create_poly_type, type_var_t
    use semantic_inference_helpers, only: process_declaration_variables
    use scope_manager, only: scope_stack_t
    use error_handling, only: create_error_result, ERROR_SEMANTIC, result_t, error_collection_t
    implicit none
    private

    public :: check_undefined_variables_generic

contains

    ! Generic implementation that works with any context type
    subroutine check_undefined_variables_generic(scopes, errors, strict_mode, arena, prog_index)
        type(scope_stack_t), intent(inout) :: scopes
        type(error_collection_t), intent(inout) :: errors
        logical, intent(in) :: strict_mode
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: prog_index
        
        ! Only perform undefined variable checking in strict mode
        if (.not. strict_mode) return
        
        ! Recursively traverse the AST to find identifier nodes and check if they're defined
        call traverse_for_undefined_variables(scopes, errors, arena, prog_index)
    end subroutine check_undefined_variables_generic
    
    ! Recursive helper to traverse AST and detect undefined variables
    subroutine traverse_for_undefined_variables(scopes, errors, arena, node_index)
        type(scope_stack_t), intent(inout) :: scopes
        type(error_collection_t), intent(inout) :: errors
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: node_index

        type node_stack_entry
            integer :: idx = 0
        end type node_stack_entry

        type(node_stack_entry), allocatable :: stack(:)
        integer :: top, capacity
        type(poly_type_t), allocatable :: scheme
        type(result_t) :: error_result
        integer :: current_index, i

        capacity = 64
        allocate(stack(capacity))
        top = 0

        call push(node_index)

        do while (top > 0)
            current_index = pop()
            if (current_index <= 0 .or. current_index > arena%size) cycle
            if (.not. allocated(arena%entries(current_index)%node)) cycle

            select type (node => arena%entries(current_index)%node)
            type is (identifier_node)
                if (.not. allocated(node%name) .or. len_trim(node%name) == 0) cycle
                call scopes%lookup(node%name, scheme)
                if (.not. allocated(scheme)) then
                    if (is_declared_in_arena(arena, node%name)) then
                        call define_from_arena(scopes, arena, node%name)
                        cycle
                    end if
                    error_result = create_error_result( &
                        "Undefined variable '" // node%name // "'", &
                        ERROR_SEMANTIC, &
                        component="semantic_analyzer", &
                        context="check_undefined_variables", &
                        suggestion="Declare the variable before using it or remove 'implicit none'" &
                    )
                    call errors%add_result(error_result)
                end if

            type is (program_node)
                if (allocated(node%body_indices)) then
                    do i = size(node%body_indices), 1, -1
                        call push(node%body_indices(i))
                    end do
                end if

            type is (binary_op_node)
                call push(node%right_index)
                call push(node%left_index)

            type is (assignment_node)
                call push(node%value_index)
                call push(node%target_index)

            type is (call_or_subscript_node)
                if (allocated(node%arg_indices)) then
                    do i = size(node%arg_indices), 1, -1
                        call push(node%arg_indices(i))
                    end do
                end if

            type is (array_literal_node)
                if (allocated(node%element_indices)) then
                    do i = size(node%element_indices), 1, -1
                        call push(node%element_indices(i))
                    end do
                end if

            type is (if_node)
                if (allocated(node%else_body_indices)) then
                    do i = size(node%else_body_indices), 1, -1
                        call push(node%else_body_indices(i))
                    end do
                end if
                if (allocated(node%then_body_indices)) then
                    do i = size(node%then_body_indices), 1, -1
                        call push(node%then_body_indices(i))
                    end do
                end if
                call push(node%condition_index)

            class default
                cycle
            end select
        end do

    contains

        subroutine push(idx)
            integer, intent(in) :: idx
            type(node_stack_entry), allocatable :: temp(:)
            if (idx <= 0) return
            if (top >= capacity) then
                allocate(temp(capacity*2))
                if (capacity > 0) temp(1:capacity) = stack(1:capacity)
                call move_alloc(temp, stack)
                capacity = size(stack)
            end if
            top = top + 1
            stack(top)%idx = idx
        end subroutine push

        integer function pop()
            if (top <= 0) then
                pop = 0
            else
                pop = stack(top)%idx
                top = top - 1
            end if
        end function pop

    end subroutine traverse_for_undefined_variables

    ! Helper: check if a name is declared by any declaration_node in the arena
    logical function is_declared_in_arena(arena, name) result(found)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: name
        integer :: i, j
        found = .false.
        do i = 1, arena%size
            if (.not. allocated(arena%entries(i)%node)) cycle
            select type (node => arena%entries(i)%node)
            type is (declaration_node)
                if (allocated(node%var_name)) then
                    if (trim(node%var_name) == trim(name)) then
                        found = .true.
                        return
                    end if
                end if
                if (node%is_multi_declaration .and. allocated(node%var_names)) then
                    do j = 1, size(node%var_names)
                        if (trim(node%var_names(j)) == trim(name)) then
                            found = .true.
                            return
                        end if
                    end do
                end if
            end select
        end do
    end function is_declared_in_arena

    ! Helper: define a symbol in scope using declaration type from arena
    subroutine define_from_arena(scopes, arena, name)
        type(scope_stack_t), intent(inout) :: scopes
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: name
        integer :: i, j
        type(mono_type_t) :: decl_type
        type(poly_type_t) :: scheme
        do i = 1, arena%size
            if (.not. allocated(arena%entries(i)%node)) cycle
            select type (node => arena%entries(i)%node)
            type is (declaration_node)
                if (allocated(node%var_name)) then
                    if (trim(node%var_name) == trim(name)) then
                        call process_declaration_variables(node, decl_type)
                        scheme = create_poly_type(forall_vars=[type_var_t::], mono=decl_type)
                        call scopes%define(name, scheme)
                        return
                    end if
                end if
                if (node%is_multi_declaration .and. allocated(node%var_names)) then
                    do j = 1, size(node%var_names)
                        if (trim(node%var_names(j)) == trim(name)) then
                            call process_declaration_variables(node, decl_type)
                            scheme = create_poly_type(forall_vars=[type_var_t::], mono=decl_type)
                            call scopes%define(name, scheme)
                            return
                        end if
                    end do
                end if
            end select
        end do
    end subroutine define_from_arena



end module semantic_undefined_variable_checker
