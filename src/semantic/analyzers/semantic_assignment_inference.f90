module semantic_assignment_inference
    ! Assignment inference logic extracted from semantic_analyzer
    ! for architectural compliance (Issue #1117)
    use type_system_unified, only: mono_type_t, poly_type_t, type_var_t, &
                                   create_mono_type, create_poly_type, &
                                   TCHAR, TARRAY, TINT, TREAL, TLOGICAL, &
                                   TCOMPLEX, TDOUBLE, TDERIVED
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: identifier_node, binary_op_node, assignment_node, &
                              call_or_subscript_node, literal_node
    use ast_nodes_loops, only: do_loop_node
    use semantic_validation_utils, only: update_identifier_type_in_arena
    use error_handling, only: result_t, create_error_result, ERROR_SEMANTIC
    use scope_manager, only: scope_stack_t
    use standardizer_types, only: calculate_loop_size
    ! No direct dependency on function analysis here
    use error_handling, only: error_collection_t
    use parser_type_hooks_module, only: type_annotation_t
    use semantic_annotation_utils, only: type_from_annotation
    use semantic_declaration_utils, only: fetch_declaration_type
    use lexer_core, only: to_lower
    use string_utils_mod, only: int_to_string
    implicit none
    private

    public :: process_assignment_inference
    public :: ensure_var_declared_from_arena

contains

    ! Process assignment inference with scope and error handling
    subroutine process_assignment_inference(arena, assignment, assignment_index, &
                                            lhs_index, expr_typ, updated_expr_typ, &
                                            scopes, errors, strict_mode, next_var_id, &
                                            type_hints)
        type(ast_arena_t), intent(inout) :: arena
        type(assignment_node), intent(in) :: assignment
        integer, intent(in) :: assignment_index, lhs_index
        type(mono_type_t), intent(in) :: expr_typ
        type(mono_type_t), intent(out) :: updated_expr_typ
        type(scope_stack_t), intent(inout) :: scopes
        type(error_collection_t), intent(inout) :: errors
        logical, intent(in) :: strict_mode
        integer, intent(inout) :: next_var_id
        type(type_annotation_t), intent(in), optional :: type_hints(:)
        type(poly_type_t) :: scheme
        type(poly_type_t), allocatable :: existing_scheme
        type(result_t) :: error_result

        updated_expr_typ = expr_typ

        if (lhs_index > 0 .and. lhs_index <= arena%size) then
            if (allocated(arena%entries(lhs_index)%node)) then
                select type (lhs_node => arena%entries(lhs_index)%node)
                type is (identifier_node)
                    ! Check if already defined in current or parent scope
                    call scopes%lookup(lhs_node%name, existing_scheme)

                    if (.not. allocated(existing_scheme)) then
                        ! Fallback: if the arena has a declaration, define it in scope
                        ! before flagging it undefined. Handles multi-var inits and
                        ! re-ordered nodes.
                        if (present(type_hints)) then
                            call ensure_declared_from_arena_local(scopes, arena, &
                                                                  lhs_node%name, &
                                                                  type_hints)
                        else
                            call ensure_declared_from_arena_local(scopes, arena, &
                                                                  lhs_node%name)
                        end if
                        call scopes%lookup(lhs_node%name, existing_scheme)
                    end if

                    if (.not. allocated(existing_scheme)) then
                        ! Do not raise an error here. A centralized undefined-variable
                        ! check runs after inference and handles strict-mode diagnostics
                        ! with proper arena-backed discovery. Keeping this path silent
                        ! avoids duplicate or premature errors for multi-declarations.
                    end if

                    ! Handle allocatable character detection
                    if (updated_expr_typ%kind == TCHAR) then
                        call handle_character_allocation(arena, assignment, &
                                                         updated_expr_typ, &
                                                         lhs_node%name)
                    end if

                    ! Update all identifier nodes in the arena with the inferred type
                    call update_identifier_type_in_arena(arena, lhs_node%name, &
                                                         updated_expr_typ)

                    ! Generalize the expression type and define/update in scope
                    scheme = create_poly_type(forall_vars=[type_var_t ::], &
                                              mono=updated_expr_typ)
                    call scopes%define(lhs_node%name, scheme)
                type is (call_or_subscript_node)
                    call handle_array_assignment(arena, assignment_index, lhs_node, &
                                                 expr_typ, updated_expr_typ, scopes)
                end select
            end if
        end if
    end subroutine process_assignment_inference

    ! Local helper: best-effort define symbol from any declaration present in the arena
    subroutine ensure_declared_from_arena_local(scopes, arena, name, type_hints)
        type(scope_stack_t), intent(inout) :: scopes
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: name
        type(type_annotation_t), intent(in), optional :: type_hints(:)
        integer :: i, j
        type(mono_type_t) :: decl_type
        type(poly_type_t) :: scheme

        if (present(type_hints)) then
            do i = 1, size(type_hints)
                do j = 1, size(type_hints(i)%var_names)
                    if (trim(type_hints(i)%var_names(j)) == trim(name)) then
                        call type_from_annotation(type_hints(i), decl_type)
                        scheme = create_poly_type(forall_vars=[type_var_t ::], &
                                                  mono=decl_type)
                        call scopes%define(name, scheme)
                        return
                    end if
                end do
            end do
        end if

        if (fetch_declaration_type(arena, name, decl_type)) then
            scheme = create_poly_type(forall_vars=[type_var_t ::], mono=decl_type)
            call scopes%define(name, scheme)
            return
        end if
    end subroutine ensure_declared_from_arena_local

    ! Handle character allocation detection for string concatenation
    subroutine handle_character_allocation(arena, assignment, expr_typ, var_name)
        type(ast_arena_t), intent(inout) :: arena
        type(assignment_node), intent(in) :: assignment
        type(mono_type_t), intent(inout) :: expr_typ
        character(len=*), intent(in) :: var_name

        if (assignment%value_index > 0 .and. assignment%value_index <= arena%size) then
            if (allocated(arena%entries(assignment%value_index)%node)) then
                select type (value_node => arena%entries(assignment%value_index)%node)
                type is (binary_op_node)
                    if (value_node%operator == "//") then
                        ! Only mark as allocatable if size was not calculated
                        if (expr_typ%size < 0) then
                            expr_typ%alloc_info%is_allocatable = .true.
                            expr_typ%alloc_info%needs_allocatable_string = .true.
                            expr_typ%size = 0  ! Deferred length
                        end if

                        ! Update all existing identifier nodes with this name
                        call update_identifier_type_in_arena(arena, var_name, expr_typ)
                    end if
                end select
            end if
        end if
    end subroutine handle_character_allocation

    ! Process array element assignments and infer multi-dimensional array types
    subroutine handle_array_assignment(arena, assignment_index, call_node, expr_typ, &
                                       updated_expr_typ, scopes)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: assignment_index
        class(call_or_subscript_node), intent(inout) :: call_node
        type(mono_type_t), intent(in) :: expr_typ
        type(mono_type_t), intent(out) :: updated_expr_typ
        type(scope_stack_t), intent(inout) :: scopes

        character(len=:), allocatable :: base_name
        integer :: rank, i
        integer, allocatable :: dim_sizes(:)
        type(mono_type_t) :: array_type
        type(mono_type_t) :: element_type
        type(poly_type_t) :: scheme
        character(len=:), allocatable :: decl_string

        updated_expr_typ = expr_typ

        if (.not. allocated(call_node%name)) return
        base_name = trim(call_node%name)
        if (len_trim(base_name) == 0) return

        if (.not. allocated(call_node%arg_indices)) return
        rank = size(call_node%arg_indices)
        if (rank <= 0) return

        allocate (dim_sizes(rank))
        do i = 1, rank
            dim_sizes(i) = infer_dimension_size_from_index(arena, assignment_index, &
                                                           call_node%arg_indices(i))
        end do

        element_type = expr_typ
        if (element_type%kind <= 0) then
            element_type = create_mono_type(TINT)
        end if

        array_type = build_array_type_from_dims(element_type, dim_sizes)

        call_node%is_array_access = .true.
        call_node%inferred_type = element_type
        updated_expr_typ = element_type

        call update_identifier_type_in_arena(arena, base_name, array_type)

        scheme = create_poly_type(forall_vars=[type_var_t ::], mono=array_type)
        call scopes%define(base_name, scheme)

        decl_string = build_array_declaration_string(element_type, dim_sizes)

        if (assignment_index > 0 .and. assignment_index <= arena%size) then
            if (allocated(arena%entries(assignment_index)%node)) then
                select type (assign_node => arena%entries(assignment_index)%node)
                type is (assignment_node)
                    assign_node%type_was_inferred = .true.
                    assign_node%inferred_type_name = decl_string
                end select
            end if
        end if

        if (allocated(dim_sizes)) deallocate (dim_sizes)
    end subroutine handle_array_assignment

    ! Determine inferred dimension size from an index expression
    integer function infer_dimension_size_from_index(arena, assignment_index, &
                                                     expr_index) result(dim_size)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: assignment_index, expr_index

        dim_size = 0
        if (expr_index <= 0 .or. expr_index > arena%size) return
        if (.not. allocated(arena%entries(expr_index)%node)) return

        select type (arg_node => arena%entries(expr_index)%node)
        type is (identifier_node)
            dim_size = find_loop_extent_for_variable(arena, assignment_index, &
                                                     arg_node%name)
        type is (literal_node)
            dim_size = 0
        class default
            dim_size = 0
        end select
    end function infer_dimension_size_from_index

    ! Traverse parent nodes to find the loop bounds for a given index variable
    integer function find_loop_extent_for_variable(arena, start_index, var_name) &
        result(extent)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: start_index
        character(len=*), intent(in) :: var_name

        integer :: parent_idx
        integer :: candidate_idx
        integer :: body_pos
        integer :: body_idx

        extent = 0
        if (start_index <= 0 .or. start_index > arena%size) return

        parent_idx = arena%entries(start_index)%parent_index
        do while (parent_idx > 0 .and. parent_idx <= arena%size)
            if (allocated(arena%entries(parent_idx)%node)) then
                select type (loop_node => arena%entries(parent_idx)%node)
                type is (do_loop_node)
                    if (allocated(loop_node%var_name)) then
                        if (trim(loop_node%var_name) == trim(var_name)) then
                            extent = calculate_loop_size(arena, &
                                                         loop_node%start_expr_index, &
                                                         loop_node%end_expr_index, &
                                                         loop_node%step_expr_index)
                            if (extent < 0) extent = 0
                            return
                        end if
                    end if
                end select
            end if
            parent_idx = arena%entries(parent_idx)%parent_index
        end do

        ! Fallback: check enclosing loop bodies for preceding loops with matching index
        parent_idx = arena%entries(start_index)%parent_index
        do while (parent_idx > 0 .and. parent_idx <= arena%size)
            if (.not. allocated(arena%entries(parent_idx)%node)) then
                parent_idx = arena%entries(parent_idx)%parent_index
                cycle
            end if

            select type (container => arena%entries(parent_idx)%node)
            type is (do_loop_node)
                if (allocated(container%body_indices)) then
                    body_pos = 0
                    do body_idx = 1, size(container%body_indices)
                        if (container%body_indices(body_idx) == start_index) then
                            body_pos = body_idx
                            exit
                        end if
                    end do

                    if (body_pos > 1) then
                        do body_idx = body_pos - 1, 1, -1
                            candidate_idx = container%body_indices(body_idx)
                            if (candidate_idx <= 0 .or. candidate_idx > &
                                arena%size) cycle
                            if (.not. &
                                allocated(arena%entries(candidate_idx)%node)) cycle

                            select type (sibling_loop => &
                                         arena%entries(candidate_idx)%node)
                            type is (do_loop_node)
                                if (allocated(sibling_loop%var_name)) then
                                    if (trim(sibling_loop%var_name) == &
                                        trim(var_name)) then
                                        extent = calculate_loop_size( &
                                                 arena, &
                                                 sibling_loop%start_expr_index, &
                                                 sibling_loop%end_expr_index, &
                                                 sibling_loop%step_expr_index)
                                        if (extent < 0) extent = 0
                                        if (extent > 0) return
                                    end if
                                end if
                            end select
                        end do
                    end if
                end if
            end select

            parent_idx = arena%entries(parent_idx)%parent_index
        end do

        do parent_idx = start_index - 1, 1, -1
            if (parent_idx > arena%size) cycle
            if (.not. allocated(arena%entries(parent_idx)%node)) cycle
            select type (loop_node => arena%entries(parent_idx)%node)
            type is (do_loop_node)
                if (allocated(loop_node%var_name)) then
                    if (trim(loop_node%var_name) == trim(var_name)) then
                        extent = calculate_loop_size(arena, &
                                                     loop_node%start_expr_index, &
                                                     loop_node%end_expr_index, &
                                                     loop_node%step_expr_index)
                        if (extent < 0) extent = 0
                        if (extent > 0) return
                    end if
                end if
            end select
        end do
    end function find_loop_extent_for_variable

    ! Build nested array type information from inferred dimension sizes
    function build_array_type_from_dims(element_type, dim_sizes) result(array_type)
        type(mono_type_t), intent(in) :: element_type
        integer, intent(in) :: dim_sizes(:)
        type(mono_type_t) :: array_type
        type(mono_type_t) :: current_type
        type(mono_type_t), allocatable :: args(:)
        integer :: idx

        current_type = element_type
        if (size(dim_sizes) == 0) then
            array_type = current_type
            return
        end if

        do idx = size(dim_sizes), 1, -1
            allocate (args(1))
            args(1) = current_type
            if (dim_sizes(idx) > 0) then
                current_type = create_mono_type(TARRAY, args=args, &
                                                array_size=dim_sizes(idx))
            else
                current_type = create_mono_type(TARRAY, args=args)
                current_type%alloc_info%is_allocatable = .true.
                current_type%alloc_info%needs_allocation_check = .true.
            end if
            deallocate (args)
        end do

        array_type = current_type
    end function build_array_type_from_dims

    function build_array_declaration_string(element_type, dim_sizes) &
        result(decl_string)
        type(mono_type_t), intent(in) :: element_type
        integer, intent(in) :: dim_sizes(:)
        character(len=:), allocatable :: decl_string
        character(len=:), allocatable :: base_type
        character(len=:), allocatable :: dims_string
        character(len=32) :: dim_component
        integer :: i

        base_type = get_base_type_name(element_type)

        dims_string = ""
        do i = 1, size(dim_sizes)
            if (i > 1) dims_string = dims_string // ","
            if (dim_sizes(i) > 0) then
                dim_component = int_to_string(dim_sizes(i))
                dims_string = dims_string // trim(dim_component)
            else
                dims_string = dims_string // ":"
            end if
        end do

        decl_string = trim(base_type)
        if (len_trim(dims_string) > 0) then
            decl_string = decl_string // ', dimension(' // trim(dims_string) // ')'
        end if
    end function build_array_declaration_string

    function get_base_type_name(element_type) result(base_type)
        type(mono_type_t), intent(in) :: element_type
        character(len=:), allocatable :: base_type
        character(len=64) :: buffer

        select case (element_type%kind)
        case (TINT)
            base_type = 'integer'
        case (TREAL)
            base_type = 'real'
        case (TCHAR)
            if (element_type%size > 0) then
                write (buffer, '("character(len=",I0,")")') element_type%size
                base_type = trim(buffer)
            else
                base_type = 'character(len=:), allocatable'
            end if
        case (TLOGICAL)
            base_type = 'logical'
        case default
            base_type = 'real'
        end select
    end function get_base_type_name

    subroutine ensure_var_declared_from_arena(arena, name, scopes, &
                                              generalize_fn, next_var_id)
        use ast_nodes_data, only: declaration_node
        use semantic_inference_helpers, only: process_declaration_variables
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: name
        type(scope_stack_t), intent(inout) :: scopes
        interface
            function generalize_fn(t) result(s)
                import :: mono_type_t, poly_type_t
                type(mono_type_t), intent(in) :: t
                type(poly_type_t) :: s
            end function generalize_fn
        end interface
        integer, intent(inout) :: next_var_id
        integer :: i, j
        type(poly_type_t) :: scheme
        type(mono_type_t) :: decl_type

        do i = 1, arena%size
            if (.not. allocated(arena%entries(i)%node)) cycle
            select type (node => arena%entries(i)%node)
            type is (declaration_node)
                if (allocated(node%var_name)) then
                    if (trim(node%var_name) == trim(name)) then
                        call process_declaration_variables(node, decl_type)
                        scheme = generalize_fn(decl_type)
                        call scopes%define(name, scheme)
                        return
                    end if
                end if
                if (node%is_multi_declaration .and. allocated(node%var_names)) then
                    do j = 1, size(node%var_names)
                        if (trim(node%var_names(j)) == trim(name)) then
                            call process_declaration_variables(node, decl_type)
                            scheme = generalize_fn(decl_type)
                            call scopes%define(name, scheme)
                            return
                        end if
                    end do
                end if
            end select
        end do
    end subroutine ensure_var_declared_from_arena

end module semantic_assignment_inference
