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
    use ast_nodes_misc, only: complex_literal_node
    use ast_nodes_loops, only: do_loop_node
    use semantic_validation_utils, only: update_identifier_type_in_arena
    use error_handling, only: result_t, create_error_result, ERROR_SEMANTIC
    use scope_manager, only: scope_stack_t
    use semantic_unsigned_integer_mix_diagnostics, only: &
        emit_unsigned_integer_mix_error, extract_integer_signedness, &
        is_integer_literal_expr
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

    ! Set assignment node type fields for standardizer (Issue #1851)
    subroutine set_assignment_type_fields(arena, assignment_index, &
                                          value_index, expr_typ)
        use type_string_utils, only: mono_type_to_string
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: assignment_index, value_index
        type(mono_type_t), intent(in) :: expr_typ
        type(mono_type_t) :: assign_type
        character(len=:), allocatable :: type_str

        if (.not. arena%has_node_at(assignment_index)) return

        select type (assign_node => arena%entries(assignment_index)%node)
        type is (assignment_node)
            assign_type = expr_typ
            ! Check for complex literal override
            if (value_index > 0 .and. value_index <= arena%size) then
                if (allocated(arena%entries(value_index)%node)) then
                    select type (v => arena%entries(value_index)%node)
                    type is (complex_literal_node)
                        assign_type = create_mono_type(TCOMPLEX)
                    end select
                end if
            end if
            type_str = mono_type_to_string(assign_type)
            assign_node%inferred_type_name = type_str
            assign_node%type_was_inferred = .true.
        end select
    end subroutine set_assignment_type_fields

    ! Process assignment inference with scope and error handling
    subroutine process_assignment_inference(arena, assignment, assignment_index, &
                                            lhs_index, expr_typ, updated_expr_typ, &
                                            scopes, errors, input_mode, &
                                            next_var_id, &
                                            type_hints)
        type(ast_arena_t), intent(inout) :: arena
        type(assignment_node), intent(in) :: assignment
        integer, intent(in) :: assignment_index, lhs_index
        type(mono_type_t), intent(in) :: expr_typ
        type(mono_type_t), intent(out) :: updated_expr_typ
        type(scope_stack_t), intent(inout) :: scopes
        type(error_collection_t), intent(inout) :: errors
        integer, intent(in) :: input_mode
        integer, intent(inout) :: next_var_id
        type(type_annotation_t), intent(in), optional :: type_hints(:)

        updated_expr_typ = expr_typ

        ! Override type for complex literals (Issue #1851)
        call override_type_for_complex_literal(arena, assignment, updated_expr_typ)

        ! Normalize inquiry intrinsic return types
        call normalize_intrinsic_return_types(arena, assignment, updated_expr_typ)

        ! Process LHS based on node type
        if (lhs_index > 0 .and. lhs_index <= arena%size) then
            if (allocated(arena%entries(lhs_index)%node)) then
                select type (lhs_node => arena%entries(lhs_index)%node)
                type is (identifier_node)
                    block
                        type(mono_type_t) :: lhs_typ
                        type(poly_type_t), allocatable :: scheme
                        logical :: lhs_is_int
                        logical :: rhs_is_int
                        logical :: lhs_is_unsigned
                        logical :: rhs_is_unsigned
                        logical :: rhs_is_int_literal
                        character(len=:), allocatable :: lowered

                        lhs_typ%kind = 0
                        call scopes%lookup(lhs_node%name, scheme)
                        if (allocated(scheme)) then
                            call scheme%sync_mono()
                            lhs_typ = scheme%get_mono()
                        else if (.not. fetch_declaration_type(arena, &
                                                              lhs_node%name, &
                                                              lhs_typ)) then
                            lhs_typ = lhs_node%inferred_type
                        end if

                        call extract_integer_signedness(lhs_typ, lhs_is_int, &
                                                        lhs_is_unsigned)
                        rhs_is_int_literal = is_integer_literal_expr(arena, &
                                                                     assignment% &
                                                                     value_index)

                        rhs_is_int = .false.
                        rhs_is_unsigned = .false.
                        call extract_integer_signedness(updated_expr_typ, &
                                                        rhs_is_int, &
                                                        rhs_is_unsigned)

                        if (arena%has_node_at(assignment%value_index)) then
                            select type (rhs_node => arena%entries( &
                                         assignment%value_index)%node)
                            type is (call_or_subscript_node)
                                if (allocated(rhs_node%name)) then
                                    lowered = to_lower(trim(rhs_node%name))
                                    select case (lowered)
                                    case ("uint", "wrap_add", "wrap_sub", &
                                          "wrap_mul")
                                        rhs_is_int = .true.
                                        rhs_is_unsigned = .true.
                                        updated_expr_typ = create_mono_type( &
                                                           TINT, is_unsigned=.true.)
                                        rhs_node%inferred_type = updated_expr_typ
                                    case ("int")
                                        rhs_is_int = .true.
                                        rhs_is_unsigned = .false.
                                        updated_expr_typ = create_mono_type(TINT)
                                        rhs_node%inferred_type = updated_expr_typ
                                    end select
                                end if
                            end select
                        end if

                        if (lhs_is_int .and. rhs_is_int) then
                            if (lhs_is_unsigned .neqv. rhs_is_unsigned) then
                                if (.not. rhs_is_int_literal) then
                                    call emit_unsigned_integer_mix_error(errors)
                                end if
                            end if
                        end if
                    end block
                    call process_identifier_assignment(arena, assignment, &
                                                       assignment_index, lhs_node, &
                                                       updated_expr_typ, scopes, &
                                                       input_mode, type_hints)
                type is (call_or_subscript_node)
                    call handle_array_assignment(arena, assignment_index, &
                                                 lhs_node, &
                                                 expr_typ, updated_expr_typ, scopes)
                end select
            end if
        end if
    end subroutine process_assignment_inference

    ! Override type for complex literal values
    subroutine override_type_for_complex_literal(arena, assignment, expr_typ)
        type(ast_arena_t), intent(inout) :: arena
        type(assignment_node), intent(in) :: assignment
        type(mono_type_t), intent(inout) :: expr_typ

        if (assignment%value_index > 0 .and. assignment%value_index <= arena%size) then
            if (allocated(arena%entries(assignment%value_index)%node)) then
                select type (value_node => arena%entries(assignment%value_index)%node)
                type is (complex_literal_node)
                    expr_typ = create_mono_type(TCOMPLEX)
                end select
            end if
        end if
    end subroutine override_type_for_complex_literal

    ! Normalize inquiry intrinsic return types (size, lbound, ubound)
    subroutine normalize_intrinsic_return_types(arena, assignment, expr_typ)
        type(ast_arena_t), intent(inout) :: arena
        type(assignment_node), intent(in) :: assignment
        type(mono_type_t), intent(inout) :: expr_typ
        character(len=:), allocatable :: call_name_lower
        logical :: dim_present, override_expr_type

        override_expr_type = .false.

        if (assignment%value_index > 0 .and. assignment%value_index <= arena%size) then
            if (allocated(arena%entries(assignment%value_index)%node)) then
                select type (value_node => arena%entries(assignment%value_index)%node)
                type is (call_or_subscript_node)
                    if (allocated(value_node%name)) then
                        call_name_lower = to_lower(trim(value_node%name))
                        select case (call_name_lower)
                        case ("size")
                            expr_typ = create_mono_type(TINT)
                            override_expr_type = .true.
                        case ("lbound", "ubound")
                            dim_present = call_has_dim_argument(value_node)
                            if (dim_present) then
                                expr_typ = create_mono_type(TINT)
                                override_expr_type = .true.
                            else
                                block
                                    type(mono_type_t) :: int_type
                                    int_type = create_mono_type(TINT)
                                    expr_typ = rebuild_array_with_base( &
                                               value_node%inferred_type, int_type)
                                end block
                                override_expr_type = .true.
                            end if
                        case ("len", "len_trim")
                            expr_typ = create_mono_type(TINT)
                            override_expr_type = .true.
                        end select
                        if (override_expr_type) then
                            value_node%inferred_type = expr_typ
                        end if
                    end if
                end select
            end if
        end if
    end subroutine normalize_intrinsic_return_types

    ! Process identifier assignment with scope management
    subroutine process_identifier_assignment(arena, assignment, assignment_index, &
                                             identifier, expr_typ, scopes, &
                                             input_mode, type_hints)
        type(ast_arena_t), intent(inout) :: arena
        type(assignment_node), intent(in) :: assignment
        integer, intent(in) :: assignment_index
        type(identifier_node), intent(in) :: identifier
        type(mono_type_t), intent(inout) :: expr_typ
        type(scope_stack_t), intent(inout) :: scopes
        integer, intent(in) :: input_mode
        type(type_annotation_t), intent(in), optional :: type_hints(:)
        type(poly_type_t) :: scheme
        type(poly_type_t), allocatable :: existing_scheme
        type(mono_type_t) :: final_type

        ! Check if already defined in current or parent scope
        call scopes%lookup(identifier%name, existing_scheme)

        if (.not. allocated(existing_scheme)) then
            if (present(type_hints)) then
                call ensure_declared_from_arena_local(scopes, arena, &
                                                      identifier%name, type_hints)
            else
                call ensure_declared_from_arena_local(scopes, arena, &
                                                      identifier%name)
            end if
            call scopes%lookup(identifier%name, existing_scheme)
        end if

        ! Handle allocatable character detection
        if (expr_typ%kind == TCHAR) then
            call handle_character_allocation(arena, assignment, expr_typ, &
                                             identifier%name)
        end if

        ! Handle allocatable array assignment from function returns (Issue #2075)
        ! When assigning from a function returning allocatable array, preserve
        ! allocatable.
        call handle_allocatable_array_assignment(arena, assignment, expr_typ)

        ! Force complex type for variables assigned from complex literals
        final_type = get_final_assignment_type(arena, assignment, expr_typ)

        ! Issue #2152: Preserve array type if assigning scalar to array
        ! (scalar broadcast is valid in Fortran, e.g., in WHERE blocks)
        if (allocated(existing_scheme)) then
            if (existing_scheme%mono_kind == TARRAY .and. &
                final_type%kind /= TARRAY) then
                ! Keep the existing array type, don't downgrade to scalar
                final_type = existing_scheme%get_mono()
            end if
        end if

        if (allocated(existing_scheme)) then
            call existing_scheme%sync_mono()
            if (existing_scheme%mono_alloc_info%needs_allocatable_string) then
                final_type%alloc_info%needs_allocatable_string = .true.
            end if
            if (existing_scheme%mono_alloc_info%is_allocatable) then
                if (final_type%kind == TARRAY) then
                    if (.not. array_has_explicit_extents(final_type)) then
                        final_type%alloc_info%is_allocatable = .true.
                    end if
                end if
            end if
        end if

        ! Update all identifier nodes in the arena with the inferred type
        ! Skip propagation for standard Fortran (input_mode == INPUT_MODE_STANDARD)
        call update_identifier_type_in_arena(arena, identifier%name, final_type, &
                                             input_mode)

        scheme = create_poly_type(forall_vars=[type_var_t ::], mono=final_type)
        call scopes%define(identifier%name, scheme)

        ! Set assignment node fields for standardizer (Issue #1851)
        call set_assignment_type_fields(arena, assignment_index, &
                                        assignment%value_index, expr_typ)
    end subroutine process_identifier_assignment

    recursive logical function array_has_explicit_extents(typ) &
        result(has_explicit)
        type(mono_type_t), intent(in) :: typ
        type(mono_type_t) :: element_type

        has_explicit = .false.
        if (typ%kind /= TARRAY) return

        if (typ%size > 0) then
            has_explicit = .true.
            return
        end if

        if (typ%has_args()) then
            element_type = typ%get_arg(1)
            has_explicit = array_has_explicit_extents(element_type)
        end if
    end function array_has_explicit_extents

    ! Get final type for assignment, handling complex literal override
    function get_final_assignment_type(arena, assignment, expr_typ) &
        result(final_type)
        type(ast_arena_t), intent(inout) :: arena
        type(assignment_node), intent(in) :: assignment
        type(mono_type_t), intent(in) :: expr_typ
        type(mono_type_t) :: final_type
        integer :: value_idx
        logical :: needs_complex_override

        final_type = expr_typ
        value_idx = assignment%value_index
        needs_complex_override = value_idx > 0 .and. value_idx <= arena%size

        if (needs_complex_override) then
            associate (entry => arena%entries(value_idx))
                if (allocated(entry%node)) then
                    select type (v => entry%node)
                    type is (complex_literal_node)
                        final_type = create_mono_type(TCOMPLEX)
                    end select
                end if
            end associate
        end if
    end function get_final_assignment_type

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

    ! Handle allocatable array assignment from function returns (Issue #2075)
    subroutine handle_allocatable_array_assignment(arena, assignment, expr_typ)
        type(ast_arena_t), intent(inout) :: arena
        type(assignment_node), intent(in) :: assignment
        type(mono_type_t), intent(inout) :: expr_typ

        ! If RHS is a call returning an allocatable array, use the call's full type
        if (assignment%value_index > 0 .and. assignment%value_index <= arena%size) then
            if (allocated(arena%entries(assignment%value_index)%node)) then
                select type (value_node => arena%entries(assignment%value_index)%node)
                type is (call_or_subscript_node)
                    ! Check if the call returns an allocatable array
                    if (value_node%inferred_type%kind == TARRAY) then
                        if (value_node%inferred_type%alloc_info% &
                            is_allocatable) then
                            ! Use the full array type from the function call.
                            ! This ensures expr_typ has both TARRAY kind and
                            ! allocatable flag.
                            expr_typ = value_node%inferred_type
                        end if
                    end if
                end select
            end if
        end if
    end subroutine handle_allocatable_array_assignment

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
        if (.not. arena%has_node_at(expr_index)) return

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
        if (.not. arena%has_node_at(start_index)) return

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

    logical function call_has_dim_argument(call_node) result(has_dim)
        type(call_or_subscript_node), intent(in) :: call_node

        if (.not. allocated(call_node%arg_indices)) then
            has_dim = .false.
        else
            has_dim = size(call_node%arg_indices) >= 2
        end if
    end function call_has_dim_argument

    recursive function rebuild_array_with_base(original, base_type) &
        result(result_type)
        type(mono_type_t), intent(in) :: original
        type(mono_type_t), intent(in) :: base_type
        type(mono_type_t) :: result_type
        type(mono_type_t) :: element_type
        type(mono_type_t), allocatable :: args(:)

        if (original%kind /= TARRAY) then
            result_type = base_type
            return
        end if

        if (original%has_args()) then
            element_type = rebuild_array_with_base(original%get_arg(1), base_type)
        else
            element_type = base_type
        end if

        allocate (args(1))
        args(1) = element_type
        result_type = create_mono_type(TARRAY, args=args)
        result_type%size = original%size
        result_type%alloc_info = original%alloc_info
        deallocate (args)
    end function rebuild_array_with_base

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
