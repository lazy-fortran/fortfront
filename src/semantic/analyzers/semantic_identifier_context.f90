module semantic_identifier_context
    use type_system_unified, only: type_var_t, mono_type_t, poly_type_t, &
                                   create_mono_type, TVAR, TREAL, TARRAY, TFUN
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: identifier_node, assignment_node, &
                              call_or_subscript_node, binary_op_node, program_node
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node
    use ast_nodes_data, only: declaration_node
    use scope_manager, only: scope_stack_t
    use semantic_type_operations, only: instantiate_type_scheme_op
    use semantic_array_type_builders, only: build_deferred_shape_array, &
                                            collapse_array_rank
    use semantic_procedure_utils, only: declaration_type_to_mono
    use semantic_expression_context, only: infer_expression_type_static
    implicit none
    private

    public :: infer_identifier_type_from_context
    public :: find_nearest_scope_owner
    public :: find_program_owner

contains

    function infer_identifier_type_from_context(arena, ident_name, param_names, &
                                                param_types, scopes, anchor_index, &
                                                next_var_id) result(typ)
        type(ast_arena_t), intent(in) :: arena
        character(len=*), intent(in) :: ident_name
        character(len=64), allocatable, intent(in) :: param_names(:)
        type(mono_type_t), allocatable, intent(in) :: param_types(:)
        type(scope_stack_t), intent(in) :: scopes
        integer, intent(in) :: anchor_index
        integer, intent(inout) :: next_var_id
        type(mono_type_t) :: typ
        integer :: scope_index
        integer :: program_index
        integer :: search_start
        character(len=64) :: lowered_name

        typ%kind = 0
        lowered_name = trim(ident_name)
        if (len_trim(lowered_name) == 0) return
        if (arena%size <= 0) return

        call instantiate_scope_type(scopes, lowered_name, next_var_id, typ)
        if (typ%kind /= 0) return

        call determine_anchor_context(arena, anchor_index, scope_index, &
                                      program_index, search_start)
        call search_identifier_type_bidirectional(arena, lowered_name, param_names, &
                                                  param_types, scope_index, &
                                                  program_index, search_start, &
                                                  anchor_index, typ)
    end function infer_identifier_type_from_context

    subroutine instantiate_scope_type(scopes, lowered_name, next_var_id, typ)
        type(scope_stack_t), intent(in) :: scopes
        character(len=*), intent(in) :: lowered_name
        integer, intent(inout) :: next_var_id
        type(mono_type_t), intent(inout) :: typ
        type(poly_type_t), allocatable :: scheme

        call scopes%lookup(lowered_name, scheme)
        if (allocated(scheme)) then
            typ = instantiate_type_scheme_op(scheme, next_var_id)
            deallocate (scheme)
        end if
    end subroutine instantiate_scope_type

    subroutine determine_anchor_context(arena, anchor_index, scope_index, &
                                        program_index, search_start)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: anchor_index
        integer, intent(out) :: scope_index
        integer, intent(out) :: program_index
        integer, intent(out) :: search_start

        scope_index = -1
        program_index = -1
        if (anchor_index > 0 .and. anchor_index <= arena%size) then
            scope_index = find_nearest_scope_owner(arena, anchor_index)
            program_index = find_program_owner(arena, anchor_index)
            search_start = anchor_index - 1
        else
            search_start = arena%size
        end if
    end subroutine determine_anchor_context

    subroutine search_identifier_type_bidirectional(arena, lowered_name, param_names, &
                                                    param_types, scope_index, &
                                                    program_index, search_start, &
                                                    anchor_index, typ)
        type(ast_arena_t), intent(in) :: arena
        character(len=*), intent(in) :: lowered_name
        character(len=64), allocatable, intent(in) :: param_names(:)
        type(mono_type_t), allocatable, intent(in) :: param_types(:)
        integer, intent(in) :: scope_index
        integer, intent(in) :: program_index
        integer, intent(in) :: search_start
        integer, intent(in) :: anchor_index
        type(mono_type_t), intent(inout) :: typ
        integer :: forward_start

        if (typ%kind /= 0) return

        if (search_start >= 1) then
            call search_identifier_type_range(arena, lowered_name, param_names, &
                                              param_types, scope_index, &
                                              program_index, search_start, 1, -1, typ)
            if (typ%kind /= 0) return
        end if

        forward_start = compute_forward_start(anchor_index, arena%size)
        if (forward_start <= arena%size) then
            call search_identifier_type_range(arena, lowered_name, param_names, &
                                              param_types, scope_index, &
                                              program_index, forward_start, &
                                              arena%size, 1, typ)
        end if
    end subroutine search_identifier_type_bidirectional

    integer function compute_forward_start(anchor_index, arena_size) result(start)
        integer, intent(in) :: anchor_index
        integer, intent(in) :: arena_size

        if (anchor_index > 0 .and. anchor_index < arena_size) then
            start = anchor_index + 1
        else
            start = 1
        end if
    end function compute_forward_start

    subroutine search_identifier_type_range(arena, lowered_name, param_names, &
                                            param_types, scope_index, program_index, &
                                            start_idx, end_idx, step, typ)
        type(ast_arena_t), intent(in) :: arena
        character(len=*), intent(in) :: lowered_name
        character(len=64), allocatable, intent(in) :: param_names(:)
        type(mono_type_t), allocatable, intent(in) :: param_types(:)
        integer, intent(in) :: scope_index
        integer, intent(in) :: program_index
        integer, intent(in) :: start_idx
        integer, intent(in) :: end_idx
        integer, intent(in) :: step
        type(mono_type_t), intent(inout) :: typ
        type(mono_type_t) :: candidate
        integer :: idx

        if (typ%kind /= 0 .or. step == 0) return
        do idx = start_idx, end_idx, step
            if (.not. allocated(arena%entries(idx)%node)) cycle
            if (.not. identifier_visible_in_scope(arena, idx, scope_index, &
                                                  program_index)) cycle
            candidate = infer_identifier_type_at_index(arena, idx, lowered_name, &
                                                       param_names, param_types, &
                                                       scope_index, program_index)
            if (candidate%kind /= 0) then
                typ = candidate
                return
            end if
        end do
    end subroutine search_identifier_type_range

    function infer_identifier_type_at_index(arena, entry_index, lowered_name, &
                                            param_names, param_types, scope_index, &
                                            program_index) result(candidate)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: entry_index
        character(len=*), intent(in) :: lowered_name
        character(len=64), allocatable, intent(in) :: param_names(:)
        type(mono_type_t), allocatable, intent(in) :: param_types(:)
        integer, intent(in) :: scope_index
        integer, intent(in) :: program_index
        type(mono_type_t) :: candidate
        integer :: name_idx

        candidate%kind = 0
        if (.not. allocated(arena%entries(entry_index)%node)) return

        select type (node => arena%entries(entry_index)%node)
        type is (declaration_node)
            if (allocated(node%var_name)) then
                if (trim(node%var_name) == lowered_name) then
                    candidate = declaration_type_to_mono(node%type_name)
                    if (candidate%kind /= 0) return
                end if
            end if
            if (node%is_multi_declaration .and. allocated(node%var_names)) then
                do name_idx = 1, size(node%var_names)
                    if (trim(node%var_names(name_idx)) == lowered_name) then
                        candidate = declaration_type_to_mono(node%type_name)
                        if (candidate%kind /= 0) return
                    end if
                end do
            end if
        type is (assignment_node)
            candidate = check_assignment_for_identifier(arena, node, lowered_name, &
                                                        param_names, param_types, &
                                                        scope_index, program_index)
        type is (call_or_subscript_node)
            candidate = check_call_for_identifier(arena, node, lowered_name, &
                                                  param_names, param_types)
        type is (binary_op_node)
            candidate = check_binary_op_for_identifier(arena, node, lowered_name, &
                                                       param_names, param_types, &
                                                       entry_index)
        end select
    end function infer_identifier_type_at_index

    function check_assignment_for_identifier(arena, node, lowered_name, &
                                             param_names, param_types, scope_index, &
                                             program_index) result(candidate)
        type(ast_arena_t), intent(in) :: arena
        type(assignment_node), intent(in) :: node
        character(len=*), intent(in) :: lowered_name
        character(len=64), allocatable, intent(in) :: param_names(:)
        type(mono_type_t), allocatable, intent(in) :: param_types(:)
        integer, intent(in) :: scope_index
        integer, intent(in) :: program_index
        type(mono_type_t) :: candidate
        integer :: target_index
        integer :: rank
        type(mono_type_t) :: element_type

        candidate%kind = 0
        target_index = node%target_index
        if (.not. arena%has_node_at(target_index)) return

        select type (target => arena%entries(target_index)%node)
        type is (identifier_node)
            if (trim(target%name) /= lowered_name) return
            candidate = infer_expression_type_static(arena, node%value_index, &
                                                     param_names, param_types)
        type is (call_or_subscript_node)
            if (.not. allocated(target%name)) return
            if (trim(target%name) /= lowered_name) return
            if (.not. allocated(target%arg_indices)) return
            rank = size(target%arg_indices)
            if (rank <= 0) return
            element_type = infer_expression_type_static(arena, node%value_index, &
                                                        param_names, param_types)
            if (element_type%kind == 0) element_type = create_mono_type(TREAL)
            candidate = build_deferred_shape_array(element_type, rank)
        end select
    end function check_assignment_for_identifier

    function check_call_for_identifier(arena, node, lowered_name, &
                                       param_names, param_types) result(candidate)
        type(ast_arena_t), intent(in) :: arena
        type(call_or_subscript_node), intent(in) :: node
        character(len=*), intent(in) :: lowered_name
        character(len=64), allocatable, intent(in) :: param_names(:)
        type(mono_type_t), allocatable, intent(in) :: param_types(:)
        type(mono_type_t) :: candidate
        type(mono_type_t) :: element_type
        integer :: rank
        integer :: i

        candidate%kind = 0
        if (.not. allocated(node%name)) return
        if (trim(node%name) /= lowered_name) return
        if (.not. allocated(node%arg_indices)) return
        rank = size(node%arg_indices)
        if (rank <= 0) return

        element_type%kind = 0
        do i = 1, size(param_names)
            if (trim(param_names(i)) /= lowered_name) cycle
            element_type = param_types(i)
            exit
        end do

        if (element_type%kind == TARRAY) element_type = &
            collapse_array_rank(element_type, rank)
        if (element_type%kind <= 0 .or. element_type%kind == TFUN) then
            element_type = create_mono_type(TREAL)
        end if
        candidate = build_deferred_shape_array(element_type, rank)
    end function check_call_for_identifier

    function check_binary_op_for_identifier(arena, node, lowered_name, &
                                            param_names, param_types, &
                                            entry_index) result(candidate)
        type(ast_arena_t), intent(in) :: arena
        type(binary_op_node), intent(in) :: node
        character(len=*), intent(in) :: lowered_name
        character(len=64), allocatable, intent(in) :: param_names(:)
        type(mono_type_t), allocatable, intent(in) :: param_types(:)
        integer, intent(in) :: entry_index
        type(mono_type_t) :: candidate

        candidate%kind = 0
        if (is_identifier_reference(arena, node%left_index, lowered_name)) then
            candidate = infer_expression_type_static(arena, node%right_index, &
                                                     param_names, param_types)
            if (candidate%kind == 0) then
                candidate = infer_expression_type_static(arena, entry_index, &
                                                         param_names, param_types)
            end if
            if (candidate%kind /= 0) return
        end if

        if (is_identifier_reference(arena, node%right_index, lowered_name)) then
            candidate = infer_expression_type_static(arena, node%left_index, &
                                                     param_names, param_types)
            if (candidate%kind == 0) then
                candidate = infer_expression_type_static(arena, entry_index, &
                                                         param_names, param_types)
            end if
        end if
    end function check_binary_op_for_identifier

    logical function is_identifier_reference(arena, node_index, lowered_name)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=*), intent(in) :: lowered_name

        is_identifier_reference = .false.
        if (.not. arena%has_node_at(node_index)) return

        select type (node => arena%entries(node_index)%node)
        type is (identifier_node)
            if (allocated(node%name)) then
                if (trim(node%name) == lowered_name) then
                    is_identifier_reference = .true.
                end if
            end if
        end select
    end function is_identifier_reference

    integer function find_nearest_scope_owner(arena, node_index) result(scope_index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer :: current

        scope_index = 0
        current = node_index
        do while (current > 0 .and. current <= arena%size)
            if (.not. allocated(arena%entries(current)%node)) then
                current = arena%entries(current)%parent_index
                cycle
            end if
            select type (owner => arena%entries(current)%node)
            type is (function_def_node)
                scope_index = current
                return
            type is (subroutine_def_node)
                scope_index = current
                return
            end select
            current = arena%entries(current)%parent_index
        end do
    end function find_nearest_scope_owner

    integer function find_program_owner(arena, node_index) result(program_index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer :: current

        program_index = 0
        current = node_index
        do while (current > 0 .and. current <= arena%size)
            if (.not. allocated(arena%entries(current)%node)) then
                current = arena%entries(current)%parent_index
                cycle
            end if
            select type (owner => arena%entries(current)%node)
            type is (program_node)
                program_index = current
                return
            end select
            current = arena%entries(current)%parent_index
        end do
    end function find_program_owner

    logical function identifier_visible_in_scope(arena, candidate_index, &
                                                 scope_index, program_index) &
        result(is_visible)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: candidate_index
        integer, intent(in) :: scope_index
        integer, intent(in) :: program_index
        integer :: candidate_scope
        integer :: candidate_program

        is_visible = .false.
        if (.not. arena%has_node_at(candidate_index)) return

        if (scope_index < 0) then
            is_visible = .true.
            return
        end if

        candidate_scope = find_nearest_scope_owner(arena, candidate_index)
        if (scope_index > 0) then
            if (candidate_scope == scope_index) is_visible = .true.
            return
        end if

        candidate_program = find_program_owner(arena, candidate_index)
        if (candidate_scope == 0) then
            if (program_index < 0) then
                is_visible = .true.
            else if (candidate_program == program_index) then
                is_visible = .true.
            end if
        end if
    end function identifier_visible_in_scope

end module semantic_identifier_context
