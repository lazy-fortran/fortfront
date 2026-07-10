module ast_arena_clone
    ! Deep clone support for ast_arena_t covering all node types.
    ! Retires the "AST nodes MUST NOT be copied" policy (issue #2842).
    !
    ! clone_arena()  - full arena deep copy via existing assignment operator
    ! clone_subtree() - deep copy of a subtree rooted at a given index,
    !                   with index remapping so internal references stay valid

    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use ast_base, only: ast_node
    implicit none
    private

    public :: clone_arena, clone_subtree, clone_result_t

    ! Result type for subtree clone
    type :: clone_result_t
        type(ast_arena_t) :: cloned_arena
        integer :: root_index = 0 ! New root index in cloned arena
        integer, allocatable :: index_map(:) ! old_index -> new_index mapping
    end type clone_result_t

contains

    ! ---------------------------------------------------------------------------
    ! Full arena clone: delegates to the arena's deep-copy assignment operator.
    ! The assignment operator (ast_arena_compat_assign + ast_arena_modern_assign)
    ! already performs a recursive deep copy of every entry's node, node_type,
    ! child_indices, and the core arena arrays.
    ! ---------------------------------------------------------------------------
    function clone_arena(source) result(cloned)
        type(ast_arena_t), intent(in) :: source
        type(ast_arena_t) :: cloned

        cloned = source
    end function clone_arena

    ! ---------------------------------------------------------------------------
    ! Subtree clone: collect all descendant indices, then copy into a fresh
    ! arena with a contiguous index mapping so that internal index references
    ! (child_indices, body_indices, etc.) remain valid.
    ! ---------------------------------------------------------------------------
    function clone_subtree(source, root_index) result(res)
        type(ast_arena_t), intent(in) :: source
        integer, intent(in) :: root_index
        type(clone_result_t) :: res

        integer, allocatable :: subtree_indices(:)
        integer :: n, i, new_idx
        integer, allocatable :: map(:)

        ! Validate root
        if (.not. source%has_node_at(root_index)) then
            res%cloned_arena = create_ast_arena()
            return
        end if

        ! Collect all indices reachable from root (BFS via child_indices)
        subtree_indices = collect_subtree_indices(source, root_index)
        n = size(subtree_indices)

        ! Build old->new mapping: original index -> position in subtree list
        allocate (map(source%compat_size))
        map = 0
        do i = 1, n
            map(subtree_indices(i)) = i
        end do

        ! Create new arena and copy nodes in order
        res%cloned_arena = create_ast_arena(n)
        do i = 1, n
            new_idx = copy_one_entry(source, res%cloned_arena, subtree_indices(i))
            if (i == 1) res%root_index = new_idx
        end do

        ! Remap internal index references in the cloned nodes
        call remap_indices_in_cloned(res%cloned_arena, map, subtree_indices)

        res%index_map = map
    end function clone_subtree

    ! ---------------------------------------------------------------------------
    ! BFS collection of all descendant indices from root_index.
    ! ---------------------------------------------------------------------------
    function collect_subtree_indices(arena, root_index) result(indices)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: root_index
        integer, allocatable :: indices(:)
        integer, allocatable :: queue(:), visited(:)
        integer :: head, tail, cap, curr, n_children, j

        cap = max(arena%compat_size, 64)
        allocate (queue(cap), visited(cap))
        visited = 0
        head = 1; tail = 1
        queue(1) = root_index
        visited(root_index) = 1

        do while (head <= tail)
            curr = queue(head); head = head + 1
            if (curr <= 0 .or. curr > arena%compat_size) cycle
            if (.not. allocated(arena%entries(curr)%child_indices)) cycle
            n_children = arena%entries(curr)%child_count
            if (n_children <= 0) cycle
            do j = 1, n_children
                if (j > size(arena%entries(curr)%child_indices)) exit
                if (queue(head - 1) == arena%entries(curr)%child_indices(j)) cycle
                if (visited(arena%entries(curr)%child_indices(j)) == 0) then
                    visited(arena%entries(curr)%child_indices(j)) = 1
                    if (tail < cap) then
                        tail = tail + 1
                        queue(tail) = arena%entries(curr)%child_indices(j)
                    end if
                end if
            end do
        end do

        allocate (indices(tail))
        indices = queue(1:tail)
    end function collect_subtree_indices

    ! ---------------------------------------------------------------------------
    ! Copy a single arena entry into the target arena. Returns new index.
    ! Uses allocate(source=) which invokes the type's assignment operator for
    ! deep copy of allocatable components.
    ! ---------------------------------------------------------------------------
    function copy_one_entry(source, target, idx) result(new_idx)
        type(ast_arena_t), intent(in) :: source
        type(ast_arena_t), intent(inout) :: target
        integer, intent(in) :: idx
        integer :: new_idx
        class(ast_node), allocatable :: cloned_node

        if (.not. allocated(source%entries(idx)%node)) then
            new_idx = 0
            return
        end if

        allocate (cloned_node, source=source%entries(idx)%node)
        call target%push(cloned_node, source%entries(idx)%node_type, 0)
        new_idx = target%size
    end function copy_one_entry

    ! ---------------------------------------------------------------------------
    ! Remap integer index references inside cloned nodes to point to the new
    ! contiguous indices in the cloned arena.
    !
    ! This is the critical step: nodes store child/body/param references as
    ! integer indices into the original arena. After cloning into a new arena
    ! with renumbered indices, those references must be updated.
    !
    ! We use the visitor pattern to traverse every node and remap its index
    ! fields. The remapping is done via select-type on every known node kind.
    ! ---------------------------------------------------------------------------
    subroutine remap_indices_in_cloned(arena, index_map, subtree_indices)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: index_map(:)
        integer, intent(in) :: subtree_indices(:)

        integer :: i, old_idx, new_idx

        do i = 1, size(subtree_indices)
            old_idx = subtree_indices(i)
            new_idx = index_map(old_idx)
            if (new_idx <= 0 .or. new_idx > arena%size) cycle
            if (.not. arena%has_node_at(new_idx)) cycle
            call remap_node_indices(arena, new_idx, index_map)
            call remap_public_query_indices(arena, new_idx, index_map)
        end do
    end subroutine remap_indices_in_cloned

    subroutine remap_node_indices(arena, idx, index_map)
        use ast_nodes_core, only: program_node, assignment_node, &
            pointer_assignment_node, binary_op_node, &
            call_or_subscript_node, array_literal_node, &
            component_access_node, range_subscript_node
        use ast_nodes_procedure, only: function_def_node, subroutine_def_node, &
            subroutine_call_node
        use ast_nodes_data, only: declaration_node, parameter_declaration_node, &
            module_node, submodule_node, block_data_node, &
            derived_type_node, type_binding_node, &
            mixed_construct_container_node, multi_unit_container_node
        use ast_nodes_conditional, only: if_node, select_case_node, &
            case_block_node, case_default_node, select_type_node, &
            type_guard_block_node, select_rank_node, rank_block_node
        use ast_nodes_loops, only: do_loop_node, do_while_node, forall_node
        use ast_nodes_array, only: where_node, where_stmt_node
        use ast_nodes_associate, only: associate_node, block_construct_node
        use ast_nodes_io, only: print_statement_node, io_implied_do_node, &
            write_statement_node, read_statement_node, open_statement_node, &
            close_statement_node, inquire_statement_node, &
            backspace_statement_node, rewind_statement_node, &
            endfile_statement_node
        use ast_nodes_transfer, only: entry_node, nullify_node
        use ast_nodes_bounds, only: array_slice_node, range_expression_node, &
            array_operation_node
        use ast_nodes_legacy, only: common_block_node, enum_node
        use ast_nodes_misc, only: allocate_statement_node, &
            deallocate_statement_node, data_statement_node, &
            import_statement_node, statement_function_node, &
            interface_block_node, module_procedure_node, &
            implicit_statement_node
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: idx
        integer, intent(in) :: index_map(:)
        integer :: bi, cl_idx, as_idx, block_idx

        if (.not. allocated(arena%entries(idx)%node)) return

        select type (n => arena%entries(idx)%node)
            ! -- Core nodes --
            type is (program_node)
            if (allocated(n%body_indices)) &
                n%body_indices = remap_array(index_map, n%body_indices)

            type is (assignment_node)
            n%target_index = remap_one(index_map, n%target_index)
            n%value_index = remap_one(index_map, n%value_index)

            type is (pointer_assignment_node)
            n%pointer_index = remap_one(index_map, n%pointer_index)
            n%target_index = remap_one(index_map, n%target_index)

            type is (binary_op_node)
            n%left_index = remap_one(index_map, n%left_index)
            n%right_index = remap_one(index_map, n%right_index)

            type is (call_or_subscript_node)
            n%base_expr_index = remap_one(index_map, n%base_expr_index)
            if (allocated(n%arg_indices)) &
                n%arg_indices = remap_array(index_map, n%arg_indices)

            type is (array_literal_node)
            if (allocated(n%element_indices)) &
                n%element_indices = remap_array(index_map, n%element_indices)

            type is (component_access_node)
            n%base_expr_index = remap_one(index_map, n%base_expr_index)

            type is (range_subscript_node)
            n%base_expr_index = remap_one(index_map, n%base_expr_index)
            n%start_index = remap_one(index_map, n%start_index)
            n%end_index = remap_one(index_map, n%end_index)

            ! -- Procedure nodes --
            type is (function_def_node)
            if (allocated(n%param_indices)) &
                n%param_indices = remap_array(index_map, n%param_indices)
            if (allocated(n%body_indices)) &
                n%body_indices = remap_array(index_map, n%body_indices)

            type is (subroutine_def_node)
            if (allocated(n%param_indices)) &
                n%param_indices = remap_array(index_map, n%param_indices)
            if (allocated(n%body_indices)) &
                n%body_indices = remap_array(index_map, n%body_indices)

            type is (subroutine_call_node)
            if (allocated(n%arg_indices)) &
                n%arg_indices = remap_array(index_map, n%arg_indices)

            ! -- Data nodes --
            type is (declaration_node)
            n%initializer_index = remap_one(index_map, n%initializer_index)
            if (allocated(n%dimension_indices)) &
                n%dimension_indices = remap_array(index_map, n%dimension_indices)

            type is (parameter_declaration_node)
            if (allocated(n%dimension_indices)) &
                n%dimension_indices = remap_array(index_map, n%dimension_indices)

            type is (module_node)
            if (allocated(n%declaration_indices)) &
                n%declaration_indices = remap_array(index_map, n%declaration_indices)
            if (allocated(n%procedure_indices)) &
                n%procedure_indices = remap_array(index_map, n%procedure_indices)

            type is (submodule_node)
            if (allocated(n%declaration_indices)) &
                n%declaration_indices = remap_array(index_map, n%declaration_indices)
            if (allocated(n%procedure_indices)) &
                n%procedure_indices = remap_array(index_map, n%procedure_indices)

            type is (block_data_node)
            if (allocated(n%statement_indices)) &
                n%statement_indices = remap_array(index_map, n%statement_indices)

            type is (derived_type_node)
            if (allocated(n%component_indices)) &
                n%component_indices = remap_array(index_map, n%component_indices)
            if (allocated(n%param_indices)) &
                n%param_indices = remap_array(index_map, n%param_indices)
            if (allocated(n%binding_indices)) &
                n%binding_indices = remap_array(index_map, n%binding_indices)

            type is (type_binding_node)
            ! No index fields to remap

            type is (mixed_construct_container_node)
            if (allocated(n%implicit_declaration_indices)) &
                n%implicit_declaration_indices = &
                remap_array(index_map, n%implicit_declaration_indices)
            if (allocated(n%explicit_program_indices)) &
                n%explicit_program_indices = &
                remap_array(index_map, n%explicit_program_indices)

            type is (multi_unit_container_node)
            if (allocated(n%body_indices)) &
                n%body_indices = remap_array(index_map, n%body_indices)

            ! -- Conditional nodes --
            type is (if_node)
            n%condition_index = remap_one(index_map, n%condition_index)
            if (allocated(n%then_body_indices)) &
                n%then_body_indices = remap_array(index_map, n%then_body_indices)
            if (allocated(n%else_body_indices)) &
                n%else_body_indices = remap_array(index_map, n%else_body_indices)
            ! elseif_blocks: each has body_indices(array) and condition_index
            if (allocated(n%elseif_blocks)) then
                do block_idx = 1, size(n%elseif_blocks)
                    n%elseif_blocks(block_idx)%condition_index = &
                        remap_one(index_map, &
                        n%elseif_blocks(block_idx)%condition_index)
                    if (allocated(n%elseif_blocks(block_idx)%body_indices)) &
                        n%elseif_blocks(block_idx)%body_indices = &
                        remap_array(index_map, &
                        n%elseif_blocks(block_idx)%body_indices)
                end do
            end if

            type is (select_case_node)
            n%selector_index = remap_one(index_map, n%selector_index)
            if (allocated(n%case_indices)) &
                n%case_indices = remap_array(index_map, n%case_indices)
            n%default_index = remap_one(index_map, n%default_index)

            type is (case_block_node)
            if (allocated(n%value_indices)) &
                n%value_indices = remap_array(index_map, n%value_indices)
            if (allocated(n%body_indices)) &
                n%body_indices = remap_array(index_map, n%body_indices)

            type is (case_default_node)
            if (allocated(n%body_indices)) &
                n%body_indices = remap_array(index_map, n%body_indices)

            type is (select_type_node)
            n%selector_index = remap_one(index_map, n%selector_index)
            if (allocated(n%guard_indices)) &
                n%guard_indices = remap_array(index_map, n%guard_indices)
            n%default_index = remap_one(index_map, n%default_index)

            type is (type_guard_block_node)
            n%type_name_index = remap_one(index_map, n%type_name_index)
            if (allocated(n%body_indices)) &
                n%body_indices = remap_array(index_map, n%body_indices)

            type is (select_rank_node)
            n%selector_index = remap_one(index_map, n%selector_index)
            if (allocated(n%rank_indices)) &
                n%rank_indices = remap_array(index_map, n%rank_indices)
            n%default_index = remap_one(index_map, n%default_index)

            type is (rank_block_node)
            if (allocated(n%body_indices)) &
                n%body_indices = remap_array(index_map, n%body_indices)

            ! -- Loop nodes --
            type is (do_loop_node)
            n%start_expr_index = remap_one(index_map, n%start_expr_index)
            n%end_expr_index = remap_one(index_map, n%end_expr_index)
            n%step_expr_index = remap_one(index_map, n%step_expr_index)
            if (allocated(n%body_indices)) &
                n%body_indices = remap_array(index_map, n%body_indices)

            type is (do_while_node)
            n%condition_index = remap_one(index_map, n%condition_index)
            if (allocated(n%body_indices)) &
                n%body_indices = remap_array(index_map, n%body_indices)

            type is (forall_node)
            if (allocated(n%lower_bound_indices)) &
                n%lower_bound_indices = remap_array(index_map, n%lower_bound_indices)
            if (allocated(n%upper_bound_indices)) &
                n%upper_bound_indices = remap_array(index_map, n%upper_bound_indices)
            if (allocated(n%stride_indices)) &
                n%stride_indices = remap_array(index_map, n%stride_indices)
            n%mask_expr_index = remap_one(index_map, n%mask_expr_index)
            if (allocated(n%body_indices)) &
                n%body_indices = remap_array(index_map, n%body_indices)

            ! -- Array nodes --
            type is (where_node)
            n%mask_expr_index = remap_one(index_map, n%mask_expr_index)
            if (allocated(n%where_body_indices)) &
                n%where_body_indices = remap_array(index_map, n%where_body_indices)
            ! elsewhere_clauses: each has body_indices(array)
            if (allocated(n%elsewhere_clauses)) then
                do cl_idx = 1, size(n%elsewhere_clauses)
                    if (allocated(n%elsewhere_clauses(cl_idx)%body_indices)) &
                        n%elsewhere_clauses(cl_idx)%body_indices = &
                        remap_array(index_map, &
                        n%elsewhere_clauses(cl_idx)%body_indices)
                end do
            end if

            type is (where_stmt_node)
            n%mask_expr_index = remap_one(index_map, n%mask_expr_index)
            n%assignment_index = remap_one(index_map, n%assignment_index)

            ! -- Associate nodes --
            type is (associate_node)
            ! associations: each has expr_index
            if (allocated(n%associations)) then
                do as_idx = 1, size(n%associations)
                    n%associations(as_idx)%expr_index = &
                        remap_one(index_map, n%associations(as_idx)%expr_index)
                end do
            end if
            if (allocated(n%body_indices)) &
                n%body_indices = remap_array(index_map, n%body_indices)

            type is (block_construct_node)
            if (allocated(n%body_indices)) &
                n%body_indices = remap_array(index_map, n%body_indices)

            ! -- IO nodes --
            type is (print_statement_node)
            if (allocated(n%expression_indices)) &
                n%expression_indices = remap_array(index_map, n%expression_indices)

            type is (io_implied_do_node)
            n%expr_index = remap_one(index_map, n%expr_index)
            if (allocated(n%object_indices)) &
                n%object_indices = remap_array(index_map, n%object_indices)
            n%start_expr_index = remap_one(index_map, n%start_expr_index)
            n%end_expr_index = remap_one(index_map, n%end_expr_index)
            n%step_expr_index = remap_one(index_map, n%step_expr_index)

            type is (write_statement_node)
            if (allocated(n%arg_indices)) &
                n%arg_indices = remap_array(index_map, n%arg_indices)
            n%iostat_var_index = remap_one(index_map, n%iostat_var_index)
            n%err_label_index = remap_one(index_map, n%err_label_index)
            n%end_label_index = remap_one(index_map, n%end_label_index)
            n%format_expr_index = remap_one(index_map, n%format_expr_index)

            type is (read_statement_node)
            if (allocated(n%var_indices)) &
                n%var_indices = remap_array(index_map, n%var_indices)
            n%iostat_var_index = remap_one(index_map, n%iostat_var_index)
            n%err_label_index = remap_one(index_map, n%err_label_index)
            n%end_label_index = remap_one(index_map, n%end_label_index)
            n%format_expr_index = remap_one(index_map, n%format_expr_index)

            type is (open_statement_node)
            n%iostat_var_index = remap_one(index_map, n%iostat_var_index)
            n%err_label_index = remap_one(index_map, n%err_label_index)

            type is (close_statement_node)
            n%iostat_var_index = remap_one(index_map, n%iostat_var_index)
            n%err_label_index = remap_one(index_map, n%err_label_index)

            type is (inquire_statement_node)
            n%iostat_var_index = remap_one(index_map, n%iostat_var_index)
            n%err_label_index = remap_one(index_map, n%err_label_index)

            type is (backspace_statement_node)
            n%iostat_var_index = remap_one(index_map, n%iostat_var_index)
            n%err_label_index = remap_one(index_map, n%err_label_index)

            type is (rewind_statement_node)
            n%iostat_var_index = remap_one(index_map, n%iostat_var_index)
            n%err_label_index = remap_one(index_map, n%err_label_index)

            type is (endfile_statement_node)
            n%iostat_var_index = remap_one(index_map, n%iostat_var_index)
            n%err_label_index = remap_one(index_map, n%err_label_index)

            ! -- Transfer nodes --
            type is (entry_node)
            if (allocated(n%param_indices)) &
                n%param_indices = remap_array(index_map, n%param_indices)

            type is (nullify_node)
            if (allocated(n%pointer_indices)) &
                n%pointer_indices = remap_array(index_map, n%pointer_indices)

            ! -- Bounds nodes --
            type is (array_slice_node)
            n%array_index = remap_one(index_map, n%array_index)
            ! bounds_indices is fixed-size(10) - remap each element
            do bi = 1, n%num_dimensions
                if (bi <= 10) &
                    n%bounds_indices(bi) = remap_one(index_map, n%bounds_indices(bi))
            end do

            type is (range_expression_node)
            n%start_index = remap_one(index_map, n%start_index)
            n%end_index = remap_one(index_map, n%end_index)
            n%stride_index = remap_one(index_map, n%stride_index)

            type is (array_operation_node)
            n%left_operand_index = remap_one(index_map, n%left_operand_index)
            n%right_operand_index = remap_one(index_map, n%right_operand_index)

            ! -- Legacy nodes --
            type is (common_block_node)
            ! No index fields (uses string_t arrays)

            type is (enum_node)
            ! No index fields

            ! -- Misc nodes --
            type is (allocate_statement_node)
            if (allocated(n%var_indices)) &
                n%var_indices = remap_array(index_map, n%var_indices)
            if (allocated(n%shape_indices)) &
                n%shape_indices = remap_array(index_map, n%shape_indices)
            n%stat_var_index = remap_one(index_map, n%stat_var_index)
            n%errmsg_var_index = remap_one(index_map, n%errmsg_var_index)
            n%source_expr_index = remap_one(index_map, n%source_expr_index)
            n%mold_expr_index = remap_one(index_map, n%mold_expr_index)

            type is (deallocate_statement_node)
            if (allocated(n%var_indices)) &
                n%var_indices = remap_array(index_map, n%var_indices)
            n%stat_var_index = remap_one(index_map, n%stat_var_index)
            n%errmsg_var_index = remap_one(index_map, n%errmsg_var_index)

            type is (data_statement_node)
            if (allocated(n%object_indices)) &
                n%object_indices = remap_array(index_map, n%object_indices)
            if (allocated(n%value_indices)) &
                n%value_indices = remap_array(index_map, n%value_indices)

            type is (statement_function_node)
            n%body_expr_index = remap_one(index_map, n%body_expr_index)

            type is (interface_block_node)
            if (allocated(n%procedure_indices)) &
                n%procedure_indices = remap_array(index_map, n%procedure_indices)

            type is (implicit_statement_node)
            ! No index fields (uses letter_specs and type_spec value types)

            ! -- Nodes with no index fields (identifier, literal, etc.) --
            ! These are handled by the class default below - no remapping needed
        class default
            ! Node has no integer index fields to remap
        end select
    end subroutine remap_node_indices

    subroutine remap_public_query_indices(arena, idx, index_map)
        use ast_nodes_array, only: where_node
        use ast_nodes_io, only: write_statement_node, read_statement_node, &
            open_statement_node, close_statement_node, inquire_statement_node, &
            backspace_statement_node, rewind_statement_node, &
            endfile_statement_node
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: idx, index_map(:)
        integer :: i

        select type (node => arena%entries(idx)%node)
            type is (where_node)
            if (allocated(node%elsewhere_clauses)) then
                do i = 1, size(node%elsewhere_clauses)
                    node%elsewhere_clauses(i)%mask_index = &
                        remap_one(index_map, &
                        node%elsewhere_clauses(i)%mask_index)
                end do
            end if
            type is (write_statement_node)
            call remap_io_specifiers(index_map, node%specifiers)
            type is (read_statement_node)
            call remap_io_specifiers(index_map, node%specifiers)
            type is (open_statement_node)
            call remap_io_specifiers(index_map, node%specifiers)
            type is (close_statement_node)
            call remap_io_specifiers(index_map, node%specifiers)
            type is (inquire_statement_node)
            call remap_io_specifiers(index_map, node%specifiers)
            type is (backspace_statement_node)
            call remap_io_specifiers(index_map, node%specifiers)
            type is (rewind_statement_node)
            call remap_io_specifiers(index_map, node%specifiers)
            type is (endfile_statement_node)
            call remap_io_specifiers(index_map, node%specifiers)
        end select
    end subroutine remap_public_query_indices

    subroutine remap_io_specifiers(index_map, specifiers)
        use ast_nodes_io, only: io_specifier_t
        integer, intent(in) :: index_map(:)
        type(io_specifier_t), allocatable, intent(inout) :: specifiers(:)
        integer :: i

        if (.not. allocated(specifiers)) return
        do i = 1, size(specifiers)
            specifiers(i)%value_node_index = &
                remap_one(index_map, specifiers(i)%value_node_index)
        end do
    end subroutine remap_io_specifiers

    ! Remap a single index: if old_idx is in the map, return new idx; else unchanged
    pure integer function remap_one(index_map, old_idx) result(new_idx)
        integer, intent(in) :: index_map(:), old_idx
        if (old_idx > 0 .and. old_idx <= size(index_map) .and. index_map(old_idx) > 0) then
            new_idx = index_map(old_idx)
        else
            new_idx = old_idx
        end if
    end function remap_one

    ! Remap an array of indices
    pure function remap_array(index_map, indices) result(mapped)
        integer, intent(in) :: index_map(:), indices(:)
        integer :: mapped(size(indices)), i
        do i = 1, size(indices)
            mapped(i) = remap_one(index_map, indices(i))
        end do
    end function remap_array

end module ast_arena_clone
