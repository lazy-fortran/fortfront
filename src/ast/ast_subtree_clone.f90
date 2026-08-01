module ast_subtree_clone
    ! Deep-copies an AST subtree into fresh arena nodes so a transformation
    ! can give a generated procedure its own body instead of sharing the
    ! original procedure's statements (issue #2958). Each clone is linked to
    ! its cloned parent, so a name reference inside the clone resolves inside
    ! the cloning procedure rather than in the original one.
    use ast_arena_modern, only: ast_arena_t, link_children_to_parent
    use ast_base, only: ast_node
    use ast_nodes_array, only: where_node, where_stmt_node
    use ast_nodes_associate, only: associate_node, block_construct_node
    use ast_nodes_bounds, only: array_bounds_node, array_operation_node, &
        array_slice_node, range_expression_node
    use ast_nodes_conditional, only: case_block_node, case_default_node, &
        if_node, rank_block_node, &
        select_case_node, select_rank_node, &
        select_type_node, type_guard_block_node
    use ast_nodes_core, only: array_literal_node, assignment_node, &
        binary_op_node, call_or_subscript_node, &
        component_access_node, pointer_assignment_node, &
        program_node, range_subscript_node
    use ast_nodes_data, only: block_data_node, declaration_node, &
        derived_type_node, mixed_construct_container_node, &
        module_node, multi_unit_container_node, &
        parameter_declaration_node, submodule_node
    use ast_nodes_generics, only: implements_block_node, requirement_block_node, &
        template_block_node, trait_block_node
    use ast_nodes_io, only: backspace_statement_node, close_statement_node, &
        endfile_statement_node, inquire_statement_node, &
        io_implied_do_node, open_statement_node, &
        print_statement_node, read_statement_node, &
        rewind_statement_node, write_statement_node
    use ast_nodes_loops, only: do_loop_node, do_while_node, &
        forall_node
    use ast_nodes_misc, only: allocate_statement_node, complex_literal_node, &
        data_statement_node, deallocate_statement_node, &
        interface_block_node, statement_function_node
    use ast_nodes_procedure, only: function_def_node, subroutine_call_node, &
        subroutine_def_node
    use ast_nodes_transfer, only: entry_node, error_stop_node, &
        goto_node, nullify_node, &
        pause_node, return_node, &
        stop_node
    use uid_generator, only: generate_uid
    implicit none
    private

    public :: clone_ast_subtree

contains

    ! Clone the node at node_index and, recursively, everything it owns.
    ! Returns the index of the cloned root; an invalid index is returned as-is.
    recursive function clone_ast_subtree(arena, node_index) result(new_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: node_index
        integer :: new_index
        class(ast_node), allocatable :: work
        character(len=:), allocatable :: node_type

        new_index = node_index
        if (node_index < 1) return
        if (node_index > arena%size) return
        if (.not. arena%has_node_at(node_index)) return

        allocate (work, source=arena%entries(node_index)%node)
        node_type = trim(arena%entries(node_index)%node_type)
        new_index = clone_work_node(arena, work, node_type)
        if (new_index <= 0) new_index = node_index
    end function clone_ast_subtree

    recursive function clone_child_list(arena, indices) result(new_indices)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: indices(:)
        integer :: new_indices(size(indices))
        integer :: i

        do i = 1, size(indices)
            new_indices(i) = clone_ast_subtree(arena, indices(i))
        end do
    end function clone_child_list

    subroutine link_child(arena, parent_index, child_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: parent_index
        integer, intent(in) :: child_index
        integer :: one_child(1)

        if (child_index < 1) return
        one_child(1) = child_index
        call link_children_to_parent(arena, parent_index, one_child)
    end subroutine link_child

    recursive function clone_work_node(arena, work, node_type) result(new_index)
        type(ast_arena_t), intent(inout) :: arena
        class(ast_node), intent(in) :: work
        character(len=*), intent(in) :: node_type
        integer :: new_index

        new_index = 0
        select type (src => work)
        type is (allocate_statement_node)
            block
                type(allocate_statement_node) :: copy

                copy = src
                copy%uid = generate_uid()
                if (allocated(copy%var_indices)) then
                    copy%var_indices = clone_child_list(arena, copy%var_indices)
                end if
                if (allocated(copy%shape_indices)) then
                    copy%shape_indices = clone_child_list(arena, copy%shape_indices)
                end if
                copy%stat_var_index = clone_ast_subtree(arena, copy%stat_var_index)
                copy%errmsg_var_index = clone_ast_subtree(arena, copy%errmsg_var_index)
                copy%source_expr_index = clone_ast_subtree( &
                    arena, copy%source_expr_index)
                copy%mold_expr_index = clone_ast_subtree(arena, copy%mold_expr_index)
                call arena%push(copy, node_type)
                new_index = arena%size
                if (allocated(copy%var_indices)) then
                    call link_children_to_parent(arena, new_index, &
                                                 copy%var_indices)
                end if
                if (allocated(copy%shape_indices)) then
                    call link_children_to_parent(arena, new_index, &
                                                 copy%shape_indices)
                end if
                call link_child(arena, new_index, copy%stat_var_index)
                call link_child(arena, new_index, copy%errmsg_var_index)
                call link_child(arena, new_index, copy%source_expr_index)
                call link_child(arena, new_index, copy%mold_expr_index)
            end block
        type is (array_bounds_node)
            block
                type(array_bounds_node) :: copy

                copy = src
                copy%uid = generate_uid()
                copy%lower_bound_index = clone_ast_subtree( &
                    arena, copy%lower_bound_index)
                copy%upper_bound_index = clone_ast_subtree( &
                    arena, copy%upper_bound_index)
                copy%stride_index = clone_ast_subtree(arena, copy%stride_index)
                call arena%push(copy, node_type)
                new_index = arena%size
                call link_child(arena, new_index, copy%lower_bound_index)
                call link_child(arena, new_index, copy%upper_bound_index)
                call link_child(arena, new_index, copy%stride_index)
            end block
        type is (array_literal_node)
            block
                type(array_literal_node) :: copy

                copy = src
                copy%uid = generate_uid()
                if (allocated(copy%element_indices)) then
                    copy%element_indices = clone_child_list(arena, copy%element_indices)
                end if
                call arena%push(copy, node_type)
                new_index = arena%size
                if (allocated(copy%element_indices)) then
                    call link_children_to_parent(arena, new_index, &
                                                 copy%element_indices)
                end if
            end block
        type is (array_operation_node)
            block
                type(array_operation_node) :: copy

                copy = src
                copy%uid = generate_uid()
                copy%left_operand_index = clone_ast_subtree( &
                    arena, copy%left_operand_index)
                copy%right_operand_index = clone_ast_subtree( &
                    arena, copy%right_operand_index)
                call arena%push(copy, node_type)
                new_index = arena%size
                call link_child(arena, new_index, copy%left_operand_index)
                call link_child(arena, new_index, copy%right_operand_index)
            end block
        type is (array_slice_node)
            block
                type(array_slice_node) :: copy

                copy = src
                copy%uid = generate_uid()
                copy%array_index = clone_ast_subtree(arena, copy%array_index)
                copy%bounds_indices = clone_child_list(arena, copy%bounds_indices)
                call arena%push(copy, node_type)
                new_index = arena%size
                call link_child(arena, new_index, copy%array_index)
                call link_children_to_parent(arena, new_index, copy%bounds_indices)
            end block
        type is (assignment_node)
            block
                type(assignment_node) :: copy

                copy = src
                copy%uid = generate_uid()
                copy%target_index = clone_ast_subtree(arena, copy%target_index)
                copy%value_index = clone_ast_subtree(arena, copy%value_index)
                call arena%push(copy, node_type)
                new_index = arena%size
                call link_child(arena, new_index, copy%target_index)
                call link_child(arena, new_index, copy%value_index)
            end block
        type is (associate_node)
            block
                type(associate_node) :: copy

                copy = src
                copy%uid = generate_uid()
                if (allocated(copy%body_indices)) then
                    copy%body_indices = clone_child_list(arena, copy%body_indices)
                end if
                call arena%push(copy, node_type)
                new_index = arena%size
                if (allocated(copy%body_indices)) then
                    call link_children_to_parent(arena, new_index, &
                                                 copy%body_indices)
                end if
            end block
        type is (backspace_statement_node)
            block
                type(backspace_statement_node) :: copy

                copy = src
                copy%uid = generate_uid()
                copy%iostat_var_index = clone_ast_subtree(arena, copy%iostat_var_index)
                call arena%push(copy, node_type)
                new_index = arena%size
                call link_child(arena, new_index, copy%iostat_var_index)
            end block
        type is (binary_op_node)
            block
                type(binary_op_node) :: copy

                copy = src
                copy%uid = generate_uid()
                copy%left_index = clone_ast_subtree(arena, copy%left_index)
                copy%right_index = clone_ast_subtree(arena, copy%right_index)
                call arena%push(copy, node_type)
                new_index = arena%size
                call link_child(arena, new_index, copy%left_index)
                call link_child(arena, new_index, copy%right_index)
            end block
        type is (block_construct_node)
            block
                type(block_construct_node) :: copy

                copy = src
                copy%uid = generate_uid()
                if (allocated(copy%body_indices)) then
                    copy%body_indices = clone_child_list(arena, copy%body_indices)
                end if
                call arena%push(copy, node_type)
                new_index = arena%size
                if (allocated(copy%body_indices)) then
                    call link_children_to_parent(arena, new_index, &
                                                 copy%body_indices)
                end if
            end block
        type is (block_data_node)
            block
                type(block_data_node) :: copy

                copy = src
                copy%uid = generate_uid()
                if (allocated(copy%statement_indices)) then
                    copy%statement_indices = clone_child_list( &
                        arena, copy%statement_indices)
                end if
                call arena%push(copy, node_type)
                new_index = arena%size
                if (allocated(copy%statement_indices)) then
                    call link_children_to_parent(arena, new_index, &
                                                 copy%statement_indices)
                end if
            end block
        type is (call_or_subscript_node)
            block
                type(call_or_subscript_node) :: copy

                copy = src
                copy%uid = generate_uid()
                copy%base_expr_index = clone_ast_subtree(arena, copy%base_expr_index)
                if (allocated(copy%arg_indices)) then
                    copy%arg_indices = clone_child_list(arena, copy%arg_indices)
                end if
                call arena%push(copy, node_type)
                new_index = arena%size
                call link_child(arena, new_index, copy%base_expr_index)
                if (allocated(copy%arg_indices)) then
                    call link_children_to_parent(arena, new_index, &
                                                 copy%arg_indices)
                end if
            end block
        type is (case_block_node)
            block
                type(case_block_node) :: copy

                copy = src
                copy%uid = generate_uid()
                if (allocated(copy%value_indices)) then
                    copy%value_indices = clone_child_list(arena, copy%value_indices)
                end if
                if (allocated(copy%body_indices)) then
                    copy%body_indices = clone_child_list(arena, copy%body_indices)
                end if
                call arena%push(copy, node_type)
                new_index = arena%size
                if (allocated(copy%value_indices)) then
                    call link_children_to_parent(arena, new_index, &
                                                 copy%value_indices)
                end if
                if (allocated(copy%body_indices)) then
                    call link_children_to_parent(arena, new_index, &
                                                 copy%body_indices)
                end if
            end block
        type is (case_default_node)
            block
                type(case_default_node) :: copy

                copy = src
                copy%uid = generate_uid()
                if (allocated(copy%body_indices)) then
                    copy%body_indices = clone_child_list(arena, copy%body_indices)
                end if
                call arena%push(copy, node_type)
                new_index = arena%size
                if (allocated(copy%body_indices)) then
                    call link_children_to_parent(arena, new_index, &
                                                 copy%body_indices)
                end if
            end block
        type is (close_statement_node)
            block
                type(close_statement_node) :: copy

                copy = src
                copy%uid = generate_uid()
                copy%iostat_var_index = clone_ast_subtree(arena, copy%iostat_var_index)
                call arena%push(copy, node_type)
                new_index = arena%size
                call link_child(arena, new_index, copy%iostat_var_index)
            end block
        type is (complex_literal_node)
            block
                type(complex_literal_node) :: copy

                copy = src
                copy%uid = generate_uid()
                copy%real_index = clone_ast_subtree(arena, copy%real_index)
                copy%imag_index = clone_ast_subtree(arena, copy%imag_index)
                call arena%push(copy, node_type)
                new_index = arena%size
                call link_child(arena, new_index, copy%real_index)
                call link_child(arena, new_index, copy%imag_index)
            end block
        type is (component_access_node)
            block
                type(component_access_node) :: copy

                copy = src
                copy%uid = generate_uid()
                copy%base_expr_index = clone_ast_subtree(arena, copy%base_expr_index)
                call arena%push(copy, node_type)
                new_index = arena%size
                call link_child(arena, new_index, copy%base_expr_index)
            end block
        type is (data_statement_node)
            block
                type(data_statement_node) :: copy

                copy = src
                copy%uid = generate_uid()
                if (allocated(copy%object_indices)) then
                    copy%object_indices = clone_child_list(arena, copy%object_indices)
                end if
                if (allocated(copy%value_indices)) then
                    copy%value_indices = clone_child_list(arena, copy%value_indices)
                end if
                call arena%push(copy, node_type)
                new_index = arena%size
                if (allocated(copy%object_indices)) then
                    call link_children_to_parent(arena, new_index, &
                                                 copy%object_indices)
                end if
                if (allocated(copy%value_indices)) then
                    call link_children_to_parent(arena, new_index, &
                                                 copy%value_indices)
                end if
            end block
        type is (deallocate_statement_node)
            block
                type(deallocate_statement_node) :: copy

                copy = src
                copy%uid = generate_uid()
                if (allocated(copy%var_indices)) then
                    copy%var_indices = clone_child_list(arena, copy%var_indices)
                end if
                copy%stat_var_index = clone_ast_subtree(arena, copy%stat_var_index)
                copy%errmsg_var_index = clone_ast_subtree(arena, copy%errmsg_var_index)
                call arena%push(copy, node_type)
                new_index = arena%size
                if (allocated(copy%var_indices)) then
                    call link_children_to_parent(arena, new_index, &
                                                 copy%var_indices)
                end if
                call link_child(arena, new_index, copy%stat_var_index)
                call link_child(arena, new_index, copy%errmsg_var_index)
            end block
        type is (declaration_node)
            block
                type(declaration_node) :: copy

                copy = src
                copy%uid = generate_uid()
                copy%initializer_index = clone_ast_subtree( &
                    arena, copy%initializer_index)
                if (allocated(copy%dimension_indices)) then
                    copy%dimension_indices = clone_child_list( &
                        arena, copy%dimension_indices)
                end if
                if (allocated(copy%type_param_indices)) then
                    copy%type_param_indices = clone_child_list( &
                        arena, copy%type_param_indices)
                end if
                call arena%push(copy, node_type)
                new_index = arena%size
                call link_child(arena, new_index, copy%initializer_index)
                if (allocated(copy%dimension_indices)) then
                    call link_children_to_parent(arena, new_index, &
                                                 copy%dimension_indices)
                end if
                if (allocated(copy%type_param_indices)) then
                    call link_children_to_parent(arena, new_index, &
                                                 copy%type_param_indices)
                end if
            end block
        type is (derived_type_node)
            block
                type(derived_type_node) :: copy

                copy = src
                copy%uid = generate_uid()
                if (allocated(copy%component_indices)) then
                    copy%component_indices = clone_child_list( &
                        arena, copy%component_indices)
                end if
                if (allocated(copy%param_indices)) then
                    copy%param_indices = clone_child_list(arena, copy%param_indices)
                end if
                if (allocated(copy%binding_indices)) then
                    copy%binding_indices = clone_child_list(arena, copy%binding_indices)
                end if
                call arena%push(copy, node_type)
                new_index = arena%size
                if (allocated(copy%component_indices)) then
                    call link_children_to_parent(arena, new_index, &
                                                 copy%component_indices)
                end if
                if (allocated(copy%param_indices)) then
                    call link_children_to_parent(arena, new_index, &
                                                 copy%param_indices)
                end if
                if (allocated(copy%binding_indices)) then
                    call link_children_to_parent(arena, new_index, &
                                                 copy%binding_indices)
                end if
            end block
        type is (do_loop_node)
            block
                type(do_loop_node) :: copy

                copy = src
                copy%uid = generate_uid()
                copy%start_expr_index = clone_ast_subtree(arena, copy%start_expr_index)
                copy%end_expr_index = clone_ast_subtree(arena, copy%end_expr_index)
                copy%step_expr_index = clone_ast_subtree(arena, copy%step_expr_index)
                if (allocated(copy%body_indices)) then
                    copy%body_indices = clone_child_list(arena, copy%body_indices)
                end if
                call arena%push(copy, node_type)
                new_index = arena%size
                call link_child(arena, new_index, copy%start_expr_index)
                call link_child(arena, new_index, copy%end_expr_index)
                call link_child(arena, new_index, copy%step_expr_index)
                if (allocated(copy%body_indices)) then
                    call link_children_to_parent(arena, new_index, &
                                                 copy%body_indices)
                end if
            end block
        type is (do_while_node)
            block
                type(do_while_node) :: copy

                copy = src
                copy%uid = generate_uid()
                copy%condition_index = clone_ast_subtree(arena, copy%condition_index)
                if (allocated(copy%body_indices)) then
                    copy%body_indices = clone_child_list(arena, copy%body_indices)
                end if
                call arena%push(copy, node_type)
                new_index = arena%size
                call link_child(arena, new_index, copy%condition_index)
                if (allocated(copy%body_indices)) then
                    call link_children_to_parent(arena, new_index, &
                                                 copy%body_indices)
                end if
            end block
        type is (endfile_statement_node)
            block
                type(endfile_statement_node) :: copy

                copy = src
                copy%uid = generate_uid()
                copy%iostat_var_index = clone_ast_subtree(arena, copy%iostat_var_index)
                call arena%push(copy, node_type)
                new_index = arena%size
                call link_child(arena, new_index, copy%iostat_var_index)
            end block
        type is (entry_node)
            block
                type(entry_node) :: copy

                copy = src
                copy%uid = generate_uid()
                if (allocated(copy%param_indices)) then
                    copy%param_indices = clone_child_list(arena, copy%param_indices)
                end if
                call arena%push(copy, node_type)
                new_index = arena%size
                if (allocated(copy%param_indices)) then
                    call link_children_to_parent(arena, new_index, &
                                                 copy%param_indices)
                end if
            end block
        type is (error_stop_node)
            block
                type(error_stop_node) :: copy

                copy = src
                copy%uid = generate_uid()
                copy%error_code_index = clone_ast_subtree(arena, copy%error_code_index)
                call arena%push(copy, node_type)
                new_index = arena%size
                call link_child(arena, new_index, copy%error_code_index)
            end block
        type is (forall_node)
            block
                type(forall_node) :: copy

                copy = src
                copy%uid = generate_uid()
                if (allocated(copy%lower_bound_indices)) then
                    copy%lower_bound_indices = clone_child_list( &
                        arena, copy%lower_bound_indices)
                end if
                if (allocated(copy%upper_bound_indices)) then
                    copy%upper_bound_indices = clone_child_list( &
                        arena, copy%upper_bound_indices)
                end if
                if (allocated(copy%stride_indices)) then
                    copy%stride_indices = clone_child_list(arena, copy%stride_indices)
                end if
                copy%mask_expr_index = clone_ast_subtree(arena, copy%mask_expr_index)
                if (allocated(copy%body_indices)) then
                    copy%body_indices = clone_child_list(arena, copy%body_indices)
                end if
                call arena%push(copy, node_type)
                new_index = arena%size
                if (allocated(copy%lower_bound_indices)) then
                    call link_children_to_parent(arena, new_index, &
                                                 copy%lower_bound_indices)
                end if
                if (allocated(copy%upper_bound_indices)) then
                    call link_children_to_parent(arena, new_index, &
                                                 copy%upper_bound_indices)
                end if
                if (allocated(copy%stride_indices)) then
                    call link_children_to_parent(arena, new_index, &
                                                 copy%stride_indices)
                end if
                call link_child(arena, new_index, copy%mask_expr_index)
                if (allocated(copy%body_indices)) then
                    call link_children_to_parent(arena, new_index, &
                                                 copy%body_indices)
                end if
            end block
        type is (function_def_node)
            block
                type(function_def_node) :: copy

                copy = src
                copy%uid = generate_uid()
                if (allocated(copy%param_indices)) then
                    copy%param_indices = clone_child_list(arena, copy%param_indices)
                end if
                if (allocated(copy%body_indices)) then
                    copy%body_indices = clone_child_list(arena, copy%body_indices)
                end if
                call arena%push(copy, node_type)
                new_index = arena%size
                if (allocated(copy%param_indices)) then
                    call link_children_to_parent(arena, new_index, &
                                                 copy%param_indices)
                end if
                if (allocated(copy%body_indices)) then
                    call link_children_to_parent(arena, new_index, &
                                                 copy%body_indices)
                end if
            end block
        type is (goto_node)
            block
                type(goto_node) :: copy

                copy = src
                copy%uid = generate_uid()
                copy%selector_index = clone_ast_subtree(arena, copy%selector_index)
                call arena%push(copy, node_type)
                new_index = arena%size
                call link_child(arena, new_index, copy%selector_index)
            end block
        type is (if_node)
            block
                type(if_node) :: copy

                copy = src
                copy%uid = generate_uid()
                copy%condition_index = clone_ast_subtree(arena, copy%condition_index)
                if (allocated(copy%then_body_indices)) then
                    copy%then_body_indices = clone_child_list( &
                        arena, copy%then_body_indices)
                end if
                if (allocated(copy%else_body_indices)) then
                    copy%else_body_indices = clone_child_list( &
                        arena, copy%else_body_indices)
                end if
                call arena%push(copy, node_type)
                new_index = arena%size
                call link_child(arena, new_index, copy%condition_index)
                if (allocated(copy%then_body_indices)) then
                    call link_children_to_parent(arena, new_index, &
                                                 copy%then_body_indices)
                end if
                if (allocated(copy%else_body_indices)) then
                    call link_children_to_parent(arena, new_index, &
                                                 copy%else_body_indices)
                end if
            end block
        type is (implements_block_node)
            block
                type(implements_block_node) :: copy

                copy = src
                copy%uid = generate_uid()
                if (allocated(copy%declaration_indices)) then
                    copy%declaration_indices = clone_child_list( &
                        arena, copy%declaration_indices)
                end if
                if (allocated(copy%procedure_indices)) then
                    copy%procedure_indices = clone_child_list( &
                        arena, copy%procedure_indices)
                end if
                call arena%push(copy, node_type)
                new_index = arena%size
                if (allocated(copy%declaration_indices)) then
                    call link_children_to_parent(arena, new_index, &
                                                 copy%declaration_indices)
                end if
                if (allocated(copy%procedure_indices)) then
                    call link_children_to_parent(arena, new_index, &
                                                 copy%procedure_indices)
                end if
            end block
        type is (inquire_statement_node)
            block
                type(inquire_statement_node) :: copy

                copy = src
                copy%uid = generate_uid()
                copy%iostat_var_index = clone_ast_subtree(arena, copy%iostat_var_index)
                call arena%push(copy, node_type)
                new_index = arena%size
                call link_child(arena, new_index, copy%iostat_var_index)
            end block
        type is (interface_block_node)
            block
                type(interface_block_node) :: copy

                copy = src
                copy%uid = generate_uid()
                if (allocated(copy%procedure_indices)) then
                    copy%procedure_indices = clone_child_list( &
                        arena, copy%procedure_indices)
                end if
                call arena%push(copy, node_type)
                new_index = arena%size
                if (allocated(copy%procedure_indices)) then
                    call link_children_to_parent(arena, new_index, &
                                                 copy%procedure_indices)
                end if
            end block
        type is (io_implied_do_node)
            block
                type(io_implied_do_node) :: copy

                copy = src
                copy%uid = generate_uid()
                copy%expr_index = clone_ast_subtree(arena, copy%expr_index)
                if (allocated(copy%object_indices)) then
                    copy%object_indices = clone_child_list(arena, copy%object_indices)
                end if
                copy%start_expr_index = clone_ast_subtree(arena, copy%start_expr_index)
                copy%end_expr_index = clone_ast_subtree(arena, copy%end_expr_index)
                copy%step_expr_index = clone_ast_subtree(arena, copy%step_expr_index)
                call arena%push(copy, node_type)
                new_index = arena%size
                call link_child(arena, new_index, copy%expr_index)
                if (allocated(copy%object_indices)) then
                    call link_children_to_parent(arena, new_index, &
                                                 copy%object_indices)
                end if
                call link_child(arena, new_index, copy%start_expr_index)
                call link_child(arena, new_index, copy%end_expr_index)
                call link_child(arena, new_index, copy%step_expr_index)
            end block
        type is (mixed_construct_container_node)
            block
                type(mixed_construct_container_node) :: copy

                copy = src
                copy%uid = generate_uid()
                if (allocated(copy%implicit_declaration_indices)) then
                    copy%implicit_declaration_indices = clone_child_list( &
                        arena, copy%implicit_declaration_indices)
                end if
                if (allocated(copy%explicit_program_indices)) then
                    copy%explicit_program_indices = clone_child_list( &
                        arena, copy%explicit_program_indices)
                end if
                call arena%push(copy, node_type)
                new_index = arena%size
                if (allocated(copy%implicit_declaration_indices)) then
                    call link_children_to_parent(arena, new_index, &
                                                 copy%implicit_declaration_indices)
                end if
                if (allocated(copy%explicit_program_indices)) then
                    call link_children_to_parent(arena, new_index, &
                                                 copy%explicit_program_indices)
                end if
            end block
        type is (module_node)
            block
                type(module_node) :: copy

                copy = src
                copy%uid = generate_uid()
                if (allocated(copy%declaration_indices)) then
                    copy%declaration_indices = clone_child_list( &
                        arena, copy%declaration_indices)
                end if
                if (allocated(copy%procedure_indices)) then
                    copy%procedure_indices = clone_child_list( &
                        arena, copy%procedure_indices)
                end if
                call arena%push(copy, node_type)
                new_index = arena%size
                if (allocated(copy%declaration_indices)) then
                    call link_children_to_parent(arena, new_index, &
                                                 copy%declaration_indices)
                end if
                if (allocated(copy%procedure_indices)) then
                    call link_children_to_parent(arena, new_index, &
                                                 copy%procedure_indices)
                end if
            end block
        type is (multi_unit_container_node)
            block
                type(multi_unit_container_node) :: copy

                copy = src
                copy%uid = generate_uid()
                if (allocated(copy%body_indices)) then
                    copy%body_indices = clone_child_list(arena, copy%body_indices)
                end if
                call arena%push(copy, node_type)
                new_index = arena%size
                if (allocated(copy%body_indices)) then
                    call link_children_to_parent(arena, new_index, &
                                                 copy%body_indices)
                end if
            end block
        type is (nullify_node)
            block
                type(nullify_node) :: copy

                copy = src
                copy%uid = generate_uid()
                if (allocated(copy%pointer_indices)) then
                    copy%pointer_indices = clone_child_list(arena, copy%pointer_indices)
                end if
                call arena%push(copy, node_type)
                new_index = arena%size
                if (allocated(copy%pointer_indices)) then
                    call link_children_to_parent(arena, new_index, &
                                                 copy%pointer_indices)
                end if
            end block
        type is (open_statement_node)
            block
                type(open_statement_node) :: copy

                copy = src
                copy%uid = generate_uid()
                copy%iostat_var_index = clone_ast_subtree(arena, copy%iostat_var_index)
                call arena%push(copy, node_type)
                new_index = arena%size
                call link_child(arena, new_index, copy%iostat_var_index)
            end block
        type is (parameter_declaration_node)
            block
                type(parameter_declaration_node) :: copy

                copy = src
                copy%uid = generate_uid()
                if (allocated(copy%dimension_indices)) then
                    copy%dimension_indices = clone_child_list( &
                        arena, copy%dimension_indices)
                end if
                call arena%push(copy, node_type)
                new_index = arena%size
                if (allocated(copy%dimension_indices)) then
                    call link_children_to_parent(arena, new_index, &
                                                 copy%dimension_indices)
                end if
            end block
        type is (pause_node)
            block
                type(pause_node) :: copy

                copy = src
                copy%uid = generate_uid()
                copy%pause_code_index = clone_ast_subtree(arena, copy%pause_code_index)
                call arena%push(copy, node_type)
                new_index = arena%size
                call link_child(arena, new_index, copy%pause_code_index)
            end block
        type is (pointer_assignment_node)
            block
                type(pointer_assignment_node) :: copy

                copy = src
                copy%uid = generate_uid()
                copy%pointer_index = clone_ast_subtree(arena, copy%pointer_index)
                copy%target_index = clone_ast_subtree(arena, copy%target_index)
                call arena%push(copy, node_type)
                new_index = arena%size
                call link_child(arena, new_index, copy%pointer_index)
                call link_child(arena, new_index, copy%target_index)
            end block
        type is (print_statement_node)
            block
                type(print_statement_node) :: copy

                copy = src
                copy%uid = generate_uid()
                if (allocated(copy%expression_indices)) then
                    copy%expression_indices = clone_child_list( &
                        arena, copy%expression_indices)
                end if
                call arena%push(copy, node_type)
                new_index = arena%size
                if (allocated(copy%expression_indices)) then
                    call link_children_to_parent(arena, new_index, &
                                                 copy%expression_indices)
                end if
            end block
        type is (program_node)
            block
                type(program_node) :: copy

                copy = src
                copy%uid = generate_uid()
                if (allocated(copy%body_indices)) then
                    copy%body_indices = clone_child_list(arena, copy%body_indices)
                end if
                call arena%push(copy, node_type)
                new_index = arena%size
                if (allocated(copy%body_indices)) then
                    call link_children_to_parent(arena, new_index, &
                                                 copy%body_indices)
                end if
            end block
        type is (range_expression_node)
            block
                type(range_expression_node) :: copy

                copy = src
                copy%uid = generate_uid()
                copy%start_index = clone_ast_subtree(arena, copy%start_index)
                copy%end_index = clone_ast_subtree(arena, copy%end_index)
                copy%stride_index = clone_ast_subtree(arena, copy%stride_index)
                call arena%push(copy, node_type)
                new_index = arena%size
                call link_child(arena, new_index, copy%start_index)
                call link_child(arena, new_index, copy%end_index)
                call link_child(arena, new_index, copy%stride_index)
            end block
        type is (range_subscript_node)
            block
                type(range_subscript_node) :: copy

                copy = src
                copy%uid = generate_uid()
                copy%base_expr_index = clone_ast_subtree(arena, copy%base_expr_index)
                copy%start_index = clone_ast_subtree(arena, copy%start_index)
                copy%end_index = clone_ast_subtree(arena, copy%end_index)
                call arena%push(copy, node_type)
                new_index = arena%size
                call link_child(arena, new_index, copy%base_expr_index)
                call link_child(arena, new_index, copy%start_index)
                call link_child(arena, new_index, copy%end_index)
            end block
        type is (rank_block_node)
            block
                type(rank_block_node) :: copy

                copy = src
                copy%uid = generate_uid()
                if (allocated(copy%body_indices)) then
                    copy%body_indices = clone_child_list(arena, copy%body_indices)
                end if
                call arena%push(copy, node_type)
                new_index = arena%size
                if (allocated(copy%body_indices)) then
                    call link_children_to_parent(arena, new_index, &
                                                 copy%body_indices)
                end if
            end block
        type is (read_statement_node)
            block
                type(read_statement_node) :: copy

                copy = src
                copy%uid = generate_uid()
                if (allocated(copy%var_indices)) then
                    copy%var_indices = clone_child_list(arena, copy%var_indices)
                end if
                copy%iostat_var_index = clone_ast_subtree(arena, copy%iostat_var_index)
                copy%format_expr_index = clone_ast_subtree( &
                    arena, copy%format_expr_index)
                call arena%push(copy, node_type)
                new_index = arena%size
                if (allocated(copy%var_indices)) then
                    call link_children_to_parent(arena, new_index, &
                                                 copy%var_indices)
                end if
                call link_child(arena, new_index, copy%iostat_var_index)
                call link_child(arena, new_index, copy%format_expr_index)
            end block
        type is (requirement_block_node)
            block
                type(requirement_block_node) :: copy

                copy = src
                copy%uid = generate_uid()
                if (allocated(copy%declaration_indices)) then
                    copy%declaration_indices = clone_child_list( &
                        arena, copy%declaration_indices)
                end if
                if (allocated(copy%procedure_indices)) then
                    copy%procedure_indices = clone_child_list( &
                        arena, copy%procedure_indices)
                end if
                call arena%push(copy, node_type)
                new_index = arena%size
                if (allocated(copy%declaration_indices)) then
                    call link_children_to_parent(arena, new_index, &
                                                 copy%declaration_indices)
                end if
                if (allocated(copy%procedure_indices)) then
                    call link_children_to_parent(arena, new_index, &
                                                 copy%procedure_indices)
                end if
            end block
        type is (return_node)
            block
                type(return_node) :: copy

                copy = src
                copy%uid = generate_uid()
                copy%selector_index = clone_ast_subtree(arena, copy%selector_index)
                call arena%push(copy, node_type)
                new_index = arena%size
                call link_child(arena, new_index, copy%selector_index)
            end block
        type is (rewind_statement_node)
            block
                type(rewind_statement_node) :: copy

                copy = src
                copy%uid = generate_uid()
                copy%iostat_var_index = clone_ast_subtree(arena, copy%iostat_var_index)
                call arena%push(copy, node_type)
                new_index = arena%size
                call link_child(arena, new_index, copy%iostat_var_index)
            end block
        type is (select_case_node)
            block
                type(select_case_node) :: copy

                copy = src
                copy%uid = generate_uid()
                copy%selector_index = clone_ast_subtree(arena, copy%selector_index)
                if (allocated(copy%case_indices)) then
                    copy%case_indices = clone_child_list(arena, copy%case_indices)
                end if
                copy%default_index = clone_ast_subtree(arena, copy%default_index)
                call arena%push(copy, node_type)
                new_index = arena%size
                call link_child(arena, new_index, copy%selector_index)
                if (allocated(copy%case_indices)) then
                    call link_children_to_parent(arena, new_index, &
                                                 copy%case_indices)
                end if
                call link_child(arena, new_index, copy%default_index)
            end block
        type is (select_rank_node)
            block
                type(select_rank_node) :: copy

                copy = src
                copy%uid = generate_uid()
                copy%selector_index = clone_ast_subtree(arena, copy%selector_index)
                if (allocated(copy%rank_indices)) then
                    copy%rank_indices = clone_child_list(arena, copy%rank_indices)
                end if
                copy%default_index = clone_ast_subtree(arena, copy%default_index)
                call arena%push(copy, node_type)
                new_index = arena%size
                call link_child(arena, new_index, copy%selector_index)
                if (allocated(copy%rank_indices)) then
                    call link_children_to_parent(arena, new_index, &
                                                 copy%rank_indices)
                end if
                call link_child(arena, new_index, copy%default_index)
            end block
        type is (select_type_node)
            block
                type(select_type_node) :: copy

                copy = src
                copy%uid = generate_uid()
                copy%selector_index = clone_ast_subtree(arena, copy%selector_index)
                if (allocated(copy%guard_indices)) then
                    copy%guard_indices = clone_child_list(arena, copy%guard_indices)
                end if
                copy%default_index = clone_ast_subtree(arena, copy%default_index)
                call arena%push(copy, node_type)
                new_index = arena%size
                call link_child(arena, new_index, copy%selector_index)
                if (allocated(copy%guard_indices)) then
                    call link_children_to_parent(arena, new_index, &
                                                 copy%guard_indices)
                end if
                call link_child(arena, new_index, copy%default_index)
            end block
        type is (statement_function_node)
            block
                type(statement_function_node) :: copy

                copy = src
                copy%uid = generate_uid()
                copy%body_expr_index = clone_ast_subtree(arena, copy%body_expr_index)
                call arena%push(copy, node_type)
                new_index = arena%size
                call link_child(arena, new_index, copy%body_expr_index)
            end block
        type is (stop_node)
            block
                type(stop_node) :: copy

                copy = src
                copy%uid = generate_uid()
                copy%stop_code_index = clone_ast_subtree(arena, copy%stop_code_index)
                call arena%push(copy, node_type)
                new_index = arena%size
                call link_child(arena, new_index, copy%stop_code_index)
            end block
        type is (submodule_node)
            block
                type(submodule_node) :: copy

                copy = src
                copy%uid = generate_uid()
                if (allocated(copy%declaration_indices)) then
                    copy%declaration_indices = clone_child_list( &
                        arena, copy%declaration_indices)
                end if
                if (allocated(copy%procedure_indices)) then
                    copy%procedure_indices = clone_child_list( &
                        arena, copy%procedure_indices)
                end if
                call arena%push(copy, node_type)
                new_index = arena%size
                if (allocated(copy%declaration_indices)) then
                    call link_children_to_parent(arena, new_index, &
                                                 copy%declaration_indices)
                end if
                if (allocated(copy%procedure_indices)) then
                    call link_children_to_parent(arena, new_index, &
                                                 copy%procedure_indices)
                end if
            end block
        type is (subroutine_call_node)
            block
                type(subroutine_call_node) :: copy

                copy = src
                copy%uid = generate_uid()
                if (allocated(copy%arg_indices)) then
                    copy%arg_indices = clone_child_list(arena, copy%arg_indices)
                end if
                call arena%push(copy, node_type)
                new_index = arena%size
                if (allocated(copy%arg_indices)) then
                    call link_children_to_parent(arena, new_index, &
                                                 copy%arg_indices)
                end if
            end block
        type is (subroutine_def_node)
            block
                type(subroutine_def_node) :: copy

                copy = src
                copy%uid = generate_uid()
                if (allocated(copy%param_indices)) then
                    copy%param_indices = clone_child_list(arena, copy%param_indices)
                end if
                if (allocated(copy%body_indices)) then
                    copy%body_indices = clone_child_list(arena, copy%body_indices)
                end if
                call arena%push(copy, node_type)
                new_index = arena%size
                if (allocated(copy%param_indices)) then
                    call link_children_to_parent(arena, new_index, &
                                                 copy%param_indices)
                end if
                if (allocated(copy%body_indices)) then
                    call link_children_to_parent(arena, new_index, &
                                                 copy%body_indices)
                end if
            end block
        type is (template_block_node)
            block
                type(template_block_node) :: copy

                copy = src
                copy%uid = generate_uid()
                if (allocated(copy%declaration_indices)) then
                    copy%declaration_indices = clone_child_list( &
                        arena, copy%declaration_indices)
                end if
                if (allocated(copy%procedure_indices)) then
                    copy%procedure_indices = clone_child_list( &
                        arena, copy%procedure_indices)
                end if
                call arena%push(copy, node_type)
                new_index = arena%size
                if (allocated(copy%declaration_indices)) then
                    call link_children_to_parent(arena, new_index, &
                                                 copy%declaration_indices)
                end if
                if (allocated(copy%procedure_indices)) then
                    call link_children_to_parent(arena, new_index, &
                                                 copy%procedure_indices)
                end if
            end block
        type is (trait_block_node)
            block
                type(trait_block_node) :: copy

                copy = src
                copy%uid = generate_uid()
                if (allocated(copy%declaration_indices)) then
                    copy%declaration_indices = clone_child_list( &
                        arena, copy%declaration_indices)
                end if
                if (allocated(copy%procedure_indices)) then
                    copy%procedure_indices = clone_child_list( &
                        arena, copy%procedure_indices)
                end if
                call arena%push(copy, node_type)
                new_index = arena%size
                if (allocated(copy%declaration_indices)) then
                    call link_children_to_parent(arena, new_index, &
                                                 copy%declaration_indices)
                end if
                if (allocated(copy%procedure_indices)) then
                    call link_children_to_parent(arena, new_index, &
                                                 copy%procedure_indices)
                end if
            end block
        type is (type_guard_block_node)
            block
                type(type_guard_block_node) :: copy

                copy = src
                copy%uid = generate_uid()
                if (allocated(copy%body_indices)) then
                    copy%body_indices = clone_child_list(arena, copy%body_indices)
                end if
                call arena%push(copy, node_type)
                new_index = arena%size
                if (allocated(copy%body_indices)) then
                    call link_children_to_parent(arena, new_index, &
                                                 copy%body_indices)
                end if
            end block
        type is (where_node)
            block
                type(where_node) :: copy

                copy = src
                copy%uid = generate_uid()
                copy%mask_expr_index = clone_ast_subtree(arena, copy%mask_expr_index)
                if (allocated(copy%where_body_indices)) then
                    copy%where_body_indices = clone_child_list( &
                        arena, copy%where_body_indices)
                end if
                call arena%push(copy, node_type)
                new_index = arena%size
                call link_child(arena, new_index, copy%mask_expr_index)
                if (allocated(copy%where_body_indices)) then
                    call link_children_to_parent(arena, new_index, &
                                                 copy%where_body_indices)
                end if
            end block
        type is (where_stmt_node)
            block
                type(where_stmt_node) :: copy

                copy = src
                copy%uid = generate_uid()
                copy%mask_expr_index = clone_ast_subtree(arena, copy%mask_expr_index)
                copy%assignment_index = clone_ast_subtree(arena, copy%assignment_index)
                call arena%push(copy, node_type)
                new_index = arena%size
                call link_child(arena, new_index, copy%mask_expr_index)
                call link_child(arena, new_index, copy%assignment_index)
            end block
        type is (write_statement_node)
            block
                type(write_statement_node) :: copy

                copy = src
                copy%uid = generate_uid()
                if (allocated(copy%arg_indices)) then
                    copy%arg_indices = clone_child_list(arena, copy%arg_indices)
                end if
                copy%iostat_var_index = clone_ast_subtree(arena, copy%iostat_var_index)
                copy%format_expr_index = clone_ast_subtree( &
                    arena, copy%format_expr_index)
                call arena%push(copy, node_type)
                new_index = arena%size
                if (allocated(copy%arg_indices)) then
                    call link_children_to_parent(arena, new_index, &
                                                 copy%arg_indices)
                end if
                call link_child(arena, new_index, copy%iostat_var_index)
                call link_child(arena, new_index, copy%format_expr_index)
            end block
        class default
            block
                class(ast_node), allocatable :: copy

                allocate (copy, source=src)
                copy%uid = generate_uid()
                call arena%push(copy, node_type)
                new_index = arena%size
            end block
        end select
    end function clone_work_node

end module ast_subtree_clone
