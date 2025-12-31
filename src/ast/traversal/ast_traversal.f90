module ast_traversal
    use ast_arena_modern, only: ast_arena_t
    use ast_base, only: ast_node
    use ast_nodes_core
    use ast_nodes_procedure
    use ast_nodes_control
    use ast_nodes_loops
    use ast_nodes_io
    use ast_nodes_data
    use ast_nodes_misc
    use ast_visitor
    implicit none
    private

    ! Public traversal procedures
    public :: traverse_ast, traverse_preorder, traverse_postorder
    public :: visit_node  ! Added for ast_introspection

    ! Public node type checking functions
    public :: is_program_node, is_assignment_node, is_binary_op_node
    public :: is_function_def_node, is_subroutine_def_node
    public :: is_identifier_node, is_literal_node, is_declaration_node
    public :: is_if_node, is_do_loop_node, is_do_while_node
    public :: is_call_or_subscript_node, is_subroutine_call_node
    public :: is_print_statement_node, is_use_statement_node
    public :: is_select_case_node, is_derived_type_node
    public :: is_module_node, is_interface_block_node

contains

    subroutine traverse_procedure_def_indices(arena, param_indices, body_indices, &
                                              visitor, is_preorder)
        type(ast_arena_t), intent(in) :: arena
        integer, allocatable, intent(in) :: param_indices(:), body_indices(:)
        class(ast_visitor_t), intent(inout) :: visitor
        logical, intent(in) :: is_preorder
        integer :: i

        if (allocated(param_indices)) then
            do i = 1, size(param_indices)
                call traverse_node(arena, param_indices(i), visitor, is_preorder)
            end do
        end if
        if (allocated(body_indices)) then
            do i = 1, size(body_indices)
                call traverse_node(arena, body_indices(i), visitor, is_preorder)
            end do
        end if
    end subroutine traverse_procedure_def_indices

    ! Main traversal entry point
    subroutine traverse_ast(arena, root_index, visitor)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: root_index
        class(ast_visitor_t), intent(inout) :: visitor

        ! Default to pre-order traversal
        call traverse_preorder(arena, root_index, visitor)
    end subroutine traverse_ast

    ! Pre-order traversal (visit node before children)
    subroutine traverse_preorder(arena, node_index, visitor)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        class(ast_visitor_t), intent(inout) :: visitor

        integer, allocatable :: stack(:)
        integer :: top, capacity
        integer :: current_index
        integer, allocatable :: children(:)
        integer :: i

        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return

        capacity = 128
        allocate (stack(capacity))
        top = 0
        call push(node_index)

        do while (top > 0)
            current_index = stack(top)
            top = top - 1

            if (current_index <= 0 .or. current_index > arena%size) cycle
            if (.not. allocated(arena%entries(current_index)%node)) cycle

            select type (node => arena%entries(current_index)%node)
            class is (ast_node)
                call visit_node(node, visitor)
            end select

            call gather_child_indices(arena, current_index, children)
            if (size(children) > 0) then
                do i = size(children), 1, -1
                    call push(children(i))
                end do
            end if
            if (allocated(children)) deallocate (children)
        end do

    contains
        subroutine push(idx)
            integer, intent(in) :: idx
            integer, allocatable :: tmp(:)

            if (idx <= 0) return
            if (top >= capacity) then
                capacity = capacity * 2
                allocate (tmp(capacity))
                if (top > 0) tmp(1:top) = stack(1:top)
                call move_alloc(tmp, stack)
            end if
            top = top + 1
            stack(top) = idx
        end subroutine push
    end subroutine traverse_preorder

    ! Post-order traversal (visit children before node)
    subroutine traverse_postorder(arena, node_index, visitor)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        class(ast_visitor_t), intent(inout) :: visitor

        integer, allocatable :: stack(:)
        integer, allocatable :: output(:)
        integer :: top_stack, top_output, capacity_stack, capacity_output
        integer :: current_index
        integer, allocatable :: children(:)
        integer :: i

        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return

        capacity_stack = 128
        capacity_output = 128
        allocate (stack(capacity_stack))
        allocate (output(capacity_output))
        top_stack = 0
        top_output = 0
        call push_stack(node_index)

        do while (top_stack > 0)
            current_index = stack(top_stack)
            top_stack = top_stack - 1

            if (current_index <= 0 .or. current_index > arena%size) cycle
            if (.not. allocated(arena%entries(current_index)%node)) cycle

            call push_output(current_index)

            call gather_child_indices(arena, current_index, children)
            if (size(children) > 0) then
                do i = 1, size(children)
                    call push_stack(children(i))
                end do
            end if
            if (allocated(children)) deallocate (children)
        end do

        do while (top_output > 0)
            current_index = output(top_output)
            top_output = top_output - 1

            select type (node => arena%entries(current_index)%node)
            class is (ast_node)
                call visit_node(node, visitor)
            end select
        end do

    contains
        subroutine push_stack(idx)
            integer, intent(in) :: idx
            integer, allocatable :: tmp(:)

            if (idx <= 0) return
            if (top_stack >= capacity_stack) then
                capacity_stack = capacity_stack * 2
                allocate (tmp(capacity_stack))
                if (top_stack > 0) tmp(1:top_stack) = stack(1:top_stack)
                call move_alloc(tmp, stack)
            end if
            top_stack = top_stack + 1
            stack(top_stack) = idx
        end subroutine push_stack

        subroutine push_output(idx)
            integer, intent(in) :: idx
            integer, allocatable :: tmp(:)

            if (idx <= 0) return
            if (top_output >= capacity_output) then
                capacity_output = capacity_output * 2
                allocate (tmp(capacity_output))
                if (top_output > 0) tmp(1:top_output) = output(1:top_output)
                call move_alloc(tmp, output)
            end if
            top_output = top_output + 1
            output(top_output) = idx
        end subroutine push_output
    end subroutine traverse_postorder

    subroutine gather_child_indices(arena, node_index, children)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer, allocatable, intent(out) :: children(:)

        integer, allocatable :: buffer(:)
        integer :: count

        count = 0
        allocate (buffer(0))

        if (node_index <= 0 .or. node_index > arena%size) then
            allocate (children(0))
            return
        end if
        if (.not. allocated(arena%entries(node_index)%node)) then
            allocate (children(0))
            return
        end if

        select type (node => arena%entries(node_index)%node)
        type is (program_node)
            if (allocated(node%body_indices)) call append_array(node%body_indices)

        type is (assignment_node)
            call append_index(node%target_index)
            call append_index(node%value_index)

        type is (binary_op_node)
            call append_index(node%left_index)
            call append_index(node%right_index)

        type is (function_def_node)
            if (allocated(node%param_indices)) call append_array(node%param_indices)
            if (allocated(node%body_indices)) call append_array(node%body_indices)

        type is (subroutine_def_node)
            if (allocated(node%param_indices)) call append_array(node%param_indices)
            if (allocated(node%body_indices)) call append_array(node%body_indices)

        type is (call_or_subscript_node)
            if (allocated(node%arg_indices)) call append_array(node%arg_indices)

        type is (subroutine_call_node)
            if (allocated(node%arg_indices)) call append_array(node%arg_indices)

        type is (if_node)
            call append_index(node%condition_index)
            if (allocated(node%then_body_indices)) call &
                append_array(node%then_body_indices)
            if (allocated(node%elseif_blocks)) then
                block
                    integer :: i, j
                    do i = 1, size(node%elseif_blocks)
                        call append_index(node%elseif_blocks(i)%condition_index)
                        if (allocated(node%elseif_blocks(i)%body_indices)) then
                            do j = 1, size(node%elseif_blocks(i)%body_indices)
                                call append_index(node%elseif_blocks(i)%body_indices(j))
                            end do
                        end if
                    end do
                end block
            end if
            if (allocated(node%else_body_indices)) call &
                append_array(node%else_body_indices)

        type is (do_loop_node)
            call append_index(node%start_expr_index)
            call append_index(node%end_expr_index)
            call append_index(node%step_expr_index)
            if (allocated(node%body_indices)) call append_array(node%body_indices)

        type is (do_while_node)
            call append_index(node%condition_index)
            if (allocated(node%body_indices)) call append_array(node%body_indices)

        type is (select_case_node)
            call append_index(node%selector_index)
            if (allocated(node%case_indices)) call append_array(node%case_indices)
            call append_index(node%default_index)

        type is (module_node)
            if (allocated(node%declaration_indices)) call &
                append_array(node%declaration_indices)
            if (allocated(node%procedure_indices)) call &
                append_array(node%procedure_indices)

        type is (derived_type_node)
            if (allocated(node%component_indices)) call &
                append_array(node%component_indices)

        type is (interface_block_node)
            if (allocated(node%procedure_indices)) call &
                append_array(node%procedure_indices)

        type is (print_statement_node)
            if (allocated(node%expression_indices)) call &
                append_array(node%expression_indices)

        class default
            ! Other node types intentionally yield no children here
        end select

        if (count == 0) then
            if (allocated(buffer)) deallocate (buffer)
            allocate (children(0))
        else
            if (allocated(children)) deallocate (children)
            allocate (children(count))
            children = buffer(1:count)
            if (allocated(buffer)) deallocate (buffer)
        end if

    contains
        subroutine ensure_capacity(required)
            integer, intent(in) :: required
            integer, allocatable :: tmp(:)
            integer :: current_size

            if (.not. allocated(buffer)) then
                allocate (buffer(max(32, required)))
                return
            end if

            current_size = size(buffer)
            if (required <= current_size) return

            allocate (tmp(max(current_size * 2, required)))
            if (count > 0) tmp(1:count) = buffer(1:count)
            call move_alloc(tmp, buffer)
        end subroutine ensure_capacity

        subroutine append_index(idx)
            integer, intent(in) :: idx
            if (idx <= 0) return
            call ensure_capacity(count + 1)
            count = count + 1
            buffer(count) = idx
        end subroutine append_index

        subroutine append_array(values)
            integer, intent(in) :: values(:)
            integer :: k
            if (size(values) <= 0) return
            call ensure_capacity(count + size(values))
            do k = 1, size(values)
                if (values(k) > 0) then
                    count = count + 1
                    buffer(count) = values(k)
                end if
            end do
        end subroutine append_array
    end subroutine gather_child_indices

    ! Helper to visit a node using the visitor pattern
    subroutine visit_node(node, visitor)
        class(ast_node), intent(in) :: node
        class(ast_visitor_t), intent(inout) :: visitor

        select type (n => node)
        type is (program_node)
            call visitor%visit_program(n)
        type is (assignment_node)
            call visitor%visit_assignment(n)
        type is (binary_op_node)
            call visitor%visit_binary_op(n)
        type is (function_def_node)
            call visitor%visit_function_def(n)
        type is (subroutine_def_node)
            call visitor%visit_subroutine_def(n)
        type is (call_or_subscript_node)
            call visitor%visit_call_or_subscript(n)
        type is (subroutine_call_node)
            call visitor%visit_subroutine_call(n)
        type is (identifier_node)
            call visitor%visit_identifier(n)
        type is (literal_node)
            call visitor%visit_literal(n)
        type is (declaration_node)
            call visitor%visit_declaration(n)
        type is (print_statement_node)
            call visitor%visit_print_statement(n)
        type is (if_node)
            call visitor%visit_if(n)
        type is (do_loop_node)
            call visitor%visit_do_loop(n)
        type is (do_while_node)
            call visitor%visit_do_while(n)
        type is (select_case_node)
            call visitor%visit_select_case(n)
        type is (derived_type_node)
            call visitor%visit_derived_type(n)
        type is (interface_block_node)
            call visitor%visit_interface_block(n)
        type is (module_node)
            call visitor%visit_module(n)
        type is (use_statement_node)
            call visitor%visit_use_statement(n)
        type is (include_statement_node)
            call visitor%visit_include_statement(n)
        end select
    end subroutine visit_node

    ! Generic traversal of children to avoid code duplication
    subroutine traverse_children(arena, node, visitor, is_preorder)
        type(ast_arena_t), intent(in) :: arena
        class(ast_node), intent(in) :: node
        class(ast_visitor_t), intent(inout) :: visitor
        logical, intent(in) :: is_preorder

        select type (n => node)
        type is (program_node)
            call traverse_program_children(arena, n, visitor, is_preorder)

        type is (assignment_node)
            call traverse_assignment_children(arena, n, visitor, is_preorder)

        type is (binary_op_node)
            call traverse_binary_op_children(arena, n, visitor, is_preorder)

        type is (function_def_node)
            call traverse_function_def_children(arena, n, visitor, is_preorder)

        type is (subroutine_def_node)
            call traverse_subroutine_def_children(arena, n, visitor, is_preorder)

        type is (call_or_subscript_node)
            call traverse_call_or_subscript_children(arena, n, visitor, is_preorder)

        type is (subroutine_call_node)
            call traverse_subroutine_call_children(arena, n, visitor, is_preorder)

        type is (if_node)
            call traverse_if_children(arena, n, visitor, is_preorder)

        type is (do_loop_node)
            call traverse_do_loop_children(arena, n, visitor, is_preorder)

        type is (do_while_node)
            call traverse_do_while_children(arena, n, visitor, is_preorder)

        type is (select_case_node)
            call traverse_select_case_children(arena, n, visitor, is_preorder)

        type is (module_node)
            call traverse_module_children(arena, n, visitor, is_preorder)

        type is (derived_type_node)
            call traverse_derived_type_children(arena, n, visitor, is_preorder)

        type is (interface_block_node)
            call traverse_interface_block_children(arena, n, visitor, is_preorder)

        type is (print_statement_node)
            call traverse_print_statement_children(arena, n, visitor, is_preorder)
        end select
    end subroutine traverse_children

    ! Per-node child traversal helpers (kept small and focused)
    subroutine traverse_program_children(arena, n, visitor, is_preorder)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: n
        class(ast_visitor_t), intent(inout) :: visitor
        logical, intent(in) :: is_preorder
        integer :: i
        if (allocated(n%body_indices)) then
            do i = 1, size(n%body_indices)
                call traverse_node(arena, n%body_indices(i), visitor, is_preorder)
            end do
        end if
    end subroutine traverse_program_children

    subroutine traverse_assignment_children(arena, n, visitor, is_preorder)
        type(ast_arena_t), intent(in) :: arena
        type(assignment_node), intent(in) :: n
        class(ast_visitor_t), intent(inout) :: visitor
        logical, intent(in) :: is_preorder
        call traverse_node(arena, n%target_index, visitor, is_preorder)
        call traverse_node(arena, n%value_index, visitor, is_preorder)
    end subroutine traverse_assignment_children

    subroutine traverse_binary_op_children(arena, n, visitor, is_preorder)
        type(ast_arena_t), intent(in) :: arena
        type(binary_op_node), intent(in) :: n
        class(ast_visitor_t), intent(inout) :: visitor
        logical, intent(in) :: is_preorder
        call traverse_node(arena, n%left_index, visitor, is_preorder)
        call traverse_node(arena, n%right_index, visitor, is_preorder)
    end subroutine traverse_binary_op_children

    subroutine traverse_function_def_children(arena, n, visitor, is_preorder)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: n
        class(ast_visitor_t), intent(inout) :: visitor
        logical, intent(in) :: is_preorder

        call traverse_procedure_def_indices(arena, n%param_indices, n%body_indices, &
                                            visitor, is_preorder)
    end subroutine traverse_function_def_children

    subroutine traverse_subroutine_def_children(arena, n, visitor, is_preorder)
        type(ast_arena_t), intent(in) :: arena
        type(subroutine_def_node), intent(in) :: n
        class(ast_visitor_t), intent(inout) :: visitor
        logical, intent(in) :: is_preorder

        call traverse_procedure_def_indices(arena, n%param_indices, n%body_indices, &
                                            visitor, is_preorder)
    end subroutine traverse_subroutine_def_children

    subroutine traverse_call_or_subscript_children(arena, n, visitor, is_preorder)
        type(ast_arena_t), intent(in) :: arena
        type(call_or_subscript_node), intent(in) :: n
        class(ast_visitor_t), intent(inout) :: visitor
        logical, intent(in) :: is_preorder
        integer :: i
        if (allocated(n%arg_indices)) then
            do i = 1, size(n%arg_indices)
                call traverse_node(arena, n%arg_indices(i), visitor, is_preorder)
            end do
        end if
    end subroutine traverse_call_or_subscript_children

    subroutine traverse_subroutine_call_children(arena, n, visitor, is_preorder)
        type(ast_arena_t), intent(in) :: arena
        type(subroutine_call_node), intent(in) :: n
        class(ast_visitor_t), intent(inout) :: visitor
        logical, intent(in) :: is_preorder
        integer :: i
        if (allocated(n%arg_indices)) then
            do i = 1, size(n%arg_indices)
                call traverse_node(arena, n%arg_indices(i), visitor, is_preorder)
            end do
        end if
    end subroutine traverse_subroutine_call_children

    subroutine traverse_if_children(arena, n, visitor, is_preorder)
        type(ast_arena_t), intent(in) :: arena
        type(if_node), intent(in) :: n
        class(ast_visitor_t), intent(inout) :: visitor
        logical, intent(in) :: is_preorder
        integer :: i
        call traverse_node(arena, n%condition_index, visitor, is_preorder)
        if (allocated(n%then_body_indices)) then
            do i = 1, size(n%then_body_indices)
                call traverse_node(arena, n%then_body_indices(i), visitor, is_preorder)
            end do
        end if
        if (allocated(n%else_body_indices)) then
            do i = 1, size(n%else_body_indices)
                call traverse_node(arena, n%else_body_indices(i), visitor, is_preorder)
            end do
        end if
    end subroutine traverse_if_children

    subroutine traverse_do_loop_children(arena, n, visitor, is_preorder)
        type(ast_arena_t), intent(in) :: arena
        type(do_loop_node), intent(in) :: n
        class(ast_visitor_t), intent(inout) :: visitor
        logical, intent(in) :: is_preorder
        integer :: i
        call traverse_node(arena, n%start_expr_index, visitor, is_preorder)
        call traverse_node(arena, n%end_expr_index, visitor, is_preorder)
        if (n%step_expr_index > 0) then
            call traverse_node(arena, n%step_expr_index, visitor, is_preorder)
        end if
        if (allocated(n%body_indices)) then
            do i = 1, size(n%body_indices)
                call traverse_node(arena, n%body_indices(i), visitor, is_preorder)
            end do
        end if
    end subroutine traverse_do_loop_children

    subroutine traverse_do_while_children(arena, n, visitor, is_preorder)
        type(ast_arena_t), intent(in) :: arena
        type(do_while_node), intent(in) :: n
        class(ast_visitor_t), intent(inout) :: visitor
        logical, intent(in) :: is_preorder
        integer :: i
        call traverse_node(arena, n%condition_index, visitor, is_preorder)
        if (allocated(n%body_indices)) then
            do i = 1, size(n%body_indices)
                call traverse_node(arena, n%body_indices(i), visitor, is_preorder)
            end do
        end if
    end subroutine traverse_do_while_children

    subroutine traverse_select_case_children(arena, n, visitor, is_preorder)
        type(ast_arena_t), intent(in) :: arena
        type(select_case_node), intent(in) :: n
        class(ast_visitor_t), intent(inout) :: visitor
        logical, intent(in) :: is_preorder
        integer :: i
        call traverse_node(arena, n%selector_index, visitor, is_preorder)
        if (allocated(n%case_indices)) then
            do i = 1, size(n%case_indices)
                call traverse_node(arena, n%case_indices(i), visitor, is_preorder)
            end do
        end if
        if (n%default_index > 0) then
            call traverse_node(arena, n%default_index, visitor, is_preorder)
        end if
    end subroutine traverse_select_case_children

    subroutine traverse_module_children(arena, n, visitor, is_preorder)
        type(ast_arena_t), intent(in) :: arena
        type(module_node), intent(in) :: n
        class(ast_visitor_t), intent(inout) :: visitor
        logical, intent(in) :: is_preorder
        integer :: i
        if (allocated(n%declaration_indices)) then
            do i = 1, size(n%declaration_indices)
                call traverse_node(arena, n%declaration_indices(i), visitor, &
                                   is_preorder)
            end do
        end if
        if (allocated(n%procedure_indices)) then
            do i = 1, size(n%procedure_indices)
                call traverse_node(arena, n%procedure_indices(i), visitor, is_preorder)
            end do
        end if
    end subroutine traverse_module_children

    subroutine traverse_derived_type_children(arena, n, visitor, is_preorder)
        type(ast_arena_t), intent(in) :: arena
        type(derived_type_node), intent(in) :: n
        class(ast_visitor_t), intent(inout) :: visitor
        logical, intent(in) :: is_preorder
        integer :: i
        if (allocated(n%component_indices)) then
            do i = 1, size(n%component_indices)
                call traverse_node(arena, n%component_indices(i), visitor, is_preorder)
            end do
        end if
    end subroutine traverse_derived_type_children

    subroutine traverse_interface_block_children(arena, n, visitor, is_preorder)
        type(ast_arena_t), intent(in) :: arena
        type(interface_block_node), intent(in) :: n
        class(ast_visitor_t), intent(inout) :: visitor
        logical, intent(in) :: is_preorder
        integer :: i
        if (allocated(n%procedure_indices)) then
            do i = 1, size(n%procedure_indices)
                call traverse_node(arena, n%procedure_indices(i), visitor, is_preorder)
            end do
        end if
    end subroutine traverse_interface_block_children

    subroutine traverse_print_statement_children(arena, n, visitor, is_preorder)
        type(ast_arena_t), intent(in) :: arena
        type(print_statement_node), intent(in) :: n
        class(ast_visitor_t), intent(inout) :: visitor
        logical, intent(in) :: is_preorder
        integer :: i
        if (allocated(n%expression_indices)) then
            do i = 1, size(n%expression_indices)
                call traverse_node(arena, n%expression_indices(i), visitor, is_preorder)
            end do
        end if
    end subroutine traverse_print_statement_children

    ! Helper to traverse a single node
    recursive subroutine traverse_node(arena, node_index, visitor, is_preorder)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        class(ast_visitor_t), intent(inout) :: visitor
        logical, intent(in) :: is_preorder

        if (is_preorder) then
            call traverse_preorder(arena, node_index, visitor)
        else
            call traverse_postorder(arena, node_index, visitor)
        end if
    end subroutine traverse_node

    ! Node type checking functions with consistent formatting
    function is_program_node(arena, index) result(is_program)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        logical :: is_program

        is_program = .false.
        if (index <= 0 .or. index > arena%size) return
        if (.not. allocated(arena%entries(index)%node)) return

        select type (n => arena%entries(index)%node)
        type is (program_node)
            is_program = .true.
        end select
    end function is_program_node

    function is_assignment_node(arena, index) result(is_assignment)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        logical :: is_assignment

        is_assignment = .false.
        if (index <= 0 .or. index > arena%size) return
        if (.not. allocated(arena%entries(index)%node)) return

        select type (n => arena%entries(index)%node)
        type is (assignment_node)
            is_assignment = .true.
        end select
    end function is_assignment_node

    function is_binary_op_node(arena, index) result(is_binary_op)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        logical :: is_binary_op

        is_binary_op = .false.
        if (index <= 0 .or. index > arena%size) return
        if (.not. allocated(arena%entries(index)%node)) return

        select type (n => arena%entries(index)%node)
        type is (binary_op_node)
            is_binary_op = .true.
        end select
    end function is_binary_op_node

    function is_function_def_node(arena, index) result(is_function_def)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        logical :: is_function_def

        is_function_def = .false.
        if (index <= 0 .or. index > arena%size) return
        if (.not. allocated(arena%entries(index)%node)) return

        select type (n => arena%entries(index)%node)
        type is (function_def_node)
            is_function_def = .true.
        end select
    end function is_function_def_node

    function is_subroutine_def_node(arena, index) result(is_subroutine_def)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        logical :: is_subroutine_def

        is_subroutine_def = .false.
        if (index <= 0 .or. index > arena%size) return
        if (.not. allocated(arena%entries(index)%node)) return

        select type (n => arena%entries(index)%node)
        type is (subroutine_def_node)
            is_subroutine_def = .true.
        end select
    end function is_subroutine_def_node

    function is_identifier_node(arena, index) result(is_identifier)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        logical :: is_identifier

        is_identifier = .false.
        if (index <= 0 .or. index > arena%size) return
        if (.not. allocated(arena%entries(index)%node)) return

        select type (n => arena%entries(index)%node)
        type is (identifier_node)
            is_identifier = .true.
        end select
    end function is_identifier_node

    function is_literal_node(arena, index) result(is_literal)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        logical :: is_literal

        is_literal = .false.
        if (index <= 0 .or. index > arena%size) return
        if (.not. allocated(arena%entries(index)%node)) return

        select type (n => arena%entries(index)%node)
        type is (literal_node)
            is_literal = .true.
        end select
    end function is_literal_node

    function is_declaration_node(arena, index) result(is_declaration)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        logical :: is_declaration

        is_declaration = .false.
        if (index <= 0 .or. index > arena%size) return
        if (.not. allocated(arena%entries(index)%node)) return

        select type (n => arena%entries(index)%node)
        type is (declaration_node)
            is_declaration = .true.
        end select
    end function is_declaration_node

    function is_if_node(arena, index) result(is_if)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        logical :: is_if

        is_if = .false.
        if (index <= 0 .or. index > arena%size) return
        if (.not. allocated(arena%entries(index)%node)) return

        select type (n => arena%entries(index)%node)
        type is (if_node)
            is_if = .true.
        end select
    end function is_if_node

    function is_do_loop_node(arena, index) result(is_do_loop)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        logical :: is_do_loop

        is_do_loop = .false.
        if (index <= 0 .or. index > arena%size) return
        if (.not. allocated(arena%entries(index)%node)) return

        select type (n => arena%entries(index)%node)
        type is (do_loop_node)
            is_do_loop = .true.
        end select
    end function is_do_loop_node

    function is_do_while_node(arena, index) result(is_do_while)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        logical :: is_do_while

        is_do_while = .false.
        if (index <= 0 .or. index > arena%size) return
        if (.not. allocated(arena%entries(index)%node)) return

        select type (n => arena%entries(index)%node)
        type is (do_while_node)
            is_do_while = .true.
        end select
    end function is_do_while_node

    function is_call_or_subscript_node(arena, index) result(is_call_or_subscript)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        logical :: is_call_or_subscript

        is_call_or_subscript = .false.
        if (index <= 0 .or. index > arena%size) return
        if (.not. allocated(arena%entries(index)%node)) return

        select type (n => arena%entries(index)%node)
        type is (call_or_subscript_node)
            is_call_or_subscript = .true.
        end select
    end function is_call_or_subscript_node

    function is_subroutine_call_node(arena, index) result(is_subroutine_call)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        logical :: is_subroutine_call

        is_subroutine_call = .false.
        if (index <= 0 .or. index > arena%size) return
        if (.not. allocated(arena%entries(index)%node)) return

        select type (n => arena%entries(index)%node)
        type is (subroutine_call_node)
            is_subroutine_call = .true.
        end select
    end function is_subroutine_call_node

    function is_print_statement_node(arena, index) result(is_print_statement)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        logical :: is_print_statement

        is_print_statement = .false.
        if (index <= 0 .or. index > arena%size) return
        if (.not. allocated(arena%entries(index)%node)) return

        select type (n => arena%entries(index)%node)
        type is (print_statement_node)
            is_print_statement = .true.
        end select
    end function is_print_statement_node

    function is_use_statement_node(arena, index) result(is_use_statement)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        logical :: is_use_statement

        is_use_statement = .false.
        if (index <= 0 .or. index > arena%size) return
        if (.not. allocated(arena%entries(index)%node)) return

        select type (n => arena%entries(index)%node)
        type is (use_statement_node)
            is_use_statement = .true.
        end select
    end function is_use_statement_node

    function is_select_case_node(arena, index) result(is_select_case)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        logical :: is_select_case

        is_select_case = .false.
        if (index <= 0 .or. index > arena%size) return
        if (.not. allocated(arena%entries(index)%node)) return

        select type (n => arena%entries(index)%node)
        type is (select_case_node)
            is_select_case = .true.
        end select
    end function is_select_case_node

    function is_derived_type_node(arena, index) result(is_derived_type)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        logical :: is_derived_type

        is_derived_type = .false.
        if (index <= 0 .or. index > arena%size) return
        if (.not. allocated(arena%entries(index)%node)) return

        select type (n => arena%entries(index)%node)
        type is (derived_type_node)
            is_derived_type = .true.
        end select
    end function is_derived_type_node

    function is_module_node(arena, index) result(is_module)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        logical :: is_module

        is_module = .false.
        if (index <= 0 .or. index > arena%size) return
        if (.not. allocated(arena%entries(index)%node)) return

        select type (n => arena%entries(index)%node)
        type is (module_node)
            is_module = .true.
        end select
    end function is_module_node

    function is_interface_block_node(arena, index) result(is_interface_block)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        logical :: is_interface_block

        is_interface_block = .false.
        if (index <= 0 .or. index > arena%size) return
        if (.not. allocated(arena%entries(index)%node)) return

        select type (n => arena%entries(index)%node)
        type is (interface_block_node)
            is_interface_block = .true.
        end select
    end function is_interface_block_node

end module ast_traversal
