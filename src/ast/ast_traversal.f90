module ast_traversal
    use ast_core
    use ast_visitor
    implicit none
    private
    
    ! Public traversal procedures
    public :: traverse_ast, traverse_preorder, traverse_postorder
    
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
    
    ! Main traversal entry point
    subroutine traverse_ast(arena, root_index, visitor)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: root_index
        class(ast_visitor_t), intent(inout) :: visitor
        
        ! Default to pre-order traversal
        call traverse_preorder(arena, root_index, visitor)
    end subroutine traverse_ast
    
    ! Pre-order traversal (visit node before children)
    recursive subroutine traverse_preorder(arena, node_index, visitor)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        class(ast_visitor_t), intent(inout) :: visitor
        
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        
        ! Visit current node first
        select type (node => arena%entries(node_index)%node)
        class is (ast_node)
            call visit_node(node, visitor)
            ! Then visit children
            call traverse_children(arena, node, visitor, .true.)
        end select
    end subroutine traverse_preorder
    
    ! Post-order traversal (visit children before node)
    recursive subroutine traverse_postorder(arena, node_index, visitor)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        class(ast_visitor_t), intent(inout) :: visitor
        
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        
        select type (node => arena%entries(node_index)%node)
        class is (ast_node)
            ! Visit children first
            call traverse_children(arena, node, visitor, .false.)
            
            ! Then visit current node
            call visit_node(node, visitor)
        end select
    end subroutine traverse_postorder
    
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
        integer :: i
        
        select type (n => node)
        type is (program_node)
            if (allocated(n%body_indices)) then
                do i = 1, size(n%body_indices)
                    call traverse_node(arena, n%body_indices(i), visitor, is_preorder)
                end do
            end if
            
        type is (assignment_node)
            call traverse_node(arena, n%target_index, visitor, is_preorder)
            call traverse_node(arena, n%value_index, visitor, is_preorder)
            
        type is (binary_op_node)
            call traverse_node(arena, n%left_index, visitor, is_preorder)
            call traverse_node(arena, n%right_index, visitor, is_preorder)
            
        type is (function_def_node)
            if (allocated(n%param_indices)) then
                do i = 1, size(n%param_indices)
                    call traverse_node(arena, n%param_indices(i), visitor, is_preorder)
                end do
            end if
            if (allocated(n%body_indices)) then
                do i = 1, size(n%body_indices)
                    call traverse_node(arena, n%body_indices(i), visitor, is_preorder)
                end do
            end if
            
        type is (subroutine_def_node)
            if (allocated(n%param_indices)) then
                do i = 1, size(n%param_indices)
                    call traverse_node(arena, n%param_indices(i), visitor, is_preorder)
                end do
            end if
            if (allocated(n%body_indices)) then
                do i = 1, size(n%body_indices)
                    call traverse_node(arena, n%body_indices(i), visitor, is_preorder)
                end do
            end if
            
        type is (call_or_subscript_node)
            if (allocated(n%arg_indices)) then
                do i = 1, size(n%arg_indices)
                    call traverse_node(arena, n%arg_indices(i), visitor, is_preorder)
                end do
            end if
            
        type is (subroutine_call_node)
            if (allocated(n%arg_indices)) then
                do i = 1, size(n%arg_indices)
                    call traverse_node(arena, n%arg_indices(i), visitor, is_preorder)
                end do
            end if
            
        type is (if_node)
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
            
        type is (do_loop_node)
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
            
        type is (do_while_node)
            call traverse_node(arena, n%condition_index, visitor, is_preorder)
            if (allocated(n%body_indices)) then
                do i = 1, size(n%body_indices)
                    call traverse_node(arena, n%body_indices(i), visitor, is_preorder)
                end do
            end if
            
        type is (select_case_node)
            call traverse_node(arena, n%selector_index, visitor, is_preorder)
            if (allocated(n%case_indices)) then
                do i = 1, size(n%case_indices)
                    call traverse_node(arena, n%case_indices(i), visitor, is_preorder)
                end do
            end if
            if (n%default_index > 0) then
                call traverse_node(arena, n%default_index, visitor, is_preorder)
            end if
            
        type is (module_node)
            if (allocated(n%declaration_indices)) then
                do i = 1, size(n%declaration_indices)
                    call traverse_node(arena, n%declaration_indices(i), visitor, is_preorder)
                end do
            end if
            if (allocated(n%procedure_indices)) then
                do i = 1, size(n%procedure_indices)
                    call traverse_node(arena, n%procedure_indices(i), visitor, is_preorder)
                end do
            end if
            
        type is (derived_type_node)
            if (allocated(n%component_indices)) then
                do i = 1, size(n%component_indices)
                    call traverse_node(arena, n%component_indices(i), visitor, is_preorder)
                end do
            end if
            
        type is (interface_block_node)
            if (allocated(n%procedure_indices)) then
                do i = 1, size(n%procedure_indices)
                    call traverse_node(arena, n%procedure_indices(i), visitor, is_preorder)
                end do
            end if
            
        type is (print_statement_node)
            if (allocated(n%expression_indices)) then
                do i = 1, size(n%expression_indices)
                    call traverse_node(arena, n%expression_indices(i), visitor, is_preorder)
                end do
            end if
        end select
    end subroutine traverse_children
    
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