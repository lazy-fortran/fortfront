module codegen_core
    use iso_fortran_env, only: error_unit
    use ast_core
    use codegen_expressions
    use codegen_statements  
    use codegen_control_flow
    use codegen_declarations
    use codegen_utilities, only: set_type_standardization, get_type_standardization, &
        add_line_continuations
    implicit none
    private

    public :: generate_code_from_arena
    public :: generate_code_polymorphic
    public :: safe_generate_code_from_arena
    public :: set_type_standardization, get_type_standardization

contains

    ! Main entry point for code generation from AST arena
    function generate_code_from_arena(arena, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code

        code = ""
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return

        ! Dispatch to appropriate generator based on node type
        select type (node => arena%entries(node_index)%node)
        
        ! Expression nodes
        type is (literal_node)
            code = generate_code_literal(node)
        type is (identifier_node)
            code = generate_code_identifier(node)
        type is (binary_op_node)
            code = generate_code_binary_op(arena, node, node_index)
        type is (call_or_subscript_node)
            code = generate_code_call_or_subscript(arena, node, node_index)
        type is (array_literal_node)
            code = generate_code_array_literal(arena, node, node_index)
        type is (range_expression_node)
            code = generate_code_range_expression(arena, node, node_index)
        type is (array_bounds_node)
            code = generate_code_array_bounds(arena, node, node_index)
        type is (array_slice_node)
            code = generate_code_array_slice(arena, node, node_index)
        type is (array_operation_node)
            code = generate_code_array_operation(arena, node, node_index)
            
        ! Statement nodes
        type is (assignment_node)
            code = generate_code_assignment(arena, node, node_index)
        type is (subroutine_call_node)
            code = generate_code_subroutine_call(arena, node, node_index)
        type is (print_statement_node)
            code = generate_code_print_statement(arena, node, node_index)
        type is (write_statement_node)
            code = generate_code_write_statement(arena, node, node_index)
        type is (read_statement_node)
            code = generate_code_read_statement(arena, node, node_index)
        type is (stop_node)
            code = generate_code_stop(arena, node, node_index)
        type is (return_node)
            code = generate_code_return(arena, node, node_index)
        type is (goto_node)
            code = generate_code_goto(arena, node, node_index)
        type is (error_stop_node)
            code = generate_code_error_stop(arena, node, node_index)
        type is (cycle_node)
            code = generate_code_cycle(arena, node, node_index)
        type is (exit_node)
            code = generate_code_exit(arena, node, node_index)
        type is (use_statement_node)
            code = generate_code_use_statement(node)
        type is (implicit_statement_node)
            code = generate_code_implicit_statement(node)
        type is (comment_node)
            code = generate_code_comment(node)
        type is (blank_line_node)
            code = generate_code_blank_line(node)
            
        ! Control flow nodes
        type is (if_node)
            code = generate_code_if(arena, node, node_index)
        type is (do_loop_node)
            code = generate_code_do_loop(arena, node, node_index)
        type is (do_while_node)
            code = generate_code_do_while(arena, node, node_index)
        type is (select_case_node)
            code = generate_code_select_case(arena, node, node_index)
        type is (where_node)
            code = generate_code_where(arena, node, node_index)
        type is (forall_node)
            code = generate_code_forall(arena, node, node_index)
            
        ! Declaration and definition nodes
        type is (declaration_node)
            code = generate_code_declaration(arena, node, node_index)
        type is (parameter_declaration_node)
            code = generate_code_parameter_declaration(arena, node, node_index)
        type is (function_def_node)
            code = generate_code_function_def(arena, node, node_index)
        type is (subroutine_def_node)
            code = generate_code_subroutine_def(arena, node, node_index)
        type is (module_node)
            code = generate_code_module(arena, node, node_index)
        type is (program_node)
            code = generate_code_program(arena, node, node_index)
        type is (derived_type_node)
            code = generate_code_derived_type(arena, node, node_index)
            
        ! Special nodes
        type is (contains_node)
            code = "contains"
        type is (end_statement_node)
            code = "end"
            
        class default
            ! Unknown node type
            code = "! Unknown node type"
        end select
    end function generate_code_from_arena

    ! Polymorphic code generator (same as generate_code_from_arena)
    function generate_code_polymorphic(arena, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code

        code = generate_code_from_arena(arena, node_index)
    end function generate_code_polymorphic

    subroutine safe_generate_code_from_arena(arena, node_index, code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: code

        if (node_index <= 0 .or. node_index > arena%size) then
            code = ""
            return
        end if

        if (.not. allocated(arena%entries(node_index)%node)) then
            code = ""
            return
        end if

        code = generate_code_from_arena(arena, node_index)

        ! Add line continuations for overly long lines
        code = add_line_continuations(code)
    end subroutine safe_generate_code_from_arena

end module codegen_core