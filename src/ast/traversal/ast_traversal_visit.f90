module ast_traversal_visit
    use ast_base, only: ast_node
    use ast_nodes_control, only: if_node, select_case_node
    use ast_nodes_core, only: assignment_node, binary_op_node, &
                              call_or_subscript_node, identifier_node, &
                              literal_node, program_node
    use ast_nodes_data, only: declaration_node, derived_type_node, module_node, &
                              submodule_node
    use ast_nodes_io, only: print_statement_node
    use ast_nodes_loops, only: do_loop_node, do_while_node
    use ast_nodes_misc, only: include_statement_node, interface_block_node, &
                              use_statement_node, visibility_statement_node
    use ast_nodes_procedure, only: function_def_node, subroutine_call_node, &
                                   subroutine_def_node
    use ast_visitor, only: ast_visitor_t
    implicit none
    private

    public :: visit_node

contains

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
        type is (submodule_node)
            call visitor%visit_submodule(n)
        type is (use_statement_node)
            call visitor%visit_use_statement(n)
        type is (visibility_statement_node)
            call visitor%visit_visibility_statement(n)
        type is (include_statement_node)
            call visitor%visit_include_statement(n)
        end select
    end subroutine visit_node

end module ast_traversal_visit
