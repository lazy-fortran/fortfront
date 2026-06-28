module ast_traversal
    use ast_traversal_core, only: traverse_ast, traverse_preorder, traverse_postorder
    use ast_traversal_predicates, only: is_program_node, is_assignment_node, &
        is_binary_op_node, is_function_def_node, &
        is_subroutine_def_node, is_identifier_node, &
        is_literal_node, is_declaration_node, &
        is_if_node, is_do_loop_node, &
        is_do_while_node, is_call_or_subscript_node, &
        is_subroutine_call_node, &
        is_print_statement_node, &
        is_use_statement_node, is_select_case_node, &
        is_derived_type_node, is_module_node, &
        is_interface_block_node
    use ast_traversal_visit, only: visit_node
    implicit none
    private

    public :: traverse_ast, traverse_preorder, traverse_postorder
    public :: visit_node
    public :: is_program_node, is_assignment_node, is_binary_op_node
    public :: is_function_def_node, is_subroutine_def_node
    public :: is_identifier_node, is_literal_node, is_declaration_node
    public :: is_if_node, is_do_loop_node, is_do_while_node
    public :: is_call_or_subscript_node, is_subroutine_call_node
    public :: is_print_statement_node, is_use_statement_node
    public :: is_select_case_node, is_derived_type_node
    public :: is_module_node, is_interface_block_node

end module ast_traversal
