module ast_api
    ! Public AST API for library consumers
    ! Provides access to AST node types and arena operations
    use ast_base, only: &
        ast_node, &
        ast_node_wrapper, &
        ast_visitor_base_t, &
        visit_interface, &
        string_t, &
        LITERAL_INTEGER, &
        LITERAL_REAL, &
        LITERAL_STRING, &
        LITERAL_LOGICAL, &
        LITERAL_ARRAY, &
        LITERAL_COMPLEX
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: &
        program_node, &
        assignment_node, &
        pointer_assignment_node, &
        identifier_node, &
        literal_node, &
        binary_op_node, &
        call_or_subscript_node, &
        array_literal_node, &
        component_access_node, &
        range_subscript_node
    use ast_nodes_procedure, only: &
        function_def_node, &
        subroutine_def_node, &
        subroutine_call_node
    use ast_nodes_control, only: &
        if_node, &
        do_loop_node, &
        do_while_node, &
        select_case_node, &
        case_block_node, &
        exit_node, &
        cycle_node
    use ast_nodes_data, only: &
        module_node, &
        derived_type_node, &
        declaration_node
    use ast_nodes_misc, only: interface_block_node
    use ast_visitor, only: ast_visitor_t
    use ast_traversal_utils, only: &
        traverse_ast, &
        find_nodes_by_type

    implicit none
    private

    ! Base types and interfaces
    public :: ast_node
    public :: ast_node_wrapper
    public :: ast_visitor_base_t
    public :: visit_interface
    public :: string_t

    ! Literal type constants
    public :: LITERAL_INTEGER
    public :: LITERAL_REAL
    public :: LITERAL_STRING
    public :: LITERAL_LOGICAL
    public :: LITERAL_ARRAY
    public :: LITERAL_COMPLEX

    ! Arena type
    public :: ast_arena_t

    ! Core node types
    public :: program_node
    public :: assignment_node
    public :: pointer_assignment_node
    public :: identifier_node
    public :: literal_node
    public :: binary_op_node
    public :: call_or_subscript_node
    public :: array_literal_node
    public :: component_access_node
    public :: range_subscript_node

    ! Procedure node types
    public :: function_def_node
    public :: subroutine_def_node
    public :: subroutine_call_node

    ! Control flow node types
    public :: if_node
    public :: do_loop_node
    public :: do_while_node
    public :: select_case_node
    public :: case_block_node
    public :: exit_node
    public :: cycle_node

    ! Data structure node types
    public :: module_node
    public :: interface_block_node
    public :: derived_type_node
    public :: declaration_node

    ! Visitor support
    public :: ast_visitor_t

    ! Traversal utilities
    public :: traverse_ast
    public :: find_nodes_by_type

end module ast_api
