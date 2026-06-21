module codegen_expressions
    use, intrinsic :: iso_fortran_env, only: error_unit
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core
    use ast_nodes_data
    use ast_base, only: LITERAL_INTEGER, LITERAL_REAL
    use ast_nodes_bounds, only: array_slice_node, array_bounds_node, &
                                range_expression_node
    use ast_nodes_misc, only: complex_literal_node
    use ast_nodes_loops, only: do_loop_node
    use ast_nodes_io, only: io_implied_do_node
    use type_system_unified
    use codegen_indent
    use codegen_arena_interface, only: generate_code_from_arena
    use codegen_type_utils, only: get_type_standardization
    implicit none
    private

    public :: generate_code_literal
    public :: generate_code_identifier
    public :: generate_code_binary_op
    public :: generate_code_component_access
    public :: generate_code_range_subscript
    public :: generate_code_call_or_subscript
    public :: generate_code_array_literal
    public :: generate_code_complex_literal
    public :: generate_code_range_expression
    public :: generate_code_array_bounds
    public :: generate_code_array_slice
    public :: generate_code_array_operation
    public :: generate_code_io_implied_do
    public :: generate_code_implied_do
    public :: get_operator_precedence
    public :: needs_parentheses
    public :: get_node_operator

contains

    include 'codegen_expressions_part1.inc'
    include 'codegen_expressions_part2.inc'

end module codegen_expressions
