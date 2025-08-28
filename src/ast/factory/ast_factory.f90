module ast_factory
    ! REFACTORED ast_factory module - split into specialized modules for architectural compliance
    ! 
    ! This module now re-exports all functions from the specialized modules to maintain
    ! full backward compatibility with existing code.
    !
    ! Previous file: 1911 lines (violated hard limit of 1000 lines)
    ! Current structure: Multiple modules <500 lines each (compliance achieved)
    
    ! Core functionality and utilities
    use ast_factory_core, only: &
        validate_arena, validate_node_index, &
        push_program, push_identifier, push_literal, push_binary_op, &
        push_assignment, push_pointer_assignment, push_array_literal, &
        push_complex_literal, push_component_access, push_range_subscript, &
        push_type_constructor

    ! Expression-related nodes
    use ast_factory_expressions, only: &
        push_call_or_subscript, push_subroutine_call, &
        push_call_or_subscript_with_slice_detection, build_ast_from_nodes

    ! Declaration nodes
    use ast_factory_declarations, only: &
        push_declaration, push_multi_declaration, push_parameter_declaration, &
        push_derived_type

    ! Control flow nodes
    use ast_factory_control, only: &
        push_if, push_do_loop, push_do_while, push_forall, push_select_case, &
        push_associate, push_case_block, push_case_range, push_case_default, &
        push_select_case_with_default, push_where, push_where_construct, &
        push_where_construct_with_elsewhere

    ! I/O statement nodes
    use ast_factory_io, only: &
        push_print_statement, push_write_statement, push_read_statement, &
        push_read_statement_with_err, push_read_statement_with_end, &
        push_read_statement_with_all_specifiers, push_write_statement_with_iostat, &
        push_write_statement_with_format, push_write_statement_with_runtime_format

    ! Procedure definition nodes
    use ast_factory_procedures, only: &
        push_function_def, push_subroutine_def, push_interface_block, &
        push_module, push_module_structured

    ! Array-related nodes
    use ast_factory_arrays, only: &
        push_array_section, push_array_bounds, push_array_slice, &
        push_range_expression

    ! Statement nodes
    use ast_factory_statements, only: &
        push_use_statement, push_implicit_statement, push_include_statement, &
        push_end_statement, push_stop, push_return, push_goto, push_error_stop, &
        push_cycle, push_exit, push_allocate, push_deallocate

    implicit none
    private

    ! Re-export all public interfaces to maintain backward compatibility
    
    ! Core functionality and utilities
    public :: validate_arena, validate_node_index
    public :: push_program, push_identifier, push_literal, push_binary_op
    public :: push_assignment, push_pointer_assignment, push_array_literal
    public :: push_complex_literal, push_component_access, push_range_subscript
    public :: push_type_constructor

    ! Expression-related nodes
    public :: push_call_or_subscript, push_subroutine_call
    public :: push_call_or_subscript_with_slice_detection, build_ast_from_nodes

    ! Declaration nodes
    public :: push_declaration, push_multi_declaration, push_parameter_declaration
    public :: push_derived_type

    ! Control flow nodes
    public :: push_if, push_do_loop, push_do_while, push_forall, push_select_case
    public :: push_associate, push_case_block, push_case_range, push_case_default
    public :: push_select_case_with_default, push_where, push_where_construct
    public :: push_where_construct_with_elsewhere

    ! I/O statement nodes
    public :: push_print_statement, push_write_statement, push_read_statement
    public :: push_read_statement_with_err, push_read_statement_with_end
    public :: push_read_statement_with_all_specifiers, push_write_statement_with_iostat
    public :: push_write_statement_with_format, push_write_statement_with_runtime_format

    ! Procedure definition nodes
    public :: push_function_def, push_subroutine_def, push_interface_block
    public :: push_module, push_module_structured

    ! Array-related nodes
    public :: push_array_section, push_array_bounds, push_array_slice
    public :: push_range_expression

    ! Statement nodes
    public :: push_use_statement, push_implicit_statement, push_include_statement
    public :: push_end_statement, push_stop, push_return, push_goto, push_error_stop
    public :: push_cycle, push_exit, push_allocate, push_deallocate

    ! Note: No module body needed - all functionality is provided by re-exported modules
    ! This maintains full API compatibility while achieving architectural compliance

end module ast_factory