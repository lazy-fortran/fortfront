module standardizer
    ! AST Standardization Stage - transforms lazy fortran AST to standard Fortran AST
    ! 
    ! This module provides a unified interface to all standardizer functionality
    ! by re-exporting procedures from specialized sub-modules.
    !
    ! Sub-modules:
    ! - standardizer_core: Main entry points and orchestration
    ! - standardizer_program: Program-specific transformations
    ! - standardizer_module: Module-specific transformations
    ! - standardizer_declarations: Variable declaration generation
    ! - standardizer_subprograms: Function/subroutine standardization
    ! - standardizer_types: Type inference and utilities
    ! - standardizer_allocatable: Allocatable marking logic

    use standardizer_core, only: &
        standardize_ast, &
        standardize_ast_json, &
        set_standardizer_type_standardization, &
        get_standardizer_type_standardization
        
    use standardizer_types, only: &
        string_result_t, &
        INVALID_INTEGER

    use standardizer_program, only: &
        standardize_program, &
        analyze_program_content, &
        find_contains_insertion_point, &
        insert_contains_statement

    use standardizer_module, only: &
        standardize_module

    use standardizer_declarations, only: &
        insert_variable_declarations, &
        has_implicit_none, &
        program_has_variable_declarations, &
        find_declaration_insertion_point, &
        generate_and_insert_declarations, &
        has_explicit_declaration, &
        collect_statement_vars, &
        collect_assignment_vars, &
        collect_identifier_var, &
        collect_identifier_var_with_type, &
        add_variable, &
        mark_variable_declared, &
        standardize_declarations

    use standardizer_subprograms, only: &
        standardize_subprograms, &
        standardize_function_def, &
        standardize_subroutine_def, &
        standardize_function_parameters, &
        standardize_subroutine_parameters, &
        wrap_function_in_program, &
        wrap_subroutine_in_program, &
        infer_parameter_type

    use standardizer_types, only: &
        is_array_type, &
        get_expression_type, &
        has_array_slice_args, &
        is_array_expression, &
        has_implied_do_loop, &
        get_implied_do_size, &
        calculate_loop_size, &
        get_integer_literal_value, &
        get_array_var_type, &
        infer_element_type_from_literal, &
        get_fortran_type_string

    use standardizer_allocatable, only: &
        mark_allocatable_for_array_reassignments, &
        mark_allocatable_for_string_length_changes, &
        count_variable_assignments, &
        mark_declarations_allocatable, &
        handle_multi_variable_declaration_allocatable, &
        split_multi_variable_declaration, &
        update_program_body_indices, &
        is_array_assignment, &
        is_procedure_parameter, &
        collect_string_vars_needing_allocatable

    implicit none
    
    ! Re-export all public interfaces
    public :: &
        ! Core functionality
        standardize_ast, &
        standardize_ast_json, &
        set_standardizer_type_standardization, &
        get_standardizer_type_standardization, &
        string_result_t, &
        INVALID_INTEGER, &
        
        ! Program standardization
        standardize_program, &
        analyze_program_content, &
        find_contains_insertion_point, &
        insert_contains_statement, &
        
        ! Module standardization
        standardize_module, &
        
        ! Declaration generation
        insert_variable_declarations, &
        has_implicit_none, &
        program_has_variable_declarations, &
        find_declaration_insertion_point, &
        generate_and_insert_declarations, &
        has_explicit_declaration, &
        collect_statement_vars, &
        collect_assignment_vars, &
        collect_identifier_var, &
        collect_identifier_var_with_type, &
        add_variable, &
        mark_variable_declared, &
        standardize_declarations, &
        
        ! Subprogram standardization
        standardize_subprograms, &
        standardize_function_def, &
        standardize_subroutine_def, &
        standardize_function_parameters, &
        standardize_subroutine_parameters, &
        wrap_function_in_program, &
        wrap_subroutine_in_program, &
        infer_parameter_type, &
        
        ! Type utilities
        is_array_type, &
        get_expression_type, &
        has_array_slice_args, &
        is_array_expression, &
        has_implied_do_loop, &
        get_implied_do_size, &
        calculate_loop_size, &
        get_integer_literal_value, &
        get_array_var_type, &
        infer_element_type_from_literal, &
        get_fortran_type_string, &
        
        ! Allocatable marking
        mark_allocatable_for_array_reassignments, &
        mark_allocatable_for_string_length_changes, &
        count_variable_assignments, &
        mark_declarations_allocatable, &
        handle_multi_variable_declaration_allocatable, &
        split_multi_variable_declaration, &
        update_program_body_indices, &
        is_array_assignment, &
        is_procedure_parameter, &
        collect_string_vars_needing_allocatable

end module standardizer