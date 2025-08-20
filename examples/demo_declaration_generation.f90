program demo_declaration_generation
    ! Demonstration of automatic variable declaration generation for Issue #350
    use iso_fortran_env, only: output_unit
    use semantic_analyzer, only: semantic_context_t, create_semantic_context, &
                                 declaration_t, undeclared_variable_t, &
                                 USAGE_PARAMETER, USAGE_LOCAL_VAR, USAGE_RESULT_VAR
    use type_system_hm, only: create_mono_type, TINT, TREAL, TCHAR, TLOGICAL, TARRAY
    use ast_nodes_data, only: INTENT_IN, INTENT_OUT, INTENT_INOUT, INTENT_NONE
    use codegen_core, only: generate_declaration_code, insert_generated_declarations
    use ast_core, only: ast_arena_t
    implicit none

    type(semantic_context_t) :: ctx
    type(undeclared_variable_t), allocatable :: undeclared_vars(:)
    type(declaration_t), allocatable :: declarations(:)
    type(ast_arena_t) :: dummy_arena
    character(len=:), allocatable :: body_code, enhanced_code
    integer :: i

    write(output_unit, '(A)') "=== Declaration Generation Demo for Issue #350 ==="
    write(output_unit, '(A)') ""

    ! Create semantic context
    ctx = create_semantic_context()
    dummy_arena%size = 0

    ! Create example undeclared variables as discovered by semantic analysis
    allocate(undeclared_vars(5))
    
    ! 1. Integer parameter with intent(in)
    undeclared_vars(1)%name = "n"
    undeclared_vars(1)%usage_type = USAGE_PARAMETER
    undeclared_vars(1)%inferred_type = create_mono_type(TINT)
    undeclared_vars(1)%inferred_intent = INTENT_IN
    
    ! 2. Real array parameter with intent(out)
    undeclared_vars(2)%name = "result_array"
    undeclared_vars(2)%usage_type = USAGE_PARAMETER
    undeclared_vars(2)%inferred_type = create_mono_type(TARRAY, &
                                        args=[create_mono_type(TREAL)])
    undeclared_vars(2)%inferred_intent = INTENT_OUT
    
    ! 3. Character parameter with intent(in)
    undeclared_vars(3)%name = "message"
    undeclared_vars(3)%usage_type = USAGE_PARAMETER
    undeclared_vars(3)%inferred_type = create_mono_type(TCHAR)
    undeclared_vars(3)%inferred_intent = INTENT_IN
    
    ! 4. Local variable (real)
    undeclared_vars(4)%name = "temp_value"
    undeclared_vars(4)%usage_type = USAGE_LOCAL_VAR
    undeclared_vars(4)%inferred_type = create_mono_type(TREAL)
    undeclared_vars(4)%inferred_intent = INTENT_NONE
    
    ! 5. Function result variable
    undeclared_vars(5)%name = "calculation_result"
    undeclared_vars(5)%usage_type = USAGE_RESULT_VAR
    undeclared_vars(5)%inferred_type = create_mono_type(TREAL)
    undeclared_vars(5)%inferred_intent = INTENT_NONE

    write(output_unit, '(A)') "Undeclared variables discovered by semantic analysis:"
    do i = 1, size(undeclared_vars)
        write(output_unit, '(A,I0,A,A,A,I0)') "  ", i, ". ", &
              undeclared_vars(i)%name, " (usage_type: ", undeclared_vars(i)%usage_type, ")"
    end do
    write(output_unit, '(A)') ""

    ! Generate declarations
    call ctx%generate_variable_declarations(dummy_arena, 1, undeclared_vars, declarations)

    write(output_unit, '(A)') "Generated Fortran declarations:"
    do i = 1, size(declarations)
        write(output_unit, '(A,A)') "  ", generate_declaration_code(declarations(i))
    end do
    write(output_unit, '(A)') ""

    ! Demonstrate insertion into function body
    body_code = "implicit none" // new_line('a') // &
               new_line('a') // &
               "! Calculate something" // new_line('a') // &
               "temp_value = 3.14" // new_line('a') // &
               "result_array(1:n) = temp_value" // new_line('a') // &
               "calculation_result = sum(result_array(1:n))" // new_line('a') // &
               "print *, trim(message), calculation_result"

    write(output_unit, '(A)') "Original function body:"
    write(output_unit, '(A)') body_code
    write(output_unit, '(A)') ""

    ! Insert declarations
    enhanced_code = insert_generated_declarations(body_code, declarations, "    ")

    write(output_unit, '(A)') "Enhanced function body with auto-generated declarations:"
    write(output_unit, '(A)') enhanced_code
    write(output_unit, '(A)') ""

    ! Show complete function example
    write(output_unit, '(A)') "Complete function example:"
    write(output_unit, '(A)') "function calculate_sum(n, result_array, message) result(calculation_result)"
    write(output_unit, '(A)') enhanced_code
    write(output_unit, '(A)') "end function calculate_sum"
    write(output_unit, '(A)') ""

    write(output_unit, '(A)') "=== Declaration Generation System Implementation Complete ==="
    write(output_unit, '(A)') ""
    write(output_unit, '(A)') "Key Features Implemented:"
    write(output_unit, '(A)') "  ✓ declaration_t data structure for variable declarations"
    write(output_unit, '(A)') "  ✓ generate_variable_declarations() method in semantic analyzer"
    write(output_unit, '(A)') "  ✓ Type mapping from mono_type_t to Fortran type specifications"
    write(output_unit, '(A)') "  ✓ Intent attribute inference and formatting"
    write(output_unit, '(A)') "  ✓ Array specification generation (assumed-shape)"
    write(output_unit, '(A)') "  ✓ Declaration code generation with proper Fortran syntax"
    write(output_unit, '(A)') "  ✓ Automatic insertion after 'implicit none'"
    write(output_unit, '(A)') "  ✓ Deduplication system to prevent duplicate declarations"
    write(output_unit, '(A)') "  ✓ Integration with existing variable discovery and type inference"

end program demo_declaration_generation