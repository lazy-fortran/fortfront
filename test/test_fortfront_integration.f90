program test_fortfront_integration
    ! Comprehensive integration test for the fortfront public API
    use fortfront
    use iso_fortran_env, only: error_unit
    implicit none
    
    integer :: failures = 0
    
    ! Run comprehensive integration test
    call test_complete_pipeline()
    
    if (failures == 0) then
        print *, "All fortfront integration tests passed!"
    else
        print *, "fortfront integration tests failed with", failures, "failures"
        stop 1
    end if
    
contains
    
    subroutine test_complete_pipeline()
        type(ast_arena_t) :: arena
        type(semantic_context_t) :: ctx
        type(token_t), allocatable :: tokens(:)
        integer :: prog_index
        character(len=:), allocatable :: error_msg, output_code, json_str
        type(compilation_options_t) :: options
        integer, allocatable :: func_nodes(:), assign_nodes(:)
        type(mono_type_t), allocatable :: node_type
        type(source_range_t) :: range
        type(diagnostic_t), allocatable :: diagnostics(:)
        character(len=*), parameter :: source = &
            "program integration_test" // char(10) // &
            "    implicit none" // char(10) // &
            "    integer :: x, y, z" // char(10) // &
            "    real :: result" // char(10) // &
            "" // char(10) // &
            "    x = 10" // char(10) // &
            "    y = 20" // char(10) // &
            "    z = x + y" // char(10) // &
            "    result = real(z) * 3.14" // char(10) // &
            "" // char(10) // &
            "    print *, 'Result:', result" // char(10) // &
            "end program integration_test"
        
        print *, "Testing complete fortfront pipeline integration..."
        
        ! 1. Lexical analysis
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "Lex error:", trim(error_msg)
                failures = failures + 1
                return
            end if
        end if
        print *, "  ✓ Lexical analysis completed,", size(tokens), "tokens"
        
        ! 2. Create arena and parse
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "Parse error:", trim(error_msg)
                failures = failures + 1
                return
            end if
        end if
        print *, "  ✓ Parsing completed, root node:", prog_index
        
        ! 3. Semantic analysis
        ctx = create_semantic_context()
        call analyze_semantics(arena, prog_index)
        print *, "  ✓ Semantic analysis completed"
        
        ! 4. Test arena navigation
        assign_nodes = find_nodes_by_type(arena, "assignment")
        if (size(assign_nodes) > 0) then
            print *, "  ✓ Found", size(assign_nodes), "assignment nodes"
        else
            print *, "  ! No assignment nodes found"
            failures = failures + 1
        end if
        
        ! 5. Test type information
        if (allocated(assign_nodes) .and. size(assign_nodes) > 0) then
            node_type = get_type_for_node(arena, assign_nodes(1))
            if (allocated(node_type)) then
                print *, "  ✓ Got type information for assignment"
            else
                print *, "  ! No type information available"
            end if
        end if
        
        ! 6. Test source location
        range = get_node_range(arena, prog_index)
        print *, "  ✓ Source location: line", range%start%line, "col", range%start%column
        
        ! 7. Test diagnostics
        diagnostics = get_diagnostics(ctx)
        print *, "  ✓ Diagnostics:", size(diagnostics), "items"
        
        ! 8. Test JSON serialization
        call ast_to_json(arena, prog_index, json_str)
        if (allocated(json_str)) then
            if (len(json_str) > 0) then
                print *, "  ✓ JSON serialization successful,", len(json_str), "chars"
            else
                print *, "  ! Empty JSON output"
                failures = failures + 1
            end if
        else
            print *, "  ! JSON serialization failed"
            failures = failures + 1
        end if
        
        ! 9. Test code generation
        call emit_fortran(arena, prog_index, output_code)
        if (allocated(output_code)) then
            if (len(output_code) > 0) then
                print *, "  ✓ Code generation successful,", len(output_code), "chars"
            else
                print *, "  ! Empty code output"
                failures = failures + 1
            end if
        else
            print *, "  ! Code generation failed"
            failures = failures + 1
        end if
        
        ! 10. Test transformation pipeline
        call transform_lazy_fortran_string(source, output_code, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "  ! Transformation error:", trim(error_msg)
                failures = failures + 1
            else
                print *, "  ✓ Transformation pipeline successful"
            end if
        else
            if (allocated(output_code) .and. len(output_code) > 0) then
                print *, "  ✓ Transformation pipeline successful"
            else
                print *, "  ! Transformation produced no output"
                failures = failures + 1
            end if
        end if
        
        print *, "Integration test completed"
    end subroutine test_complete_pipeline
    
end program test_fortfront_integration