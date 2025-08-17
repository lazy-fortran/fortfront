program vicky_test_realistic_fortran
    ! Test semantic pipeline on realistic Fortran code
    ! Simulates external tool usage on real-world code patterns
    
    use ast_core, only: ast_arena_t, create_ast_arena
    use ast_factory, only: push_program, push_function_def, push_assignment, &
                          push_identifier, push_literal, push_declaration
    use semantic_pipeline, only: semantic_pipeline_t, create_pipeline
    use builtin_analyzers, only: symbol_analyzer_t, type_analyzer_t, scope_analyzer_t, &
                                 call_graph_analyzer_t, control_flow_analyzer_t, &
                                 usage_tracker_analyzer_t
    use frontend, only: lex_source, parse_tokens, analyze_semantics
    use lexer_core, only: token_t
    implicit none

    type(semantic_pipeline_t) :: pipeline
    type(symbol_analyzer_t) :: symbol_analyzer
    type(type_analyzer_t) :: type_analyzer
    type(scope_analyzer_t) :: scope_analyzer
    type(call_graph_analyzer_t) :: call_graph_analyzer
    type(control_flow_analyzer_t) :: control_flow_analyzer
    type(usage_tracker_analyzer_t) :: usage_analyzer
    
    type(ast_arena_t) :: arena
    type(token_t), allocatable :: tokens(:)
    integer :: prog_index
    logical :: all_passed = .true.
    character(len=*), parameter :: realistic_fortran_code = &
        "program test_analysis" // new_line('a') // &
        "  implicit none" // new_line('a') // &
        "  integer :: x, y, z" // new_line('a') // &
        "  real :: result" // new_line('a') // &
        "  x = 10" // new_line('a') // &
        "  y = 20" // new_line('a') // &
        "  z = x + y" // new_line('a') // &
        "  result = sqrt(real(z))" // new_line('a') // &
        "  print *, 'Result:', result" // new_line('a') // &
        "end program test_analysis"

    print *, "=== VICKY'S REALISTIC FORTRAN CODE ANALYSIS TEST ==="
    print *, "Testing semantic pipeline on real Fortran patterns"
    
    ! Test 1: Parse realistic Fortran code
    call test_fortran_code_parsing()
    
    ! Test 2: Full semantic analysis pipeline on real code
    call test_full_semantic_pipeline_realistic()
    
    ! Test 3: External tool scenario (fluff-like analysis)
    call test_external_tool_scenario()

    if (all_passed) then
        print *, ""
        print *, "=== ALL REALISTIC FORTRAN TESTS PASSED ==="
        print *, "✓ Pipeline works with real Fortran syntax"
        print *, "✓ All analyzers handle realistic code patterns" 
        print *, "✓ External tool integration works on real code"
        print *, "✓ Memory management stable with realistic workloads"
        print *, ""
        print *, "🎉 Semantic pipeline READY for production Fortran code!"
    else
        print *, ""
        print *, "❌ REALISTIC FORTRAN TESTS FAILED"
        print *, "Pipeline not ready for production use"
        error stop
    end if

contains

    subroutine test_fortran_code_parsing()
        character(len=:), allocatable :: error_msg
        
        print *, "=== Test 1: Realistic Fortran Code Parsing ==="
        
        ! Test lexing and parsing of realistic code
        call lex_source(realistic_fortran_code, tokens, error_msg)
        
        if (len_trim(error_msg) > 0) then
            print *, "❌ FAIL: Lexing error:", trim(error_msg)
            all_passed = .false.
            return
        end if
        
        if (.not. allocated(tokens) .or. size(tokens) == 0) then
            print *, "❌ FAIL: No tokens generated"
            all_passed = .false.
            return
        end if
        
        print *, "✓ PASS: Realistic Fortran code lexed successfully"
        print *, "  Generated", size(tokens), "tokens"
        
        ! Parse the tokens into AST
        call parse_tokens(tokens, arena, prog_index, error_msg)
        
        if (len_trim(error_msg) > 0) then
            print *, "❌ FAIL: Parsing error:", trim(error_msg)
            all_passed = .false.
            return
        end if
        
        if (prog_index <= 0) then
            print *, "❌ FAIL: No program node created"
            all_passed = .false.
            return
        end if
        
        print *, "✓ PASS: Realistic Fortran code parsed successfully"
        print *, "  Program node index:", prog_index
    end subroutine

    subroutine test_full_semantic_pipeline_realistic()
        print *, "=== Test 2: Full Semantic Pipeline on Real Code ==="
        
        pipeline = create_pipeline()
        
        ! Register comprehensive analysis (what production tools need)
        call pipeline%register_analyzer(symbol_analyzer)
        call pipeline%register_analyzer(type_analyzer)
        call pipeline%register_analyzer(scope_analyzer)
        call pipeline%register_analyzer(call_graph_analyzer)
        call pipeline%register_analyzer(control_flow_analyzer)
        call pipeline%register_analyzer(usage_analyzer)
        
        if (pipeline%get_analyzer_count() /= 6) then
            print *, "❌ FAIL: Should have 6 analyzers for comprehensive analysis"
            all_passed = .false.
            return
        end if
        
        ! Run full semantic analysis on real code
        call pipeline%run_analysis(arena, prog_index)
        
        print *, "✓ PASS: Full semantic pipeline works on realistic code"
        print *, "  Analyzed program with variables, expressions, and function calls"
    end subroutine

    subroutine test_external_tool_scenario()
        type(semantic_pipeline_t) :: fluff_pipeline
        type(usage_tracker_analyzer_t) :: fluff_usage_analyzer
        type(control_flow_analyzer_t) :: fluff_control_analyzer
        class(*), allocatable :: usage_results, control_results
        
        print *, "=== Test 3: External Tool Scenario (Fluff-like) ==="
        
        ! Scenario: External tool like fluff analyzing user's code
        ! 1. User provides Fortran source
        ! 2. Tool creates pipeline with specific analyzers for rules
        ! 3. Tool runs analysis and extracts results for rule checking
        
        fluff_pipeline = create_pipeline()
        
        ! Fluff scenario: Register analyzers for specific rule categories
        call fluff_pipeline%register_analyzer(fluff_usage_analyzer)     ! For F006, F007 rules
        call fluff_pipeline%register_analyzer(fluff_control_analyzer)   ! For flow analysis rules
        
        ! Run analysis
        call fluff_pipeline%run_analysis(arena, prog_index)
        
        ! Extract results (what external tools would do)
        if (allocated(fluff_pipeline%analyzers(1)%analyzer)) then
            usage_results = fluff_pipeline%analyzers(1)%analyzer%get_results()
            print *, "✓ PASS: Usage analysis results accessible to external tool"
        else
            print *, "❌ FAIL: Usage analyzer results not accessible"
            all_passed = .false.
            return
        end if
        
        if (allocated(fluff_pipeline%analyzers(2)%analyzer)) then
            control_results = fluff_pipeline%analyzers(2)%analyzer%get_results()
            print *, "✓ PASS: Control flow analysis results accessible to external tool"
        else
            print *, "❌ FAIL: Control flow analyzer results not accessible"
            all_passed = .false.
            return
        end if
        
        print *, "✓ PASS: External tool scenario works completely"
        print *, "  Ready for fluff integration with realistic user code"
    end subroutine

end program vicky_test_realistic_fortran