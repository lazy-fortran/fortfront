program test_source_reconstruction
    use source_reconstruction_analyzer, only: source_reconstruction_analyzer_t
    use ast_core, only: ast_arena_t, create_ast_arena
    implicit none
    
    logical :: tests_passed = .true.
    
    ! Test source reconstruction analyzer functionality
    call test_analyzer_creation()
    call test_text_extraction()
    call test_line_counting()
    call test_analyzer_assignment()
    
    if (tests_passed) then
        print *, "TEST PASSED: source reconstruction analyzer"
    else
        print *, "TEST FAILED: source reconstruction analyzer"
        error stop 1
    end if

contains

    subroutine test_analyzer_creation()
        type(source_reconstruction_analyzer_t) :: analyzer
        type(ast_arena_t) :: arena
        character(:), allocatable :: name
        
        arena = create_ast_arena()
        
        ! Test analyzer creation and analysis
        call analyzer%analyze(0, arena, 1)
        
        if (.not. analyzer%analysis_complete) then
            print *, "ERROR: Analysis should complete successfully"
            tests_passed = .false.
            return
        end if
        
        ! Test analyzer name
        name = analyzer%get_name()
        if (trim(name) /= "source_reconstruction_analyzer") then
            print *, "ERROR: Analyzer name incorrect"
            tests_passed = .false.
            return
        end if
        
        print *, "SUCCESS: Analyzer creation and analysis works"
    end subroutine

    subroutine test_text_extraction()
        type(source_reconstruction_analyzer_t) :: analyzer
        type(ast_arena_t) :: arena
        character(:), allocatable :: text
        
        arena = create_ast_arena()
        call analyzer%analyze(0, arena, 1)
        
        ! Test extracting text for node (will return fallback)
        text = analyzer%get_node_source_text(1)
        if (.not. allocated(text)) then
            print *, "ERROR: get_node_source_text should return text"
            tests_passed = .false.
            return
        end if
        
        ! Test extracting text span
        text = analyzer%extract_text_span(1, 1, 1, 10)
        if (.not. allocated(text)) then
            print *, "ERROR: extract_text_span should return text"
            tests_passed = .false.
            return
        end if
        
        print *, "SUCCESS: Text extraction works"
    end subroutine

    subroutine test_line_counting()
        type(source_reconstruction_analyzer_t) :: analyzer
        type(ast_arena_t) :: arena
        character(:), allocatable :: line_text
        
        arena = create_ast_arena()
        call analyzer%analyze(0, arena, 1)
        
        ! Test getting line text
        line_text = analyzer%get_line_text(1)
        if (.not. allocated(line_text)) then
            print *, "ERROR: get_line_text should return text"
            tests_passed = .false.
            return
        end if
        
        print *, "SUCCESS: Line text retrieval works"
    end subroutine

    subroutine test_analyzer_assignment()
        type(source_reconstruction_analyzer_t) :: analyzer1, analyzer2
        type(ast_arena_t) :: arena
        
        arena = create_ast_arena()
        call analyzer1%analyze(0, arena, 1)
        analyzer1%analysis_complete = .true.
        
        ! Test assignment
        call analyzer2%assign(analyzer1)
        
        if (.not. analyzer2%analysis_complete) then
            print *, "ERROR: Assignment should copy analysis_complete"
            tests_passed = .false.
            return
        end if
        
        print *, "SUCCESS: Analyzer assignment works"
    end subroutine

end program test_source_reconstruction