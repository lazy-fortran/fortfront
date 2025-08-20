program test_source_reconstruction_integration
    use source_reconstruction_analyzer, only: source_reconstruction_analyzer_t
    use ast_core, only: ast_arena_t, create_ast_arena, &
                        create_identifier, identifier_node, &
                        create_literal, literal_node, LITERAL_INTEGER
    implicit none

    integer :: total_tests, passed_tests

    total_tests = 0
    passed_tests = 0

    print *, "=== Source Reconstruction Integration Tests ==="
    print *, ""

    ! Test 1: Full source reconstruction from arena
    call test_start("Full source reconstruction from arena")
    block
        type(source_reconstruction_analyzer_t) :: analyzer
        type(ast_arena_t) :: arena
        type(identifier_node) :: id_node
        type(literal_node) :: lit_node
        character(:), allocatable :: reconstructed_text
        integer :: node_index
        
        ! Create arena with some nodes
        arena = create_ast_arena()
        
        ! Add identifier node at line 1
        id_node = create_identifier("my_var", line=1, column=1)
        call arena%push(id_node, "identifier")
        if (arena%current_index <= 0) then
            print *, "ERROR: Failed to push identifier to arena"
            stop 1
        end if
        
        ! Add literal node at line 2
        lit_node = create_literal("42", LITERAL_INTEGER, line=2, column=1)
        call arena%push(lit_node, "literal")
        
        ! Analyze with the arena
        call analyzer%analyze(arena, arena, 1)
        
        ! Test that we can get node source text
        reconstructed_text = analyzer%get_node_source_text(1)
        
        if (len_trim(reconstructed_text) > 0 .and. &
            reconstructed_text /= "<source not available>") then
            call test_pass()
        else
            call test_fail()
            print *, "  Expected: non-empty source text"
            print *, "  Got: '", reconstructed_text, "'"
        end if
    end block

    ! Test 2: Context around node functionality
    call test_start("Context around node functionality")
    block
        type(source_reconstruction_analyzer_t) :: analyzer
        type(ast_arena_t) :: arena
        type(identifier_node) :: id_node
        character(:), allocatable :: context_text
        
        ! Create arena with a node
        arena = create_ast_arena()
        id_node = create_identifier("test_var", line=2, column=5)
        call arena%push(id_node, "identifier")
        if (arena%current_index <= 0) then
            print *, "ERROR: Failed to push identifier to arena"
            stop 1
        end if
        
        ! Analyze with the arena
        call analyzer%analyze(arena, arena, 1)
        
        ! Get context around the node
        context_text = analyzer%get_context_around_node(1, 1)
        
        if (len_trim(context_text) > 0) then
            call test_pass()
        else
            call test_fail()
            print *, "  Expected: non-empty context"
            print *, "  Got: '", context_text, "'"
        end if
    end block

    ! Test 3: Format source location
    call test_start("Format source location")
    block
        type(source_reconstruction_analyzer_t) :: analyzer
        type(ast_arena_t) :: arena
        type(identifier_node) :: id_node
        character(:), allocatable :: location_str
        
        ! Create arena with a node at specific location
        arena = create_ast_arena()
        id_node = create_identifier("var", line=5, column=10)
        call arena%push(id_node, "identifier")
        if (arena%current_index <= 0) then
            print *, "ERROR: Failed to push identifier to arena"
            stop 1
        end if
        
        ! Analyze with the arena
        call analyzer%analyze(arena, arena, 1)
        
        ! Format the location
        location_str = analyzer%format_source_location(1)
        
        if (location_str == "5:10") then
            call test_pass()
        else
            call test_fail()
            print *, "  Expected: '5:10'"
            print *, "  Got: '", location_str, "'"
        end if
    end block

    ! Test 4: Invalid node handling
    call test_start("Invalid node handling")
    block
        type(source_reconstruction_analyzer_t) :: analyzer
        type(ast_arena_t) :: arena
        character(:), allocatable :: result
        
        ! Create empty arena
        arena = create_ast_arena()
        
        ! Analyze with empty arena
        call analyzer%analyze(arena, arena, 1)
        
        ! Try to get text for non-existent node
        result = analyzer%get_node_source_text(999)
        
        if (result == "<source not available>") then
            call test_pass()
        else
            call test_fail()
            print *, "  Expected: '<source not available>'"
            print *, "  Got: '", result, "'"
        end if
    end block

    ! Test 5: Extract text span edge cases
    call test_start("Extract text span edge cases")
    block
        type(source_reconstruction_analyzer_t) :: analyzer
        character(:), allocatable :: extracted
        
        ! Test with uninitialized analyzer
        extracted = analyzer%extract_text_span(1, 1, 1, 5)
        
        if (extracted == "") then
            call test_pass()
        else
            call test_fail()
            print *, "  Expected: empty string for uninitialized analyzer"
            print *, "  Got: '", extracted, "'"
        end if
    end block

    print *, ""
    print *, "=== Test Summary ==="
    write(*, '(A,I0,A,I0,A)') "Passed: ", passed_tests, "/", total_tests, " tests"

    if (passed_tests == total_tests) then
        print *, "All source reconstruction integration tests passed!"
        stop 0
    else
        print *, "Some source reconstruction integration tests failed!"
        stop 1
    end if

contains

    subroutine test_start(test_name)
        character(len=*), intent(in) :: test_name
        total_tests = total_tests + 1
        write(*, '(A,A)', advance='no') "Testing: ", test_name
    end subroutine test_start

    subroutine test_pass()
        print *, " ... PASSED"
        passed_tests = passed_tests + 1
    end subroutine test_pass

    subroutine test_fail()
        print *, " ... FAILED"
    end subroutine test_fail

end program test_source_reconstruction_integration