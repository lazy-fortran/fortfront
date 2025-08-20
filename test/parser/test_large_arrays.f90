program test_large_arrays
    use ast_core, only: ast_arena_t, create_ast_arena, array_literal_node
    use parser_expressions_module
    use lexer_core
    implicit none

    logical :: all_passed
    type(ast_arena_t) :: arena

    all_passed = .true.
    print *, '=== Large Arrays Tests ==='
    print *

    arena = create_ast_arena()

    if (.not. run_large_arrays_tests()) all_passed = .false.

    print *
    if (all_passed) then
        print *, 'All large array tests passed!'
        stop 0
    else
        print *, 'Some large array tests failed!'
        stop 1
    end if

contains

    logical function run_large_arrays_tests()
        integer :: expr_index
        character(len=1024) :: large_array_str
        integer :: i
        
        run_large_arrays_tests = .true.
        print *, 'Test 1: Large arrays (>100 elements)'
        
        ! Build a string with 105 elements
        large_array_str = "["
        do i = 1, 105
            if (i > 1) large_array_str = trim(large_array_str) // ", "
            write(large_array_str, '(A,I0)') trim(large_array_str), i
        end do
        large_array_str = trim(large_array_str) // "]"
        
        arena = create_ast_arena()
        block
            type(token_t), allocatable :: tokens(:)
            call tokenize_core(trim(large_array_str), tokens)
            expr_index = parse_expression(tokens, arena)
            
            if (expr_index == 0) then
                print *, '  FAIL: Failed to parse large array'
                run_large_arrays_tests = .false.
            else
                select type (node => arena%entries(expr_index)%node)
                class is (array_literal_node)
                    if (size(node%element_indices) == 105) then
                        print *, '  OK: Large array with 105 elements parsed'
                    else
                        print *, '  FAIL: Expected 105 elements, got', size(node%element_indices)
                        run_large_arrays_tests = .false.
                    end if
                class default
                    print *, '  FAIL: Expected array literal node'
                    run_large_arrays_tests = .false.
                end select
            end if
        end block
        
        if (run_large_arrays_tests) then
            print *, 'PASS: Large arrays'
        end if
        
    end function run_large_arrays_tests

end program test_large_arrays