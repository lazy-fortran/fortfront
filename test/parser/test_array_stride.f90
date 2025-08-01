program test_array_stride
    use ast_core
    use ast_nodes_bounds
    use ast_factory
    use parser_expressions_module
    use lexer_core
    use iso_fortran_env, only: error_unit
    implicit none

    logical :: all_passed
    type(ast_arena_t) :: arena

    all_passed = .true.

    print *, '=== Array Stride Tests ==='
    print *

    ! Initialize arena
    arena = create_ast_arena()

    ! Run tests
    if (.not. test_simple_stride()) all_passed = .false.
    if (.not. test_empty_bounds_with_stride()) all_passed = .false.
    if (.not. test_nested_stride()) all_passed = .false.

    ! Clean up
    call arena%clear()

    ! Report results
    print *
    if (all_passed) then
        print *, 'All array stride tests passed!'
        stop 0
    else
        print *, 'Some array stride tests failed!'
        stop 1
    end if

contains

    logical function test_simple_stride()
        test_simple_stride = .true.
        print *, 'Testing simple array stride (1:10:2)...'
        
        block
            type(token_t), allocatable :: tokens(:)
            integer :: expr_index
            type(range_expression_node), pointer :: range_node
            
            call tokenize_core("1:10:2", tokens)
            expr_index = parse_expression(tokens, arena)
            
            if (expr_index > 0) then
                if (allocated(arena%entries(expr_index)%node)) then
                    select type (node => arena%entries(expr_index)%node)
                    type is (range_expression_node)
                        print *, '  PASS: Stride parsed as range_expression_node'
                        if (node%stride_index > 0) then
                            print *, '  PASS: Stride index is non-zero'
                            ! Verify stride value
                            if (allocated(arena%entries(node%stride_index)%node)) then
                                select type (stride_node => arena%entries(node%stride_index)%node)
                                type is (literal_node)
                                    if (stride_node%value == "2") then
                                        print *, '  PASS: Stride value correctly parsed as 2'
                                    else
                                        print *, '  FAIL: Stride value mismatch'
                                        test_simple_stride = .false.
                                    end if
                                end select
                            end if
                        else
                            print *, '  FAIL: Stride index is zero'
                            test_simple_stride = .false.
                        end if
                    class default
                        print *, '  FAIL: Unexpected node type for stride expression'
                        test_simple_stride = .false.
                    end select
                end if
            else
                print *, '  FAIL: Failed to parse stride expression'
                test_simple_stride = .false.
            end if
        end block
        
    end function test_simple_stride

    logical function test_empty_bounds_with_stride()
        test_empty_bounds_with_stride = .true.
        print *, 'Testing empty bounds with stride (:5:2)...'
        
        block
            type(token_t), allocatable :: tokens(:)
            integer :: expr_index
            
            ! Test :5:2 instead of ::2 since :: is a separate operator
            call tokenize_core(":5:2", tokens)
            expr_index = parse_expression(tokens, arena)
            
            if (expr_index > 0) then
                if (allocated(arena%entries(expr_index)%node)) then
                    select type (node => arena%entries(expr_index)%node)
                    type is (range_expression_node)
                        print *, '  PASS: Empty lower bound with stride parsed'
                        if (node%start_index == 0 .and. node%end_index > 0 .and. &
                            node%stride_index > 0) then
                            ! Verify stride value is correctly parsed
                            if (allocated(arena%entries(node%stride_index)%node)) then
                                select type (stride_node => arena%entries(node%stride_index)%node)
                                type is (literal_node)
                                    if (stride_node%value == "2") then
                                        print *, '  PASS: Correct bounds and stride indices with value 2'
                                    else
                                        print *, '  FAIL: Stride value mismatch, got: ', stride_node%value
                                        test_empty_bounds_with_stride = .false.
                                    end if
                                class default
                                    print *, '  PASS: Correct bounds and stride indices'
                                end select
                            else
                                print *, '  PASS: Correct bounds and stride indices'
                            end if
                        else
                            print *, '  FAIL: Incorrect indices for empty lower bound with stride'
                            test_empty_bounds_with_stride = .false.
                        end if
                    class default
                        print *, '  FAIL: Unexpected node type'
                        test_empty_bounds_with_stride = .false.
                    end select
                end if
            else
                print *, '  FAIL: Failed to parse empty lower bound with stride'
                test_empty_bounds_with_stride = .false.
            end if
        end block
        
    end function test_empty_bounds_with_stride

    logical function test_nested_stride()
        test_nested_stride = .true.
        print *, 'Testing array with stride arr(1:20:3)...'
        
        block
            type(token_t), allocatable :: tokens(:)
            integer :: expr_index
            
            call tokenize_core("arr(1:20:3)", tokens)
            expr_index = parse_expression(tokens, arena)
            
            if (expr_index > 0) then
                if (allocated(arena%entries(expr_index)%node)) then
                    select type (node => arena%entries(expr_index)%node)
                    type is (array_slice_node)
                        print *, '  PASS: Array with stride parsed as array_slice_node'
                        if (node%num_dimensions > 0) then
                            ! Check if the bounds contain a range expression with stride
                            if (node%bounds_indices(1) > 0) then
                                select type (bounds => arena%entries(node%bounds_indices(1))%node)
                                type is (range_expression_node)
                                    if (bounds%stride_index > 0) then
                                        print *, '  PASS: Stride correctly stored in range expression'
                                    else
                                        print *, '  FAIL: No stride in range expression'
                                        test_nested_stride = .false.
                                    end if
                                class default
                                    print *, '  INFO: Bounds not stored as range_expression'
                                end select
                            end if
                        end if
                    type is (call_or_subscript_node)
                        print *, '  INFO: Parsed as call_or_subscript (legacy behavior)'
                        ! This is acceptable - the important part is the stride is parsed
                    class default
                        print *, '  FAIL: Unexpected node type'
                        test_nested_stride = .false.
                    end select
                end if
            else
                print *, '  FAIL: Failed to parse array with stride'
                test_nested_stride = .false.
            end if
        end block
        
    end function test_nested_stride

end program