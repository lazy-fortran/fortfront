program test_range_subscript_errors
    use ast_core
    use ast_factory, only: push_range_subscript
    use codegen_core, only: generate_code_from_arena
    implicit none
    
    logical :: all_passed
    all_passed = .true.
    
    call test_invalid_base_index(all_passed)
    call test_out_of_bounds_base(all_passed)
    call test_empty_arena(all_passed)
    
    if (all_passed) then
        print *, "All range_subscript error tests passed!"
    else
        print *, "Some range_subscript error tests FAILED!"
        stop 1
    end if
    
contains

    subroutine test_invalid_base_index(all_passed)
        logical, intent(inout) :: all_passed
        type(ast_arena_t) :: arena
        integer :: node_idx
        character(len=:), allocatable :: code
        logical :: test_passed
        
        test_passed = .true.
        arena = create_ast_arena()
        
        ! Test with negative base index
        node_idx = push_range_subscript(arena, -1, 1, 2, 1, 1)
        
        if (node_idx > 0) then
            code = generate_code_from_arena(arena, node_idx)
            if (index(code, "ERROR") > 0 .or. index(code, "error") > 0) then
                print *, "PASS: Invalid base index creates error node"
            else
                print *, "FAIL: Invalid base index should create error node"
                print *, "  Got: ", code
                test_passed = .false.
                all_passed = .false.
            end if
        else
            print *, "FAIL: push_range_subscript returned invalid index"
            test_passed = .false.
            all_passed = .false.
        end if
    end subroutine test_invalid_base_index
    
    subroutine test_out_of_bounds_base(all_passed)
        logical, intent(inout) :: all_passed
        type(ast_arena_t) :: arena
        integer :: node_idx
        character(len=:), allocatable :: code
        logical :: test_passed
        
        test_passed = .true.
        arena = create_ast_arena()
        
        ! Test with base index beyond arena size
        node_idx = push_range_subscript(arena, 9999, 1, 2, 1, 1)
        
        if (node_idx > 0) then
            code = generate_code_from_arena(arena, node_idx)
            if (index(code, "ERROR") > 0 .or. index(code, "error") > 0) then
                print *, "PASS: Out-of-bounds base index creates error node"
            else
                print *, "FAIL: Out-of-bounds base should create error node"
                print *, "  Got: ", code
                test_passed = .false.
                all_passed = .false.
            end if
        else
            print *, "FAIL: push_range_subscript returned invalid index"
            test_passed = .false.
            all_passed = .false.
        end if
    end subroutine test_out_of_bounds_base
    
    subroutine test_empty_arena(all_passed)
        logical, intent(inout) :: all_passed
        type(ast_arena_t) :: arena
        integer :: node_idx
        character(len=:), allocatable :: code
        
        arena = create_ast_arena()
        
        ! Test with index 0 (invalid)
        node_idx = push_range_subscript(arena, 0, 1, 2, 1, 1)
        
        if (node_idx > 0) then
            code = generate_code_from_arena(arena, node_idx)
            if (index(code, "ERROR") > 0 .or. index(code, "error") > 0) then
                print *, "PASS: Zero base index creates error node"
            else
                print *, "FAIL: Zero base index should create error node"
                all_passed = .false.
            end if
        else
            print *, "FAIL: push_range_subscript should handle zero index"
            all_passed = .false.
        end if
    end subroutine test_empty_arena

end program test_range_subscript_errors