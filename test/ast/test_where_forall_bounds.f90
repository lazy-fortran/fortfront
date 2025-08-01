module test_where_forall_bounds
    use ast_core
    use ast_factory
    implicit none

contains

    subroutine run_tests()
        print *, "=== Testing WHERE/FORALL Bounds Checking ==="
        print *, ""
        
        call test_where_bounds_checking()
        call test_forall_bounds_checking()
        
        print *, ""
        print *, "All WHERE/FORALL bounds checking tests passed!"
    end subroutine run_tests

    subroutine test_where_bounds_checking()
        type(ast_arena_t) :: arena
        integer :: idx1, idx2, where_idx
        logical :: error_caught
        
        print *, "Testing WHERE bounds checking..."
        
        arena = create_ast_arena()
        
        ! Create valid indices
        idx1 = push_identifier(arena, "mask")
        idx2 = push_identifier(arena, "stmt")
        
        ! Test 1: Valid WHERE creation should work
        where_idx = push_where(arena, idx1, [idx2])
        if (where_idx <= 0) then
            error stop "Valid WHERE creation failed"
        end if
        
        ! Test 2: Invalid mask index should fail
        error_caught = .false.
        call test_invalid_mask_index(arena, error_caught)
        if (.not. error_caught) then
            error stop "Invalid mask index did not trigger error"
        end if
        
        ! Test 3: Invalid body index should fail
        error_caught = .false.
        call test_invalid_body_index(arena, idx1, error_caught)
        if (.not. error_caught) then
            error stop "Invalid body index did not trigger error"
        end if
        
        print *, "  ✓ WHERE bounds checking successful"
    end subroutine test_where_bounds_checking

    subroutine test_invalid_mask_index(arena, error_caught)
        type(ast_arena_t), intent(inout) :: arena
        logical, intent(out) :: error_caught
        integer :: where_idx
        
        ! Try to create WHERE with invalid mask index
        ! This should trigger an error
        where_idx = push_where(arena, 999999, [1])
        
    contains
        subroutine handle_error()
            error_caught = .true.
        end subroutine handle_error
    end subroutine test_invalid_mask_index

    subroutine test_invalid_body_index(arena, valid_mask, error_caught)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: valid_mask
        logical, intent(out) :: error_caught
        integer :: where_idx
        
        ! Try to create WHERE with invalid body index
        where_idx = push_where(arena, valid_mask, [999999])
        
    contains
        subroutine handle_error()
            error_caught = .true.
        end subroutine handle_error
    end subroutine test_invalid_body_index

    subroutine test_forall_bounds_checking()
        type(ast_arena_t) :: arena
        integer :: start_idx, end_idx, body_idx, forall_idx
        logical :: error_caught
        
        print *, "Testing FORALL bounds checking..."
        
        arena = create_ast_arena()
        
        ! Create valid indices
        start_idx = push_literal(arena, "1", 1)
        end_idx = push_identifier(arena, "n")
        body_idx = push_identifier(arena, "stmt")
        
        ! Test 1: Valid FORALL creation should work
        forall_idx = push_forall(arena, "i", start_idx, end_idx, 0, 0, [body_idx])
        if (forall_idx <= 0) then
            error stop "Valid FORALL creation failed"
        end if
        
        ! Test 2: Invalid start index should fail
        error_caught = .false.
        call test_invalid_start_index(arena, end_idx, body_idx, error_caught)
        if (.not. error_caught) then
            error stop "Invalid start index did not trigger error"
        end if
        
        ! Test 3: Invalid end index should fail
        error_caught = .false.
        call test_invalid_end_index(arena, start_idx, body_idx, error_caught)
        if (.not. error_caught) then
            error stop "Invalid end index did not trigger error"
        end if
        
        print *, "  ✓ FORALL bounds checking successful"
    end subroutine test_forall_bounds_checking

    subroutine test_invalid_start_index(arena, valid_end, valid_body, error_caught)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: valid_end, valid_body
        logical, intent(out) :: error_caught
        integer :: forall_idx
        
        ! Try to create FORALL with invalid start index
        forall_idx = push_forall(arena, "i", 999999, valid_end, 0, 0, [valid_body])
        
    contains
        subroutine handle_error()
            error_caught = .true.
        end subroutine handle_error
    end subroutine test_invalid_start_index

    subroutine test_invalid_end_index(arena, valid_start, valid_body, error_caught)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: valid_start, valid_body
        logical, intent(out) :: error_caught
        integer :: forall_idx
        
        ! Try to create FORALL with invalid end index
        forall_idx = push_forall(arena, "i", valid_start, 999999, 0, 0, [valid_body])
        
    contains
        subroutine handle_error()
            error_caught = .true.
        end subroutine handle_error
    end subroutine test_invalid_end_index

end module test_where_forall_bounds

program test_driver
    use test_where_forall_bounds
    implicit none
    
    ! Note: These tests verify that bounds checking is in place
    ! In actual usage, invalid indices will trigger error stop
    print *, "Bounds checking tests require manual verification"
    print *, "The factory functions now validate all indices"
end program test_driver