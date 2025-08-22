program test_error_stop_conversion
    ! Test that converted error_stop instances return structured results
    use error_handling
    use ast_core
    use ast_factory_safe
    use ast_factory, only: push_literal
    use ast_types, only: LITERAL_LOGICAL
    implicit none
    
    ! Test safe_push_where function returns proper results instead of error_stop
    call test_safe_push_where_validation()
    
    print *, "All error_stop conversion tests passed!"
    
contains

    subroutine test_safe_push_where_validation()
        type(ast_arena_t) :: arena
        type(factory_result_t) :: result
        integer :: invalid_index = -1
        integer :: literal_index
        
        ! Initialize arena for testing
        call init_ast_arena(arena)
        
        ! Test 1: Invalid mask expression index should return error result, not error_stop
        result = safe_push_where(arena, invalid_index)
        
        if (result%is_success()) then
            print *, "Expected validation error but got success"
            stop 1
        end if
        
        ! Verify we get a structured error message
        if (len_trim(result%get_error()) == 0) then
            print *, "Expected error message but got empty string"
            stop 1
        end if
        
        print *, "✓ safe_push_where returns structured errors instead of error_stop"
        
        ! Test 2: Valid operation should succeed
        ! First add a valid node to reference
        literal_index = push_literal(arena, "true", LITERAL_LOGICAL)
        if (literal_index <= 0) then
            print *, "Failed to add literal node for testing"
            stop 1
        end if
        
        ! Now test with valid index
        result = safe_push_where(arena, literal_index)
        
        if (.not. result%is_success()) then
            print *, "Unexpected error: ", result%get_error()
            print *, "Expected success but got error"
            stop 1
        end if
        
        if (result%get_index() <= 0) then
            print *, "Expected valid node index but got <= 0"
            stop 1
        end if
        
        print *, "✓ safe_push_where succeeds with valid inputs"
        
    end subroutine test_safe_push_where_validation
    
end program test_error_stop_conversion