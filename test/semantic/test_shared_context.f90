program test_shared_context
    use semantic_pipeline, only: shared_context_t, create_shared_context
    implicit none
    
    logical :: tests_passed = .true.
    
    ! Test shared context functionality
    call test_context_storage()
    call test_context_retrieval()
    call test_context_has_result()
    
    if (tests_passed) then
        print *, "TEST PASSED: shared context system"
    else
        print *, "TEST FAILED: shared context system"
        stop 1
    end if

contains

    subroutine test_context_storage()
        type(shared_context_t) :: context
        integer :: test_data
        
        context = create_shared_context()
        test_data = 42
        
        ! Store a result
        call context%store_result("test_analyzer", test_data)
        
        ! Check that it was stored
        if (.not. context%has_result("test_analyzer")) then
            print *, "ERROR: Result not stored correctly"
            tests_passed = .false.
            return
        end if
        
        print *, "SUCCESS: Context storage works"
    end subroutine

    subroutine test_context_retrieval()
        type(shared_context_t) :: context
        integer :: test_data, retrieved_data
        class(*), allocatable :: result
        
        context = create_shared_context()
        test_data = 123
        
        ! Store and retrieve
        call context%store_result("retrieval_test", test_data)
        result = context%get_result("retrieval_test")
        
        if (.not. allocated(result)) then
            print *, "ERROR: Result not retrieved"
            tests_passed = .false.
            return
        end if
        
        ! Check that we got the right data
        select type(result)
        type is (integer)
            retrieved_data = result
            if (retrieved_data /= 123) then
                print *, "ERROR: Retrieved wrong data"
                tests_passed = .false.
                return
            end if
        class default
            print *, "ERROR: Retrieved wrong type"
            tests_passed = .false.
            return
        end select
        
        print *, "SUCCESS: Context retrieval works"
    end subroutine

    subroutine test_context_has_result()
        type(shared_context_t) :: context
        logical :: test_data
        
        context = create_shared_context()
        test_data = .true.
        
        ! Test has_result for non-existent result
        if (context%has_result("nonexistent")) then
            print *, "ERROR: Should not have nonexistent result"
            tests_passed = .false.
            return
        end if
        
        ! Store and test has_result
        call context%store_result("exists", test_data)
        if (.not. context%has_result("exists")) then
            print *, "ERROR: Should have existing result"
            tests_passed = .false.
            return
        end if
        
        print *, "SUCCESS: has_result works correctly"
    end subroutine

end program test_shared_context