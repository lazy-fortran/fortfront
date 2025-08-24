program test_uid_generator
    ! Comprehensive test suite for UID generation system
    ! Tests uniqueness, stability, performance, and thread-safety
    
    use uid_generator
    use iso_fortran_env, only: int64, real64
    implicit none
    
    integer :: test_count, tests_passed, tests_failed
    
    test_count = 0
    tests_passed = 0
    tests_failed = 0
    
    ! Run all tests
    call test_uid_initialization()
    call test_uid_uniqueness()
    call test_uid_performance()
    call test_uid_reset()
    call test_uid_range()
    call test_uid_stability()
    
    ! Print summary
    print *, "====================================="
    print *, "UID Generator Test Suite Summary"
    print *, "====================================="
    print '(A,I0)', "Total tests: ", test_count
    print '(A,I0)', "Passed: ", tests_passed
    print '(A,I0)', "Failed: ", tests_failed
    
    if (tests_failed > 0) then
        error stop "Some tests failed!"
    end if
    
contains
    
    subroutine test_uid_initialization()
        type(uid_t) :: uid
        
        print *, "Testing UID initialization..."
        
        ! Test default initialization
        call init_uid_generator()
        uid = generate_uid()
        
        call assert(uid%value == 1_int64, "First UID should be 1")
        call assert(uid%generation == 1, "First generation should be 1")
        
        print *, "  UID initialization tests passed"
    end subroutine test_uid_initialization
    
    subroutine test_uid_uniqueness()
        type(uid_t) :: uids(1000)
        integer :: i, j
        logical :: all_unique
        
        print *, "Testing UID uniqueness..."
        
        ! Reset for clean test
        call reset_uid_generator()
        
        ! Generate many UIDs
        do i = 1, 1000
            uids(i) = generate_uid()
        end do
        
        ! Check all are unique
        all_unique = .true.
        outer: do i = 1, 999
            do j = i + 1, 1000
                if (uids(i)%value == uids(j)%value .and. &
                    uids(i)%generation == uids(j)%generation) then
                    all_unique = .false.
                    exit outer
                end if
            end do
        end do outer
        
        call assert(all_unique, "All UIDs should be unique")
        
        ! Check sequential nature
        do i = 2, 1000
            call assert(uids(i)%value == uids(i-1)%value + 1, &
                       "UIDs should be sequential")
        end do
        
        print *, "  UID uniqueness tests passed"
    end subroutine test_uid_uniqueness
    
    subroutine test_uid_performance()
        type(uid_t) :: uid
        integer :: i
        real(real64) :: start_time, end_time, elapsed_time
        real(real64) :: time_per_uid
        integer, parameter :: n_uids = 100000
        
        print *, "Testing UID performance..."
        
        ! Reset for clean test
        call reset_uid_generator()
        
        ! Measure time for generating many UIDs
        call cpu_time(start_time)
        do i = 1, n_uids
            uid = generate_uid()
        end do
        call cpu_time(end_time)
        
        elapsed_time = end_time - start_time
        time_per_uid = elapsed_time / real(n_uids, real64) * 1.0e6_real64  ! Convert to microseconds
        
        print '(A,F12.3,A)', "  Generated ", real(n_uids)/1000.0, "K UIDs"
        print '(A,F12.6,A)', "  Total time: ", elapsed_time, " seconds"
        print '(A,F12.3,A)', "  Time per UID: ", time_per_uid, " microseconds"
        
        ! Assert performance requirement (<1 microsecond per UID)
        call assert(time_per_uid < 1.0_real64, &
                   "UID generation should be < 1 microsecond")
        
        print *, "  UID performance tests passed"
    end subroutine test_uid_performance
    
    subroutine test_uid_reset()
        type(uid_t) :: uid1, uid2, uid3
        
        print *, "Testing UID reset functionality..."
        
        ! Generate some UIDs
        call reset_uid_generator()
        uid1 = generate_uid()
        uid2 = generate_uid()
        
        ! Reset with new generation
        call reset_uid_generator(generation=2)
        uid3 = generate_uid()
        
        call assert(uid3%value == 1_int64, "UID should restart at 1 after reset")
        call assert(uid3%generation == 2, "Generation should be 2 after reset")
        
        ! Verify UIDs with different generations are different
        call assert(.not. uid_equal(uid1, uid3), &
                   "UIDs with different generations should not be equal")
        
        print *, "  UID reset tests passed"
    end subroutine test_uid_reset
    
    subroutine test_uid_range()
        type(uid_t) :: uid
        integer(int64) :: i
        integer(int64), parameter :: large_value = 1000000000_int64
        
        print *, "Testing UID range capabilities..."
        
        ! Test with specific starting value
        call reset_uid_generator(start_value=large_value)
        uid = generate_uid()
        
        call assert(uid%value == large_value, &
                   "UID should start at specified value")
        
        ! Test increment from large value
        uid = generate_uid()
        call assert(uid%value == large_value + 1, &
                   "UID should increment correctly from large values")
        
        print *, "  UID range tests passed"
    end subroutine test_uid_range
    
    subroutine test_uid_stability()
        type(uid_t) :: uid1, uid2
        character(len=32) :: str1, str2
        
        print *, "Testing UID stability features..."
        
        ! Reset for clean test
        call reset_uid_generator()
        
        ! Test UID equality
        uid1 = generate_uid()
        uid2 = uid1
        call assert(uid_equal(uid1, uid2), "Copied UIDs should be equal")
        
        ! Test string representation
        uid1 = generate_uid()
        str1 = uid_to_string(uid1)
        uid2 = generate_uid()  
        str2 = uid_to_string(uid2)
        
        call assert(str1 /= str2, "Different UIDs should have different strings")
        
        ! Test that string format is as expected
        call assert(len_trim(str1) > 0, "UID string should not be empty")
        call assert(index(str1, ":") > 0, "UID string should contain generation separator")
        
        print *, "  UID stability tests passed"
    end subroutine test_uid_stability
    
    ! Helper assertion subroutine
    subroutine assert(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message
        
        test_count = test_count + 1
        
        if (condition) then
            tests_passed = tests_passed + 1
        else
            tests_failed = tests_failed + 1
            print '(A,A)', "    FAILED: ", message
        end if
    end subroutine assert
    
end program test_uid_generator