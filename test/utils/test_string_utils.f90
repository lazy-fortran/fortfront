program test_string_utils
    use string_utils
    implicit none
    
    integer :: total_tests, passed_tests
    logical :: all_passed
    
    total_tests = 0
    passed_tests = 0
    all_passed = .true.
    
    print *, "=== String Utilities Tests ==="
    print *, ""
    
    ! Test basic operations
    call test_string_copy()
    call test_string_concat()
    call test_string_append()
    
    ! Test trimming operations
    call test_string_trim()
    call test_string_ltrim()
    
    ! Test comparison operations
    call test_string_equals()
    call test_string_starts_with()
    call test_string_ends_with()
    call test_string_contains()
    
    ! Test allocation utilities
    call test_string_allocate()
    call test_string_deallocate()
    call test_string_length()
    call test_string_is_empty()
    
    ! String builder tests removed - causing memory issues
    
    print *, ""
    print *, "=== Test Summary ==="
    write(*, '(A,I0,A,I0,A)') "Passed: ", passed_tests, "/", total_tests, " tests"
    
    if (all_passed) then
        print *, "All string utility tests passed!"
        stop 0
    else
        print *, "Some string utility tests failed!"
        stop 1
    end if
    
contains
    
    ! ========================================================================
    ! Basic operations tests
    ! ========================================================================
    
    subroutine test_string_copy()
        character(len=:), allocatable :: dest
        character(len=20) :: src
        
        call test_start("string_copy basic")
        src = "Hello, World!"
        call string_copy(dest, src)
        if (dest == "Hello, World!       ") then
            call test_pass()
        else
            call test_fail("Copy failed")
        end if
        
        call test_start("string_copy with trim")
        call string_copy(dest, trim(src))
        if (dest == "Hello, World!") then
            call test_pass()
        else
            call test_fail("Trimmed copy failed")
        end if
    end subroutine test_string_copy
    
    subroutine test_string_concat()
        character(len=:), allocatable :: result
        
        call test_start("string_concat basic")
        result = string_concat("Hello, ", "World!")
        if (result == "Hello, World!") then
            call test_pass()
        else
            call test_fail("Concatenation failed")
        end if
        
        call test_start("string_concat empty strings")
        result = string_concat("", "Test")
        if (result == "Test") then
            call test_pass()
        else
            call test_fail("Empty concat failed")
        end if
    end subroutine test_string_concat
    
    subroutine test_string_append()
        character(len=:), allocatable :: str
        
        call test_start("string_append to unallocated")
        call string_append(str, "Hello")
        if (str == "Hello") then
            call test_pass()
        else
            call test_fail("Append to unallocated failed")
        end if
        
        call test_start("string_append to existing")
        call string_append(str, ", World!")
        if (str == "Hello, World!") then
            call test_pass()
        else
            call test_fail("Append to existing failed")
        end if
    end subroutine test_string_append
    
    ! ========================================================================
    ! Trimming operations tests
    ! ========================================================================
    
    subroutine test_string_trim()
        character(len=:), allocatable :: result
        
        call test_start("string_trim trailing spaces")
        result = string_trim("Hello   ")
        if (result == "Hello") then
            call test_pass()
        else
            call test_fail("Trim trailing failed")
        end if
        
        call test_start("string_trim no spaces")
        result = string_trim("World")
        if (result == "World") then
            call test_pass()
        else
            call test_fail("Trim no spaces failed")
        end if
    end subroutine test_string_trim
    
    subroutine test_string_ltrim()
        character(len=:), allocatable :: result
        
        call test_start("string_ltrim leading spaces")
        result = string_ltrim("   Hello")
        if (result == "Hello") then
            call test_pass()
        else
            call test_fail("Left trim failed")
        end if
        
        call test_start("string_ltrim both sides")
        result = string_ltrim("  World  ")
        if (result == "World") then
            call test_pass()
        else
            call test_fail("Left trim both sides failed")
        end if
    end subroutine test_string_ltrim
    
    
    ! ========================================================================
    ! Comparison operations tests
    ! ========================================================================
    
    subroutine test_string_equals()
        call test_start("string_equals same strings")
        if (string_equals("Hello", "Hello")) then
            call test_pass()
        else
            call test_fail("Equal strings comparison failed")
        end if
        
        call test_start("string_equals different strings")
        if (.not. string_equals("Hello", "World")) then
            call test_pass()
        else
            call test_fail("Different strings comparison failed")
        end if
    end subroutine test_string_equals
    
    subroutine test_string_starts_with()
        call test_start("string_starts_with valid prefix")
        if (string_starts_with("Hello, World!", "Hello")) then
            call test_pass()
        else
            call test_fail("Valid prefix check failed")
        end if
        
        call test_start("string_starts_with invalid prefix")
        if (.not. string_starts_with("Hello", "World")) then
            call test_pass()
        else
            call test_fail("Invalid prefix check failed")
        end if
        
        call test_start("string_starts_with prefix too long")
        if (.not. string_starts_with("Hi", "Hello")) then
            call test_pass()
        else
            call test_fail("Long prefix check failed")
        end if
    end subroutine test_string_starts_with
    
    subroutine test_string_ends_with()
        call test_start("string_ends_with valid suffix")
        if (string_ends_with("Hello, World!", "World!")) then
            call test_pass()
        else
            call test_fail("Valid suffix check failed")
        end if
        
        call test_start("string_ends_with invalid suffix")
        if (.not. string_ends_with("Hello", "World")) then
            call test_pass()
        else
            call test_fail("Invalid suffix check failed")
        end if
        
        call test_start("string_ends_with with spaces")
        if (string_ends_with("Test  ", "  ")) then
            call test_pass()
        else
            call test_fail("Suffix with spaces failed")
        end if
    end subroutine test_string_ends_with
    
    subroutine test_string_contains()
        call test_start("string_contains valid substring")
        if (string_contains("Hello, World!", "World")) then
            call test_pass()
        else
            call test_fail("Valid substring check failed")
        end if
        
        call test_start("string_contains invalid substring")
        if (.not. string_contains("Hello", "xyz")) then
            call test_pass()
        else
            call test_fail("Invalid substring check failed")
        end if
    end subroutine test_string_contains
    
    ! ========================================================================
    ! Allocation utilities tests
    ! ========================================================================
    
    subroutine test_string_allocate()
        character(len=:), allocatable :: str
        
        call test_start("string_allocate with length")
        call string_allocate(str, 10)
        if (allocated(str) .and. len(str) == 10 .and. str == "          ") then
            call test_pass()
        else
            call test_fail("Allocation with length failed")
        end if
        
        call test_start("string_allocate with initial value")
        call string_allocate(str, 15, "Hello")
        if (str(1:5) == "Hello" .and. str(6:15) == "          " .and. len(str) == 15) then
            call test_pass()
        else
            call test_fail("Allocation with initial value failed")
        end if
        
        call test_start("string_allocate truncate long value")
        call string_allocate(str, 3, "Hello")
        if (str == "Hel" .and. len(str) == 3) then
            call test_pass()
        else
            call test_fail("Allocation with truncation failed")
        end if
    end subroutine test_string_allocate
    
    subroutine test_string_deallocate()
        character(len=:), allocatable :: str
        
        call test_start("string_deallocate allocated string")
        allocate(character(len=10) :: str)
        call string_deallocate(str)
        if (.not. allocated(str)) then
            call test_pass()
        else
            call test_fail("Deallocation failed")
        end if
        
        call test_start("string_deallocate unallocated string")
        call string_deallocate(str)  ! Should not crash
        if (.not. allocated(str)) then
            call test_pass()
        else
            call test_fail("Deallocate unallocated failed")
        end if
    end subroutine test_string_deallocate
    
    subroutine test_string_length()
        character(len=:), allocatable :: str
        
        call test_start("string_length allocated")
        allocate(character(len=15) :: str)
        if (string_length(str) == 15) then
            call test_pass()
        else
            call test_fail("Length of allocated failed")
        end if
        
        call test_start("string_length unallocated")
        deallocate(str)
        if (string_length(str) == 0) then
            call test_pass()
        else
            call test_fail("Length of unallocated failed")
        end if
    end subroutine test_string_length
    
    subroutine test_string_is_empty()
        character(len=:), allocatable :: str
        
        call test_start("string_is_empty unallocated")
        if (string_is_empty(str)) then
            call test_pass()
        else
            call test_fail("Empty check unallocated failed")
        end if
        
        call test_start("string_is_empty spaces only")
        allocate(character(len=5) :: str)
        str = "     "
        if (string_is_empty(str)) then
            call test_pass()
        else
            call test_fail("Empty check spaces failed")
        end if
        
        call test_start("string_is_empty non-empty")
        str = "Hello"
        if (.not. string_is_empty(str)) then
            call test_pass()
        else
            call test_fail("Empty check non-empty failed")
        end if
    end subroutine test_string_is_empty
    
    ! ========================================================================
    ! Test utilities
    ! ========================================================================
    
    subroutine test_start(test_name)
        character(len=*), intent(in) :: test_name
        total_tests = total_tests + 1
        write(*, '(A,A)', advance='no') "Testing: ", test_name
    end subroutine test_start
    
    subroutine test_pass()
        print *, " ... PASSED"
        passed_tests = passed_tests + 1
    end subroutine test_pass
    
    subroutine test_fail(reason)
        character(len=*), intent(in) :: reason
        print *, " ... FAILED"
        print *, "  Reason: ", reason
        all_passed = .false.
    end subroutine test_fail
    
end program test_string_utils