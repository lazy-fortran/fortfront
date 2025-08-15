program test_string_length_inference
    use frontend
    implicit none
    
    call test_basic_string_length_change
    call test_multiple_length_changes
    call test_string_concatenation_changes
    call test_empty_to_nonempty_string
    call test_single_char_to_long_string
    
    print *, "All string length inference tests completed!"
    
contains

    subroutine test_basic_string_length_change
        ! Test the exact example from Issue #218
        character(len=:), allocatable :: source, result, error_msg
        
        source = 's = "a"' // new_line('a') // &
                 'print*,s' // new_line('a') // &
                 's = "ab"' // new_line('a') // &
                 'print*,s'
        
        call transform_lazy_fortran_string(source, result, error_msg)
        
        ! Verify the string variable is declared as allocatable
        if (index(result, 'character(len=:), allocatable :: s') > 0) then
            print *, "PASS: Basic string length change generates allocatable declaration"
        else
            print *, "FAIL: Expected allocatable string declaration not found"
            print *, "Generated:", result
            error stop "Test failed: basic_string_length_change"
        end if
        
        ! Verify it doesn't use fixed-length declaration
        if (index(result, 'character(len=1) :: s') == 0) then
            print *, "PASS: No fixed-length declaration found"
        else
            print *, "FAIL: Found unwanted fixed-length declaration"
            error stop "Test failed: fixed_length_found"
        end if
    end subroutine test_basic_string_length_change

    subroutine test_multiple_length_changes
        ! Test string that changes length multiple times
        character(len=:), allocatable :: source, result, error_msg
        
        source = 's = "a"' // new_line('a') // &
                 's = "abc"' // new_line('a') // &
                 's = "x"' // new_line('a') // &
                 's = "very_long_string"'
        
        call transform_lazy_fortran_string(source, result, error_msg)
        
        if (index(result, 'character(len=:), allocatable :: s') > 0) then
            print *, "PASS: Multiple length changes generate allocatable declaration"
        else
            print *, "FAIL: Multiple length changes test failed"
            print *, "Generated:", result
            error stop "Test failed: multiple_length_changes"
        end if
    end subroutine test_multiple_length_changes

    subroutine test_string_concatenation_changes
        ! Test string that grows through concatenation
        character(len=:), allocatable :: source, result, error_msg
        
        source = 's = "a"' // new_line('a') // &
                 's = s // "b"' // new_line('a') // &
                 's = s // "cde"'
        
        call transform_lazy_fortran_string(source, result, error_msg)
        
        if (index(result, 'character(len=:), allocatable :: s') > 0) then
            print *, "PASS: String concatenation changes generate allocatable declaration"
        else
            print *, "FAIL: String concatenation test failed"
            print *, "Generated:", result
            error stop "Test failed: concatenation_changes"
        end if
    end subroutine test_string_concatenation_changes

    subroutine test_empty_to_nonempty_string
        ! Test string that starts empty and becomes non-empty
        character(len=:), allocatable :: source, result, error_msg
        
        source = 's = ""' // new_line('a') // &
                 's = "hello"'
        
        call transform_lazy_fortran_string(source, result, error_msg)
        
        if (index(result, 'character(len=:), allocatable :: s') > 0) then
            print *, "PASS: Empty to non-empty string generates allocatable declaration"
        else
            print *, "FAIL: Empty to non-empty test failed"
            print *, "Generated:", result
            error stop "Test failed: empty_to_nonempty"
        end if
    end subroutine test_empty_to_nonempty_string

    subroutine test_single_char_to_long_string
        ! Test extreme length change from single character to very long string
        character(len=:), allocatable :: source, result, error_msg
        
        source = 's = "x"' // new_line('a') // &
                 's = "this_is_a_very_long_string_that_is_much_longer_than_one_character"'
        
        call transform_lazy_fortran_string(source, result, error_msg)
        
        if (index(result, 'character(len=:), allocatable :: s') > 0) then
            print *, "PASS: Single char to long string generates allocatable declaration"
        else
            print *, "FAIL: Single char to long string test failed"
            print *, "Generated:", result
            error stop "Test failed: single_to_long"
        end if
    end subroutine test_single_char_to_long_string

end program test_string_length_inference