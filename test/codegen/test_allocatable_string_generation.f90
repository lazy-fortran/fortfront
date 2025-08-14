program test_allocatable_string_generation
    use frontend, only: transform_lazy_fortran_string
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Allocatable String Code Generation Tests ==='
    print *

    ! Test code generation for allocatable strings
    if (.not. test_end_to_end_compilation()) all_passed = .false.
    if (.not. test_single_string_codegen()) all_passed = .false.

    ! Report results
    print *
    if (all_passed) then
        print *, 'All allocatable string codegen tests passed!'
        stop 0
    else
        print *, 'Some allocatable string codegen tests failed!'
        stop 1
    end if

contains

    logical function test_single_string_codegen()
        test_single_string_codegen = .true.
        print *, 'Testing single string code generation...'

        block
            character(len=*), parameter :: test_code = 's = "hello"'
            character(len=:), allocatable :: result, error_msg
            
            ! Use the main compilation interface
            call transform_lazy_fortran_string(test_code, result, error_msg)
            
            if (len_trim(error_msg) > 0) then
                print *, 'FAIL: Compilation error:', error_msg
                test_single_string_codegen = .false.
                return
            end if
            
            ! Single assignment should generate fixed-length character declaration
            if (index(result, 'character(len=5)') == 0) then
                print *, 'FAIL: Single string should generate fixed-length declaration'
                print *, 'Result:'
                print *, result
                test_single_string_codegen = .false.
                return
            end if

            ! Should NOT contain allocatable for single assignment
            if (index(result, 'allocatable') > 0) then
                print *, 'FAIL: Single string should not be allocatable'
                print *, 'Result:'
                print *, result
                test_single_string_codegen = .false.
                return
            end if

            print *, 'PASS: Single string code generation correct'
        end block
    end function test_single_string_codegen

    logical function test_end_to_end_compilation()
        test_end_to_end_compilation = .true.
        print *, 'Testing end-to-end compilation...'

        block
            character(len=*), parameter :: test_code = &
                's = "a"' // new_line('a') // &
                'print*,s' // new_line('a') // &
                's = "ab"' // new_line('a') // &
                'print*,s'
            character(len=:), allocatable :: result, error_msg
            
            ! Use the main compilation interface
            call transform_lazy_fortran_string(test_code, result, error_msg)
            
            if (len_trim(error_msg) > 0) then
                print *, 'FAIL: Compilation error:', error_msg
                test_end_to_end_compilation = .false.
                return
            end if
            
            ! Check the result contains proper allocatable declaration
            if (index(result, 'character(len=:), allocatable :: s') == 0) then
                print *, 'FAIL: End-to-end compilation does not generate allocatable string'
                print *, 'Result:'
                print *, result
                test_end_to_end_compilation = .false.
                return
            end if

            print *, 'PASS: End-to-end compilation correct'
        end block
    end function test_end_to_end_compilation

end program test_allocatable_string_generation