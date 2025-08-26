module test_issue_495_undefined_variables
    use iso_fortran_env, only: stdout => output_unit
    use frontend, only: compile_source, compilation_options_t
    implicit none

contains

    subroutine run_tests()
        call test_issue_495_original_reproduction()
        call test_undefined_in_if_statement()
        call test_undefined_in_do_loop()
        call test_undefined_in_where_construct()
        call test_undefined_in_select_case()
        call test_undefined_in_associate()
        call test_defined_variables_pass()
        print *, "All Issue #495 tests passed!"
    end subroutine run_tests

    subroutine test_issue_495_original_reproduction()
        ! Test the exact reproduction case from Issue #495
        type(compilation_options_t) :: options
        character(len=512) :: error_msg
        character(len=:), allocatable :: source
        
        print *, "Testing: Issue #495 original reproduction case"
        
        source = 'program test' // new_line('a') // &
                 '    real :: x' // new_line('a') // &
                 '    y = x + 1  ! y is undefined' // new_line('a') // &
                 'end program'
        
        ! Create temporary file
        call write_temp_file("test_495.f90", source)
        
        ! Try to compile - should fail with semantic error
        call compile_source("test_495.f90", options, error_msg)
        
        if (error_msg == "") then
            error stop "FAIL: Issue #495 - undefined variable 'y' not detected"
        end if
        
        if (index(error_msg, "Semantic") == 0 .and. &
            index(error_msg, "undefined") == 0) then
            error stop "FAIL: Issue #495 - wrong error message"
        end if
        
        print *, "  PASS: Undefined variable 'y' detected"
    end subroutine test_issue_495_original_reproduction

    subroutine test_undefined_in_if_statement()
        type(compilation_options_t) :: options
        character(len=512) :: error_msg
        character(len=:), allocatable :: source
        
        print *, "Testing: Undefined variable in if statement"
        
        source = 'program test' // new_line('a') // &
                 '    real :: x' // new_line('a') // &
                 '    x = 1.0' // new_line('a') // &
                 '    if (x > 0) then' // new_line('a') // &
                 '        y = x + 1  ! y is undefined' // new_line('a') // &
                 '    end if' // new_line('a') // &
                 'end program'
        
        call write_temp_file("test_if.f90", source)
        call compile_source("test_if.f90", options, error_msg)
        
        if (error_msg == "") then
            error stop "FAIL: Undefined variable in if statement not detected"
        end if
        
        print *, "  PASS: Undefined variable in if detected"
    end subroutine test_undefined_in_if_statement

    subroutine test_undefined_in_do_loop()
        type(compilation_options_t) :: options
        character(len=512) :: error_msg
        character(len=:), allocatable :: source
        
        print *, "Testing: Undefined variable in do loop"
        
        source = 'program test' // new_line('a') // &
                 '    integer :: i' // new_line('a') // &
                 '    do i = 1, 10' // new_line('a') // &
                 '        x = i + j  ! j is undefined' // new_line('a') // &
                 '    end do' // new_line('a') // &
                 'end program'
        
        call write_temp_file("test_do.f90", source)
        call compile_source("test_do.f90", options, error_msg)
        
        if (error_msg == "") then
            error stop "FAIL: Undefined variable in do loop not detected"
        end if
        
        print *, "  PASS: Undefined variable in do loop detected"
    end subroutine test_undefined_in_do_loop

    subroutine test_undefined_in_where_construct()
        type(compilation_options_t) :: options
        character(len=512) :: error_msg
        character(len=:), allocatable :: source
        
        print *, "Testing: Undefined variable in where construct"
        
        source = 'program test' // new_line('a') // &
                 '    real :: a(10)' // new_line('a') // &
                 '    a = 1.0' // new_line('a') // &
                 '    where (a > 0)' // new_line('a') // &
                 '        b = a * 2  ! b is undefined' // new_line('a') // &
                 '    end where' // new_line('a') // &
                 'end program'
        
        call write_temp_file("test_where.f90", source)
        call compile_source("test_where.f90", options, error_msg)
        
        if (error_msg == "") then
            error stop "FAIL: Undefined variable in where construct not detected"
        end if
        
        print *, "  PASS: Undefined variable in where detected"
    end subroutine test_undefined_in_where_construct

    subroutine test_undefined_in_select_case()
        type(compilation_options_t) :: options
        character(len=512) :: error_msg
        character(len=:), allocatable :: source
        
        print *, "Testing: Undefined variable in select case"
        
        source = 'program test' // new_line('a') // &
                 '    integer :: i' // new_line('a') // &
                 '    i = 1' // new_line('a') // &
                 '    select case (i)' // new_line('a') // &
                 '    case (1)' // new_line('a') // &
                 '        x = y + 1  ! y is undefined' // new_line('a') // &
                 '    end select' // new_line('a') // &
                 'end program'
        
        call write_temp_file("test_select.f90", source)
        call compile_source("test_select.f90", options, error_msg)
        
        if (error_msg == "") then
            error stop "FAIL: Undefined variable in select case not detected"
        end if
        
        print *, "  PASS: Undefined variable in select case detected"
    end subroutine test_undefined_in_select_case

    subroutine test_undefined_in_associate()
        type(compilation_options_t) :: options
        character(len=512) :: error_msg
        character(len=:), allocatable :: source
        
        print *, "Testing: Undefined variable in associate"
        
        source = 'program test' // new_line('a') // &
                 '    real :: x' // new_line('a') // &
                 '    x = 1.0' // new_line('a') // &
                 '    associate (a => x)' // new_line('a') // &
                 '        b = a + c  ! c is undefined' // new_line('a') // &
                 '    end associate' // new_line('a') // &
                 'end program'
        
        call write_temp_file("test_assoc.f90", source)
        call compile_source("test_assoc.f90", options, error_msg)
        
        if (error_msg == "") then
            error stop "FAIL: Undefined variable in associate not detected"
        end if
        
        print *, "  PASS: Undefined variable in associate detected"
    end subroutine test_undefined_in_associate

    subroutine test_defined_variables_pass()
        type(compilation_options_t) :: options
        character(len=512) :: error_msg
        character(len=:), allocatable :: source
        
        print *, "Testing: Properly defined variables should compile"
        
        source = 'program test' // new_line('a') // &
                 '    real :: x, y, z' // new_line('a') // &
                 '    x = 1.0' // new_line('a') // &
                 '    y = 2.0' // new_line('a') // &
                 '    z = x + y' // new_line('a') // &
                 '    if (z > 0) then' // new_line('a') // &
                 '        x = z * 2' // new_line('a') // &
                 '    end if' // new_line('a') // &
                 'end program'
        
        call write_temp_file("test_valid.f90", source)
        call compile_source("test_valid.f90", options, error_msg)
        
        if (error_msg /= "") then
            print *, "Error message: ", trim(error_msg)
            error stop "FAIL: Valid program incorrectly reported errors"
        end if
        
        print *, "  PASS: Valid program compiled successfully"
    end subroutine test_defined_variables_pass

    subroutine write_temp_file(filename, content)
        character(len=*), intent(in) :: filename, content
        integer :: unit
        
        open(newunit=unit, file=filename, status='replace', action='write')
        write(unit, '(a)') content
        close(unit)
    end subroutine write_temp_file

end module test_issue_495_undefined_variables

program test_runner
    use test_issue_495_undefined_variables
    call run_tests()
end program test_runner