program test_program_scaffold_roundtrip
    use transformation_api, only: transform_lazy_fortran_string
    implicit none
    character(len=:), allocatable :: input, output, error_msg
    logical :: test_passed

    test_passed = .true.

    call test_preserves_program_scaffold()
    call test_preserves_implicit_none()
    call test_preserves_program_name()

    if (test_passed) then
        print *, "test_program_scaffold_roundtrip PASSED"
    else
        print *, "test_program_scaffold_roundtrip FAILED"
        error stop 1
    end if

contains

    subroutine test_preserves_program_scaffold()
        call read_example('examples/f90/program_scaffold_preserve.f90', input)
        call transform_lazy_fortran_string(input, output, error_msg)

        if (len_trim(error_msg) > 0) then
            print *, "ERROR: Transformation failed:", trim(error_msg)
            test_passed = .false.
            return
        end if

        if (index(output, 'program scaffold_test') == 0) then
            print *, "FAIL: program declaration was dropped"
            test_passed = .false.
        end if

        if (index(output, 'implicit none') == 0) then
            print *, "FAIL: implicit none was dropped"
            test_passed = .false.
        end if

        if (index(output, 'end program scaffold_test') == 0) then
            print *, "FAIL: end program was dropped or name changed"
            test_passed = .false.
        end if
    end subroutine test_preserves_program_scaffold

    subroutine test_preserves_implicit_none()
        input = 'program test' // new_line('a') // &
                '  implicit none' // new_line('a') // &
                '  integer :: x' // new_line('a') // &
                '  x = 5' // new_line('a') // &
                'end program test'

        call transform_lazy_fortran_string(input, output, error_msg)

        if (len_trim(error_msg) > 0) then
            print *, "ERROR: Transformation failed:", trim(error_msg)
            test_passed = .false.
            return
        end if

        if (index(output, 'implicit none') == 0) then
            print *, "FAIL: implicit none was dropped from inline test"
            test_passed = .false.
        end if

        if (index(output, 'program test') == 0) then
            print *, "FAIL: program declaration was dropped from inline test"
            test_passed = .false.
        end if
    end subroutine test_preserves_implicit_none

    subroutine test_preserves_program_name()
        input = 'program my_special_name' // new_line('a') // &
                '  implicit none' // new_line('a') // &
                '  print *, 42' // new_line('a') // &
                'end program my_special_name'

        call transform_lazy_fortran_string(input, output, error_msg)

        if (len_trim(error_msg) > 0) then
            print *, "ERROR: Transformation failed:", trim(error_msg)
            test_passed = .false.
            return
        end if

        if (index(output, 'program my_special_name') == 0) then
            print *, "FAIL: Program name was changed or dropped"
            test_passed = .false.
        end if

        if (index(output, 'end program my_special_name') == 0) then
            print *, "FAIL: End program name was changed or dropped"
            test_passed = .false.
        end if
    end subroutine test_preserves_program_name

    subroutine read_example(filepath, content)
        character(len=*), intent(in) :: filepath
        character(len=:), allocatable, intent(out) :: content
        integer :: unit, ios
        character(len=10000) :: line
        character(len=:), allocatable :: temp

        content = ""
        open (newunit=unit, file=filepath, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            print *, "ERROR: Failed to open example file:", filepath
            error stop 1
        end if

        do
            read (unit, '(A)', iostat=ios) line
            if (ios /= 0) exit
            if (len(content) > 0) content = content // new_line('a')
            content = content // trim(line)
        end do

        close (unit)
    end subroutine read_example

end program test_program_scaffold_roundtrip
