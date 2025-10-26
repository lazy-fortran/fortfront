program test_monomorphization_three_types
    use, intrinsic :: iso_fortran_env, only: error_unit
    use fortfront, only: transform_lazy_fortran_string
    implicit none
    character(len=:), allocatable :: input, output, error_msg
    character(len=*), parameter :: tmp_file = 'fortfront_mono_three.f90'
    character(len=*), parameter :: compile_cmd = 'gfortran -fsyntax-only '
    integer :: exit_code, unit

    input = 'function add(a, b)' // new_line('A') // &
            '    add = a + b' // new_line('A') // &
            'end function' // new_line('A') // &
            '' // new_line('A') // &
            'x = add(5, 3)' // new_line('A') // &
            'y = add(2.5d0, 1.5d0)' // new_line('A') // &
            'z = add((1.0, 2.0), (3.0, 4.0))'

    call transform_lazy_fortran_string(input, output, error_msg)

    if (len_trim(error_msg) > 0) then
        write (error_unit, '(A)') &
            'three_types: unexpected error ' // trim(error_msg)
        error stop 1
    end if

    call assert_contains(output, 'add__i32_i32', &
        'missing integer specialization in three type test')
    call assert_contains(output, 'add__r64_r64', &
        'missing real specialization in three type test')
    call assert_contains(output, 'add__c64_c64', &
        'missing complex specialization in three type test')

    open (newunit=unit, file=tmp_file, status='replace', action='write')
    write (unit, '(A)') trim(output)
    close (unit)

    call execute_command_line(compile_cmd // tmp_file, exitstat=exit_code, &
                              wait=.true.)
    if (exit_code /= 0) then
        write (error_unit, '(A)') 'gfortran rejected multi-type output'
        call execute_command_line('rm -f ' // tmp_file, wait=.true.)
        error stop 1
    end if

    call execute_command_line('rm -f ' // tmp_file, wait=.true.)

contains

    subroutine assert_contains(text, token, message)
        character(len=*), intent(in) :: text
        character(len=*), intent(in) :: token
        character(len=*), intent(in) :: message

        if (index(text, token) <= 0) then
            write (error_unit, '(A)') trim(message)
            error stop 1
        end if
    end subroutine assert_contains

end program test_monomorphization_three_types
