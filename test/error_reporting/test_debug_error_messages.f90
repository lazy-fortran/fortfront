program test_debug_error_messages
    use transformation_api, only: transform_lazy_fortran_string
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, iostat_eor

    character(len=:), allocatable :: source, output, error_msg

    print *, '=== Debugging Current Error Message Behavior ==='
    print *

    ! Test 1: Invalid syntax - missing 'then'
    print *, 'Test 1: Missing "then" in if statement'
    call read_example('examples/f90/debug_error_missing_then.f90', source)

    call transform_lazy_fortran_string(source, output, error_msg)
    print *, 'Error message: "' // error_msg // '"'
    print *, 'Output length:', len_trim(output)
    if (len_trim(output) > 0) then
        print *, 'Output: "' // output // '"'
    end if
    print *

    ! Test 2: Parameter declaration
    print *, 'Test 2: Parameter declaration'
    call read_example('examples/f90/debug_error_parameter_decl.f90', source)

    call transform_lazy_fortran_string(source, output, error_msg)
    print *, 'Error message: "' // error_msg // '"'
    print *, 'Output length:', len_trim(output)
    if (len_trim(output) > 0) then
        print *, 'Output: "' // output // '"'
    end if
    print *

    ! Test 3: Incomplete expression
    print *, 'Test 3: Incomplete expression (trailing operator)'
    call read_example('examples/f90/debug_error_incomplete_expr.f90', source)

    call transform_lazy_fortran_string(source, output, error_msg)
    print *, 'Error message: "' // error_msg // '"'
    print *, 'Output length:', len_trim(output)
    if (len_trim(output) > 0) then
        print *, 'Output: "' // output // '"'
    end if
    print *

    ! Test 4: Complete garbage
    print *, 'Test 4: Complete garbage input'
    call read_example('examples/f90/debug_error_garbage.f90', source)

    call transform_lazy_fortran_string(source, output, error_msg)
    print *, 'Error message: "' // error_msg // '"'
    print *, 'Output length:', len_trim(output)
    if (len_trim(output) > 0) then
        print *, 'Output: "' // output // '"'
    end if
    print *

    ! Test 5: Missing end program
    print *, 'Test 5: Missing end program'
    call read_example('examples/f90/debug_error_missing_end.f90', source)

    call transform_lazy_fortran_string(source, output, error_msg)
    print *, 'Error message: "' // error_msg // '"'
    print *, 'Output length:', len_trim(output)
    if (len_trim(output) > 0) then
        print *, 'Output: "' // output // '"'
    end if
    print *

contains

    include '../common/cli_io_reader.inc'

    subroutine read_example(path, content)
        character(len=*), intent(in) :: path
        character(len=:), allocatable, intent(out) :: content
        integer :: status

        call read_all_stdin_or_file(.true., path, content, status)
        if (status /= 0) then
            write (error_unit, '(A)') 'FAIL: failed to read ' // trim(path)
            error stop 1
        end if
    end subroutine read_example

end program test_debug_error_messages
