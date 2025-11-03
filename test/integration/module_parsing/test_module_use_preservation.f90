program test_module_use_preservation
    use transformation_api, only: transform_lazy_fortran_string
    use, intrinsic :: iso_fortran_env, only: &
        error_unit, input_unit, iostat_end, iostat_eor
    implicit none

    logical :: all_passed

    all_passed = .true.

    if (.not. verify_interface_block_uses()) all_passed = .false.

    if (all_passed) then
        stop 0
    else
        stop 1
    end if

contains

    include '../../common/cli_io_reader.inc'

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

    logical function verify_interface_block_uses()
        character(len=:), allocatable :: input, output, error_msg
        character(len=:), allocatable :: forbidden_pattern

        call read_example('examples/f90/module_interface_block.f90', input)

        call transform_lazy_fortran_string(input, output, error_msg)

        if (len_trim(error_msg) > 0) then
            verify_interface_block_uses = .false.
            return
        end if

        if (index(output, &
                  'use, intrinsic :: iso_fortran_env, only: dp => real64') == 0) then
            verify_interface_block_uses = .false.
            return
        end if

        forbidden_pattern = new_line('A') // '    dp => real64'
        if (index(output, forbidden_pattern) > 0) then
            verify_interface_block_uses = .false.
            return
        end if

        verify_interface_block_uses = .true.
    end function verify_interface_block_uses

end program test_module_use_preservation
