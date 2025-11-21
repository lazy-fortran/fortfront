program test_data_keyword_variable
    use, intrinsic :: iso_fortran_env, only: error_unit
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source, output, error_msg

    call read_example('examples/f90/issue_2419_data_variable_in_allocate.f90', &
                      source)
    call transform_lazy_fortran_string(source, output, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            write (error_unit, '(a)') &
                'FAIL: parser rejected allocate with data identifier'
            write (error_unit, '(a)') trim(error_msg)
            error stop 1
        end if
    end if

    if (.not. allocated(output)) then
        write (error_unit, '(a)') 'FAIL: transformation produced no output'
        error stop 1
    end if

    if (len_trim(output) == 0) then
        write (error_unit, '(a)') 'FAIL: transformation produced empty output'
        error stop 1
    end if

    print *, 'PASS: allocate statement with data identifier parsed'

contains

    subroutine read_example(filepath, content)
        character(len=*), intent(in) :: filepath
        character(len=:), allocatable, intent(out) :: content
        integer :: unit, stat
        character(len=4096) :: buffer

        open (newunit=unit, file=filepath, status='old', action='read', &
              iostat=stat)
        if (stat /= 0) then
            write (error_unit, '(a)') 'FAIL: unable to open example file'
            error stop 1
        end if

        content = ''
        do
            read (unit, '(A)', iostat=stat) buffer
            if (stat /= 0) exit
            if (len_trim(content) > 0) content = content // new_line('a')
            content = content // trim(buffer)
        end do
        close (unit)
    end subroutine read_example
end program test_data_keyword_variable
