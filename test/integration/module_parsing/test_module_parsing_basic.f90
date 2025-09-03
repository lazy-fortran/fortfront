program test_module_parsing_basic
    use frontend, only: transform_lazy_fortran_string
    implicit none

    logical :: all_passed
    all_passed = .true.

    if (.not. test_single_module_not_wrapped()) all_passed = .false.

    if (all_passed) then
        stop 0
    else
        stop 1
    end if

contains

    logical function test_single_module_not_wrapped()
        character(len=*), parameter :: input = &
            'module m' // new_line('a') // &
            '  implicit none' // new_line('a') // &
            'contains' // new_line('a') // &
            '  function add(a,b) result(c)' // new_line('a') // &
            '    integer :: a,b,c' // new_line('a') // &
            '    c = a + b' // new_line('a') // &
            '  end function add' // new_line('a') // &
            'end module m'
        character(len=:), allocatable :: output, error_msg

        call transform_lazy_fortran_string(input, output, error_msg)

        ! Add diagnostic output for CI debugging
        write(*,*) 'DEBUG: error_msg=', trim(error_msg)
        write(*,*) 'DEBUG: has program main=', index(output, 'program main') > 0
        write(*,*) 'DEBUG: has module m=', index(output, 'module m') > 0
        write(*,*) 'DEBUG: has function add=', index(output, 'function add') > 0
        if (index(output, 'program main') > 0) then
            write(*,*) 'DEBUG: Full output:'
            write(*,*) output
        end if

        if (len_trim(error_msg) > 0) then
            test_single_module_not_wrapped = .false.
            return
        end if

        if (index(output, 'program main') > 0) then
            test_single_module_not_wrapped = .false.
            return
        end if

        if (index(output, 'module m') == 0) then
            test_single_module_not_wrapped = .false.
            return
        end if

        if (index(output, 'function add') == 0) then
            test_single_module_not_wrapped = .false.
            return
        end if

        test_single_module_not_wrapped = .true.
    end function test_single_module_not_wrapped

end program test_module_parsing_basic

