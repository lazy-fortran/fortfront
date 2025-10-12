program test_module_implicit_none_insertion
    use frontend, only: transform_lazy_fortran_string
    implicit none

    logical :: all_passed
    all_passed = .true.

    if (.not. test_module_adds_implicit_none()) all_passed = .false.

    if (all_passed) then
        stop 0
    else
        stop 1
    end if

contains

    logical function test_module_adds_implicit_none()
        character(len=*), parameter :: input = &
                                       'module m' // new_line('a') // &
                                       'contains' // new_line('a') // &
                                       '  subroutine s()' // new_line('a') // &
                                       '    integer :: x' // new_line('a') // &
                                       '    x = 1' // new_line('a') // &
                                       '  end subroutine s' // new_line('a') // &
                                       'end module m'
        character(len=:), allocatable :: output, error_msg

        call transform_lazy_fortran_string(input, output, error_msg)

        if (len_trim(error_msg) > 0) then
            test_module_adds_implicit_none = .false.
            return
        end if

        if (index(output, 'module m') == 0) then
            test_module_adds_implicit_none = .false.
            return
        end if

        if (index(output, 'implicit none') == 0) then
            test_module_adds_implicit_none = .false.
            return
        end if

        test_module_adds_implicit_none = .true.
    end function test_module_adds_implicit_none

end program test_module_implicit_none_insertion

