program test_issue_2928_interface_in_module_proc
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source, output, error_msg
    logical :: all_passed

    all_passed = .true.

    source = 'module m2'//new_line('a')// &
             'contains'//new_line('a')// &
             '   subroutine run()'//new_line('a')// &
             '      interface'//new_line('a')// &
             '         integer function g()'//new_line('a')// &
             '         end function g'//new_line('a')// &
             '      end interface'//new_line('a')// &
             '      print *, 5'//new_line('a')// &
             '   end subroutine run'//new_line('a')// &
             'end module m2'//new_line('a')// &
             'program p2'//new_line('a')// &
             '   use m2'//new_line('a')// &
             '   call run()'//new_line('a')// &
             'end program p2'//new_line('a')

    call transform_lazy_fortran_string(source, output, error_msg)

    if (len_trim(error_msg) > 0) then
        print *, "FAIL: transform error: ", trim(error_msg)
        all_passed = .false.
    end if

    if (index(output, 'module m2') == 0) then
        print *, "FAIL: module m2 missing from output"
        all_passed = .false.
    end if

    if (index(output, 'program p2') == 0) then
        print *, "FAIL: program p2 unit dropped from output"
        print *, "Output was:"
        print *, output
        all_passed = .false.
    end if

    if (all_passed) then
        print *, "PASS: issue 2928 interface in module procedure body"
    else
        stop 1
    end if

end program test_issue_2928_interface_in_module_proc
