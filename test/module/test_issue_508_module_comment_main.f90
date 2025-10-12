program test_issue_508_module_comment_main
    use frontend, only: transform_lazy_fortran_string
    implicit none

    logical :: passed
    passed = .true.

    call run_test()

    if (passed) then
        print *, 'SUCCESS: Module-level comment preserved with main entry'
        stop 0
    else
        print *, 'FAILURE: Module-level comment handling broken'
        stop 1
    end if

contains

    subroutine run_test()
        character(len=*), parameter :: input = &
                                       '! Top-level comment about the module' // new_line('a') // &
                                       'module m' // new_line('a') // &
                                       'contains' // new_line('a') // &
                                       '  subroutine main()' // new_line('a') // &
                                       "    print *, 'ok'" // new_line('a') // &
                                       '  end subroutine main' // new_line('a') // &
                                       'end module m'

        character(len=:), allocatable :: output, error_msg

        call transform_lazy_fortran_string(input, output, error_msg)

        if (len_trim(error_msg) > 0) then
            print *, '  FAIL: Compilation error - ', trim(error_msg)
            passed = .false.
            return
        end if

        ! Verify module and main are present
        if (index(output, 'module m') <= 0) then
            print *, '  FAIL: module header missing in output'
            print *, trim(output)
            passed = .false.
        end if

        if (index(output, 'subroutine main') <= 0) then
            print *, '  FAIL: main subroutine missing in output'
            print *, trim(output)
            passed = .false.
        end if

        ! Verify top-level comment is preserved (at least once)
        if (index(output, '! Top-level comment about the module') <= 0) then
            print *, '  FAIL: module-level comment not preserved'
            print *, trim(output)
            passed = .false.
        else
            print *, '  PASS: module-level comment preserved'
        end if
    end subroutine run_test

end program test_issue_508_module_comment_main

