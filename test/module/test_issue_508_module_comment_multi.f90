program test_issue_508_module_comment_multi
    use frontend, only: transform_lazy_fortran_string
    implicit none

    logical :: passed
    passed = .true.

    call run_test()

    if (passed) then
        print *, 'SUCCESS: Multi-line module comments preserved without duplication'
        stop 0
    else
        print *, 'FAILURE: Multi-line module comment handling broken'
        stop 1
    end if

contains

    subroutine run_test()
        character(len=*), parameter :: c1 = '! First top comment line'
        character(len=*), parameter :: c2 = '! Second top comment line'
        character(len=*), parameter :: input = &
            c1 // new_line('a') // &
            c2 // new_line('a') // &
            ''  // new_line('a') // &
            'module mm' // new_line('a') // &
            'contains' // new_line('a') // &
            '  subroutine main()' // new_line('a') // &
            "    print *, 'ok'" // new_line('a') // &
            '  end subroutine main' // new_line('a') // &
            'end module mm'

        character(len=:), allocatable :: output, error_msg
        integer :: p1, p2

        call transform_lazy_fortran_string(input, output, error_msg)

        if (len_trim(error_msg) > 0) then
            print *, '  FAIL: Compilation error - ', trim(error_msg)
            passed = .false.
            return
        end if

        ! Output should start with a comment line
        p1 = 1
        do while (p1 <= len(output) .and. (output(p1:p1) == ' ' .or. &
                output(p1:p1) == new_line('a') .or. &
                iachar(output(p1:p1)) == 9))
            p1 = p1 + 1
        end do
        if (p1 > len(output) .or. output(p1:p1) /= '!') then
            print *, '  FAIL: output does not start with a comment'
            print *, trim(output)
            passed = .false.
        end if

        ! Each unique comment should appear exactly once (no duplication)
        p1 = index(output, c1)
        if (p1 <= 0) then
            print *, '  FAIL: first comment missing'
            print *, trim(output)
            passed = .false.
        else
            p2 = index(output(p1+len(c1):), c1)
            if (p2 > 0) then
                print *, '  FAIL: first comment duplicated'
                print *, trim(output)
                passed = .false.
            end if
        end if

        p1 = index(output, c2)
        if (p1 <= 0) then
            print *, '  FAIL: second comment missing'
            print *, trim(output)
            passed = .false.
        else
            p2 = index(output(p1+len(c2):), c2)
            if (p2 > 0) then
                print *, '  FAIL: second comment duplicated'
                print *, trim(output)
                passed = .false.
            end if
        end if

        if (passed) then
            print *, '  PASS: multi-line comments preserved once at top'
        end if
    end subroutine run_test

end program test_issue_508_module_comment_multi

