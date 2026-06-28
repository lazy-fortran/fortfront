program test_issue_1568_where_lazy_syntax
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Issue #1568: WHERE construct for lazy syntax ==='

    if (.not. test_lazy_where_single_line()) all_passed = .false.

    print *
    if (all_passed) then
        print *, 'Issue #1568 fixed!'
    else
        print *, 'Issue #1568 regression detected!'
        stop 1
    end if

contains

    logical function test_lazy_where_single_line()
        character(len=:), allocatable :: source
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg
        logical :: has_where
        logical :: has_body
        logical :: has_print

        test_lazy_where_single_line = .true.
        print *, 'Testing lazy WHERE with single-line syntax...'

        source = '! Lazy WHERE syntax' // new_line('a') // &
            'arr = [1, -2, 3, -4, 5]' // new_line('a') // &
            'where arr < 0: arr = 0' // new_line('a') // &
            'print *, arr'

        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: Unexpected error -', trim(error_msg)
                test_lazy_where_single_line = .false.
                return
            end if
        end if

        if (.not. allocated(output)) then
            print *, '  FAIL: No output generated'
            test_lazy_where_single_line = .false.
            return
        end if

        has_where = index(output, 'where (arr < 0)') > 0
        has_body = index(output, 'arr = 0') > 0
        has_print = index(output, 'print *, arr') > 0

        if (.not. has_where) then
            print *, '  FAIL: WHERE construct missing in output'
            test_lazy_where_single_line = .false.
        end if

        if (.not. has_body) then
            print *, '  FAIL: WHERE body missing assignment'
            test_lazy_where_single_line = .false.
        end if

        if (.not. has_print) then
            print *, '  FAIL: Statement following WHERE removed'
            test_lazy_where_single_line = .false.
        end if

        if (test_lazy_where_single_line) then
            print *, '  PASS: Single-line lazy WHERE preserved'
        end if
    end function test_lazy_where_single_line

end program test_issue_1568_where_lazy_syntax
