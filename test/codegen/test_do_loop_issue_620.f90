program test_do_loop_issue_620
    ! Test that do loops generate proper code, not TODO placeholders (Issue #620)
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
        iostat_eor
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source, output, error_msg
    logical :: found_do, found_todo

    print *, "=== Testing Do Loop Code Generation (Issue #620) ==="

    call read_example('examples/f90/do_loop_issue_620.f90', source)

    call transform_lazy_fortran_string(source, output, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, 'FAIL: Compilation error:', trim(error_msg)
            stop 1
        end if
    end if

    found_do = .false.
    found_todo = .false.

    if (index(output, 'do i = 1, 3') > 0 .or. index(output, 'do i=1,3') > 0) then
        found_do = .true.
        print *, 'Found do loop in output'
    end if

    if (index(output, 'TODO') > 0 .and. index(output, 'codegen') > 0) then
        found_todo = .true.
        print *, 'ERROR - Found TODO placeholder in output'
    end if

    if (found_todo) then
        print *, 'FAIL: TODO placeholder found - Issue #620 NOT FIXED'
        stop 1
    else if (.not. found_do) then
        print *, 'FAIL: Do loop not generated properly'
        stop 1
    else
        print *, 'PASS: Do loop generates proper code - Issue #620 FIXED!'
        stop 0
    end if


contains


    include '../common/read_example.inc'
end program test_do_loop_issue_620
