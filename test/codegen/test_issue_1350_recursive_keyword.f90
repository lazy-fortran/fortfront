program test_issue_1350_recursive_keyword
    ! Regression tests for GitHub issue #1350:
    !  - Recursive keyword dropped from function declarations
    !  - ELSE branch bodies removed from simple IF statements
    use fortfront
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg

    print *, "=== Issue #1350: recursive keyword and IF/ELSE preservation ==="

    call run_recursive_if_test()

    print *, "All issue #1350 tests completed"

contains

    subroutine run_recursive_if_test()
        logical :: ok

        call read_example('examples/f90/issue_1350_recursive_if_else.f90', source)

        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "  ERROR: ", trim(error_msg)
                stop 1
            end if
        end if

        ok = index(output, "recursive") > 0 .and. &
             index(output, "function factorial") > 0
        if (.not. ok) then
            print *, "  FAIL: recursive keyword missing"
            print *, "  Output: ", trim(output)
            stop 1
        end if

        ok = index(output, "else"//new_line('a')) > 0
        if (.not. ok) then
            print *, "  FAIL: else block missing from output"
            print *, "  Output: ", trim(output)
            stop 1
        end if

        ok = index(output, "if (!ERROR") == 0
        if (.not. ok) then
            print *, "  FAIL: fallback parser emitted unexpected placeholder"
            print *, "  Output: ", trim(output)
            stop 1
        end if

        ok = index(output, "f = n * factorial") > 0 .or. &
             index(output, "f = n*factorial") > 0
        if (.not. ok) then
            print *, "  FAIL: recursive assignment missing from ELSE branch"
            print *, "  Output: ", trim(output)
            stop 1
        end if

        print *, "  PASS: recursive keyword and IF/ELSE preserved"
    end subroutine run_recursive_if_test

    subroutine read_example(filepath, content)
        character(len=*), intent(in) :: filepath
        character(len=:), allocatable, intent(out) :: content
        integer :: unit, file_size, stat
        character(len=1), allocatable :: buffer(:)

        open (newunit=unit, file=filepath, status='old', access='stream', &
              form='unformatted', iostat=stat)
        if (stat /= 0) error stop 'Failed to open example file: ' // filepath

        inquire (unit=unit, size=file_size)
        allocate (buffer(file_size))
        read (unit, iostat=stat) buffer
        if (stat /= 0) error stop 'Failed to read example file: ' // filepath
        close (unit)

        allocate (character(len=file_size) :: content)
        content = transfer(buffer, content)
    end subroutine read_example

end program test_issue_1350_recursive_keyword

